/*
 * Copyright (c) 2020 NVI, Inc.
 *
 * This file is part of VLBI Field System
 * (see http://github.com/nvi-inc/fs).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <nng/nng.h>
#include <nng/protocol/pubsub0/pub.h>
#include <nng/protocol/reqrep0/rep.h>
#include <nng/protocol/reqrep0/req.h>
#include <nng/supplemental/util/platform.h>

#include "list.h"
#include "msg.h"
#include "stream.h"

#define fatal(msg, rv)                                                                             \
	do {                                                                                       \
		fprintf(stderr, "%s:%d (%s) error %s: %s\n", __FILE__, __LINE__, __FUNCTION__,     \
		        msg, nng_strerror(rv));                                                    \
		exit(1);                                                                           \
	} while (0)

pthread_mutex_t init_lock = PTHREAD_MUTEX_INITIALIZER;
bool inited               = 0;

struct _reaper {
	nng_mtx *mtx;
	nng_cv *cv;
	list_t *list;
	pthread_t thread;
} reaper;

void *reaper_thread(void *arg) {
	// don't fear the reaper
	if (arg != NULL) {
		fatal("unexpected arg", 0);
	}

	nng_mtx_lock(reaper.mtx);
	while (reaper.list == NULL) {
		nng_cv_wait(reaper.cv);
	}

	nng_aio *aio;
	while ((aio = list_pop(&reaper.list, NULL, NULL)) != NULL) {
		nng_aio_free(aio);
	}

	nng_mtx_unlock(reaper.mtx);
	return NULL;
}

static void init() {
	int rv;
	if (inited) {
		return;
	}

	pthread_mutex_lock(&init_lock);
	if (inited) { // check again under the lock to be sure
		pthread_mutex_unlock(&init_lock);
		return;
	}

	if ((rv = nng_mtx_alloc(&reaper.mtx)) != 0) {
		fatal("init mtx", rv);
	}

	if ((rv = nng_cv_alloc(&reaper.cv, reaper.mtx)) != 0) {
		fatal("init cv", rv);
	}

	if (pthread_create(&reaper.thread, NULL, reaper_thread, NULL) < 0) {
		fatal("creating reaper thread", errno);
	}
	pthread_mutex_unlock(&init_lock);
}

static void async_free(nng_aio *aio) {
	nng_mtx_lock(reaper.mtx);
	list_push(&reaper.list, aio);
	nng_cv_wake(reaper.cv);
	nng_mtx_unlock(reaper.mtx);
}

struct buffered_stream {
	uint64_t seq; // Next message sequence ID to send
	bool shutting_down;

	nng_aio *rep_aio;
	nng_aio *heartbeat_aio;
	nng_aio *shutdown_aio;
	nng_mtx *mtx;

	int heartbeat_millis;
	int shutdown_millis;
	int shutdown_heartbeat_millis;

	size_t msg_buffer_len;
	msg_t *msg_buffer; // Ring buffer of messages

	nng_socket pub;
	nng_socket rep;

	pthread_t dup_thread;
};

void buffered_stream_set_heartbeat(buffered_stream_t *s, int heartbeat_millis) {
	nng_mtx_lock(s->mtx);
	s->heartbeat_millis = heartbeat_millis;
	nng_mtx_unlock(s->mtx);
}

void buffered_stream_set_shutdown_period_millis(buffered_stream_t *s, int shutdown_millis) {
	nng_mtx_lock(s->mtx);
	s->shutdown_millis = shutdown_millis;
	nng_mtx_unlock(s->mtx);
}

int buffered_stream_set_len(buffered_stream_t *s, size_t len) {
	if (s->msg_buffer != NULL)
		return -1;
	s->msg_buffer_len = len;
	return 0;
}

// cmd_cb is a callback to manange out-of-band sync for the buffered stream.
// The rep socket recieves the sequence numer the client last saw and cmd_cb
// replies with all messages after that in the buffer.
//
// TODO: this should be limit the size, otherwise clients might reject the nng message.
// clients must then check if they've recieved enough and send a second sync. Perhaps can
// be done without protocol change.
void cmd_cb(void *arg) {
	nng_msg *msg, *reply_msg;
	int rv;

	buffered_stream_t *s = arg;

	if (nng_aio_result(s->rep_aio) != 0)
		return;

	msg = nng_aio_get_msg(s->rep_aio);

	uint32_t req_seq;
	rv = nng_msg_chop_u32(msg, &req_seq);
	nng_msg_free(msg);

	if (rv != 0) {
		fprintf(stderr, "spub: bad msg");
		goto end;
	}

	nng_mtx_lock(s->mtx);

	if (s->seq == 0 || req_seq >= s->seq) {
		nng_send(s->rep, "", 1, 0);
		goto end_unlock;
	}

	// Find the first message requested
	int first = s->seq % s->msg_buffer_len; // oldest message
	if (s->msg_buffer[first].data == NULL)
		first = 0; // if buffer hasn't filled yet

	while (s->msg_buffer[first].seq < req_seq) {
		first = (first + 1) % s->msg_buffer_len;
	}

	size_t rep_msg_size = 0;

	// NB (seq-1) is the last seq posted
	for (int j = first;; j = (j + 1) % s->msg_buffer_len) {
		rep_msg_size += msg_marshaled_len(&s->msg_buffer[j]);
		if (s->msg_buffer[j].seq == s->seq - 1)
			break;
	}

	rv = nng_msg_alloc(&reply_msg, rep_msg_size);
	if (rv != 0) {
		fatal("allocating new message", rv);
	}

	uint8_t *rep_ptr = nng_msg_body(reply_msg);
	size_t msg_len   = 0;

	for (int j = first;; j = (j + 1) % s->msg_buffer_len) {
		int n = msg_marshal(&s->msg_buffer[j], rep_ptr, rep_msg_size - msg_len);
		if (n < 0) {
			fatal("marshaling msg", 0);
		}
		rep_ptr += n;
		msg_len += n;

		if (s->msg_buffer[j].seq == s->seq - 1)
			break;
	}

	if (msg_len != rep_msg_size) {
		fatal("msg smaller than anticipated", 0);
	}

	rv = nng_sendmsg(s->rep, reply_msg, 0);
	if (rv != 0) {
		nng_msg_free(reply_msg);
	}

end_unlock:
	nng_mtx_unlock(s->mtx);
end:
	nng_recv_aio(s->rep, s->rep_aio);
	return;
}

ssize_t send_msg(buffered_stream_t *s, msg_t *m) {
	int rv;
	nng_msg *msg;
	ssize_t pub_len = msg_marshaled_len(m);

	rv = nng_msg_alloc(&msg, pub_len);
	if (rv != 0) {
		fatal("allocating new message", rv);
	}

	if (msg_marshal(m, nng_msg_body(msg), pub_len) < 0) {
		return -1;
	}

	rv = nng_sendmsg(s->pub, msg, 0);

	if (rv != 0) {
		return rv;
	};

	return pub_len;
}

ssize_t buffered_stream_send(buffered_stream_t *s, const void *buf, size_t n) {
	nng_mtx_lock(s->mtx);

	size_t i = s->seq % s->msg_buffer_len;

	if (s->msg_buffer == NULL) {
		s->msg_buffer = calloc(s->msg_buffer_len, sizeof(msg_t));
	}

	if (s->msg_buffer[i].data != NULL) {
		free(s->msg_buffer[i].data);
		s->msg_buffer[i].data = NULL;
	}

	char *data = malloc(n);
	if (data == NULL) {
		fatal("allocating msg", errno);
	}

	s->msg_buffer[i].type = DATA;
	s->msg_buffer[i].data = memcpy(data, buf, n);
	s->msg_buffer[i].len  = n;
	s->msg_buffer[i].seq  = s->seq++;

	if (send_msg(s, &s->msg_buffer[i]) < 0) {
		fatal("sending msg", errno);
	}

	nng_mtx_unlock(s->mtx);
	return n;
}

void heartbeat_cb(void *arg) {
	buffered_stream_t *s = arg;
	if (nng_aio_result(s->heartbeat_aio) == NNG_ECANCELED)
		return;
	nng_mtx_lock(s->mtx);
	msg_t m = {HEARTBEAT, s->seq, 0, NULL};
	if (s->shutting_down) {
		m.type = END_OF_SESSION;
	}
	if (send_msg(s, &m) < 0) {
		fatal("sending HEARTBEAT", errno);
	}
	nng_sleep_aio(s->heartbeat_millis, s->heartbeat_aio);
	nng_mtx_unlock(s->mtx);
}

int buffered_stream_open(buffered_stream_t **bs) {
	init();
	buffered_stream_t *s = (*bs) = calloc(1, sizeof(buffered_stream_t));
	int rv;

	s->heartbeat_millis          = 500;
	s->shutdown_millis           = 2000;
	s->shutdown_heartbeat_millis = 100;
	s->msg_buffer_len            = 1000;

	rv = nng_mtx_alloc(&s->mtx);
	if (rv != 0) {
		goto error;
	}
	nng_mtx_lock(s->mtx);

	rv = nng_rep0_open(&s->rep);
	if (rv != 0) {
		goto error;
	}

	rv = nng_aio_alloc(&s->rep_aio, cmd_cb, s);
	if (rv != 0) {
		goto error;
	}
	nng_recv_aio(s->rep, s->rep_aio);

	rv = nng_pub0_open(&s->pub);
	if (rv != 0) {
		goto error;
	}

	nng_aio_alloc(&s->heartbeat_aio, heartbeat_cb, s);
	nng_sleep_aio(s->heartbeat_millis, s->heartbeat_aio);

	nng_mtx_unlock(s->mtx);
	return 0;

error:
	// TODO: stop, dealloc.
	return -1;
}

int buffered_stream_listen(buffered_stream_t *s, const char *pub_url, const char *rep_url) {
	int rv;
	nng_mtx_lock(s->mtx);
	rv = nng_listen(s->pub, pub_url, NULL, 0);
	if (rv != 0) {
		nng_mtx_unlock(s->mtx);
		return rv;
	}
	rv = nng_listen(s->rep, rep_url, NULL, 0);
	if (rv != 0) {
		nng_mtx_unlock(s->mtx);
		return rv;
	}
	nng_mtx_unlock(s->mtx);
	return 0;
}

void shutdown_cb(void *arg) {
	buffered_stream_t *s = arg;
	if (nng_aio_result(s->shutdown_aio) == NNG_ECANCELED) {
		return;
	}
	buffered_stream_kill(arg);
}

void buffered_stream_close(buffered_stream_t *s) {
	if (s->shutdown_heartbeat_millis == 0) {
		buffered_stream_kill(s);
		return;
	}

	nng_mtx_lock(s->mtx);
	s->heartbeat_millis = s->shutdown_heartbeat_millis;
	s->shutting_down    = true;
	nng_aio_alloc(&s->shutdown_aio, shutdown_cb, s);
	nng_sleep_aio(s->shutdown_millis, s->shutdown_aio);
	nng_mtx_unlock(s->mtx);
}

void buffered_stream_kill(buffered_stream_t *s) {
	nng_close(s->pub);
	nng_close(s->rep);

	nng_aio_stop(s->rep_aio);
	nng_aio_stop(s->heartbeat_aio);

	nng_mtx_lock(s->mtx);
	nng_mtx *mtx = s->mtx;

	nng_aio_free(s->rep_aio);
	nng_aio_free(s->heartbeat_aio);
	if (s->shutdown_aio) {
		async_free(s->shutdown_aio);
	}

	if (s->msg_buffer) {
		for (size_t i = 0; i < s->msg_buffer_len; i++) {
			if (s->msg_buffer[i].data == NULL) {
				break;
			}
			free(s->msg_buffer[i].data);
		}
		free(s->msg_buffer);
	}
	s->mtx = NULL;
	free(s);
	nng_mtx_unlock(mtx);
	nng_mtx_free(mtx);
}

void buffered_stream_join(buffered_stream_t *s) {
	if (!s || !s->mtx)
		return;
	nng_mtx_lock(s->mtx);
	nng_aio *aio = s->shutdown_aio;
	nng_mtx_unlock(s->mtx);

	if (!aio) {
		return;
	}

	nng_aio_wait(aio);
}

struct dup_thread_args {
	int fd;
	buffered_stream_t *s;
};

static void *dup_thread(void *args) {
	struct dup_thread_args a = *((struct dup_thread_args *)args);
	free(args);

	char buf[8192];
	ssize_t n;
	for (;;) {
		if ((n = read(a.fd, buf, sizeof(buf))) <= 0) {
			break;
		}
		buffered_stream_send(a.s, buf, n);
	}

	close(a.fd);

	buffered_stream_close(a.s);
	return NULL;
}

int buffered_stream_copy_fd(buffered_stream_t *s, int fd) {
	nng_mtx_lock(s->mtx);
	struct dup_thread_args *args = malloc(sizeof(struct dup_thread_args));
	args->fd                     = fd;
	args->s                      = s;
	if (pthread_create(&s->dup_thread, NULL, dup_thread, args) < 0) {
		nng_mtx_unlock(s->mtx);
		return -1;
	}

	nng_mtx_unlock(s->mtx);
	return 0;
}
