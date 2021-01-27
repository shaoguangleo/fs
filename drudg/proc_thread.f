      subroutine proc_thread(cproc_thread)
      implicit none 
      include 'drcom.ftni'
! generate new thread procedure
      character*(*) cproc_thread

      call proc_write_define(lu_outfile, luscn,cproc_thread)
      write(lu_outfile,'(a)') "jive5ab=datastream=clear"
      if(lvdif_thread .eq. "YES") then
        write(lu_outfile,'(a)') "jive5ab=datastream=add:{thread}:*"
      endif
      write(lu_outfile,'(a)') "jive5ab=datastream=reset"
      write(lu_outfile,'(a)') "endef"
      end 
   


