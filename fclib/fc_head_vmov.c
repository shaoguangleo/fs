void fc_head_vmov__(ihead,idir,ispdhd,jm,ip,indxtp)
int *ihead;
int *idir;
int *ispdhd;
int *jm;
int ip[5];
int *indxtp;
{
    void head_vmov();

    head_vmov(*ihead,*idir,*ispdhd,*jm,ip,*indxtp);

    return;
}
