define  checkr135     00000000000
check=*,-tp,-hd
parity=,,ab,on
sfastf=15.14s
!+6s
repro=raw,6,8
!*
st=rev,135,off
!+3s
parity
!*+37s
et
!+3s
repro=byp,6,8
check=*,tp,hd
enddef
define  checkf135     97355220411
check=*,-tp,-hd
parity=,,ab,on
sfastr=15.14s
!+6s
repro=raw,5,7
!*
st=for,135,off
!+3s
parity
!*+37s
et
!+3s
repro=byp,5,7
check=*,tp,hd
enddef
