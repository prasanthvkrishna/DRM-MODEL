module module_vars
use module_rd_namelist,only : nx, ny
implicit none
  real, dimension(1:NX,1:NY,days) 	:: TCPW 
  real, dimension(1:NX,1:NY,days)	:: ET
  real, dimension(1:NX,1:NY,days)	:: PPT
  real, dimension(1:NX,1:NY,days*vt)	:: UW
  real, dimension(1:NX,1:NY,days*vt)    :: VW
  real, dimension(1:NX,1:NY)		:: BCC    
end module module_vars 
