module module_drm_fun
use module_param
implicit none
contains
!
subroutine finduvEW(ppx,ppy,hourcount,daycount,W,E,U,V,outW,outE,outU,outV,xint,yint,nx,ny,dx,dy,flag)
  !use param
  implicit none
  real,intent(in) :: ppx,ppy,dx,dy
  integer,intent(in) :: daycount,hourcount,nx,ny
  real,dimension(nx,ny),intent(in) ::U,V,E,W
  real,intent(out) :: outW,outE,outU,outV
  integer,intent(out) :: xint,yint,flag
  flag=0
  xint=ceiling(ppx/dx)
  yint=ceiling(ppy/dy)
  outW=W(xint,yint)
  outE=E(xint,yint)
  outU=U(xint,yint)
  outV=V(xint,yint)
  if (outW==NAN_value) then
    flag=1
  elseif (outE==NAN_value) then
    flag=1
  elseif (outU==NAN_value) then
    flag=1
  elseif (outV==NAN_value) then
    flag=1
  end if
end subroutine
!
subroutine finduv(xp,yp,hourcount,u,v,outu,outv,xint,yint,nx,ny,dx,dy,flag)
 ! use param
  implicit none
  real,intent(in) ::xp,yp,dx,dy
  integer,intent(in) ::hourcount,nx,ny
  real,dimension(nx,ny),intent(in) ::u,v
  real,intent(out)::outu,outv
  integer,intent(out)::xint,yint,flag
  flag=0
  xint=ceiling(xp/dx)
  yint=ceiling(yp/dy)
  outu=u(xint,yint)
  outv=v(xint,yint)
  if (outu==NAN_value) then
    flag=1
  else if (outv==NAN_value) then
    flag=1
  end if
end subroutine
!
subroutine SPTNQ(x,y,n,vt,s)
  use module_param
  implicit none
  integer,intent(in):: n,vt
  real,dimension(n),intent(in)::x,y
  real,intent(out) :: s
  integer:: i,j,k
  real :: a,b1,b2
  
  s=0
  do i=1,n-1
    a=x(i+1)-x(i)
    b1=y(i)
    b2=y(i+1)
    s=s+a*(b1+b2)/(2*3600*vt)
  !  write(600,*) a, s
  end do
end subroutine
end module module_drm_fun


