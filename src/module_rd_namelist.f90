module module_rd_namelist
implicit none
contains 
  subroutine rd_namelist(pwindir,etindir,pptindir,vwindir,uwindir,outdir,pwfname,&
  			 etfname,pptfname,vwfname,uwfname,	&
  			 zonefile,nx,ny,dx,dy,start_day,end_day,iyear, &
                         regions,dt,max_tracing,max_iteration,days,&
                         spinup,vt,v1,v2)
  
  character(len=256)	:: pwindir,etindir,pptindir,vwindir,uwindir
  character(len=256)	:: outdir
  character(len=256) 	:: pwfname,etfname,pptfname,vwfname,uwfname
  character(len=256) 	:: zonefile
  integer	     	:: nx,ny,start_day,end_day,iyear,iserr,days
  integer		:: regions
  integer		:: dt
  integer		:: max_tracing
  integer		:: max_iteration
  integer		:: spinup
  integer		:: vt,v1,v2
  real			:: dx, dy
  
  namelist / DRM/ PWINDIR,ETINDIR,PPTINDIR,VWINDIR,UWINDIR,OUTDIR,PWFNAME,&
  		  ETFNAME,PPTFNAME,VWFNAME,UWFNAME, &
  		  ZONEFILE,NX,NY,START_DAY,END_DAY,IYEAR,&
                  REGIONS,DT,MAX_TRACING,MAX_ITERATION,SPINUP,VT,DX,DY
                  
  
  open(20, file="namelist.drm", form="FORMATTED")
  read(20, DRM, iostat=iserr)
  if (iserr /= 0) then
     write(*,'(/," ERROR DRM: Problem reading namelist.drm",/)')
     rewind(20)
     read(20, DRM)
     stop " ERROR DRM: Problem reading namelist.drm"
  endif              
  close(20)
  
  if ((start_day < 1) .and. (end_day < 1)) then
     write(*, '(" ***** Namelist error: Specify correct Start day and end day****")')
     stop
  else if (( end_day < start_day )) then
     write(*, '(" ***** Namelist error: Specify Start day < end day****")')
     stop
  else
     ! All is well. :)
     v1   = (3600*vt/dt)
     v2   = (3600*24/dt)
     days = (end_day-start_day)+1
  endif
  
  end subroutine rd_namelist

end module module_rd_namelist
