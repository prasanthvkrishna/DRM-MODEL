! This is the main program of dynamic precipitation recycling
! By Prasanth Valaymkunnath, Virginia Tech

program drm
 use netcdf
 use module_param
 use drmtype
 use module_listfile
 use module_rd_namelist
 use module_netcdf_io
 use module_solver
 
 implicit none
  integer 		:: ii, jj, dd
! Global (/namelist) Variables
  character(len=256)	:: pwindir
  character(len=256)	:: etindir
  character(len=256)	:: pptindir
  character(len=256)	:: vwindir
  character(len=256)	:: uwindir
  character(len=256)	:: outdir
  character(len=256) 	:: pwfname
  character(len=256) 	:: etfname
  character(len=256) 	:: pptfname
  character(len=256) 	:: uwfname
  character(len=256) 	:: vwfname
  character(len=256) 	:: zonefile
  integer	     	:: nx
  integer	     	:: ny
  integer	     	:: start_day
  integer	     	:: end_day
  integer	     	:: iyear
  integer	     	:: iserr
  integer	     	:: days
  integer		:: regions
  integer		:: dt
  integer		:: max_tracing
  integer		:: max_iteration
  integer		:: spinup
  integer		:: vt
  integer		:: v1
  integer		:: v2
  real			:: dx, dy
  
  !Data File variables
  character(len=256), dimension(:),allocatable :: pwfilelists,etfilelists,pptfilelists,vwfilelists,uwfilelists
   integer 		:: NFiles,a,b,rg,cnt,loc  

  
  ! Data Arrays
  real, allocatable, dimension(:,:,:)	:: TCPW 
  real, allocatable, dimension(:,:,:) 	:: ET
  real, allocatable, dimension(:,:,:) 	:: PPT
  real, allocatable, dimension(:,:,:) 	:: UW
  real, allocatable, dimension(:,:,:)   :: VW
  integer,allocatable, dimension(:,:) 	:: BCC
  integer,allocatable, dimension(:,:)   :: domainij
  integer,allocatable, dimension(:)     :: domsize

  call rd_namelist(pwindir,etindir,pptindir,vwindir,uwindir,outdir,pwfname,&
  			 etfname,pptfname,vwfname,uwfname,		 & 
  			 zonefile,nx,ny,dx,dy,start_day,end_day,iyear,   &
                         regions,dt,max_tracing,max_iteration,days,      &
                         spinup,vt,v1,v2)


  !Initialize arrays
  allocate(TCPW(nx,ny,days))
  allocate(ET(nx,ny,days))
  allocate(PPT(nx,ny,days))
  allocate(UW(nx,ny,days*24/vt))
  allocate(VW(nx,ny,days*24/vt))
  allocate(BCC(nx,ny))
  allocate(domsize(regions))

  do ii=1,nx
     do jj=1,ny
        BCC(ii,jj) = 0.0
  	    do dd=1,days
  	        TCPW (ii,jj,dd)   = 0.0
  	        ET   (ii,jj,dd)   = 0.0
  	        PPT  (ii,jj,dd)   = 0.0
  	    end do
     end do
  end do   
  do ii=1,nx
     do jj=1,ny
        BCC(ii,jj) = 0.0
  	    do dd=1,days*vt
  	        UW   (ii,jj,dd)   = 0.0
  	        VW   (ii,jj,dd)   = 0.0
  	    end do
     end do
  end do 

   call list_files(pwindir,pwfname,pwfilelists,NFiles) 
   call rd_2d_TCPW_to_3d(pwfilelists,days,nx,ny,TCPW)

   call list_files(etindir,etfname,etfilelists,NFiles)
   call rd_2d_ET_to_3d(etfilelists,days,nx,ny,ET)

   call list_files(pptindir,pptfname,pptfilelists,NFiles)
   call rd_2d_PPT_to_3d(pptfilelists,days,nx,ny,PPT)

   call list_files(uwindir,uwfname,uwfilelists,NFiles)
   call rd_2d_UW_to_3d(uwfilelists,days,nx,ny,vt,UW) 

   call list_files(vwindir,vwfname,vwfilelists,NFiles)
   call rd_2d_VW_to_3d(vwfilelists,days,nx,ny,vt,VW) 
      
   call rd_2d_regions(zonefile,nx,ny,BCC) 

   !estimate number of grids in each zone
   cnt=0
   write(*,*)'DRM Model Started!'
   do rg=1,regions
    do ii=1,nx
        do jj=1,ny
          if (BCC(ii,jj).eq.1)then
              cnt=cnt+1
          end if
        end do
    end do
    allocate(domainij(2,cnt))
    loc=1
    do ii=1,nx
        do jj=1,ny
          if (BCC(ii,jj).eq.1)then
              domainij(1,loc)=ii
              domainij(2,loc)=jj
              loc=loc+1
          end if
        end do
    end do
    domsize(rg)=cnt
    print*,domsize(rg)
              
    call precipitation_recycling(TCPW,ET,PPT,UW,VW,BCC,regions,dx,dy,nx,ny, start_day,end_day,&
                                   dt,max_tracing,max_iteration,days,spinup,vt,v1,v2,domsize(rg),domainij)
    write(*,*)'DRM Model Finished!'
   end do
end program drm
