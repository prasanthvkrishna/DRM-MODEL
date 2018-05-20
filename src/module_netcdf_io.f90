module module_netcdf_io
use netcdf
use module_param
use module_listfile

implicit none
contains

 ! Total column precipitable water
 subroutine rd_2d_TCPW_to_3d(filelists,days,nx,ny,TCPW)
	use netcdf
	integer, 	    			            intent(in)   :: nx,ny,days
    character(len=256), dimension(days),	intent(in)   :: filelists
	real, dimension(1:nx,1:ny,days),	 intent(inout)   :: TCPW      
	real, dimension(1:nx,1:ny)			                 :: TTCPW 
	integer						                         :: ncid,is_err,i,varid
	!print*, filelists
        
	! initializwe the arraysudo apt autoremove
	TTCPW(1:nx,1:ny)   = 0.      
	!print*,"NetCDF reading",filelists(i)	
	do i = 1,days !=NFiles
	      !Open NetCDF file
	      !print*,"Program is reading ",filelists(i)
	      is_err = nf90_open(filelists(i), NF90_NOWRITE, ncid)
	      if (is_err /= 0) then
	         write(*,'("module_netcdf_io TCPW:  Problem opening NetCDF file: ''", A, "''")') trim(filelists(i))
	         stop
	      endif
	      
	      is_err = nf90_inq_varid(ncid,  "PWT",  varid)
	      !print*,varid
	      if (is_err /= 0) then
		  write(*,'("module_netcdf_io TCPW:  variable not found: ''", A, "''")') trim(filelists(i))
		  stop
	      endif
	      
	      is_err = nf90_get_var(ncid, varid, values=TTCPW, start=(/1,1,1/), count=(/nx,ny,1/))
	      if (is_err /= 0) then
	          print*, 'ncid = ', ncid, "Error Reading Variable PWT in input NetCDF file."
	      else
		  TCPW(1:nx,1:ny,i) = TTCPW(1:nx,1:ny)
		  !print*,TTCPW(100,100)
	      endif
	 end do	
	 Print*,"PW reading finished"              
 end subroutine rd_2d_TCPW_to_3d

 ! Evapotranspiration, ET
 subroutine rd_2d_ET_to_3d(filelists,days,nx,ny,ET)
	use netcdf
        character(len=256), dimension(:),allocatable, intent(in) :: filelists
 	integer, 	    			      intent(in) :: nx,ny,days
	real, dimension(1:NX,1:NY,days),              intent(out):: ET      
	real, dimension(1:NX,1:NY)			 	 :: TET 
	integer							 :: ncid,is_err,i,varid

	! initializwe the array
	TET(1:nx,1:ny)   = 0. 
	      
	do i = 1,days !=NFiles
	      !Open NetCDF file
	      !print*,"Program is reading ",filelists(i)
	      is_err = nf90_open(filelists(i), NF90_NOWRITE, ncid)
	      if (is_err /= 0) then
	         write(*,'("module_netcdf_io ET:  Problem opening NetCDF file: ''", A, "''")') trim(filelists(i))
	         stop
	      endif
	      
	      is_err = nf90_inq_varid(ncid,  "ET",  varid)
	      !print*,varid
	      if (is_err /= 0) then
		  write(*,'("module_netcdf_io ET:  variable not found: ''", A, "''")') trim(filelists(i))
		  stop
	      endif
	      
	      is_err = nf90_get_var(ncid, varid, values=TET, start=(/1,1,1/), count=(/nx,ny,1/))
	      if (is_err /= 0) then
	          print*, 'ncid = ', ncid, "Error Reading Variable ET in input NetCDF file."
	      else
		  ET(:,:,i) = TET*3600*24 ! mm/day. WRF QFX output is Kg m-2 s-1. Since this is hourly values,xly by 3600*24 
	      endif
	 end do
	 Print*,"ET reading finished" 	              
 end subroutine rd_2d_ET_to_3d

 ! Total daily precipitation, PPT
 subroutine rd_2d_PPT_to_3d(filelists,days,nx,ny,PPT)
	use netcdf
        character(len=256), dimension(:),allocatable, intent(in) :: filelists
 	integer, 	    			      intent(in) :: nx,ny,days
	real, dimension(1:NX,1:NY,days),              intent(out):: PPT      
	real, dimension(1:NX,1:NY)			 	 :: TPPT 
	integer							 :: ncid,is_err,i,varid

	! initializwe the array
	TPPT(1:nx,1:ny)   = 0. 
	     
	do i = 1,days !=NFiles
	      !Open NetCDF file
	      !print*,"Program is reading ",filelists(i)
	      is_err = nf90_open(filelists(i), NF90_NOWRITE, ncid)
	      if (is_err /= 0) then
	         write(*,'("module_netcdf_io PPT:  Problem opening NetCDF file: ''", A, "''")') trim(filelists(i))
	         stop
	      endif
	      
	      is_err = nf90_inq_varid(ncid,  "RAIN",  varid)
	      !print*,varid
	      if (is_err /= 0) then
		  write(*,'("module_netcdf_io PPT:  variable not found: ''", A, "''")') trim(filelists(i))
		  stop
	      endif
	      
	      is_err = nf90_get_var(ncid, varid, values=TPPT, start=(/1,1,1/), count=(/nx,ny,1/))
	      if (is_err /= 0) then
	          print*, 'ncid = ', ncid, "Error Reading Variable PPT in input NetCDF file."
	      else
		  PPT(:,:,i) = TPPT
	      endif
	 end do
	 Print*,"PPT reading finished" 	              
 end subroutine rd_2d_PPT_to_3d
 
  ! Zonal, mosture weighted wind, UW
 subroutine rd_2d_UW_to_3d(filelists,days,nx,ny,vt,UW)
	use netcdf
        character(len=256), dimension(:),allocatable, intent(in) :: filelists
 	integer, 	    			      intent(in) :: nx,ny,days,vt				      
	real, dimension(1:NX,1:NY,1:days*vt),           intent(out):: UW      
	real, dimension(1:NX,1:NY)			 	 :: TUW 
	integer							 :: ncid,is_err,i,varid

	! initializwe the array
	TUW(1:nx,1:ny)   = 0. 
	  
	do i = 1,days*vt !=NFiles
	      !Open NetCDF file
	      !print*,"Program is reading ",filelists(i)
	      is_err = nf90_open(filelists(i), NF90_NOWRITE, ncid)
	      if (is_err /= 0) then
	         write(*,'("module_netcdf_io UW:  Problem opening NetCDF file: ''", A, "''")') trim(filelists(i))
	         stop
	      endif
	      
	      is_err = nf90_inq_varid(ncid,  "UW",  varid)
	      !print*,varid
	      if (is_err /= 0) then
		  write(*,'("module_netcdf_io UW:  variable not found: ''", A, "''")') trim(filelists(i))
		  stop
	      endif
	      
	      is_err = nf90_get_var(ncid, varid, values=TUW, start=(/1,1,1/), count=(/nx,ny,1/))
	      if (is_err /= 0) then
	          print*, 'ncid = ', ncid, "Error Reading Variable UW in input NetCDF file."
	      else
		  UW(:,:,i) = TUW
	      endif
	 end do
	 Print*,"UW reading finished" 	              
 end subroutine rd_2d_UW_to_3d

  ! Meridional, mosture weighted wind, VW
 subroutine rd_2d_VW_to_3d(filelists,days,nx,ny,vt,VW)
	use netcdf
        character(len=256), dimension(:),allocatable, intent(in) :: filelists
 	integer, 	    			      intent(in) :: nx,ny,days,vt
	real, dimension(1:NX,1:NY,1:days*vt),           intent(out):: VW      
	real, dimension(1:NX,1:NY)			 	 :: TVW 
	integer							 :: ncid,is_err,i,varid

	! initializwe the array
	TVW(1:nx,1:ny)   = 0. 
	      
	do i = 1,days*vt !=NFiles
	      !Open NetCDF file
	      !print*,"Program is reading ",filelists(i)
	      is_err = nf90_open(filelists(i), NF90_NOWRITE, ncid)
	      if (is_err /= 0) then
	         write(*,'("module_netcdf_io VW:  Problem opening NetCDF file: ''", A, "''")') trim(filelists(i))
	         stop
	      endif
	      
	      is_err = nf90_inq_varid(ncid,  "VW",  varid)
	      !print*,varid
	      if (is_err /= 0) then
		  write(*,'("module_netcdf_io VW:  variable not found: ''", A, "''")') trim(filelists(i))
		  stop
	      endif
	      
	      is_err = nf90_get_var(ncid, varid, values=TVW, start=(/1,1,1/), count=(/nx,ny,1/))
	      if (is_err /= 0) then
	          print*, 'ncid = ', ncid, "Error Reading Variable VW in input NetCDF file."
	      else
		  VW(:,:,i) = TVW
	      endif
	 end do
	 Print*,"VW reading finished" 	              
 end subroutine rd_2d_VW_to_3d
 
 ! Sub regions
 subroutine rd_2d_regions(zonefile,nx,ny,BCC)
	use netcdf
    character(len=256),			      intent(in) :: zonefile
 	integer, 	    			      intent(in) :: nx,ny      
	integer, dimension(1:NX,1:NY)			 	     :: BCC 
	integer							 :: ncid,is_err,i,varid

	!Open NetCDF file
	!print*,"Program is reading ",zonefile
	is_err = nf90_open(zonefile, NF90_NOWRITE, ncid)
	if (is_err /= 0) then
	    write(*,'("module_netcdf_io regions map:  Problem opening NetCDF file: ''", A, "''")') trim(zonefile)
	    stop
	endif
	      
	is_err = nf90_inq_varid(ncid,  "B_C_C",  varid)
	!print*,varid
	if (is_err /= 0) then
	    write(*,'("module_netcdf_io B_C_C:  variable not found: ''", A, "''")') trim(zonefile)
	    stop
	endif
	      
	is_err = nf90_get_var(ncid, varid, values=BCC, start=(/1,1,1/), count=(/nx,ny,1/))
	if (is_err /= 0) then
	    print*, 'ncid = ', ncid, "Error Reading Variable B_C_C in input NetCDF file."
	else
	    return
	endif              
 end subroutine rd_2d_regions

end module module_netcdf_io
