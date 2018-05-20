module module_listfile
implicit none
contains
  subroutine list_files(indir,fname,filelists,NFiles)
    character(len=256), dimension(:),allocatable :: filelists
    character(*) 			:: indir
    integer 				:: cnt,iserr,cntwrf
    integer           			:: NFiles
    real    				:: irer
    character(len=256)			:: fname
    ! get the files
    call system("ls "//trim(indir)//trim(fname)//"* >filelist.txt")
    open(41,FILE='filelist.txt',action="read")
    !counts of file
    cnt = 0
    do
      read(41,FMT='(a)',iostat=iserr) irer
      if (iserr/=0) EXIT
      cnt = cnt+1
    end do
    NFiles = cnt
    !print*, "Number of NetCDF files: " , NFiles
    allocate(filelists(NFiles))
    rewind(41)
    do cntwrf = 1,NFiles
     read(41,'(a)') filelists(cntwrf)
   !print*,wrfFileNames(cntwrf)
    end do
    close(41)
    call system("rm filelist.txt")
    return
   end subroutine list_files
end module module_listfile
