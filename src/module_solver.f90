module module_solver
  use module_param
  use drmtype
  use module_drm_fun
implicit none

contains
subroutine precipitation_recycling(TCPW,ET,PPT,UW,VW,BCC,regions,dx,dy,nx,ny, start_day,end_day,&
                                   dt,max_tracing,max_iteration,days,spinup,vt,v1,v2,domsize,domainij)

  integer,  intent(in):: nx
  integer,  intent(in):: ny
  integer,  intent(in):: start_day
  integer,  intent(in):: end_day
  integer,  intent(in):: days
  integer,  intent(in):: regions
  integer,  intent(in):: dt
  integer,  intent(in):: max_tracing
  integer,  intent(in):: max_iteration
  integer,  intent(in):: spinup
  integer,  intent(in):: vt
  integer,  intent(in):: v1
  integer,  intent(in):: v2
  integer,  intent(in):: domsize
  real,     intent(in):: dx, dy

  
  ! Data Arrays
  real, dimension(1:nx,1:ny,1:days),        intent(in):: TCPW 
  real, dimension(1:nx,1:ny,1:days),        intent(in):: ET
  real, dimension(1:nx,1:ny,1:days),        intent(in):: PPT
  real, dimension(1:nx,1:ny,1:days*24/vt),  intent(in):: UW
  real, dimension(1:nx,1:ny,1:days*24/vt),  intent(in):: VW
  integer, dimension(1:nx,1:ny),            intent(in):: BCC
  integer,dimension(2,domsize)                        :: domainij
  !Local Variables  

  real                                  :: TPW=0
  real                                  :: TET=0
  real                                  :: TPP=0
  real                                  :: TU_1=0
  real                                  :: TV_1=0
  real                                  :: TU_2=0
  real                                  :: TV_2=0
  real, dimension(max_tracing,3)        :: EW_vector      !matrix used to save the E/W data of each grid on specific day and the region it belongs as well
  real                                  :: E_W=0            !matrix to save integrated EW_vector of each grid on specific day
  real, dimension(domsize,start_day:end_day)    :: rrcalc           !matrix to save recycling ratio of each grid on specific day
  integer                               :: i,j,k,day,time,it
  integer                               :: intx,inty,intx1,inty1
  integer                               :: flag1,flag2
  real                                  :: oldppx,oldppy,diffx,diffy
  real, dimension(max_tracing)          :: ppx,ppy          !variable used to record the position of tracing
  integer                               :: daycount,hourcount,count1,count2
  real                                  :: temprr,sumrr,sumpp,s
  real, dimension(start_day:end_day)            :: rr
  integer                               :: fives, pentads_bef,pentads_aft, file_id    ! these variables are defined for pentads analysis

!-------new variables for computing the contribution of different regions-------
  type(start_end),  dimension(max_tracing)           :: reg_count    ! start loc, end loc, reg count
  real,             dimension(domsize,regions)       :: grid_rr         ! record contribution to one grid box
  real,             dimension(start_day:end_day,regions)     :: daily_rr        ! record daily recycling ratio of different regions
  real,             dimension(regions)               :: sum_region
  real,             dimension(nx,ny)                 :: precip_grid
  real,allocatable                                   :: various_reg(:,:) 
  integer                                            :: reg_change,old_reg,new_reg,zeroi,ein,xx,ind,L1,L2
  real                                               :: s1,pral

! start the daily loop during the chosen duration from start_day to end_day
  print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  print *,'!!!  Daily Loop Starts  !!!'
  !!! This section is contributed by Hu, Huancui hhu18@illinois.edu  !!!
  print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  daily : do day=start_day,end_day

    print *,'day:', day
    write(100,*) 'Day', day
    write(200,*) day
    write(600,*) 'Day', day
    sumrr=0
    sumpp=0
   
    precip_grid(:,:)=0.0

    ! start the domain loop
    domain : do k=1,domsize
        
        i=domainij(k,1)
        j=domainij(k,2)
        ppx(1)=(i-1)*dx+dx/2
        ppy(1)=(j-1)*dy+dy/2
        TPP=PPT(i,j,day)
        E_W=0
        
        write(100,*) 'i=',i,'j=',j
        write(600,*) 'i=',i,'j=',j
        write(400,'(F10.2,5X,F10.2)') ppx(1),ppy(1)     !test output file
        write(200,'(I3,5X,I3)') i,j         

        count1=0
        count2=0
        reg_change=0
        hourcount=day*24/vt   !starting temporal position in velocity files
        daycount=day                  !starting temporal position in other files
     
        ! allocate all to be zero
        do zeroi=1,max_tracing
          do ein=1,3
            EW_vector(zeroi,ein)=0
          end do
          reg_count(zeroi)%start_l=0
          reg_count(zeroi)%end_l=0
          reg_count(zeroi)%renum=0
        end do

!====================================================================================================
!====================================================================================================
        ! start the time loop
        timeloop : do time=1,max_tracing ! time steps

        ! tell ppx and ppy are within large domain or not-----------------------------------
        if ((ppx(time)<=0).or.(ppx(time)>=(dx*nx))) then
        ! print *,'Beyond boundary during timeloop when time is:',time
            exit timeloop
        else if ((ppy(time)<=0).or.(ppy(time)>=(dy*ny))) then
        ! print *,'Beyond boundary during timeloop when time is:',time
            exit timeloop
        end if
        !-----------------------------------------------------------------------------------

        !read the Evaporation,precipitable water and wind velocity by subroutine 'finduvEW'
        call finduvEW(ppx(time),ppy(time),hourcount,daycount,TCPW(:,:,daycount),ET(:,:,daycount),&
                      UW(:,:,hourcount),VW(:,:,hourcount),TPW,TET,TU_1,TV_1,intx1,inty1,nx,ny,dx,dy,flag1)
        if (flag1==1) then
          exit timeloop
        else
          EW_vector(time,1)=(time-1)*dt ! time in seconds
          EW_vector(time,2)=TET/TPW  ! ratio ET /PW
          EW_vector(time,3)=BCC(intx1,inty1) ! region number
        end if
 
        if (time .eq. 1) then
           reg_count(1)%start_l=1
           reg_count(1)%renum=BCC(i,j)
           reg_change=1
        else
           old_reg=EW_vector(time-1,3)
           new_reg=EW_vector(time,3)
           if (old_reg .ne. new_reg) then
              reg_change=reg_change+1
              reg_count(reg_change-1)%end_l=time
              reg_count(reg_change)%start_l=time
              reg_count(reg_change)%renum=new_reg      
           end if
        end if
  
        ! find the next point using Iterative Technique--------------------------------------
        call finduv(ppx(time),ppy(time),hourcount,UW(:,:,hourcount),& 
                    VW(:,:,hourcount),TU_2,TV_2,intx,inty,nx,ny,dx,dy,flag2)
        oldppx=ppx(time)-(TU_1+TU_2)*dt/2
        oldppy=ppy(time)-(TV_1+TV_2)*dt/2
        if ((oldppx<=0).or.(oldppx>=(dx*nx))) exit timeloop
        if ((oldppy<=0).or.(oldppy>=(dy*ny))) exit timeloop
!================================================================================================
        iteration : do it=1,max_iteration
            !tell if the point still within the large domain
            if ((oldppx<=0).or.(oldppx>=(dx*nx))) then
            !  print *,'Beyond boundary during iteration when it is',it
              exit iteration
            else if ((oldppy<=0).or.(oldppy>=(dy*ny))) then
            !  print *,'Beyond boundary during iteration when it is',it
              exit iteration
            end if           

            call finduv(oldppx,oldppy,hourcount,UW(:,:,hourcount),&
                 VW(:,:,hourcount),TU_2,TV_2,intx,inty,nx,ny,dx,dy,flag2)
            if (flag2==1) then
                exit iteration
            else
                ppx(time+1)=ppx(time)-(TU_1+TU_2)*dt/2
                ppy(time+1)=ppy(time)-(TV_1+TV_2)*dt/2
            
                diffx=abs(ppx(time+1)-oldppx)
                diffy=abs(ppy(time+1)-oldppy)
                 
                if ((diffx<0.0001).and.(diffy<0.0001)) exit  iteration
                oldppx=ppx(time+1)
                oldppy=ppy(time+1)
            end if
          end do iteration
!================================================================================================
        ! change the temporal data to use based on back tracing------------------------------
         count1=count1+1
         if (mod(count1,v1)==0) then
           hourcount=hourcount-1
           if(hourcount<=0) exit timeloop
         end if
         
         count2=count2+1
         if (mod(count2,v2)==0)  then
           daycount=daycount-1
           if(daycount<=0) exit timeloop
         end if
         
       end do timeloop
       write(100,*) 'Timeloop completed.'

!===================================================================================================
!===================================================================================================
      ! do the integral of the entire path
      allocate(various_reg(reg_change,3))
      pral=1
      if (count2>0) then
        reg_count(reg_change)%end_l=count2
        call SPTNQ(EW_vector(1:count2,1),EW_vector(1:count2,2),count2,vt,s)
        E_W=s
        do ein=1,reg_change
          L1=reg_count(ein)%start_l
          L2=reg_count(ein)%end_l
          call SPTNQ(EW_vector(L1:L2,1),EW_vector(L1:L2,2),L2-L1+1,vt,s1)
          various_reg(ein,1)=s1
          various_reg(ein,2)=exp(-s1)
          various_reg(ein,3)=pral*(1-various_reg(ein,2))
          pral=pral*various_reg(ein,2)
          write(800,'(I5,5X,I5,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,I2)')L1,L2,various_reg(ein,1:3),&
               pral,reg_count(ein)%renum
        end do 
      else
        E_W=0
      end if
   
      ! compute the contribution of each region
      do ein=1,regions
        sum_region(ein)=0;
      end do  
      do ein=1,reg_change
        ind=reg_count(ein)%renum
        sum_region(ind)=sum_region(ind)+various_reg(ein,3)
      end do
      do ein=1,regions
        grid_rr(k,ein)=sum_region(ein)
      end do
      write(800,'(I3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3)') k,grid_rr(k,1:9)
     
      deallocate(various_reg)
!      write(200,*) E_W
      write(200,*) 'count2=',count2
      !calculate recycling ratio
      temprr=1-exp(-E_W)
      write(200,*) 'local rr=',temprr
      sumrr=sumrr+temprr*TPP
      sumpp=sumpp+TPP
      precip_grid(i,j)=TPP

   end do domain
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   print *,'tracing completed'
   rr(day)=sumrr/sumpp
   print *,rr(day)
   write(300,'(I3,3X,F5.3)') day,rr(day)
   write(100,*) 'End day',day
   write(200,*) 'End day',day
!   write(300,*) 'End day',day
   
   do ein=1,regions
     sumrr=0
     sumpp=0
     do k=1,domsize
       sumrr=sumrr+grid_rr(k,ein)*precip_grid(i,j)
       sumpp=sumpp+precip_grid(i,j)
       write(900,'(I3,5X,I3,5X,F6.3,5X,F6.3,5X,F7.3,5X,F7.3)')ein,k,grid_rr(k,ein),precip_grid(i,j),sumrr,sumpp
     end do
     daily_rr(day,ein)=sumrr/sumpp
   end do
   write(700,'(I3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3,&
         5X,F5.3,5X,F5.3,5X,F5.3,5X,F5.3)') day,daily_rr(day,1:regions) 
    
   end do daily

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!##################################################################################################
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   
   close(100)
   close(200)
   close(300)
   close(400)
   close(500)
   close(600)
   close(700)
   close(800)
   close(900)
 end subroutine precipitation_recycling  
end module module_solver
