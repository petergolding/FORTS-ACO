!  paco_dyn_const.f90 
!
!  functions:
!  paco_dyn_const - entry point of console application.
!

!****************************************************************************
!
!  program: paco_dyn_const
!
!  purpose:  entry point for the console application.
!
!  Written by: Jonathan Schulz, Peter Golding, Sam Kapadia and Stella Naylor
!  Adapted from code produced by Szemis (2014)
!
!****************************************************************************

    program paco_dyn_const
      use aco_program_control
    !  use r_num
      use ant_colony
      use ant_graph
      use non_dom
      use lin_feedback_shift_reg 
      use user
      use system_details
      
      real(8) :: r   ,sum_prob                        ! stores random number
      real(8) :: temp1,temp2,temp3,temp4    ! temporary values
      integer, allocatable, dimension(:) :: temp5 ! temp array
      integer :: i,j,k,x,w,io ,flag                  ! counters
      integer ::dummy,j1                    ! dummy values
      integer :: count,count2,countit,countpf,k_count,countit2, countit3,countit4, countit5, countit6 ! counters
      
      call cpu_time(start)
      
      open(unit=123, file="pareto_store1.txt",status="unknown")
      open(unit=562, file="pareto_store2.txt",status="unknown")
      open(unit=662, file="pareto_store3.txt",status="unknown")
      open(unit=762, file="pareto_store4.txt",status="unknown")
      open(unit=862, file="pareto_store5.txt",status="unknown")
      open(unit=962, file="pareto_store6.txt",status="unknown")
      open(unit=1062, file="pareto_store7.txt",status="unknown")
      open(unit=999, file="hypervolume.csv",status="unknown")
        
    ! initialise variable that controls final output txt file
      maxitfin=0
     
      call initialise_paco_parameters     ! program initialisation 
      call init_seeds(seed_ran_1, seed_ran_2, seed_ran_3, seed_ran_4, seed_ran_5)
    ! initialise counters for number of iterations completed and number of non-dom solutions
      countit=0
      countit2=0
      countit3=0
      countit4=0
      countit5=0
      countit6=0
      count_n=0
      count_n2=0
      count_n3=0
      count_n4=0
      count_n5=0
      count_n6=0
      reset_tau_0=0
      
      
      !need to randomise the order of the exporting and importing countries,
      !before the paco begins and after/or within the initialisation of the paco parameters
      
    !********************************p-acoa begins here*****************************************  
      do it_count=1,max_it
      
        write(*,*) it_count
        !sets deficit cost array to zero, sorting needed for calculating trade deficit
        do x = 1,max_ant
          do w=1,n_i_countries*n_years
              ant(x)%deficit_cost(w)=0.0
          end do
        end do
        countit=countit+1
        countit2=countit2+1
        countit3=countit3+1
        countit4=countit4+1
        countit5=countit5+1
        countit6=countit6+1
        
        
        do j=1,max_ant
            !define as zero for sorting purposes as countries can be either importing or exporting
            e_trade_sort=0.0
            i_trade_sort=0.0 

            !random numbers used to determine weight of each objective in prob_determination
            r= lfsr258() 
            ant(j)%pk(1)=r 
          
            r= lfsr258() 
            ant(j)%pk(2)=r

            r= lfsr258() 
            ant(j)%pk2(1)=r 
          
            r= lfsr258() 
            ant(j)%pk2(2)=r

            !ant travelling through the decision matrix for year y_count
            do y_count = 1,n_years
                
                !call trade_constraint(j) 
                if (y_count==1) then
                    call trade_constraint(y_count,j)
                end if
                
                t_count = 1
                e_count = 1
                i_count = 1
                    
                do k=1, max_path
                    !if an invalid trade, skip
                    if (valid_trade(k) /= 0) then
                        !remove as heuristics are not currently considered, slow code significantly
    !                    call heu_calc(j,k)         
                        call prob_determination(j,k)
                        call selection(j,k)
                         !not required as current formula decreases phermone on edge to tau_0 each iteration
    !                    call local_update(j,k)
                    else
                        ant(j)%decision(k+add_year)=1
                        
                        i_count=i_count+1
                        t_count=t_count+1
                        
                        !if exhausted one exporting country's trade links, move on to the next one
                        !reset to importing country one
                        if (i_count == n_i_countries+1) then
                            e_count = e_count+1         !moves to the next exporting country count
                            i_count = 1                 !resets the importing country count to 1
                        end if
                    end if
                end do
                
                !update the ea and ir values (in the matrix)
                call storage_update(y_count,j)

            end do
       
            call evaluation(j)     ! calculates objective functions for each ant

        end do
        
        call non_dom_sort        ! sorts our non-dominated solution and stores solution in an offline storage
        
        call global_update       ! undertakes a global update 
        
        !randomise the country order
        call randomise_country_order

        call cpu_time(time_breach)
        
        dummy_time = (time_breach - start)/(60*60) 
        
        !first non_dominate sort to clear max ants
        if(countit==ndup_freq .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(123)
            open(unit=123, file="pareto_store1.txt",status="unknown")
      
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
    
            do k=1,count_n
                read(123,*)num_n_zero(k),j1,cost1(k),cost2(k)
                
                if (num_n_zero(K) > 0) then
                    read(123,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(123,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(123,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if

            end do
    
            call non_dom_sort2
                
            count_n=0
            countit=0
   
            close(123)
            open(unit=123, file="pareto_store1.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)  !,decis
        end if
        
        !second non_dominate sort to clear non_dom2 files
        if(countit2==ndup_freq2 .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(562)
            open(unit=562, file="pareto_store2.txt",status="unknown")
            
            count_n = count_n2
            
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
            
            non_update = 1
            
            do k=1,count_n
                read(562,*)num_n_zero(k),j1,cost1(k),cost2(k)
                if (num_n_zero(k) > 0) then
                    read(562,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(562,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(562,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if
            end do
    
            call non_dom_sort2
    
            !write(523,'(i5)')count_n 
                
            count_n2=0
            count_n=0
            countit2=0
            
            non_update = 0
   
            close(562)
            open(unit=562, file="pareto_store2.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)    !,decis
            
        end if
        
        !third non_dominate sort to clear non_dom2 files
        if(countit3==ndup_freq3 .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(662)
            open(unit=662, file="pareto_store3.txt",status="unknown")
            
            count_n = count_n3
            
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
            
            non_update = 2
            
            do k=1,count_n
                read(662,*)num_n_zero(k),j1,cost1(k),cost2(k)
                if (num_n_zero(k) > 0) then
                    read(662,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(662,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(662,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if
            end do
    
            call non_dom_sort2
    
            !write(523,'(i5)')count_n 
                
            count_n3=0
            count_n=0
            countit3=0
            
            non_update = 0
   
            close(662)
            open(unit=662, file="pareto_store3.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)    !,decis
            
        end if
        
        !fourth non_dominate sort to clear non_dom2 files
        if(countit4==ndup_freq4 .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(762)
            open(unit=762, file="pareto_store4.txt",status="unknown")
            
            count_n = count_n4
            
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
            
            non_update = 3
            
            do k=1,count_n
                read(762,*)num_n_zero(k),j1,cost1(k),cost2(k)
                if (num_n_zero(k) > 0) then
                    read(762,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(762,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(762,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if
            end do
    
            call non_dom_sort2
    
            !write(523,'(i5)')count_n 
                
            count_n4=0
            count_n=0
            countit4=0
            
            non_update = 0
   
            close(762)
            open(unit=762, file="pareto_store4.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)    !,decis
            
        end if
        
        !fifth non_dominate sort to clear non_dom2 files
        if(countit5==ndup_freq5 .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(862)
            open(unit=862, file="pareto_store5.txt",status="unknown")
            
            count_n = count_n5
            
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
            
            non_update = 4
            
            do k=1,count_n
                read(862,*)num_n_zero(k),j1,cost1(k),cost2(k)
                if (num_n_zero(k) > 0) then
                    read(862,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(862,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(862,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if
            end do
    
            call non_dom_sort2
    
            !write(523,'(i5)')count_n 
                
            count_n5=0
            count_n=0
            countit5=0
            
            non_update = 0
   
            close(862)
            open(unit=862, file="pareto_store5.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)    !,decis
            
        end if
        
        !sixth non_dominate sort to clear non_dom2 files
        if(countit6==ndup_freq6 .or. dummy_time > maximum_allowable_runtime)then  ! non-dominate sort of offline storage
            close(962)
            open(unit=962, file="pareto_store6.txt",status="unknown")
            
            count_n = count_n6
            
            allocate(cost1(count_n))
            allocate(cost2(count_n))
            allocate(num_n_zero(count_n))
            allocate(e_trade(count_n,num_decision_pts))
            allocate(i_trade(count_n,num_decision_pts))
            allocate(c_bucket(count_n,num_decision_pts))
            
            non_update = 5
            
            do k=1,count_n
                read(962,*)num_n_zero(k),j1,cost1(k),cost2(k)
                if (num_n_zero(k) > 0) then
                    read(962,*)(e_trade(k,j),j=1,num_n_zero(k))
                    read(962,*)(i_trade(k,j),j=1,num_n_zero(k))
                    read(962,"(<num_n_zero(k)>(f20.2,',',:))") (c_bucket(k,j),j=1,num_n_zero(k))
                end if
            end do
    
            call non_dom_sort2
    
            !write(523,'(i5)')count_n 
                
            count_n6=0
            count_n=0
            countit6=0
            
            non_update = 0
   
            close(962)
            open(unit=962, file="pareto_store6.txt",status="unknown")
    
            deallocate(cost1,cost2,num_n_zero,e_trade,i_trade,c_bucket)    !,decis
            
        end if
        
        !taps out if gone for too long
        if (dummy_time > maximum_allowable_runtime) then
            exit
        end if
  
      end do  
  
!**************************************final non-dominate sort****************************************
    close(1062)
    open(unit=1062, file="pareto_store7.txt",status="unknown")
    !446 provides final pareto cost values
    open(unit=446, file="final pareto.csv",status="unknown")
    !555 provides the data behind the pareto cost values (country codes and value traded)
    open(unit=555, file="final pareto data.csv",status="unknown") 
    !447 provides final pareto cost values in text file
    open(unit=447, file="final pareto.txt",status="unknown")
    !556 provides data in text file
    open(unit=556, file="final pareto data.txt",status="unknown")
    
    countpf=0
    do  
        read(1062,*,iostat=io),dummy
        if(io>0)then
            print*,"something wrong"
            pause
            stop
        else if(io<0)then
            exit
        else 
            countpf=countpf+1
        end if
        
        !if there are trade links, don't need to keep the values
        if (dummy > 0) then
            read(1062,*)
            read(1062,*)
            read(1062,*)  
        end if
    end do      
    
    close(1062)  
    open(unit=1062, file="pareto_store7.txt",status="unknown")
    
    allocate(num_n_zero(countpf))
    allocate(e_trade(countpf,num_decision_pts))
    allocate(i_trade(countpf,num_decision_pts))
    allocate(c_bucket(countpf,num_decision_pts))
    allocate(cost1(countpf))
    allocate(cost2(countpf))

    do i=1,countpf
        if(i>2)then
            read(1062,*)num_n_zero(i),j1,temp1,temp2
            cost1(i)=temp1
            cost2(i)=temp2

            if (num_n_zero(i) > 0) then
                read(1062,*)(e_trade(i,k),k=1,num_n_zero(i))
                read(1062,*)(i_trade(i,k),k=1,num_n_zero(i))
                read(1062,"(<num_n_zero(i)>(f20.2,',',:))") (c_bucket(i,k),k=1,num_n_zero(i))
            end if
        else
            read(1062,*)num_n_zero(i),j1,temp1,temp2
            cost1(i)=temp1
            cost2(i)=temp2   
                     
            if (num_n_zero(i) > 0) then
                read(1062,*)(e_trade(i,j),j=1,num_n_zero(i))
                read(1062,*)(i_trade(i,j),j=1,num_n_zero(i))
                read(1062,"(<num_n_zero(i)>(f20.2,',',:))") (c_bucket(i,k),k=1,num_n_zero(i))
            end if
        end if 

    end do
    count_n=countpf
    maxitfin=1

    call non_dom_sort2
    
    call cpu_time(finish)
    
    !csv
    write(446,"(a7,',',f15.6)") "runtime",(finish-start)/(60*60)      !write out runtime in mins
    !txt
    write(447,"(I4,I4,I4,I4)") -1,-1,-1,-1 
    
    end program paco_dyn_const

