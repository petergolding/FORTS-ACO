!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine initialise_aco_parameters
! subroutine initialise_ant_graph 
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine initialise_paco_parameters
  
! this subroutine initialises aco parameters
  
    use aco_program_control
    use system_details
    use input_cost
    use ant_colony
    use ant_graph
    use user
    use para
    ! use r_num
    use lin_feedback_shift_reg ! random number generator
    character(30), dimension(4)::input_file
    integer :: countries
    integer :: dum1,dum2
    integer :: flag
    integer :: p
    integer :: min_trade_diff,near_current_trade_edge
    integer :: nearest_edge_pref
  
    !reading parameters input files
    open(unit=111,file="ACO_input.txt",status="unknown")
  
        read(111,*) max_it	! max number of iterations
        read(111,*) max_ant	! max number of ants
        read(111,*) alpha		! initial pheromone
        read(111,*) beta     ! alpha - pheromone importance factor
        read(111,*) rho       ! phermone evaporation
        read(111,*) seed_ran_1, seed_ran_2, seed_ran_3, seed_ran_4, seed_ran_5 !random seed values
        read(111,*) best1   !first best update
        read(111,*) best2   !second best update
        read(111,*) best12  !value if both best and 2nd best
        read(111,*) nearest_edge_pref
        read(111,*) ndup_freq ! frequency of nondom update
        read(111,*) ndup_freq2  !frequrency of second nondom update
        read(111,*) ndup_freq3 ! frequency of nondom update
        read(111,*) ndup_freq4  !frequrency of second nondom update
        read(111,*) ndup_freq5 ! frequency of nondom update
        read(111,*) ndup_freq6  !frequrency of second nondom update
        read(111,*) maximum_allowable_runtime
        
    close(111)
    
    open(unit=111, file="FORTS_input.txt",status="unknown")
    
        read(111,*) n_obj		    !number of objectives
        read(111,*) n_countries     !number of countries
        read(111,*) n_years         !number of years of simulation
        read(111,*) n_buckets       !number of buckets
        read(111,*) max_deficit
        read(111,*) max_deviation
        read(111,*) input_file(1)
        read(111,*) input_file(2)
        read(111,*) input_file(3)
        
    close(111)                                                        
    
    allocate(PDMSR(n_countries,n_years))
    allocate(trade_lim(n_countries))                
    allocate(storage(n_countries,n_years))
    allocate(exp_or_imp(n_countries))
    allocate(c_names(n_countries))
    allocate(e_names(n_countries))
    allocate(i_names(n_countries))
    allocate(c_index(n_countries))
    allocate(e_index(n_countries))
    allocate(i_index(n_countries))
     
    
    trade_lim = -1000000000000.0              !lower than any importing countries requirement
    countries = 0
    n_e_countries = 0
    n_i_countries = 0
    
    !read in country names and associated number
    open(unit=222, file=input_file(1),status='unknown')
        do i = 1,n_countries
            read(222,*)c_index(i),c_names(i)
        end do
    close(222)
    
    !read in country initial storage values
    open(unit=222,file=input_file(3),status='unknown')
        do i = 1,n_countries
            read(222,*)storage(i,1)
        end do  
    close(222)
    
    !read in country PDMSR matrix
    open(unit=222, file=input_file(2),status='unknown')
        do i = 1,n_countries
            countries = 0
            read(222,*)(PDMSR(i,j),j=1,n_years)               
            do j = 1,n_years
                if (j == 1) then
                    !if countries are exporting
                    if(PDMSR(i,j) + storage(i,1) >= 0) then
                        countries = countries + 1
                    end if
                    !determine maximum import required or export available value 
                    !used in determining bucket size
                    if (abs(PDMSR(i,j)+storage(i,1))>trade_lim(i)) then
                        trade_lim(i)=PDMSR(i,j)+storage(i,1)
                    end if
                else
                    if(PDMSR(i,j) >= 0) then
                        countries = countries + 1
                    end if
                    
                    if (abs(PDMSR(i,j))>trade_lim(i)) then
                        trade_lim(i)=abs(PDMSR(i,j))
                    end if
                end if
            end do  
            
            if (countries == n_years) then
                exp_or_imp(i) = 1               !1 represents exporting country
                n_e_countries = n_e_countries + 1
                e_names(n_e_countries)=c_names(i)
                e_index(n_e_countries)=c_index(i)
            else if (countries == 0) then
                exp_or_imp(i) = 0               !0 represents importing country
                n_i_countries = n_i_countries + 1
                i_names(n_i_countries)=c_names(i)
                i_index(n_i_countries)=c_index(i)
            else
                exp_or_imp(i) = 2               !2 represents importing and exporting country
                n_i_countries = n_i_countries + 1
                n_e_countries = n_e_countries + 1
                e_names(n_e_countries)=c_names(i)
                e_index(n_e_countries)=c_index(i)
                i_names(n_i_countries)=c_names(i)
                i_index(n_i_countries)=c_index(i)
            end if
            
        end do
    close(222)
    
    !write exporting and importing country codes to a file
    open(unit=222,file='exporting country codes.csv',status='unknown')
        do i = 1, n_e_countries
            write(222,"(I3,',',A40)")e_index(i),e_names(i)
        end do
    close(222)
    
    open(unit=222,file='importing country codes.csv',status='unknown')
        do i = 1, n_i_countries
            write(222,"(I3,',',A40)")i_index(i),i_names(i)
        end do
    close(222)  
    
    max_path = n_e_countries*n_i_countries
    dec_mat = n_countries*n_years
    num_decision_pts = n_e_countries*n_i_countries*n_years 
    write_count = 0   
                     
    allocate(e_trade_sort(n_e_countries,n_years))
    allocate(i_trade_sort(n_i_countries,n_years))                                             
    allocate(bucket(num_decision_pts))
    allocate(dyn_const(num_decision_pts))
    allocate(dyn_const_store(2,num_decision_pts,2))
    allocate(num_edges(n_years*n_countries*n_countries))              
    allocate(trade_sort(n_countries,n_years))          
    allocate(trade_con(max_path))
    allocate(counter(ndup_freq*max_ant))
    allocate(valid_trade(max_path))
    allocate(current_trade_quantity(max_path))
    allocate(current_trade_edge(max_path))
    
    allocate(ant(max_ant)) 
    
    dyn_const=0

    call initialise_ant_graph   ! calls subroutine to store system details
    
    do i = 1,max_ant
        allocate(ant(i)%decision(num_decision_pts))         
        allocate(ant(i)%deficit_cost(n_i_countries*n_years))                            
        allocate(ant(i)%random(max_path))                   
        allocate(ant(i)%cost(n_obj))                    
    end do

    !calculates the current trade links that must be abided by
    !needed only if constraining by preset conditions
    open(unit=222,file="trade_links.txt",status='unknown')
    
    v_count=0
    flag=0
    
    do while (flag == 0)
        v_count = v_count+1
        !exporting country, importing country, value traded
        read(222,*) known_trade_links(v_count,1),known_trade_links(v_count,2),known_trade_links(v_count,3)
        if (v_count > 1) then
            !if the value is the same as previous, then don't need to store in array
            if (known_trade_links(v_count,1) == known_trade_links(v_count-1,1) .and. known_trade_links(v_count,2) == known_trade_links(v_count-1,2)) then
                !sum the values to be included in total amount traded
                known_trade_links(v_count-1,3)=known_trade_links(v_count-1,3)+known_trade_links(v_count,3)
                v_count = v_count - 1
            end if
            !if one of the values is zero, then unspecified country, therefore don't need in array
            if (known_trade_links(v_count,1) == 0 .or. known_trade_links(v_count,2)== 0) then
                v_count = v_count - 1
            end if
        end if
        !-1 -1 -1 marks the end of the file (MUST BE IN TRADE_LINKS FILE)
        if (known_trade_links(v_count,1) == -1) then
            v_count = v_count - 1
            flag = 1
        end if
    end do
    
    close(222)
    
    valid_trade=0
    current_trade_quantity=0
    current_trade_edge=0
    min_trade_diff = 1000000000
    near_current_trade_egde = 0
    
    open(unit=129,file="nearest_trade_edge.csv",status="unknown")
    
    !stores the valid trade links in a file used in prob_determination
    !zero marks an invalid trade link, 1 a valid trade link
    do i = 1,v_count
        do k=1,n_e_countries
            if (known_trade_links(i,1) == e_index(k)) then
                do j = 1,n_i_countries
                    if (known_trade_links(i,2) == i_index(j)) then
                        valid_trade(j+(k-1)*n_i_countries)=1
                        current_trade_quantity(j+(k-1)*n_i_countries) = known_trade_links(i,3)
                        
                        !store decision edge relating to current trade
                        do m=1,n_buckets
                            if ((m-1)*bucket(j+(k-1)*n_i_countries) >= current_trade_quantity(j+(k-1)*n_i_countries)) then
                                current_trade_edge(j+(k-1)*n_i_countries)=m
                                exit
                            end if
                        end do
                        
                        !bias current trade quantity
                        do p = 1,n_buckets
                            if (p==current_trade_edge(j+(k-1)*n_i_countries)) then
                                near_current_trade_edge = p
                                min_trade_diff=ABS((p-1)*bucket(j+(k-1)*n_i_countries) - current_trade_quantity(j+(k-1)*n_i_countries))
                            end if
                        end do
                        
                        write(129,"(I4,',',I4,',',I4,',',F20.2,',',F20.2)") e_index(k),i_index(j),near_current_trade_edge,current_trade_quantity(j+(k-1)*n_i_countries),(near_current_trade_edge-1)*bucket(j+(k-1)*n_i_countries)
                        path(j+(k-1)*n_i_countries)%edge(near_current_trade_edge)%tau(2) = tau_0*nearest_edge_pref
                        
                        min_trade_diff = 1000000000
                        near_current_trade_egde = 0
                    end if
                end do
            end if
        end do
    end do
    
    close(129)
    
    deallocate(e_names,i_names,c_names)

end subroutine

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine initialise_ant_graph 
! joanna szemis, january 2013
! this subroutine stores system details 
  
  use ant_graph     
  use ant_colony
  use system_details
  use user
    
  integer :: i,j,k,l,p,x,temp3
  real(8) :: small_val=0.00000001 ,dum

    !call graph_details(num_decision_pts, num_edges)
    !not used as graphing function not employed
  
!    call initial_tau    !sets tau_0 to 1. not really needed
    tau_0 = 1.0
    allocate(path(num_decision_pts))
    
    open(unit=222,file="trade bucket values.csv",status='unknown')
    
    b_count = 1
    do k=1,n_years
        do x = 1,n_countries
            !if the country being investigated is an exporting country then
            if (exp_or_imp(x) == 1 .or. exp_or_imp(x) == 2) then          
                do m = 1,n_countries
                   !if the country being investigated is an importing country  
                   if (exp_or_imp(m)== 0 .or. exp_or_imp(m) == 2) then  
                        !trade can occur so define the variables
                        num_edges(b_count)=n_buckets
                        path(b_count)%max_edge=n_buckets
                        
                        !minus 2 as one is used up as the floating current trade edge value
                        bucket(b_count)=min(abs(trade_lim(x)),abs(trade_lim(m)))/(real(n_buckets)-2)
                        
                        !write to a file and store for comparitive reasons
                        write(222,"(A40,',',A40,',',F20.3)")c_names(x),c_names(m),bucket(b_count)
                        
                        allocate(path(b_count)%edge(path(b_count)%max_edge))
                        
                        do p=1,path(b_count)%max_edge
                        
                            allocate(path(b_count)%edge(p)%tau(n_obj))
                            allocate(path(b_count)%edge(p)%heu(n_obj))
                            !path(j)%edge(k)%heu = 0.5 
                            do l=1,n_obj
                                path(b_count)%edge(p)%tau(l) = tau_0
                            end do
                        end do
                        
                        b_count=b_count+1 
                       
                    end if
                    
                end do
            end if
        end do
    end do
    
    close(222)

end subroutine