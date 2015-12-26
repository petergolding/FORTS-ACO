!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine randomise_country_order
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine randomise_country_order
    
    use aco_program_control
    use system_details
    use input_cost
    use ant_colony
    use ant_graph
    use user
    use para
    use lin_feedback_shift_reg 

    !stored on the stack, for large systems may need to use allocatable arrays
    real(8):: rearrange_matrix(n_countries,5+n_years)
    real(8):: resultant_matrix(n_countries,5+n_years)
    integer:: i,j,k,x,z,v,num,p_count
    integer:: temp_e_country(n_e_countries),temp_i_country(n_i_countries)
    real(8):: random_number
    real(8):: pheromone_level_obj1(n_years,n_e_countries,n_i_countries,n_buckets)
    real(8):: pheromone_level_obj2(n_years,n_e_countries,n_i_countries,n_buckets)
    
    p_count=1
    
    !store the current order of the pheremone
    do z = 1,n_years
        do i = 1,n_e_countries
            temp_e_country(i)=e_index(i)
            do j = 1,n_i_countries
                if (i ==1) then
                    temp_i_country(j)=i_index(j)
                end if
                
                do k = 1,n_buckets
                    pheromone_level_obj1(z,i,j,k)=path(p_count)%edge(k)%tau(1)
                    pheromone_level_obj2(z,i,j,k)=path(p_count)%edge(k)%tau(2)
                end do
                p_count=p_count+1
            end do
        end do
    end do

    !setup the rearrange_matrix ready for reorder
    do i = 1,n_countries
        !random number column
        random_number= lfsr258() 
        rearrange_matrix(i,1)=random_number
        !country index
        rearrange_matrix(i,2)=c_index(i)
        !country storage
        rearrange_matrix(i,3)=storage(i,1)
        !PDMSR values
        do j=1,n_years
            rearrange_matrix(i,3+j)=PDMSR(i,j)
        end do     
    end do
   
    !reorder the rearrange matrix
    do i = 1,n_countries
        
        num=1
        !allocates the rank of the random number used in the rearrange matrix
        do j = 1,n_countries
            if (i /= j .and. rearrange_matrix(j,1) < rearrange_matrix(i,1)) then
                num = num + 1
            end if
        end do
        !redistributes the countries according to the rank of their random number
        do k=1,3+n_years
            resultant_matrix(num,k)=rearrange_matrix(i,k)
        end do
        
     end do
     
     !reassign all matrices
     do i = 1,n_countries
        !country index
        c_index(i)=resultant_matrix(i,2)
        !storage values
        storage(i,1)=resultant_matrix(i,3)
        !PDMSR values
        do j =1,n_years
            PDMSR(i,j)=resultant_matrix(i,3+j)
        end do
     end do
    
    trade_lim=0.0
    n_i_countries=0
    n_e_countries=0
        
    !read in country PDMSR matrix
    !must be reassigned according to the new order of countries
    do i = 1,n_countries
        countries = 0            
        do j = 1,n_years
            if(PDMSR(i,j) + storage(i,1) >= 0) then
                countries = countries + 1
            end if
            
            if (abs(PDMSR(i,j)+storage(i,1))>trade_lim(i)) then
                trade_lim(i)=PDMSR(i,j)+storage(i,1)
            end if
        end do  
        
        if (countries == n_years) then
            exp_or_imp(i) = 1               !1 represents exporting country
            n_e_countries = n_e_countries + 1
            e_index(n_e_countries)=c_index(i)
        else if (countries == 0) then
            exp_or_imp(i) = 0               !0 represents importing country
            n_i_countries = n_i_countries + 1
            i_index(n_i_countries)=c_index(i)
        else
            exp_or_imp(i) = 2               !2 represents importing and exporting country
            n_i_countries = n_i_countries + 1
            n_e_countries = n_e_countries + 1
            e_index(n_e_countries)=c_index(i)
            i_index(n_i_countries)=c_index(i)
        end if
        
    end do
    
    p_count=1
    !redistribute known pheromone values
    do v=1,n_years
        do i = 1,n_e_countries
            do j = 1,n_e_countries
                if (e_index(i)==temp_e_country(j)) then
                    do k = 1,n_i_countries
                        do x = 1,n_i_countries
                            if (i_index(k) == temp_i_country(x)) then
                                do z = 1,n_buckets
                                    path(p_count)%edge(z)%tau(1)=pheromone_level_obj1(v,j,x,z)
                                    path(p_count)%edge(z)%tau(2)=pheromone_level_obj2(v,j,x,z)
                                end do
                                p_count = p_count + 1
                            end if
                        end do
                    end do
                end if
            end do
        end do
    end do
 
    !redefine initialise ant graph
    b_count = 1
    bucket = 0
    
    do k=1,n_years
        do x = 1,n_countries
            if (exp_or_imp(x) == 1 .or. exp_or_imp(x) == 2) then          !if the country being investigated is an exporting country then
                do m = 1,n_countries
                    if (exp_or_imp(m) == 0 .or. exp_or_imp(m) == 2) then  !if the country being investigated is an importing country 
                        !trade can occur so define the variables
                        num_edges(b_count)=n_buckets
                        path(b_count)%max_edge=n_buckets
                        !if using total import required and export available to define bucket size
                        bucket(b_count)=min(abs(trade_lim(x)),abs(trade_lim(m)))/(real(n_buckets)-2)
                        
                        b_count=b_count+1 
                       
                    end if 
                end do
            end if
        end do
    end do
 
    !redefine allowable trade links
    valid_trade=0
    current_trade_quantity=0
    current_trade_edge=0
    
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
                        
                    end if
                end do
            end if
        end do
    end do
            
    !reset to zero so that sorts are consistent
    dyn_const=0    

end subroutine 