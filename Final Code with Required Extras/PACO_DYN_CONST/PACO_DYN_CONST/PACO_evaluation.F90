!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine evaluation(k)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine evaluation(k)
! joanna szemis, january 2013
! this subroutine calls and stores values of objective function and penalties   

    use ant_colony
    use input_cost
    use ant_graph
    use system_details
    use user
  
    integer:: i,j,k,p
    real(8)::p1,p2,dum1,dum
    real(8):: a,c,d
  
    integer :: num_it, num_ant,num_carries ! input

    !objective 1, minimise the deficit
    !sum the negative values in the trade_sort matrix
    do j = 1,n_i_countries*n_years
        if (ant(k)%deficit_cost(j) < 0) then     
            ant(k)%cost(1)=ant(k)%cost(1)+abs(ant(k)%deficit_cost(j))
        end if
    end do
    
    !objective 2, minimise the deviation from current trade quantities
    cost_count = 1

    do j = 1,num_decision_pts
        !if a trade occurs (edge 1 represents zero trade)
        if (ant(k)%decision(j) > 1 .or. current_trade_quantity(cost_count) > 0) then  
            !subtract current trade quantity from selected decision 
            if (ant(k)%decision(j)<current_trade_edge(cost_count)) then
                ant(k)%cost(2)=ant(k)%cost(2)+ABS((ant(k)%decision(j)-1)*bucket(cost_count)-current_trade_quantity(cost_count))
            elseif (ant(k)%decision(j)>current_trade_edge(cost_count)) then
                ant(k)%cost(2)=ant(k)%cost(2)+ABS((ant(k)%decision(j)-2)*bucket(cost_count)-current_trade_quantity(cost_count))
            end if  
        end if
        !bucket is allocated at max_path as remains the same across all years
        !therefore need to reset when cost_count = max_path
        if (cost_count == n_i_countries*n_e_countries) then
            cost_count = 1
        else
            cost_count=cost_count+1
        end if
        
    end do
  
  !stores best set of dynamically constrained variales for each objectives
  if (k == 1) then
    iteration_best(1,1) = max_deficit  
    iteration_best(2,1) = max_deviation
  end if
    
  if (ant(k)%cost(1)< iteration_best(1,1)) then
    iteration_best(1,2) = iteration_best(1,1)
    iteration_best(1,1) = ant(k)%cost(1)
    do i = 1,num_decision_pts
        dyn_const_store(1,i,2) = dyn_const_store(1,i,1)
        dyn_const_store(1,i,1) = dyn_const(i)
    end do
  end if 
  
  if (ant(k)%cost(2)< iteration_best(2,1)) then
    iteration_best(2,2) = iteration_best(2,1)
    iteration_best(2,1)=ant(k)%cost(2)
    do i = 1,num_decision_pts
        dyn_const_store(2,i,2) = dyn_const_store(2,i,1)
        dyn_const_store(2,i,1) = dyn_const(i)
    end do
  end if 
  
  !resets add_year to zero as will be going back to year 1 after evaluation
  add_year=0
  dyn_const = 0
    
end subroutine