!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine prob_determination(i,j)
! subroutine evaluate_objective(solution, val, pure_cost, penalty_cost, penalty)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine prob_determination(i,j)

    use ant_graph
    use ant_colony
    use para
    use system_details
    use user
    ! use r_num
  
    real(8):: tot_prob,dummy,prob,dum,dum1,dum2,dum3
    integer:: i,j,k,m,x,p
    
    
    !add a years worth of decisions if year /= 1
    if (y_count /= 1 .and. j==1) then
        add_year = add_year + max_path
    end if   
        
    !builds the constraints for each year
    !one exporting and one importing country trade at the minimum of the two ir or ea values
    !dynamically changes with time as countries trade
    trade_con(j)=min(abs(e_trade_sort(e_count,y_count)),abs(i_trade_sort(i_count,y_count)))
       
    if (e_trade_sort(e_count,y_count)<0) then
        heavens=1
    end if
    
    !calculate approriate value of path(j)%max_edge for dynamic constraining
    !if as a result of previous trades can no longer trade at max_edge    
    if (trade_con(j)/bucket(j)+2 < path(j+add_year)%max_edge) then
        !minus one to allow for rounding that occurs
        if (trade_con(j) >= current_trade_quantity(j)-1) then
            upper_lim= trade_con(j)/bucket(j)+2
        else
            !current trade quantity still available to be traded
            upper_lim= trade_con(j)/bucket(j)+1
        end if
        
        !used so that if dynamically constrained to 1 the pheremone on this path is maintained
!        if (upper_lim == 1) then
            dyn_const(j+add_year) = 1
!        end if
    !else can trade at max_edge
    else
        upper_lim=path(j+add_year)%max_edge
    end if
 
    
    tot_prob=0.0
    do k=1,upper_lim
        dummy=0.0
        !do m=1,n_obj
            dummy=dummy+ant(i)%pk(1)*path(j+add_year)%edge(k)%tau(1)
            dummy=dummy+ant(i)%pk(2)*path(j+add_year)%edge(k)%tau(2)
            !remove as heuristics not currently considered, slows code
!            dummy2=dummy2+ant(i)%pk2(m)*path(j)%edge(k)%heu(m) 
        !end do
        path(j+add_year)%edge(k)%prob=(dummy**alpha)!*(dummy2**beta) !remove as heuristics not currently considered
        
        !sums probability for scaling purposes
        tot_prob=tot_prob+path(j+add_year)%edge(k)%prob
    
    end do
    
    !scales all of the probability values so that they sum to 1
    do k=1,upper_lim
        path(j+add_year)%edge(k)%prob=path(j+add_year)%edge(k)%prob/tot_prob      !ant(i)%con(k)* removed
    end do

!    dummy2=0
     
end subroutine