!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine graph_details(temp3)
! subroutine evaluate_objective(solution, val, pure_cost, penalty_cost, penalty)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine graph_details(num_decision_pts, num_edges)
    use user
    ! this is a user-defined routine that passes the grahp details to the aco routines
    
    integer :: num_decision_pts               ! number of decision points
    integer, dimension(n_years*n_countries*n_countries) :: num_edges      ! number of edges for each decision point
    integer :: i, j                           ! counters     

!    num_decision_pts = n_countries*n_countries*n_years
	
!    do i = 1, num_decision_pts
!        num_edges(i) = n_buckets                    ! how many buckets
!    end do

end subroutine graph_details

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine heu_calc(i,j)
    use ant_graph
    use ant_colony
    use input_cost
    use system_details
  
    integer::i,j,k,p,q
    real(8)::p1,p2,dum,dum1
  
    do k=1,n_obj
        do p=1,n_buckets
            if (bucket(j)*p > min_ship_size .and. trade_con(j) > min_ship_size) then
                path(j)%edge(p)%heu(k)=trade_con(j)/(bucket(j)*p)  !prefers solutions greater than 1 carrier if possible
            else if (trade_con(j) < min_ship_size .and. p == n_buckets) then
                path(j)%edge(p)%heu(k)=best1 ! if not, then prefers max_edge (1 trade link)
            else
                path(j)%edge(p)%heu(k)=0.0  !otherwise, no preference added
            end if
        end do
    end do

end subroutine

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine  local_update(i,j)
    use ant_graph
    use para
    use ant_colony
    use system_details
  
    real(8)::dum,dumm,dum1,dum2
    integer::i,j,l

    do l=1,n_obj
    
        path(j+add_year)%edge(ant(i)%decision(j))%tau(l)=path(j+add_year)%edge(ant(i)%decision(j))%tau(l)+path(j+add_year)%edge(ant(i)%decision(j))%tau(l)
        dumm=ant(i)%decision(j)                             !has an upper limit of max_path
        dum=path(j+add_year)%edge(ant(i)%decision(j))%tau(l)
    
    end do

end subroutine

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine initial_tau
    use ant_graph
    use input_cost
  
    integer, dimension(100) :: init_d, dec
    real(8), dimension(2)::sum_d
    integer, dimension(2,100)::greedy_st
    !real(8), dimension(100)::!store_d!,con
    real(8), dimension(2,100)::greedy_dist,con,store_d
    real(8)::cheap,p1,p2
    integer::i,j,k,m,n
    tau_0=1.0

end subroutine initial_tau