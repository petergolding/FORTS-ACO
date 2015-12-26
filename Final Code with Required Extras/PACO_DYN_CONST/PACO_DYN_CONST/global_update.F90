!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine global_update
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


subroutine global_update
! joanna szemis, january 2013
! this subroutine performs a global pheromone update 

  use ant_colony
  use ant_graph
  use para
  use system_details
  use user
    
  real(8),  dimension (n_obj)::best,best_2
  real(8),  dimension (n_obj)::best_s,best_2s
  real(8), dimension (n_obj) :: cumulative
  real(8), dimension (n_obj) :: average
  real(8)::dumm,best_er,dum1
  integer:: i,j,p,k,l
   
! initalising global update counter
  do i=1,num_decision_pts
    do j =1, path(i)%max_edge                            !number of buckets
        do k=1, n_obj
            path(i)%edge(j)%gb(k)=0.0
        end do
    end do
  end do
  
  if (it_count == 1) then
    overall_best = 1000000000000000000000000.0
  end if
  
  best=10000000000000000000000000.0
  best_2=1000000000000000000000000.0
 
  cumulative = 0.0
  average = 0.0
  
  !finding best solution for both objectives
  do i=1,max_ant
    do j=1,n_obj
        if(ant(i)%cost(j)<=best(j))then
            best(j)=ant(i)%cost(j)
            best_s(j)=i
        end if 
    end do
  end do
    
! finding second best solution for both objectives
  do i=1,max_ant
    do j=1,n_obj 
        if(ant(i)%cost(j)>best(j).and.ant(i)%cost(j)<=best_2(j))then
            best_2(j)=ant(i)%cost(j)
            best_2s(j)=i
        end if 
    end do
  end do
  
  
! assessing whether selected path is in best or second best solution 
  do i=1, max_ant
    do k=1,n_obj
        if(i==best_s(k))then
            do j=1,num_decision_pts
                !if best wasn't dynamically constrained
                if (dyn_const_store(k,j,1) /= 1) then
                    path(j)%edge(ant(i)%decision(j))%gb(k)=1.0
                end if
            end do
        end if
    end do
  end do
  
  do i=1, max_ant
    do k=1,n_obj
        if(i==best_2s(k))then
            do j=1,num_decision_pts
                if(path(j)%edge(ant(i)%decision(j))%gb(k)==1.0)then
                    !if 2nd best best wasn't dynamically constrained
                    if (dyn_const_store(k,j,2) /= 1) then
                        path(j)%edge(ant(i)%decision(j))%gb(k)=3.0
                    end if 
                else
                    !if 2nd best wasn't dynamically constrained
                    if (dyn_const_store(k,j,2) /= 1) then
                        path(j)%edge(ant(i)%decision(j))%gb(k)=2.0
                    end if
                end if
            end do
        end if
    end do
  end do

 
! performing global update for deficit 
  do i=1,num_decision_pts
    !only update pheremone if it's a valid trade link
    if (valid_trade(i) /= 0) then
        if (dyn_const_store(1,i,2) /= 1 .or. dyn_const_store(1,i,2) /= 1) then
            do j = 1, path(i)%max_edge    
                if(path(i)%edge(j)%gb(1)==1.0)then
                    path(i)%edge(j)%tau(1)=(1.0-rho)*path(i)%edge(j)%tau(1)+rho*best1
                else if(path(i)%edge(j)%gb(1)==2.0)then
                    path(i)%edge(j)%tau(1)=(1.0-rho)*path(i)%edge(j)%tau(1)+rho*best2
                else if(path(i)%edge(j)%gb(1)==3.0)then
                    path(i)%edge(j)%tau(1)=(1.0-rho)*path(i)%edge(j)%tau(1)+rho*best12
                else
                    path(i)%edge(j)%tau(1)=(1.0-rho)*path(i)%edge(j)%tau(1)+rho*0
                end if
            end do
        end if
    end if
  end do
  
  ! performing global update for deviation
  do i=1,num_decision_pts
    !only update pheremone if it's a valid trade link
    if (valid_trade(i) /= 0) then
        if (dyn_const_store(2,i,2) /= 1 .or. dyn_const_store(2,i,2) /= 1) then
            do j = 1, path(i)%max_edge     
                !if it's the best,second best or both
                if(path(i)%edge(j)%gb(2)==1.0)then
                    path(i)%edge(j)%tau(2)=(1.0-rho)*path(i)%edge(j)%tau(2)+rho*best1
                else if(path(i)%edge(j)%gb(2)==2.0)then
                    path(i)%edge(j)%tau(2)=(1.0-rho)*path(i)%edge(j)%tau(2)+rho*best2
                else if(path(i)%edge(j)%gb(2)==3.0)then
                    path(i)%edge(j)%tau(2)=(1.0-rho)*path(i)%edge(j)%tau(2)+rho*best12
                else
                    path(i)%edge(j)%tau(2)=(1.0-rho)*path(i)%edge(j)%tau(2)+rho*0
                end if
            end do
        end if
    end if

    
  end do

  
  !reset constraint values
  dyn_const_store=0
  
end subroutine