!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine non_dom_sort
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine non_dom_sort
! joanna szemis, january 2013
! this subroutine sorts out non-dominated solutions and writes them to a file 
  
  use ant_colony
  use ant_graph
  use non_dom
  use system_details
  use user
  
  real(8),allocatable :: temp_cost1(:), temp_cost2(:)           !temporary arrays
  !real(8) :: temp_purecost1(max_ant), temp_purecost2(max_ant)
  integer :: temp_ant
  integer, allocatable :: temp_trade(:,:),temp_dec(:) 
  real(8),allocatable :: temp_trade_quantity(:)
  integer, allocatable :: np(:)
  integer, allocatable :: hyp_point1(:),hyp_point2(:)
  
  integer :: i,j,k, temp_count, n !counters  
  integer :: hyp_count, num
  integer :: no_trade
  real :: hyp_area
 
  
  allocate(temp_trade(num_decision_pts,2),temp_dec(num_decision_pts),temp_trade_quantity(num_decision_pts))
  allocate(np(max_ant))
  allocate(temp_cost1(max_ant), temp_cost2(max_ant))
  allocate(hyp_point1(max_ant),hyp_point2(max_ant))
  

   
! initalise temporary arrays
  temp_cost1=0.0
  temp_cost2=0.0 

  temp_ant=0.0 
  temp_dec=0

  hyp_count = 0
  n=0
 
! determining non-dominated solutions 
  do i=1,max_ant
    np(i)=0.0

    do j=1,max_ant
        dom_less=0
        dom_equal=0
        dom_more=0
        
        !compares deficit cost
        if(ant(i)%cost(1)<ant(j)%cost(1))then
            dom_less=dom_less+1
        else if (ant(i)%cost(1)==ant(j)%cost(1)) then
            dom_equal=dom_equal+1
        else
            dom_more=dom_more+1
        end if

        !compares trade deviation cost
        if(ant(i)%cost(2)<ant(j)%cost(2))then
            dom_less=dom_less+1
        else if (ant(i)%cost(2)==ant(j)%cost(2)) then
            dom_equal=dom_equal+1
        else
            dom_more=dom_more+1
        end if


        if(dom_less==0.and.dom_equal/=n_obj)then !if j dominates i then domination counter
            np(i)=np(i)+1
            exit
        end if
    
    end do
   
    if(np(i)==0)then !if np=0, no individuals dominate i then i  belongs to the first rank
     
        count_n=count_n+1
        hyp_count=hyp_count+1
        n=1
        counter1=0
        no_trade=0
        temp_count=0   
        temp_dec=0  
              
    ! storing non-dominated solutions in array
        temp_cost1(hyp_count)=ant(i)%cost(1)
        temp_cost2(hyp_count)=ant(i)%cost(2) 
        temp_ant=i

      
        do k=1,num_decision_pts
            temp_dec(k)=ant(i)%decision(k)
            if (temp_dec(k) /= 1) then
                counter1 = counter1 + 1 
            end if 
        end do
        
        !store temporary non-zero solutions       
        do k = 1,num_decision_pts
            if (k == 1 ) then
                e_count = 1
                i_count = 1
            end if
            
            if (counter1 /= 0) then
                if (temp_dec(k) > 1) then
                    temp_count=temp_count+1
                    temp_dec(temp_count)=temp_dec(k)
                    temp_trade(temp_count,1)=e_index(e_count)
                    temp_trade(temp_count,2)=i_index(i_count)
                    if (temp_dec(k) < current_trade_edge(k)) then
                        temp_trade_quantity(temp_count)=bucket(k)*(temp_dec(k)-1)
                    else if (temp_dec(k) > current_trade_edge(k)) then
                        temp_trade_quantity(temp_count)=bucket(k)*(temp_dec(k)-2)
                    else 
                        temp_trade_quantity(temp_count)=current_trade_quantity(k)
                    end if
                end if
            end if
            
            i_count=i_count+1   
            
            !resets if i_count is greater than the number of countries         
            if (i_count == n_i_countries+1) then
                e_count = e_count + 1
                i_count = 1
            end if 
            
            if (e_count == n_e_countries + 1) then
                e_count = 1
            end if
   
        end do 
        
        !write out the decisions where a trade exists
        !note, written in different order in pareto optimal than in final pareto for graphing in excel reasons
        if (counter1 /= 0) then
            !number of active trade links, ant number, deficit, deviation
            write(123,"(i5,i5,2(f25.3))") counter1,temp_ant,temp_cost1(hyp_count),temp_cost2(hyp_count)
            !exporting country code
            write(123,"(<counter1>(i5,',',:))") (temp_trade(k,1),k=1,counter1)
            !importing country code
            write(123,"(<counter1>(i5,',',:))") (temp_trade(k,2),k=1,counter1)
            !writes total quantity of trade
            write(123,"(<counter1>(f20.2,',',:))") (temp_trade_quantity(k),k=1,counter1)
        else
            write(123,"(i5,i5,2(f25.3),x)") counter1,temp_ant,temp_cost1(hyp_count),temp_cost2(hyp_count)
        end if
        
        counter(count_n)=counter1
                
    end if
  end do
  
  !hypervolume calculation
  !first need to sort the solution to highest deficit to lowest deficit
  do i = 1,hyp_count
    
    num=1
    !allocates the rank of the random number used in the rearrange matrix
    do j = 1,hyp_count
        if (i /= j .and. temp_cost1(j) > temp_cost1(i)) then
            num = num + 1
        end if
    end do
    !redistributes the countries according to the rank of their random number
    hyp_point1(num)=temp_cost1(i)
    hyp_point2(num)=temp_cost2(i)
    
  end do
  
  !then calculate the area between maximum known deviation and deficit values
  hyp_area = 0
  do i = 1, hyp_count+1
    if(i > 1) then
        if(hyp_point1(i-1) < max_deficit .and. hyp_point2(i-1) < max_deviation) then

            if (i == hyp_count+1) then
                hyp_area = hyp_area + ((real(max_deviation)- real(hyp_point2(i-1)))*(real(max_deficit)-real(hyp_point1(i-1))))/1000000000000
                exit
            end if
            
            if (hyp_point2(i) > max_deviation ) then
                hyp_area = hyp_area + ((real(max_deviation)- real(hyp_point2(i-1)))*(real(max_deficit)-real(hyp_point1(i-1))))/1000000000000
                exit
            end if
            
            if (i /= hyp_count + 1) then
                !previous area + change in x multiplied by change in y from max deficit
                hyp_area =  hyp_area + ((real(hyp_point2(i))- real(hyp_point2(i-1)))*(real(max_deficit)-(real((hyp_point1(i-1))+real(hyp_point1(i))))/2))/1000000000000
            end if
            
        end if
    end if
  end do
  
  write(999,*) hyp_area
        
end subroutine