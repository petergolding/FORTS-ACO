!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine non_dom_sort2
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine non_dom_sort2
! joanna semis, january 2013
! this subroutine sorts out non-dominated solutions in offline storage 
    
  use ant_colony
  use ant_graph
  use non_dom
  use system_details
  use user
  
  real(8),allocatable :: temp_cost1(:), temp_cost2(:)         ! tempprary arrays
  integer,allocatable :: temp_ant(:)            !,temp_dec(:,:)
  real(8),allocatable :: temp2_cost1(:), temp2_cost2(:)
  integer,allocatable :: temp2_ant(:)           !,temp2_dec(:)
  integer :: i,j,k,p1,q1,m1,temp_count, n ,n2,dum          ! counters
  integer, allocatable :: temp_e_trade(:,:),temp2_e_trade(:)
  integer, allocatable :: temp_i_trade(:,:),temp2_i_trade(:)
  real(8),allocatable :: temp_c_bucket(:,:),temp2_c_bucket(:)
  integer, allocatable :: np(:)
  
  allocate(temp_cost1(count_n), temp_cost2(count_n))
  allocate(temp_ant(count_n))                   !,temp_dec(count_n,num_decision_pts)
  allocate(temp2_cost1(count_n), temp2_cost2(count_n))
  allocate(temp2_ant(count_n))                  !,temp2_dec(num_decision_pts)
  allocate(temp_e_trade(count_n,num_decision_pts),temp2_e_trade(num_decision_pts))
  allocate(temp_i_trade(count_n,num_decision_pts),temp2_i_trade(num_decision_pts))
  allocate(temp_c_bucket(count_n,num_decision_pts),temp2_c_bucket(num_decision_pts))
  allocate(np(count_n)) 
  
! initalising temporary arrays 
  temp_cost1=0.0
  temp_cost2=0.0 
  !temp_purecost1=0.0
  !temp_purecost2=0.0
  temp_ant=0.0 
  temp_dec=0
  
  n=0   
    
!  sp_count=0.0
!  f_count=0
!  f=0.0
!  front=1
!  obj_rank=0
  
! non-dominate sort begins here 
  do i=1,count_n
    np(i)=0.0
!    sp(i)=0.0
    do j=1,count_n
        dom_less=0
        dom_equal=0
        dom_more=0
        
        !compares deficit
        if(cost1(i)<cost1(j))then
            dom_less=dom_less+1
        else if (cost1(i)==cost1(j)) then
            dom_equal=dom_equal+1
        else
            dom_more=dom_more+1
        end if
        
        !compares deviation
        if(cost2(i)<cost2(j))then
            dom_less=dom_less+1
        else if (cost2(i)==cost2(j)) then
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
        n=n+1
        if (non_update == 0) then
            count_n2=count_n2 + 1
        elseif (non_update == 1) then
            count_n3=count_n3 + 1
        elseif (non_update == 2) then
            count_n4=count_n4 + 1
        elseif (non_update == 3) then
            count_n5=count_n5 + 1
        elseif (non_update == 4) then
            count_n6=count_n6 + 1    
        end if
        
    ! storing non-dominated solutions in array
        temp_cost1(n)=cost1(i)
        temp_cost2(n)=cost2(i)
        temp_ant(n)=i
        
        do k=1,num_n_zero(i)
            temp_e_trade(n,k)=e_trade(i,k)
            temp_i_trade(n,k)=i_trade(i,k)
            temp_c_bucket(n,k)=c_bucket(i,k) 
        end do
        
        num_n_zero(n)=num_n_zero(i)         !changes the number for the pareto front sort
        
    end if
  end do

  ! sorting identical solutions and removing from offline storage
  if(maxitfin==0)then
   
    temp2_cost1=0.0
    temp2_cost2=0.0 
    temp2_ant=0.0 
    temp2_dec=0
      
    n1=0
    dum=0
    do i=1,n    
        if(i==1)then
            n1=n1+1
            temp2_cost1(n1)=temp_cost1(i)
            temp2_cost2(n1)=temp_cost2(i)
            temp2_ant(n1)=temp_ant(i)
            
            do k=1,num_n_zero(i)
                temp2_e_trade(k) = temp_e_trade(i,k)
                temp2_i_trade(k) = temp_i_trade(i,k)
                temp2_c_bucket(k) = temp_c_bucket(i,k)
            end do
            
            if (non_update == 0) then
                if (num_n_zero(n1) /= 0) then
                    write(562,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(562,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(562,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(562,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(562,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)        
                end if
            elseif (non_update == 1) then
                if (num_n_zero(n1) /= 0) then
                    write(662,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(662,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(662,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(662,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(662,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                end if
            elseif (non_update == 2) then
                if (num_n_zero(n1) /= 0) then
                    write(762,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(762,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(762,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(762,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(662,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                end if
            elseif (non_update == 3) then
                if (num_n_zero(n1) /= 0) then
                    write(862,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(862,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(862,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(862,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(862,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                end if  
            elseif (non_update == 4) then
                if (num_n_zero(n1) /= 0) then
                    write(962,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(962,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(962,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(962,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(862,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                end if
            elseif (non_update == 5) then
                if (num_n_zero(n1) /= 0) then
                    write(1062,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                    write(1062,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(1062,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(1062,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    write(1062,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                end if   
            end if  
        else
        
            dum=1
            
            if(dum==1)then
                n1=n1+1
                temp2_cost1(n1)=temp_cost1(i)
                temp2_cost2(n1)=temp_cost2(i)
                temp2_ant(n1)=temp_ant(i)
              
                do k=1,num_n_zero(i)
                    temp2_e_trade(k) = temp_e_trade(i,k)
                    temp2_i_trade(k) = temp_i_trade(i,k)
                    temp2_c_bucket(k) = temp_c_bucket(i,k)
                end do
                
                if (non_update == 0) then
                    if (num_n_zero(n1) /= 0) then
                        write(562,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(562,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(562,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(562,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(562,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)        
                    end if
                elseif (non_update == 1) then
                    if (num_n_zero(n1) /= 0) then
                        write(662,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(662,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(662,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(662,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(662,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                    end if
                elseif (non_update == 2) then
                    if (num_n_zero(n1) /= 0) then
                        write(762,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(762,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(762,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(762,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(662,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                    end if
                elseif (non_update == 3) then
                    if (num_n_zero(n1) /= 0) then
                        write(862,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(862,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(862,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(862,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(862,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                    end if  
                elseif (non_update == 4) then
                    if (num_n_zero(n1) /= 0) then
                        write(962,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(962,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(962,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(962,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(862,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                    end if
                elseif (non_update == 5) then
                    if (num_n_zero(n1) /= 0) then
                        write(1062,"(i5,x,i5,x,f25.3,x,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)
                        write(1062,"(<num_n_zero(n1)>(i5,',',:))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                        write(1062,"(<num_n_zero(n1)>(i5,',',:))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                        write(1062,"(<num_n_zero(n1)>(f20.2,',',:))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                    else
                        write(1062,"(i5,x,i5,x,f25.3,x,f25.3,x)") num_n_zero(n1),temp2_ant(n1),temp2_cost1(n1),temp2_cost2(n1)      
                    end if   
                end if  
            end if
        end if
    end do

  else 
    ! final pareto front
    temp2_cost1=0.0
    temp2_cost2=0.0 
    temp2_ant=0.0
    
    n1=0
    dum=0
    do i=1,n
        n1=n1+1
        
        if(i==1)then
            
            temp2_cost1(n1)=temp_cost1(i)
            temp2_cost2(n1)=temp_cost2(i)
            temp2_ant(n1)=temp_ant(i)
            
            do k=1,num_n_zero(n1)                   !used to be max_path
                temp2_e_trade(k) = temp_e_trade(i,k)
                temp2_i_trade(k) = temp_i_trade(i,k)
                temp2_c_bucket(k) = temp_c_bucket(i,k)
                
                if(temp2_e_trade(k) == 0) then
                    stop
                end if
                
            end do
            !csv
            write(555,"(a8,i3)") "solution", i
            
            !note the reorder of the cost values (ie, now deviation then deficit)
            if (num_n_zero(n1) /= 0) then
                !CSV
                write(446,"(a8,i4,',',i5,',',i5,',',f25.3,',',f25.3)") "solution",n1,num_n_zero(n1),temp2_ant(n1),temp2_cost2(n1)/1000000,temp2_cost1(n1)/1000000
                write(555,"(a17,',',<num_n_zero(n1)>(i5,',',:))") "exporting country",(temp2_e_trade(j),j=1,num_n_zero(n1))
                write(555,"(a17,',',<num_n_zero(n1)>(i5,',',:))") "importing country",(temp2_i_trade(j),j=1,num_n_zero(n1))
                write(555,"(a15,',',<num_n_zero(n1)>(f20.2,',',:))") "quantity traded",(temp2_c_bucket(j),j=1,num_n_zero(n1))
                !txt
                write(447,"(i10,i10,f25.3,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost2(n1)/1000000,temp2_cost1(n1)/1000000
                write(556,"(<num_n_zero(n1)>(i5))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                write(556,"(<num_n_zero(n1)>(i5))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                write(556,"(<num_n_zero(n1)>(f20.2))")(temp2_c_bucket(j),j=1,num_n_zero(n1))
            else
                !csv
                write(446,"(a8,i4,',',i5,',',i5,',',:,2(f25.3,',',:),x)") "solution", n1,num_n_zero(n1),temp2_ant(n1),temp2_cost2(i)/1000000,temp2_cost1(n1)/1000000
                write(555,"(a19)") "no trades were made"
                !txt
                write(447,"(i10,i10,2(f25.3),x)") num_n_zero(n1),temp2_ant(n1),temp2_cost2(i)/1000000,temp2_cost1(n1)/1000000
                write(556,"(a19)") "no trades were made"
            end if
            
        else
            do j=1,n1
                if(temp_cost1(i)==temp2_cost1(j).and.temp_cost2(i)==temp2_cost2(j))then
                    dum=0.0
                    exit
                else
                    dum=1
                end if
            end do
            
            if(dum==1)then
                temp2_cost1(n1)=temp_cost1(i)
                temp2_cost2(n1)=temp_cost2(i)
                !temp2_purecost1(n1)=temp_purecost1(i)
                !temp2_purecost2(n1)=temp_purecost2(i)
                temp2_ant(n1)=temp_ant(i)
            
                do k=1,num_n_zero(n1) 
                    temp2_e_trade(k) = temp_e_trade(i,k)
                    temp2_i_trade(k) = temp_i_trade(i,k)
                    temp2_c_bucket(k) = temp_c_bucket(i,k)
                end do
                !csv
                write(555,"(a8,i3)") "solution", i
                
                !note the reorder of the cost values (ie, now deviation then deficit)
                if (num_n_zero(n1) /= 0) then
                    !csv
                    write(446,"(a8,i4,',',i5,',',i5,',',f25.3,',',f25.3)") "solution",n1,num_n_zero(n1),temp2_ant(n1),temp2_cost2(n1)/1000000,temp2_cost1(n1)/1000000
                    write(555,"(a17,',',<num_n_zero(n1)>(i5,',',:))") "exporting country",(temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(555,"(a17,',',<num_n_zero(n1)>(i5,',',:))") "importing country",(temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(555,"(a15,',',<num_n_zero(n1)>(f20.2,',',:))") "quantitiy traded",(temp2_c_bucket(j),j=1,num_n_zero(n1))
                    !txt
                    write(447,"(i10,i10,f25.3,f25.3)") num_n_zero(n1),temp2_ant(n1),temp2_cost2(n1)/1000000,temp2_cost1(n1)/1000000
                    write(556,"(<num_n_zero(n1)>(i5))") (temp2_e_trade(j),j=1,num_n_zero(n1))
                    write(556,"(<num_n_zero(n1)>(i5))") (temp2_i_trade(j),j=1,num_n_zero(n1))
                    write(556,"(<num_n_zero(n1)>(f20.2))") (temp2_c_bucket(j),j=1,num_n_zero(n1))
                else
                    !csv
                    write(446,"(a8,i4,',',i5,',',i5,',',:,2(f25.3,',',:),x)") "solution", n1,num_n_zero(n1),temp2_ant(n1),temp2_cost2(i)/1000000,temp2_cost1(n1)/1000000
                    write(555,"(a19)") "no trades were made"
                    !txt
                    write(447,"(i10,i10,2(f25.3),x)") num_n_zero(n1),temp2_ant(n1),temp2_cost2(i)/1000000,temp2_cost1(n1)/1000000
                    write(556,"(a19)") "no trades were made"
                end if
                    
            end if
       end if
       
    end do 

  end if
 
end subroutine