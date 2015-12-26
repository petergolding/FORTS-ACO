!  Pareto_Front_Sort.f90 
!
!  FUNCTIONS:
!  Pareto_Front_Sort - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Pareto_Front_Sort
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

  program Pareto_Front_Sort

  implicit none
  
  real(8) :: temp_cost1(10000), temp_cost2(10000)           !temporary arrays
  real(8) :: cost1(10000), cost2(10000)
  !real(8) :: temp_purecost1(max_ant), temp_purecost2(max_ant)
  integer :: temp_ant
  integer :: temp_trade(10000,2),temp_dec(10000) 
  real(8) :: temp_bucket(10000)
  integer :: np(10000)
  integer :: hyp_point1(10000),hyp_point2(10000)
  integer :: counter1(10000)
  integer :: pareto,file
  integer :: dom_less, dom_more, dom_equal
  integer :: n_obj
  integer :: number_of_solutions, total_num_solutions
  integer :: flag
  integer :: p_count
  integer,allocatable :: pareto_number(:)
  
  integer :: i_country (10000,10000), e_country (10000,10000)
  real :: q_traded(10000,10000)
  
  integer :: i,j,k, temp_count, n !counters  
  integer :: hyp_count, num
  integer :: no_trade
  real :: hyp_area
  real :: dum_solution_number, dum_ant_number
  character(7) :: solution_character, dummy
  
  open (unit=010, file="final pareto 1.txt",status="unknown")
  open (unit=020, file="final pareto 2.txt",status="unknown")
  open (unit=030, file="final pareto 3.txt",status="unknown")
  open (unit=040, file="final pareto 4.txt",status="unknown")
  open (unit=050, file="final pareto 5.txt",status="unknown")
  open (unit=060, file="final pareto 6.txt",status="unknown")
  open (unit=070, file="final pareto 7.txt",status="unknown")
  open (unit=080, file="final pareto 8.txt",status="unknown")
  open (unit=090, file="final pareto 9.txt",status="unknown")
  open (unit=100, file="final pareto 10.txt",status="unknown")
!  open (unit=110, file="final pareto 11.txt",status="old")
!  open (unit=120, file="final pareto 12.txt",status="old")
!  open (unit=130, file="final pareto 13.txt",status="old")
!  open (unit=140, file="final pareto 14.txt",status="old")
!  open (unit=150, file="final pareto 15.txt",status="old")
!  open (unit=160, file="final pareto 16.txt",status="old")
!  open (unit=170, file="final pareto 17.txt",status="old")
!  open (unit=180, file="final pareto 18.txt",status="old")
!  open (unit=190, file="final pareto 19.txt",status="old")
  
  open (unit=015, file="final pareto data 1.txt",status="unknown")
  open (unit=025, file="final pareto data 2.txt",status="unknown")
  open (unit=035, file="final pareto data 3.txt",status="unknown")
  open (unit=045, file="final pareto data 4.txt",status="unknown")
  open (unit=055, file="final pareto data 5.txt",status="unknown")
  open (unit=065, file="final pareto data 6.txt",status="unknown")
  open (unit=075, file="final pareto data 7.txt",status="unknown")
  open (unit=085, file="final pareto data 8.txt",status="unknown")
  open (unit=095, file="final pareto data 9.txt",status="unknown")
  open (unit=105, file="final pareto data 10.txt",status="unknown")
!  open (unit=115, file="final pareto data 11.txt",status="old")
!  open (unit=125, file="final pareto data 12.txt",status="old")
!  open (unit=135, file="final pareto data 13.txt",status="old")
!  open (unit=145, file="final pareto data 14.txt",status="old")
!  open (unit=155, file="final pareto data 15.txt",status="old")
!  open (unit=165, file="final pareto data 16.txt",status="old")
!  open (unit=175, file="final pareto data 17.txt",status="old")
!  open (unit=185, file="final pareto data 18.txt",status="old")
!  open (unit=195, file="final pareto data 19.txt",status="old")
  
  
  open (unit=123, file="final pareto.csv", status="unknown")
  open (unit=223, file="final pareto data.csv", status="unknown")
  open (unit=523, file="final pareto.txt", status="unknown")
  open (unit=623, file="final pareto data.txt", status="unknown")
  
  
  file = 010
  total_num_solutions = 0
  flag = 0
  number_of_solutions = 0
  p_count=0
  
  allocate(pareto_number(10))  
    
  do pareto = 1,5
!    read(file,*) number_of_solutions
    
    i=0
    
    
    do while (flag == 0)
        i = i+1
        read (file,*) counter1(i+total_num_solutions),dum_ant_number,cost1(i+total_num_solutions), cost2(i+total_num_solutions)
        if (counter1(i+total_num_solutions) == -1) then
            total_num_solutions = number_of_solutions-1
            file = file + 10
            p_count=p_count+1
            pareto_number(p_count) = total_num_solutions
            exit
        end if
        
        read(file+5,*) (e_country(i+total_num_solutions,j), j = 1,counter1(i+total_num_solutions))
        read(file+5,*) (i_country(i+total_num_solutions,j), j = 1,counter1(i+total_num_solutions))
        read(file+5,*) (q_traded(i+total_num_solutions,j), j = 1,counter1(i+total_num_solutions))
        
        number_of_solutions = number_of_solutions+1
    end do
    
  end do
 
  total_num_solutions = total_num_solutions-1
! initalise temporary arrays
  n_obj = 2  
    
  temp_cost1=0.0
  temp_cost2=0.0 

  temp_ant=0.0 
  temp_dec=0

  hyp_count = 0
  n=0
  p_count=1
 
! determining non-dominated solutions 
  do i=1,total_num_solutions
    np(i)=0.0
    
    if (i > pareto_number(p_count)) then
        p_count=p_count+1
    end if

    do j=1,total_num_solutions
        dom_less=0
        dom_equal=0
        dom_more=0
        
        !if same solution then remove future solutions
        if (cost1(i)==cost1(j) .and. cost2(i)==cost2(j) .and. i>j) then
            np(i)=np(i)+1
            exit
         end if
        
        !compares deficit cost
        if(cost1(i)<cost1(j))then
            dom_less=dom_less+1
        else if (cost1(j)==cost1(j)) then
            dom_equal=dom_equal+1
        else
            dom_more=dom_more+1
        end if

        !compares trade deviation cost
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
     
        hyp_count=hyp_count+1
        n=1
        no_trade=0
        temp_count=0   
        temp_dec=0  
              
         ! storing non-dominated solutions in array
         temp_cost1(hyp_count)=cost1(i)
         temp_cost2(hyp_count)=cost2(i)
         
        !write out the decisions where a trade exists
        !note, written in different order in pareto optimal than in final pareto for graphing in excel reasons
        if (counter1(i) /= 0) then
            !number of active trade links, ant number, deficit, deviation
            write(123,"(A8,I4,',',i5,',',2(f25.3,',',:))") "solution",hyp_count,counter1(i),temp_cost1(hyp_count),temp_cost2(hyp_count)
            !exporting country code
            write(223,"(A8,I4)") "solution", hyp_count
            write(223,"(<counter1(i)>(i5,',',:))") (e_country(i,k),k=1,counter1(i))
            !importing country code
            write(223,"(<counter1(i)>(i5,',',:))") (i_country(i,k),k=1,counter1(i))
            !writes total quantity of trade
            write(223,"(<counter1(i)>(f20.2,',',:))") (q_traded(i,k),k=1,counter1(i))
            
            !for txt 
            write(523,"(I10,I10,f25.3,f25.3))") counter1(i),hyp_count,temp_cost1(hyp_count),temp_cost2(hyp_count)
            write(623,"(<counter1(i)>(i5))") (e_country(i,k),k=1,counter1(i))
            !importing country code
            write(623,"(<counter1(i)>(i5))") (i_country(i,k),k=1,counter1(i))
            !writes total quantity of trade
            write(623,"(<counter1(i)>(f20.2))") (q_traded(i,k),k=1,counter1(i))
        else
            write(123,"(i5,',',2(f25.3,',',:),x)") counter1(i),temp_cost1(hyp_count),temp_cost2(hyp_count)
            write(223,"(A17)") "no trade occurred"
            
            
        end if       
    end if
  end do
  
!  !hypervolume calculation
!  !first need to sort the solution to highest deficit to lowest deficit
!  do i = 1,hyp_count
!    
!    num=1
!    !allocates the rank of the random number used in the rearrange matrix
!    do j = 1,hyp_count
!        if (i /= j .and. temp_cost1(j) > temp_cost1(i)) then
!            num = num + 1
!        end if
!    end do
!    !redistributes the countries according to the rank of their random number
!    hyp_point1(num)=temp_cost1(i)
!    hyp_point2(num)=temp_cost2(i)
!    
!  end do
!  
!  !then calculate the area between maximum known deviation and deficit values
!  hyp_area = 0
!  do i = 1, hyp_count+1
!    !if (i == 1) then
!     !   hyp_area =  hyp_area + ((real(hyp_point2(i)))*(real(max_deficit)-real((hyp_point1(i)))/2))/1000000000000
!    !end if
!  
!    if (i > 1 .and. i /= hyp_count + 1) then
!        !previous area + change in x multiplied by change in y from max deficit
!        hyp_area =  hyp_area + ((real(hyp_point2(i))- real(hyp_point2(i-1)))*(real(max_deficit)-real((hyp_point1(i-1))+real(hyp_point1(i)))/2))/1000000000000
!    end if
!    
!    if (i == hyp_count+1) then
!        hyp_area = hyp_area + ((real(max_deviation)- real(hyp_point2(i-1)))*(real(max_deficit)-(real(hyp_point1(i-1)))/2))/1000000000000
!    end if
!  end do
!  
!  write(999,*) hyp_area


    end program Pareto_Front_Sort

