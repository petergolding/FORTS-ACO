!  Trade_Quantity_Comparison.f90 
!
!  FUNCTIONS:
!  Trade_Quantity_Comparison - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Trade_Quantity_Comparison
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Trade_Quantity_Comparison

    implicit none
    integer :: n_i_countries, n_e_countries, num_solutions,max_solutions
    integer,allocatable :: trade_array(:,:),temp_trade_array(:,:)
    integer,allocatable :: e_index(:)
    integer,allocatable :: i_index(:)
    integer,allocatable :: c_index(:)
    real(8),allocatable :: e_storage(:),i_deficit(:)
    integer :: i,j,k,p,m
    integer :: flag
    integer :: exp, imp, trade
    integer :: count, e_count, i_count
    integer :: num_trade_links
    integer :: min,max
    real(8) :: start,final, max_prop_imp,max_prop_exp
    integer :: current_trade_links
    real(8) :: dum1,dum2,dum3,dum10
    real(8) :: storage_reduction,deficit_reduction
    integer :: dummy,io
    real(8) :: max_deficit,trade_contribution, permanent_deficit
    real(8),allocatable :: moving_average(:)
    integer,allocatable :: e_country(:),i_country(:) 
    integer,allocatable :: trade_quantity(:)
    integer,allocatable :: valid_trade(:)
    real(8),allocatable :: deviation(:),deficit(:)
    real(8),allocatable :: temp_deviation(:),temp_deficit(:)
    real(8),allocatable :: storage_array(:),deficit_array(:)
    real(8),allocatable :: PDMSR(:),initial_storage(:),e_PDMSR(:)
    character(20), allocatable :: e_name(:),i_name(:)
    integer,allocatable :: e_interest(:),i_interest(:)
    real(8), allocatable :: e_initial_storage(:)
    
    open(unit=111,file="exporting_country_index.txt",status="old")
    open(unit=222,file="importing_country_index.txt",status="old")
    open(unit=444,file="final pareto data.txt",status="old")
    open(unit=555,file="final pareto.txt",status="old")
    open(unit=777,file="collaborated data.csv",status="unknown")
    open(unit=888,file="country_index.txt",status="unknown")
    open(unit=889,file="PDMSR_input.txt",status="unknown")
    open(unit=890,file="initial_storage.txt",status="unknown")
    open(unit=999,file="trade_links.txt",status="old") 
    
    !read in the number of solutions in the Pareto front
    num_solutions=0
    do  
        read(555,*,iostat=io),dummy
        if(io>0)then
            print*,"something wrong"
            pause
            stop
        else if(io<0 .or. dummy == -1)then
            exit
        else 
            num_solutions=num_solutions+1
        end if
    end do      
    
    close(555)  
    open(unit=555,file="final pareto.txt",status="old")
    
    !read in the number of exporting countries
    n_e_countries=0
    do  
        read(111,*,iostat=io),dummy
        if(io>0)then
            print*,"something wrong"
            pause
            stop
        else if(io<0 .or. dummy == -1)then
            exit
        else 
            n_e_countries=n_e_countries+1
        end if
    end do      
    
    close(111)  
    open(unit=111,file="exporting_country_index.txt",status="old")
    
    !read in the number of importing countries
    n_i_countries=0
    do  
        read(222,*,iostat=io),dummy
        if(io>0)then
            print*,"something wrong"
            pause
            stop
        else if(io<0 .or. dummy == -1)then
            exit
        else 
            n_i_countries=n_i_countries+1
        end if
    end do      
    
    close(222)  
    open(unit=222,file="importing_country_index.txt",status="old")
    
    !read in the number of trade links
    current_trade_links=0
    do  
        read(999,*,iostat=io),dummy
        if(io>0)then
            print*,"something wrong"
            pause
            stop
        else if(io<0 .or. dummy == -1)then
            exit
        else 
            current_trade_links=current_trade_links+1
        end if
    end do      
    
    close(999)  
    open(unit=999,file="trade_links.txt",status="old") 
    
    allocate(temp_trade_array(n_e_countries*n_i_countries,2+num_solutions),trade_array(n_e_countries*n_i_countries,2+num_solutions))
    allocate(e_index(n_e_countries),i_index(n_i_countries))
    allocate(c_index(n_e_countries+n_i_countries))
    allocate(e_name(n_e_countries),i_name(n_i_countries))
    allocate(e_country(n_e_countries*n_i_countries),i_country(n_e_countries*n_i_countries),trade_quantity(n_e_countries*n_i_countries))
    allocate(valid_trade(n_e_countries*n_i_countries))
    allocate(deviation(num_solutions),deficit(num_solutions))
    allocate(temp_deviation(num_solutions),temp_deficit(num_solutions))
    allocate(moving_average(num_solutions-99))
    allocate(e_storage(n_e_countries),i_deficit(n_i_countries)) 
    allocate(storage_array(num_solutions))
    allocate(deficit_array(num_solutions))
    allocate(i_interest(n_e_countries*n_i_countries))
    allocate(e_interest(n_e_countries*n_i_countries))
    allocate(PDMSR(n_e_countries+n_i_countries),e_PDMSR(n_e_countries))
    allocate(initial_storage(n_e_countries+n_i_countries))
    allocate(e_initial_storage(n_e_countries))
    
    trade_array = 0
    valid_trade = 0
    temp_trade_array = 0 
    deviation = 0
    deficit = 0
    
    !read in country index
    do i = 1,n_e_countries+n_i_countries
        read(888,*) c_index(i)
    end do
    
    !read in PDMSR
    do i = 1,n_e_countries+n_i_countries
        read(889,*) PDMSR(i)
    end do
    
    !read in initital storage
    do i = 1,n_e_countries+n_i_countries
        read(890,*) initial_storage(i)
    end do
    
    !read in exporting countries
    do i =1,n_e_countries
        read(111,*) e_index(i),e_name(i)
    end do
    
    !read in exporting country storages
    do i = 1,n_e_countries
        do j = 1,n_e_countries+n_i_countries
            if (e_index(i)==c_index(j)) then
                e_storage(i)=(PDMSR(j)+initial_storage(j))/1000000
                e_initial_storage(i)=initial_storage(j)/1000000
                e_PDMSR(i)=PDMSR(j)
            end if
        end do
    end do
    
    !read in importing countries
    do j = 1,n_i_countries
        read(222,*) i_index(j),i_name(j)
    end do
    
    !read in importing country deficit
    do i = 1,n_i_countries
        do j = 1,n_e_countries+n_i_countries
            if (i_index(i)==c_index(j)) then
                i_deficit(i)=(PDMSR(j)+initial_storage(j))/1000000
            end if
        end do
    end do
    
    !setup array with trade links
    count = 0
    do i=1,n_e_countries
        do j=1,n_i_countries
            count = count + 1
            temp_trade_array(count,1) = e_index(i)
            temp_trade_array(count,2) = i_index(j)
        end do
    end do
    
    !read valid trade links
    do i=1,current_trade_links
        read(999,*) dum1,dum2,dum3
        do j=1,n_e_countries
            if (e_index(j) == dum1) then
                do k = 1,n_i_countries
                    if (i_index(k) == dum2) then
                        valid_trade((j-1)*n_i_countries+k)=1
                    end if
                end do
            end if
        end do
    end do

    !place appropriate values in appropriate location
    do k = 1,num_solutions
        exp = 1
        imp = 1
        trade = 1
        read(555,*) num_trade_links, dum1, temp_deviation(k),temp_deficit(k)
        read(444,*) (e_country(exp),exp=1,num_trade_links)
        read(444,*) (i_country(imp),imp=1,num_trade_links)
        read(444,*) (trade_quantity(trade),trade=1,num_trade_links)
        
        do p = 1,num_trade_links
            do i = 1,n_e_countries*n_i_countries
                if (e_country(p) == temp_trade_array(i,1) .and. i_country(p) == temp_trade_array(i,2)) then
                    temp_trade_array(i,k+2) = trade_quantity(p)
                    exit
                end if
            end do
        end do
    end do
    
    !sort from smallest deviation to biggest
    do i=1,2
        do j = 1,n_e_countries*n_i_countries
            trade_array(j,i)=temp_trade_array(j,i)*1000000
        end do
    end do
    
    do k = 1,num_solutions
        count = 1
        flag = 0
        do p=1,num_solutions
            if (k /= p) then
                if(temp_deviation(k)>temp_deviation(p) .or. temp_deficit(k)<temp_deficit(p)) then
                    count = count +1
                end if
            end if
        end do
        
        do while (flag == 0)
            if (deviation(count)==0) then
                deviation(count)=temp_deviation(k)
                deficit(count)=temp_deficit(k)
                do i = 1,n_e_countries*n_i_countries
                    trade_array(i,count+2)=temp_trade_array(i,k+2)
                end do
                flag = 1
            else 
                count = count +1
            end if
         end do
    end do
    
    do k = 1,num_solutions
        if (deviation(k)>22.9 .or. k == num_solutions) then !needs to be changed dependent on deviation value marked
            max_solutions = k - 1
            exit
        end if
    end do
           
    
    !write out known trade values

    write(777,"(A30)") "trade links: exact quantities"
    write(777,"(A20,',',A20,',',<max_solutions>(I6,',',:))") "exporting country", "importing country", (i,i=1,max_solutions)
    write(777,"(A1,',',A1,',',<max_solutions>(F10.5,',',:))") " ", " ", (deviation(k),k=1,max_solutions)
    write(777,"(A1,',',A1,',',<max_solutions>(F10.5,',',:))") " ", " ", (deficit(k),k=1,max_solutions)
    
    do i = 1, n_e_countries*n_i_countries
        if (valid_trade(i) /= 0) then
            write(777,"(F4.0,',',F4.0,',',<max_solutions>(F12.9,',',:))")(real(trade_array(i,j))/1000000,j=1,2+max_solutions)
        end if
    end do
    
    !write out known moving averages
    write(777,*)
    write(777,"(A30)") "trade links: 100 moving average"
    write(777,"(A1,',',A20,',',<(max_solutions-99)>(I6,',',:),',',A30,',',A30,',',A30)") " ","trade link", (i,i=1,(max_solutions-99)), "average change","proportional: exporting","proportional: importing"
    
    do i = 1, n_e_countries*n_i_countries
        e_count=0
        i_count=0 
        moving_average=0
        
        if (valid_trade(i) /= 0) then
            do j = 1,max_solutions
                if (j >= 100) then
                    do k = 1,100
                        moving_average(j-99) = moving_average(j-99)+real(trade_array(i,j-k+3))/1000000
                    end do
                    moving_average(j-99)=moving_average(j-99)/100
                end if
            end do
            
            do k = 1,n_e_countries
                if(e_index(k)==temp_trade_array(i,1)) then
                    e_count = k
                    exit
                end if
            end do
            
            do k=1,n_i_countries
                if(i_index(k)==temp_trade_array(i,2)) then
                    i_count = k
                    exit
                end if
            end do 
            
            !maximum change
            start = moving_average(1)
            final = moving_average(max_solutions-99)
            
            !maximum proportional change (exporting)
            max_prop_exp=(final-start)/e_storage(e_count)
            
            !max proportional change (importing)
            max_prop_imp=(final-start)/i_deficit(i_count)
            
            write(777,"(I4,',',I4,',',<(max_solutions-99)>(F12.9,',',:),',',F15.10,',',F15.10,',',F15.10)") e_index(e_count),i_index(i_count),(moving_average(k),k=1,(max_solutions-99)),final-start,max_prop_exp,max_prop_imp  
        end if
    end do
    
    !country trade total
    write(777,*)
    write(777,"(A40)") "exporting country trade: exact quantities"
    write(777,"(A1,',',A20,',',<max_solutions>(I6,',',:),',',A40)") " ","exporting country", (i,i=1,max_solutions),"total trade (reduced deficit by) (%)"
    
    do i = 1,n_e_countries
        e_interest=0
        do j=1,n_i_countries*n_e_countries
            if (trade_array(j,1)/1000000==e_index(i)) then
                e_interest(j)=1
            end if
        end do
 
        storage_array=0
        storage_array=e_storage(i)
        trade_contribution=0
        
        do k=1,max_solutions
            do p=1,n_e_countries*n_i_countries
                if (valid_trade(p) /= 0 .and. e_interest(p) /= 0) then
                    storage_array(k)=storage_array(k)+real(trade_array(p,k+2))/1000000
                end if
            end do
        end do

        trade_contribution=-(storage_array(1)-storage_array(max_solutions))/(deficit(1)-deficit(max_solutions))*100

        write(777,"(I4,',',A30,',',<max_solutions>(F12.9,',',:),',',F10.4)") e_index(i),e_name(i),(storage_array(p),p=1,max_solutions),trade_contribution
    end do
    
    !country storage total
    write(777,*)
    write(777,"(A40)") "exporting country storage: exact quantities"
    write(777,"(A1,',',A20,',',<max_solutions>(I6,',',:),',',A30)") " ","exporting country", (i,i=1,max_solutions),"remaining storage (%)"
    
    do i = 1,n_e_countries
        e_interest=0
        do j=1,n_i_countries*n_e_countries
            if (trade_array(j,1)/1000000==e_index(i)) then
                e_interest(j)=1
            end if
        end do
 
        storage_array=0
        storage_array=e_storage(i)
        do k=1,max_solutions
            do p=1,n_e_countries*n_i_countries
                if (valid_trade(p) /= 0 .and. e_interest(p) /= 0) then
                    storage_array(k)=storage_array(k)-real(trade_array(p,k+2))/1000000
                end if
            end do
        end do

        storage_reduction = (storage_array(max_solutions)/e_initial_storage(i))*100

        write(777,"(I4,',',A30,',',<max_solutions>(F12.9,',',:),',',F10.4)") e_index(i),e_name(i),(storage_array(p),p=1,max_solutions),storage_reduction
    end do
    
    !country storage moving average
    write(777,*)
    write(777,"(A40)") "exporting country storage: 100 moving average"
    write(777,"(A1,',',A20,',',<max_solutions>(I6,',',:))") " ","exporting country", (i,i=1,(max_solutions-99))
    
    do i = 1,n_e_countries
        e_interest=0
        do j=1,n_i_countries*n_e_countries
            if (trade_array(j,1)/1000000==e_index(i)) then
                e_interest(j)=1
            end if
        end do
 
        storage_array=0
        storage_array=e_storage(i)
        do k=1,max_solutions
            do p=1,n_e_countries*n_i_countries
                if (valid_trade(p) /= 0 .and. e_interest(p) /= 0) then
                    storage_array(k)=storage_array(k)-real(trade_array(p,k+2))/1000000
                end if
            end do
        end do
        
        moving_average=0    
        do j = 1,max_solutions
            if (j >= 100) then
                do k=1,100
                    moving_average(j-99) = moving_average(j-99)+storage_array(j-k+1)
                end do
                moving_average(j-99)=moving_average(j-99)/100
             end if
        end do
        
        write(777,"(I4,',',A30,',',<max_solutions>(F12.9,',',:))") e_index(i),e_name(i),(moving_average(p),p=1,(max_solutions-99))
        
    end do

    !country deficit
    write(777,*)
    write(777,"(A40)") "importing country deficit: exact quantities"
    write(777,"(A1,',',A20,',',<max_solutions>(I6,',',:),',',A30)") " ","importing country", (i,i=1,max_solutions), "deficit remaining (%)"
    
    
    do i = 1,n_i_countries
        i_interest=0
        do j=1,n_i_countries*n_e_countries
            if (trade_array(j,2)/1000000==i_index(i)) then
                i_interest(j)=1
            end if
        end do

        deficit_array=0
        deficit_array=i_deficit(i)
        do k=1,max_solutions
            do p=1,n_i_countries*n_e_countries
                if (valid_trade(p) /= 0 .and. i_interest(p) /= 0) then
                    deficit_array(k)=deficit_array(k)+real(trade_array(p,k+2))/1000000
                end if
            end do
        end do
        
        deficit_reduction = (1-deficit_array(max_solutions)/i_deficit(i))*100
        
        write(777,"(I4,',',A30,',',<max_solutions>(F10.5,',',:),',',F10.4)") i_index(i),i_name(i),(deficit_array(p),p=1,max_solutions),deficit_reduction
    end do
    
    !country deficit moving average
    write(777,*)
    write(777,"(A40)") "importing country deficit: 100 moving average"
    write(777,"(A1,',',A20,',',<max_solutions>(I6,',',:))") " ","importing country", (i,i=1,(max_solutions-99))
    
    
    do i = 1,n_i_countries
        i_interest=0
        do j=1,n_i_countries*n_e_countries
            if (trade_array(j,2)/1000000==i_index(i)) then
                i_interest(j)=1
            end if
        end do

        deficit_array=0
        deficit_array=i_deficit(i)
        do k=1,max_solutions
            do p=1,n_i_countries*n_e_countries
                if (valid_trade(p) /= 0 .and. i_interest(p) /= 0) then
                    deficit_array(k)=deficit_array(k)+real(trade_array(p,k+2))/1000000
                end if
            end do
        end do
        
        moving_average=0    
        do j = 1,max_solutions
            if (j >= 100) then
                do k=1,100
                    moving_average(j-99) = moving_average(j-99)+deficit_array(j-k+1)
                end do
                moving_average(j-99)=moving_average(j-99)/100
             end if
        end do
        
        write(777,"(I4,',',A30,',',<max_solutions>(F12.9,',',:))") i_index(i),i_name(i),(moving_average(p),p=1,(max_solutions-99))

    end do
    
    end program Trade_Quantity_Comparison

