!  Trade_Analysis.f90 
!
!  FUNCTIONS:
!  Trade_Analysis - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Trade_Analysis
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Trade_Analysis

    implicit none
    real, dimension (142) :: e_index, i_index
    character, dimension(142):: e_names, i_names
    real,dimension (142,2) :: L_cap
    integer,dimension(140):: trade_requirement
    integer,dimension (10000,2) :: country_code
    integer,dimension (10000) :: trade_value
    integer :: flag
    integer :: i,j
    integer :: v_count
    real,dimension (1,2) :: exp_location,imp_location
    real(8) :: sum_distance_cost,distance,a,c
    real(8), dimension(10000) :: distance_cost
    integer, dimension (10000) :: e_country,i_country
    integer :: deficit_cost
    integer :: valid_exp_trade,valid_imp_trade
    integer, dimension(10000) :: num_carriers
    integer :: n_countries, n_i_countries,n_e_countries,num_trade
    integer, dimension (142,2) :: storage


    open(unit=222,file="input_trade_data.txt",status="unknown")
    open(unit=333,file="trade_requirements.txt",status="unknown")
    open(unit=555,file="country_index.txt",status="unknown")
    
    
    
    n_countries = 140
    n_i_countries = 112
    n_e_countries = 28
    storage = 0
    
    !read in known trade requirements (analysing PACO trade data)
    do i = 1,n_countries
        read(333,*) trade_requirement(i)
    end do
    
    close(333)
    
    !read in exporting countries
    open(unit=444,file="exporting_country_index.txt",status="unknown")
    
    do i = 1,n_e_countries
        read(444,*)e_index(i),e_names(i)
    end do
    
    close(444)
    
    !read in importing countries
    open(unit=444,file="importing_country_index.txt",status="unknown")
    do i =1,n_i_countries
        read(444,*)i_index(i),i_names(i)
    end do
    
    close(444)
    
    !read in country index order 
    do i =1,n_countries
        read(555,*) storage(i,1)
    end do
    
    close(555)
    
    close(555)
        
    !input trade data read in
    flag = 0
    v_count=0
    distance_cost = 0.0
    num_trade = 0
    
    do while (flag == 0)
        v_count = v_count + 1
        valid_exp_trade = 0
        valid_imp_trade = 0
        imp_location = 0
        exp_location = 0
        
        read(222,*)country_code(v_count,1),country_code(v_count,2),trade_value(v_count)
        
        if (country_code(v_count,1) == -1) then
            flag = 1
            exit
        end if
        
        do j = 1,n_countries
            if (country_code(v_count,1)==e_index(j)) then
                valid_exp_trade=valid_exp_trade + 1
            end if
            
            if (country_code(v_count,2)==i_index(j)) then
                valid_imp_trade=valid_imp_trade + 1
           end if
        end do
        
        !calculates distance cost and storage change
        if (valid_exp_trade == 1 .and. valid_imp_trade == 1 .and. trade_value(v_count) /= 0) then
            
            do j = 1, n_countries
                !for the exporting country losing wheat
                if (country_code(v_count,1) == storage(j,1)) then
                    storage(j,2) = storage(j,2) + trade_value(v_count)
                end if
                
                !for the importing country gaining wheat
                if (country_code(v_count,2) == storage(j,1)) then
                    storage(j,2) = storage(j,2) - trade_value(v_count)
                end if
            end do
            
            e_country(v_count) = country_code(v_count,1)
            i_country(v_count) = country_code(v_count,2)
            
            num_trade = num_trade + 1            
        end if
      
    end do
    
!    sum_distance_cost = 0.0
    
    !write out distance, exp country and imp country and trade value for check
    open(unit=777,file="relevant_trade_data.csv",status="unknown")
    
    do j = 1, v_count
        if (e_country(j) /= 0 .and. i_country(j) /= 0 .and. trade_value(j) /= 0) then
            write (777,"(I6,',',I6,',',I20)")e_country(j),i_country(j),trade_value(j)
        end if
    end do
    
    close(777)
    
    !calculates deficit cost
    deficit_cost = 0
    
    do i = 1,n_countries
        if (storage(i,2) < 0) then
            deficit_cost = deficit_cost + abs(storage(i,2)-trade_requirement(i))
        end if
    end do
    
    close(222)
    
    !write out final solution data
    open(unit=777,file="output_pdsmr.csv",status="unknown")
    
    write(777,"(A7,',',A8)") "deficit"
    write(777,"(I20)") deficit_cost
    write(777,"(A20)") "country storages"
    
    do i = 1,n_countries
        !country index, total left in storage or deficit, total amount traded
        write(777,"(I6,',',I20)")storage(i,1),trade_requirement(i)-storage(i,2)
    end do
    
    close(777)
    
    end program Trade_Analysis

