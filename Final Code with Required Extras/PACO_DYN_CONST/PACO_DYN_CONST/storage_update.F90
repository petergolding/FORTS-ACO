!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine storage_update
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine storage_update(x,j)
    
    use aco_program_control
    use system_details
    use input_cost
    use ant_colony
    use ant_graph
    use user
    use para
    
    integer:: k,x,z,j,w
    
    if (x==1) then
        def_count=1
        
        !must be done here as cost may begin to accumulate
        !set to zero for sorting reasons in evaluation
        do w=1,n_obj
            ant(j)%cost(w)=0.0
        end do
    end if
    
    !store in final values of trade sort
    do w = 1,n_i_countries
        !as not all import required values may be filled by exported quantities
        if (i_trade_sort(w,x) /= 0) then                       
            ant(j)%deficit_cost(def_count)=i_trade_sort(w,x)
        end if
        !need additional counter as ant travels through a number of years
        def_count=def_count+1
    end do
    
    !set to zero
    !counters keep position in trade matrices constant
    e_count=0
    i_count=0
   
    !define country ea or ir values
    if (x+1 <= n_years)then
        do z = 1,n_countries
            !if it is exporting in the following year and was exporting in the previous year
            !if country is defined as always exporting
            if (exp_or_imp(z) == 1) then
                e_count = e_count + 1
                !storage from the previous year carried through
                e_trade_sort(e_count,x+1)=PDMSR(z,x+1)+e_trade_sort(e_count,x)
            !if a country is defined as always importing
            else if (exp_or_imp(z) == 0) then
                i_count=i_count + 1
                !demand in year determined only by IR value
                !deficit in one year does not affect demand in following year
                !ie country can't make up for last year's losses
                i_trade_sort(i_count,x+1)=PDMSR(z,x+1)   
            !if a country can be either exporting or importing
            !and in this case is switching from importing to exporting
            else if (exp_or_imp(z) == 2 .and. PDMSR(z,x+1) >= 0 .and. PDMSR(z,x)+storage(z,1) < 0) then
                e_count=e_count + 1
                i_count=i_count + 1
                e_trade_sort(e_count,x+1)=PDMSR(z,x+1)
            !if a country can be either exporting or importing
            !and in this case may switch from exporting to importing
            else
                i_count=i_count + 1
                e_count=e_count + 1 
                i_trade_sort(i_count,x+1)=PDMSR(z,x+1)+e_trade_sort(e_count,x)
                !check to see if country has enough in storage that even though PDMSR suggests it is importing
                !due to storage it actually remains as an exporting country
                if (i_trade_sort(i_count,x+1) >= 0) then
                    i_trade_sort(i_count,x+1) = 0.0 !set to zero so deficit cost not impacted
                    e_trade_sort(e_count,x+1)=PDMSR(z,x+1)+e_trade_sort(e_count,x)
                end if
            end if
            
            if (e_trade_sort(e_count,x)<0) then
                heavens=1
            end if
            
        end do
        
        !reset trade constraint
        trade_con=0  
    end if

end subroutine 