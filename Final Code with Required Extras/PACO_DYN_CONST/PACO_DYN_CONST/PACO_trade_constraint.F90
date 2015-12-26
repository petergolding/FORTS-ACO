!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine trade_constraint
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine trade_constraint(x,j)
    
    use aco_program_control
    use system_details
    use input_cost
    use ant_colony
    use ant_graph
    use user
    use para
    
    integer:: k,x,z
    
    e_count = 0
    i_count = 0
    
    !x = y_count
    !define country ea or ir values
    !i_count and e_count values ensure country stays in the same position of the arrays each iteration
    do z = 1,n_countries
        trade_sort(z,x)=PDMSR(z,x)+storage(z,x)
        !requirements matrix
        !if value is greater than zero, and defined as an exporting country only
        if (trade_sort(z,x) >= 0 .and. exp_or_imp(z) == 1) then
            e_count = e_count+1
            e_trade_sort (e_count,x) = trade_sort(z,x)
        !if value is greater than zero, and can be exporting or importing
        else if (trade_sort(z,x) >= 0 .and. exp_or_imp(z) == 2) then
            e_count = e_count+1
            i_count = i_count+1
            e_trade_sort (e_count,x) = trade_sort(z,x)
        !if value is less than zero, and defined only as an importing country
        else if (trade_sort(z,x)< 0 .and. exp_or_imp(z) ==0) then
            i_count=i_count+1
            i_trade_sort(i_count,x) = trade_sort(z,x)
        !if value is less than zero, and defined as both exporting and importing
        else
            i_count=i_count+1
            e_count=e_count+1
            i_trade_sort(i_count,x) = trade_sort(z,x)
        end if
    end do 
             

end subroutine 