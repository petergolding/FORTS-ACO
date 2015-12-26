!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! contains:
! subroutine selection
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine selection(i,j)
    use ant_colony
    use ant_graph
    ! use r_num
    use lin_feedback_shift_reg 
    use system_details
    use user
  
    integer:: flag,count,i,j
    real(8) :: sum_prob,p,dum
  
    flag=0
    sum_prob=0.0
    count=0
!    ant(i)%decision(j)=0
    ant(i)%random(j)=0.0
    
    !random number used to determine selection
    p=lfsr258()  
    !ant(i)%random(j)=grnd()
    ant(i)%random(j)=p
  
       
    do while((flag==0).and.(count<upper_lim))
        count=count+1
        !works through the probability values of each edge, summing them as it moves through
        sum_prob=sum_prob+path(j+add_year)%edge(count)%prob 
        !if, for the first time, the random number is less than the summed probability,
        !then that egde is selected by the ant    
        if(ant(i)%random(j)<=sum_prob)then
            ant(i)%decision(j+add_year)=count
            
            if (count /= current_trade_edge(t_count)) then
                heavens=23
            end if
            
            if (count < current_trade_edge(t_count)) then
                !need to update the trade_sort matrix by the value traded between the two countries
                !subtract the quantity traded from the exporting country (lowers amount available for future trade)
                !add the quantity traded to the importing country (lowers the amount required from future trade)
                e_trade_sort(e_count,y_count)=e_trade_sort(e_count,y_count)-(count-1)*bucket(t_count)
                i_trade_sort(i_count,y_count)=i_trade_sort(i_count,y_count)+(count-1)*bucket(t_count)
                
            elseif (count > current_trade_edge(t_count)) then
                !bucket has been minused
                e_trade_sort(e_count,y_count)=e_trade_sort(e_count,y_count)-(count-2)*bucket(t_count)
                i_trade_sort(i_count,y_count)=i_trade_sort(i_count,y_count)+(count-2)*bucket(t_count)
            else
                !flagged as current therefore minus current trade quantity
                e_trade_sort(e_count,y_count)=e_trade_sort(e_count,y_count)-current_trade_quantity(t_count)
                i_trade_sort(i_count,y_count)=i_trade_sort(i_count,y_count)+current_trade_quantity(t_count)
            end if
            !set flag to 1 so do loop is exited
            flag=1
        end if
    end do
    
    !ensures accurate navigation through the trade_sort matrix 
    i_count=i_count+1
    t_count=t_count+1
    
    !if exhausted one exporting country's trade links, move on to the next one
    !reset to importing country one
    if (i_count == n_i_countries+1) then
        e_count = e_count+1         !moves to the next exporting country count
        i_count = 1                 !resets the importing country count to 1
    end if
    
    !this should never occur, but if it does sets the decision to max_edge
    !possibly if the random number is so high and due to rounding the values don't sum to zero
    if(upper_lim == 1 .or. flag == 0)then
        ant(i)%decision(j+add_year)=1
    end if

end subroutine selection