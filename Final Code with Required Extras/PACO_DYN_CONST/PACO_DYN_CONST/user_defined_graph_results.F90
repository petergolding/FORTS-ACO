!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE graph_results
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE  graph_results(antID, fit_func1, fit_func2, obj_func1, obj_func2,decisionn,n_points)
! Joanna Szemis, January 2013
! This subroutine develops the graphs of the trade-off between the two objectives  
  USE ant_graph
 ! USE RFortran

  INTEGER, DIMENSION(n_points) :: antID
  REAL(8), DIMENSION(n_points) :: fit_func1, fit_func2, obj_func1, obj_func2 
  INTEGER, DIMENSION(n_points,max_path) :: decisionn

  
!  
!   ok = Rinit()                             
!   If (ok/=0) then; write(*,*)"ERROR CALLING RINIT: see 'RFortran.log' for details";pause; stop; end if
!    
! ! Storing relevant arrays from Fortran to R
!   ok = Rcall(vars=(/Rput("cost1",fit_func1),Rput("cost2",fit_func2),&
!                     Rput("obj1",obj_func1), Rput("obj2",obj_func2)/),&
!                     
!              cmds=(/cl("tiff(file='Figure1.tiff', height=5.0, width=5.0, unit='in',  pointsize=10,res=200)"),&
!                     cl("plot(cost1,cost2,type='p',pch=1,xlab='Complete Set',ylab='Operational Set')"),&
!                     cl(" dev.off()  "),&   
!  
!                     cl("tiff(file='Figure2.tiff', height=5.0, width=5.0, unit='in',  pointsize=10,res=200)"),&
!                     cl("plot(obj1,obj2,type='p',pch=2,xlab='Complete Set',ylab='Operational Set',xlim=range(0.0,20.0),ylim=range(0.0,25.0))"),&
!                     cl(" dev.off()  ")/)) 
! 
!   If (ok/=0) then; pause; stop; end if 
   
 ! Closing R 
!   ok = Rclose()
 ! pause
 
 

END SUBROUTINE







