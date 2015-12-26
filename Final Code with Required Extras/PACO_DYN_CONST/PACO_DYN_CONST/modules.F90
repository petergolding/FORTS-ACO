!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module aco_program_control
    ! joanna szemis, january 2013
    integer :: max_it    ! maximum number of iterations
    integer :: aco_type  ! integer flag for aco type
    integer :: seed_ran_1, seed_ran_2, seed_ran_3 ! random seed
    integer :: seed_ran_4, seed_ran_5
         
  end module aco_program_control

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module ant_colony
    
    ! joanna szemis, april 2002

    integer :: max_ant
    integer :: count_n,ndup_freq,ndup_freq2, ndup_freq3,ndup_freq4, ndup_freq5, ndup_freq6, count_n2, count_n3, count_n4, count_n5, count_n6
  
    real(8), allocatable, dimension(:) :: cost1,cost2,obj1,obj2 ! temporary arrays for objective and fitness function
    integer, allocatable, dimension(:,:) :: decis,e_trade,i_trade ! temporary array to sotre decisions
    real(8), allocatable, dimension(:,:) :: c_bucket
!    real(8), dimension(5000,5000) :: store                    ! stores values
    
    type ant_type
        integer, allocatable, dimension(:) :: decision      ! the array of decisions the ant has selected for all decision points
        real(8), allocatable, dimension(:) :: random        ! random number with which the decision was made             
        real(8), allocatable, dimension(:) :: deficit_cost              
        real(8) :: pc1,pc2                                  ! objective function value of the ants path
        real(8) :: pen_cost                                 ! penalty cost
        real(8) :: pen                                      ! penalty
        real(8) :: pure_cost                                ! cost without inclusion of penalty
        real(8), dimension(2) :: pk,pk2
        real(8), allocatable, dimension(:) :: cost          ! objective funactiuons
!        integer, dimension(150000) ::  more_st               ! stores values during non-dominated sort
!        real(8), dimension(150000) ::con
    
    end type ant_type

    type(ant_type), allocatable, dimension(:) ::  ant    
     
  end module ant_colony

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  
  module ant_graph

! aaron zecchin, april 2002
! "ant_graph" module stores all information about the aco graph

    integer :: max_path,n_obj, dec_mat,n_buckets
    real(8) :: tau_0                  ! initial pheromone
    real(8) :: zero_cost              ! virtual cost of zero option, used to determine the visibility of a zero cost option

    type edge_type
        real(8), allocatable, dimension(:) :: tau   ,heu                ! pheromone intensity
        !real(8) :: heu                   ! visibility
        real(8) :: property              ! value that the path represents (i.e. cost or length)
        real(8) :: prob                  ! probability of edge selection
        real(8) :: gb(3)
    end type edge_type

    type path_type
        integer :: max_edge
        type(edge_type), allocatable, dimension(:) :: edge
    end type path_type

    !type obj_type
    type(path_type), allocatable, dimension(:) :: path
    ! end type obj_type
    
    ! type(obj_type), allocatable, dimension(:) :: obj

  end module ant_graph

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module para

! aaron zecchin, april 2002
! stores parameters for the ant system 

    real(8) :: alpha  ! pheromone importance factor
    real(8) :: beta   ! visibility importance factor
    real(8) :: rho    ! pheromone evaporation
    real(8) :: q      ! pheromone reward factor

  end module para

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module non_dom
  
  ! joanna szemis, january 2013
  ! stores variables associted for non-dominate sort
  
!    integer,dimension(5000) :: np !sp,sp_count		!dominace count, the number of ants which are dominated by ith ant !np moved into non_dom_sort
    integer,dimension(2) ::best_c,best2_c
!    integer,dimension(4000) ::obj_rank,q
!    integer,dimension(8000,8000):: f
!   real(8),dimension(4000,4000) :: sp1		
    real(8),dimension(2) :: best, best2  
    integer :: dom_less,dom_more ,dom_equal	!solution less dominated, solution is equal dominance, solution is more dominated
    integer :: n_front,n_count,q_count   !front,f_count
    integer::f_store
    integer:: maxitfin
 
  end module non_dom

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  module system_details

  ! joanna szemis, december 2012
  ! "system_details" module stores information of system (e.g. wetland information)

    
        integer, allocatable, dimension(:) :: num_edges     ! stores the number of edges for each decision point from user defined graph input routines
	    integer :: num_decision_pts                         ! number of decision points from the user defined graph input routines
        integer :: num_cost,num_years                      ! number of costs (e.g. water use and savings) and years
        integer :: fit_func                                 ! objective function to guide acoa 
        integer :: print_graph                              ! governs which graphs to develop (1 - all wetlands, 2 - phase 1a and 1ab)
        real(8) :: percent_pen,penaltycost                  ! max percentage variation, cost of penalty for constraint violations
        real(8) :: weight_ws, weight_wu                     ! weights for water savings and use
        real(8) :: pure_cost1, pure_cost2
        real(8) :: dummy2,temp_prob
        
        real(8),dimension(2,3) :: overall_best
        real:: start,finish,best_time, time_breach, dummy_time
        real :: maximum_allowable_runtime
        
        real(8),allocatable, dimension (:,:) :: PDMSR
        real(8),allocatable, dimension (:) :: trade_lim
        real(8),allocatable, dimension (:,:) :: storage
        real(8),allocatable, dimension (:,:) :: requirement
        real(8),allocatable, dimension (:,:) :: L_cap
        real(8),allocatable, dimension (:,:) :: trade_sort, e_trade_sort, i_trade_sort
        real(8),allocatable, dimension (:) :: trade_con
        integer,allocatable, dimension (:) :: exp_or_imp
        integer, allocatable, dimension (:) :: solution
        integer, allocatable, dimension(:) :: valid_trade
        real(8),dimension(10000,3) :: known_trade_links
        real(8), allocatable, dimension (:) :: current_trade_quantity
        integer, allocatable,dimension(:) :: current_trade_edge
        integer, allocatable, dimension(:) :: dyn_const
        integer, allocatable, dimension(:,:,:) :: dyn_const_store
        integer, dimension(2,2) :: iteration_best
        integer, allocatable, dimension(:) :: c_index, e_index, i_index
        integer, allocatable, dimension(:) :: counter, num_n_zero, num_test
        character(50), allocatable, dimension(:) :: c_names,e_names,i_names
        integer::y_count,t_count, e_count, i_count,dec_count,def_count,sort_count,dis_count,cost_count,it_count,v_count
        integer:: counter1,n1,fp_read,add_year,upper_lim, non_update
        integer:: min_ship_size
        integer:: max_deficit,max_deviation
        integer:: bucket_type
        real(8):: creep1,creep2, update_amount
        real(8), allocatable, dimension(:):: bucket
        real(8):: best1,best2,best12
        integer :: reset_tau_0
  
  
        character(60) :: graph_title1         ! stores the title of graphs developed
        character(60) :: graph_title2
  end module system_details
    
! :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module best_values

    ! joanna szemis, december 2012
    ! "best_values" module best solution from acoa run    
    
    real(8), allocatable, dimension(:) :: best_wet  ! stores the wetland starting wetland regimes
    real(8) :: best_val                             ! temporary value to store determine best solution 
    
  end module best_values
    
! :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module mad_details
    
! joanna szemis, december 2012
! "best_values" module best solution from acoa run  
     
     integer :: n_days                          ! stores the number of number of daily inputs from mad input file
     integer :: leap_yr                         ! stores which year the leap year occurs               
     integer, allocatable,dimension(:) :: ly    ! stores counter when reading in leap year information
    
   end module mad_details
   
   
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module lin_feedback_shift_reg
  ! l'ecuyer's 1999 random number generator.
  ! fortran version by alan.miller @ vic.cmis.csiro.au
  ! this version is for 64-bit integers and assumes that kind=8 identifies them.
  ! it has only been tested using lahey's lf95 compiler
  ! http://users.bigpond.net.au/amiller/
  ! http://www.ozemail.com.au/~milleraj
  ! latest revision - 12 january 2001

    integer, parameter      :: dp = selected_real_kind(14, 60)
    ! these are unsigned integers in the ! version
    integer (kind=8), save  :: s1 = 153587801, s2 = -759022222, s3 = 1288503317, s4 = -1718083407, s5 = -123456789 

    contains

        subroutine init_seeds(i1, i2, i3, i4, i5)

            integer , intent(in) :: i1, i2, i3, i4, i5
 
            s1 = i1
            s2 = i2
            s3 = i3
            s4 = i4
            s5 = i5
            if (iand(s1,      -2) == 0) s1 = i1 - 8388607
            if (iand(s2,    -512) == 0) s2 = i2 - 8388607
            if (iand(s3,   -4096) == 0) s3 = i3 - 8388607
            if (iand(s4, -131072) == 0) s4 = i4 - 8388607
            if (iand(s5,-8388608) == 0) s5 = i5 - 8388607

            return
       end subroutine init_seeds

       function lfsr258() result(random_numb)
     ! generates a random number between 0 and 1.  translated from ! function in:
     ! reference:
     ! l'ecuyer, p. (1999) `tables of maximally equidistributed combined lfsr
     ! generators', math. of comput., 68, 261-269.

     ! the cycle length is claimed to be about 2^(258) or about 4.6 x 10^77.
     ! actually - (2^63 - 1).(2^55 - 1).(2^52 - 1).(2^47 - 1).(2^41 - 1)

            real (dp) :: random_numb
            integer (kind=8)   :: b

        ! n.b. ishft(i,j) is a bitwise (non-circular) shift operation;
        !      to the left if j > 0, otherwise to the right.
 
            b  = ishft( ieor( ishft(s1,1), s1), -53)
            s1 = ieor( ishft( iand(s1,-2), 10), b)
            b  = ishft( ieor( ishft(s2,24), s2), -50)
            s2 = ieor( ishft( iand(s2,-512), 5), b)
            b  = ishft( ieor( ishft(s3,3), s3), -23)
            s3 = ieor( ishft( iand(s3,-4096), 29), b)
            b  = ishft( ieor( ishft(s4,5), s4), -24)
            s4 = ieor( ishft( iand(s4,-131072), 23), b)
            b  = ishft( ieor( ishft(s5,3), s5), -33)
            s5 = ieor( ishft( iand(s5,-8388608), 8), b)

            ! the constant below is the reciprocal of (2^64 - 1)
            random_numb = ieor( ieor( ieor( ieor(s1,s2), s3), s4), s5) * 5.4210108624275221e-20_dp + 0.5_dp

            return
       end function lfsr258

   end module lin_feedback_shift_reg

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  module input_cost
    type cost_details 
        real(8),allocatable, dimension (:,:) :: euc_d
    end type
 

    
   
    type(cost_details), allocatable, dimension (:) :: cost
    !type(cost_details), allocatable, dimension (:,:) :: PDMSR
  
  end module
 
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  module user

    integer:: n_cities
    integer:: n_countries, n_i_countries, n_e_countries
    integer:: n_years
    integer:: b_count

  end module user