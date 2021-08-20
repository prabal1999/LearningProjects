program Molecular_Dynamics
  use mod_useful_files
  implicit none
  real*8 tic, toc

  call cpu_time(tic)
  !STEP:1) Setting parameters all in a.u.
  call set_parameters 

  !STEP:2) Initial conditions
  call initial_condition 

  !STEP:3) Evolving 
  call evolve
  call cpu_time(toc)
  write(*,*)"total time taken =",toc-tic

end program Molecular_Dynamics