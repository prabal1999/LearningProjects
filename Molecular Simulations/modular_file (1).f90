module mod_useful_files
  implicit none
  !defining all the required variables
  integer npart, n_steps, natoms_side, part_num
  real*8, allocatable :: pos(:,:), vel(:,:), acc(:,:), mass(:)
  real*8 pi, dt, total_time
  real*8 omega, mass_atom
  real*8 pot, energy, epsilon
  real*8 sigma
  real*8 tim, density, lat_len

  !!!!!!!!!!!!!!!!!!!!!!!!!! REQUIRED SUBROUTINES !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  contains
  !reading the file named "mod.inp" and assigning the values to our variables
  subroutine set_parameters
    implicit none
    open(10, file ="mod.inp")
    read(10,*)npart
    read(10,*)mass_atom
    read(10,*)sigma
    read(10,*)epsilon
    read(10,*)density
    read(10,*)pi
    read(10,*)dt
    read(10,*)total_time
    close(10)

    allocate(pos(npart,3), vel(npart,3), acc(npart,3), mass(npart))
    !since we have all the identical particles, they all have the same mass and we initlize the mass array with that one value
    mass = mass_atom
    natoms_side = nint(npart**(1/3.d0))

  end subroutine set_parameters

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !this subroutine initializes the pos, vel, acc matrices
  subroutine initial_condition
    implicit none

    integer i, j, k
    call lattice_length
    
    part_num = 1
    do i = 1, natoms_side
      do j = 1, natoms_side
        do k = 1, natoms_side
          pos(part_num,1) = (i-1)*lat_len
          pos(part_num,2) = (j-1)*lat_len
          pos(part_num,3) = (k-1)*lat_len

          part_num = part_num + 1
        end do
      end do
    end do
    close(20)

    vel = 0.d0     !initial velocity
    acc = 0.d0     !initial accleration
    
    !initializing the total energy and energy of the first bond
    call compute_pot(pos, pot, acc)
    energy = pot

    tim = 0.0
  
  end subroutine initial_condition

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !evolving the system of N harmonic oscillators
  subroutine evolve
    implicit none
    integer i 
    
    open(11, file="energy_values.out")
    open(3, file="movie.xyz")

    !writing initial positions of the system in the vmd file
    call write_VMD(pos)
    n_steps = int(total_time/dt)
    do i = 1, n_steps
      
      write(11,*)tim, energy

      !writing vmd file at every 10th step of the simulation of particles
      if ( mod(i,10) == 0 ) then
        call write_VMD(pos)
      end if
      !computing the energy
      call compute_energy

      !updating the pos, velocity, PE and accleration
      call velocity_verlet(pos,vel,acc,pot)
      tim = tim + dt

    end do
    
    close(3)
    close(11)     
  
  end subroutine evolve

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! compute the potential and accleration for given position of the particles
  subroutine compute_pot(pos, pot, acc)
    implicit none
    real*8,dimension(npart,3),intent(in) :: pos    
    real*8,intent(out) :: acc(npart,3), pot
    real*8 rij, V_LJ, dV_LJ, vec(3)
    integer i, j     
    
    pot = 0.d0
    acc = 0.d0
    do i = 1, npart-1
      do j = i+1, npart
        call distance(i,j,rij,vec)
        call Leonard_Jones(rij, V_LJ, dV_LJ)
        pot = pot + V_LJ

        acc(i,:) = acc(i,:) - (1.d0/mass(i))*(dV_LJ*vec/rij)
        acc(j,:) = acc(j,:) + (1.d0/mass(j))*(dV_LJ*vec/rij)
      end do
    end do
  
  end subroutine compute_pot

  !distance formula d = sqrt((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)
  subroutine distance(i, j, rij, vec)
    implicit none
    integer,intent(in) :: i, j
    real*8,intent(out) :: rij, vec(3)
    
    vec = (pos(i,:) - pos(j,:))   !vector - vi-vj
    rij = dsqrt(sum(vec**2))
  
  end subroutine distance

  !Subroutine to Calculate the leonard jones potential for current pair of atoms
  subroutine Leonard_Jones(rij, V_LJ, dV_LJ)
    implicit none
    real*8,intent(in) :: rij
    real*8,intent(out) :: V_LJ, dV_LJ
    real*8 sigma6_x6
    sigma6_x6 = (sigma/rij)**6

    ! V_LJ = 4*epsilon*((sigma/rij)**12 - (sigma/rij)**6)
    ! dV_LJ = (4*epsilon/rij)*(-12*(sigma/rij)**12 + 6*(sigma/rij)**6)
    V_LJ = 4*epsilon*sigma6_x6*(sigma6_x6 - 1.d0)
    dV_LJ = (48*epsilon/rij)*sigma6_x6*(-sigma6_x6 + 0.5d0)
  
  end subroutine Leonard_Jones

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !the main velocity verlet algo
  subroutine velocity_verlet(pos, vel, acc, pot)
    implicit none
    real*8,dimension(npart,3),intent(inout) :: pos, vel, acc
    real*8,intent(inout) :: pot 
    real*8 acc0(npart,3)
       
    acc0 = acc      !acc0 is the initial accleration

    !x(t0 + dt) using taylor series expansion
    pos = pos + vel*dt + 0.5*acc*dt**2

    call compute_pot(pos, pot, acc)
    ! v(t0 + dt) using taylor series expansion
    ! velocity-verlet algorithm
    vel = vel + 0.5*(acc0 + acc)*dt
    
  end subroutine velocity_verlet

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !keeps track of all the energy of the system
  subroutine compute_energy
    implicit none
    energy = pot + 0.5*mass_atom*sum(vel*vel)
  
  end subroutine compute_energy

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !generate a VMD file
  subroutine write_VMD(pos)
    implicit none
    real*8,intent(in) :: pos(npart,3)
    integer atom
    
    write(3,*)npart
    write(3,*)
    
    do atom = 1, npart
      write(3,*)"C",pos(atom,:)
    end do
    
  end subroutine write_VMD

  !latticle length of the lattice
  subroutine lattice_length
    implicit none
    real*8 box_length
    box_length = (npart/density)**(1/3.d0)
    
    lat_len = box_length/real(natoms_side)
  end subroutine lattice_length

  

end module mod_useful_files