program test_intrinsinc_array_t
  !! Test direct intrinsic_array_t derive type construction and conversion to srings
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  integer j
  complex, parameter :: z = (-1., -1.)
  complex, parameter :: complex_1D(*)     =         [(z, j=1,2    )]
  complex, parameter :: complex_2D(*,*)   = reshape([(z, j=1,2*2  )], [2, 2   ])
  complex, parameter :: complex_3D(*,*,*) = reshape([(z, j=1,2*2*2)], [2, 2, 2])
  complex(kind(1.D0)), parameter :: z_double = (-1.D0, -2.D0)
  complex(kind(z_double)), parameter :: complex_double_1D(*)     =         [(z_double, j=1, 2    )]
  complex(kind(z_double)), parameter :: complex_double_2D(*,*)   = reshape([(z_double, j=1, 2*2  )], [2, 2   ])
  complex(kind(z_double)), parameter :: complex_double_3D(*,*,*) = reshape([(z_double, j=1, 2*2*2)], [2, 2, 2])
  integer, parameter :: integer_1D(*)     =         [(0, j=1,2        )]
  integer, parameter :: integer_2D(*,*)   = reshape([(1, j=1,2*2      )], [2, 2   ])
  integer, parameter :: integer_3D(*,*,*) = reshape([(2, j=1,2*2*2    )], [2, 2, 2])
  logical, parameter :: logical_1D(*)     =         [(.true., j=1,2    )]
  logical, parameter :: logical_2D(*,*)   = reshape([(.true., j=1,2*2  )], [2, 2   ])
  logical, parameter :: logical_3D(*,*,*) = reshape([(.true., j=1,2*2*2)], [2, 2, 2])

#ifndef __flang__
    if (this_image()==1) then
#endif

  print*
  print*,"An intrinsic_array_t object"
  print*,"  "//pass_fail(dble(integer_1D)) //" on construction from a 1D double-precision array and conversion to a string"
  print*,"  "//pass_fail(dble(integer_2D)) //" on construction from a 2D double-precision array and conversion to a string"
  print*,"  "//pass_fail(dble(integer_3D)) //" on construction from a 3D double-precision array and conversion to a string"
  print*,"  "//pass_fail(integer_1D)       //" on construction from a 1D integer array and conversion to a string"
  print*,"  "//pass_fail(integer_2D)       //" on construction from a 2D integer array and conversion to a string"
  print*,"  "//pass_fail(integer_3D)       //" on construction from a 3D integer array and conversion to a string"
  print*,"  "//pass_fail(logical_1D)       //" on construction from a 1D logical array and conversion to a string"
  print*,"  "//pass_fail(logical_2D)       //" on construction from a 2D logical array and conversion to a string"
  print*,"  "//pass_fail(logical_3D)       //" on construction from a 3D logical array and conversion to a string"
  print*,"  "//pass_fail(real(integer_1D)) //" on construction from a 1D real array and conversion to a string"
  print*,"  "//pass_fail(real(integer_2D)) //" on construction from a 2D real array and conversion to a string"
  print*,"  "//pass_fail(real(integer_3D)) //" on construction from a 3D real array and conversion to a string"
  print*,"  "//pass_fail(complex_1D)       //" on construction from a 1D complex array and conversion to a string"
  print*,"  "//pass_fail(complex_2D)       //" on construction from a 2D complex array and conversion to a string"
  print*,"  "//pass_fail(complex_3D)       //" on construction from a 3D complex array and conversion to a string"
  print*,"  "//pass_fail(complex_double_1D)//" on construction from a 1D double-precision complex array and conversion to a string"
  print*,"  "//pass_fail(complex_double_2D)//" on construction from a 2D double-precision complex array and conversion to a string"
  print*,"  "//pass_fail(complex_double_3D)//" on construction from a 3D double-precision complex array and conversion to a string"

#ifndef __flang__
    end if
#endif

contains
    
  
  pure function pass_fail(to_write)
    class(*), intent(in) :: to_write(..)
    character(len=:), allocatable :: pass_fail
    integer, parameter :: max_length = 2048
    character(len=max_length) array_as_string
    type(intrinsic_array_t) intrinsic_array

    select rank(to_write)
      rank(1)
        select type(to_write)
          type is(complex)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(complex(kind(0.D0)))
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(double precision)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              double precision from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(integer)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              integer from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(logical)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              logical from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read .eqv. to_write)))
            end block
          type is(real)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string, *) intrinsic_array%as_character()
            block
              real from_read(size(to_write,1))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          class default
            error stop "test_intrinsic_array_t: unrecognized rank-1 type"
        end select
      rank(2)
        select type(to_write)
          type is(complex)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(complex(kind(1.D0)))
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(double precision)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              double precision from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(integer)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              integer from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(logical)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              logical from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read .eqv. to_write)))
            end block
          type is(real)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              real from_read(size(to_write,1), size(to_write,2))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          class default
            error stop "test_intrinsic_array_t: unrecognized rank-2 type"
        end select
      rank(3)
        select type(to_write)
          type is(complex)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(complex(kind(0.D0)))
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              complex from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(double precision)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              double precision from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(integer)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              integer from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          type is(logical)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              logical from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read .eqv. to_write)))
            end block
          type is(real)
            intrinsic_array = intrinsic_array_t(to_write)
            write(array_as_string,*) intrinsic_array%as_character()
            block
              real from_read(size(to_write,1), size(to_write,2), size(to_write,3))
              read(array_as_string,*) from_read
              pass_fail = trim(merge("passes", "FAILS ", all(from_read == to_write)))
            end block
          class default
            error stop "test_intrinsic_array_t: unrecognized rank-3 type"
        end select
      rank default
        error stop "test_intrinsic_array_t: unsupported rank (3)"
    end select
  end function
  
end program test_intrinsinc_array_t
