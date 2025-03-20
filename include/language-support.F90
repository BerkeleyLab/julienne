! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  ! Define whether the compiler supports associating a procedure pointer dummy argument with an
  ! actual argument that is a valid target for the pointer dummy in a procedure assignment, a
  ! feature introduced in Fortran 2008 and described in Fortran 2023 clause 15.5.2.10 paragraph 5.
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__)
#define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 1
#else
#define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 0
#endif
#endif

#ifndef HAVE_MULTI_IMAGE_SUPPORT
  ! Define whether the compiler supports the statements and intrinsic procedures that support
  ! multi-image execution, e.g., this_image(), sync all, etc.
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__GFORTRAN__)
#define HAVE_MULTI_IMAGE_SUPPORT 1
#else
#define HAVE_MULTI_IMAGE_SUPPORT 0
#endif
#endif
