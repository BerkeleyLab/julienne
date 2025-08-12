! Copyright (c) 2024-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef _JULIENNE_LANGUAGE_SUPPORT_H
#define _JULIENNE_LANGUAGE_SUPPORT_H

#ifdef __GNUC__
#  define GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#else
#  undef GCC_VERSION
#endif

#if __GNUC__ && ( __GNUC__ < 14 || (__GNUC__ == 14 && __GNUC_MINOR__ < 3) )
#define GCC_GE_MINIMUM
#endif

! If not already determined, make a compiler-dependent determination of whether Julienne may pass
! procedure actual arguments to procedure pointer dummy arguments, a feature introduced in
! Fortran 2008 and described in Fortran 2023 clause 15.5.2.10 paragraph 5.

#ifndef HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__) || (GCC_VERSION > 140200)
#    define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 1
#  else
#    define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 0
#  endif
#endif

! If not already determined, make a compiler-dependent determination of whether Julienne may use
! multi-image features such as `this_image()` and `sync all`.
#ifndef HAVE_MULTI_IMAGE_SUPPORT
#  if defined(__flang__)
#    define HAVE_MULTI_IMAGE_SUPPORT 0
#  else
#    define HAVE_MULTI_IMAGE_SUPPORT 1
#  endif
#endif

! If not already determined, make a compiler-dependent determination of whether Julienne may use
! kind type parameters for derived types.
#ifndef HAVE_DERIVED_TYPE_KIND_PARAMETERS
#  if defined(__GFORTRAN__) || defined(__LFORTRAN__)
#    define HAVE_DERIVED_TYPE_KIND_PARAMETERS 0
#  else
#    define HAVE_DERIVED_TYPE_KIND_PARAMETERS 1
#  endif
#endif

#endif
