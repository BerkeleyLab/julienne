! assert_macros.h: provides preprocessor-based assertion macros
! that are guaranteed to compile away statically when disabled.

! Enable repeated includes to toggle assertions based on current settings:
#undef call_assert
#undef call_assert_describe
#undef call_assert_diagnose

#ifndef ASSERTIONS
! Assertions are off by default
#define ASSERTIONS 0
#endif

! Deal with stringification issues:
! https://gcc.gnu.org/legacy-ml/fortran/2009-06/msg00131.html
#ifndef CPP_STRINGIFY_SOURCE
# if defined(__GFORTRAN__) || defined(_CRAYFTN) || defined(NAGFOR)
#  define CPP_STRINGIFY_SOURCE(x) "x"
# else
#  define CPP_STRINGIFY_SOURCE(x) #x
# endif
#endif

#if ASSERTIONS
# define call_assert(assertion) call assert_always(assertion, "call_assert(" // CPP_STRINGIFY_SOURCE(assertion) // ") in file " // __FILE__ // ", line " // fortran_stringify_integer(__LINE__))
# define call_assert_describe(assertion, description) call assert_always(assertion, description // " in file " // __FILE__ // ", line " // fortran_stringify_integer(__LINE__))
# define call_assert_diagnose(assertion, description, diagnostic_data) call assert_always(assertion, description // " in file " // __FILE__ // ", line " // fortran_stringify_integer(__LINE__), diagnostic_data)
#else
# define call_assert(assertion)
# define call_assert_describe(assertion, description)
# define call_assert_diagnose(assertion, description, diagnostic_data)
#endif
