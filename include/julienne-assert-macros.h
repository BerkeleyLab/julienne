! julienne-assert-macros.h: provides preprocessor-based assertion macros
! that are guaranteed to compile away statically when disabled.

#ifndef ASSERTIONS
! Assertions are off by default
#define ASSERTIONS 0
#endif

! Enable repeated includes to toggle assertions based on current settings:
#undef call_julienne_assert

#if ASSERTIONS
# define call_julienne_assert(assertion) call call_julienne_assert(assertion, __FILE__, __LINE__)
#else
# define call_julienne_assert(assertion)
#endif
