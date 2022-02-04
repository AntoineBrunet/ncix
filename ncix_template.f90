#define PASTER(x,y,z) x ## _ ## y ## _ ## z
#define EVALUATOR(x,y,z)  PASTER(x,y,z)


#ifndef NCIX_NOATTR
#include "ncix_attrs_template.f90"
#endif

#include "ncix_records_template.f90"

#undef NCIX_TYPENAME
#undef NCIX_TYPE

#undef NCIX_NOATTR
#undef NCIX_DTYPE
#undef NCIX_TL
