#define NCIX_FUNCNAME__(F1,type,F2,shape) F1 ## _ ## type ## _ ## F2 ## shape
#define NCIX_FUNCNAME_(F1,type,F2,shape) NCIX_FUNCNAME__(F1,type,F2,shape)
#define NCIX_FUNCNAME(type,shape) NCIX_FUNCNAME_(NCIX_F1,type,NCIX_F2,shape)

        procedure, pass :: NCIX_FUNCNAME(float,sca)
        procedure, pass :: NCIX_FUNCNAME(double,sca)
        procedure, pass :: NCIX_FUNCNAME(int2,sca)
        procedure, pass :: NCIX_FUNCNAME(int4,sca)
        procedure, pass :: NCIX_FUNCNAME(epoch,sca)
        procedure, pass :: NCIX_FUNCNAME(epoch16,sca)
#ifdef HAS_STR
        procedure, pass :: NCIX_FUNCNAME(str,sca)
#endif
#ifdef DATETIME_FORTRAN
        procedure, pass :: NCIX_FUNCNAME(datetime,sca)
#endif
        procedure, pass :: NCIX_FUNCNAME(detailepoch,sca)

#ifndef ONLY_SCA
        procedure, pass :: NCIX_FUNCNAME(float,vec)
        procedure, pass :: NCIX_FUNCNAME(float,mat)
        procedure, pass :: NCIX_FUNCNAME(float,ter)
        procedure, pass :: NCIX_FUNCNAME(float,qad)
        procedure, pass :: NCIX_FUNCNAME(double,vec)
        procedure, pass :: NCIX_FUNCNAME(double,mat)
        procedure, pass :: NCIX_FUNCNAME(double,ter)
        procedure, pass :: NCIX_FUNCNAME(double,qad)
        procedure, pass :: NCIX_FUNCNAME(int2,vec)
        procedure, pass :: NCIX_FUNCNAME(int2,mat)
        procedure, pass :: NCIX_FUNCNAME(int2,ter)
        procedure, pass :: NCIX_FUNCNAME(int2,qad)
        procedure, pass :: NCIX_FUNCNAME(int4,vec)
        procedure, pass :: NCIX_FUNCNAME(int4,mat)
        procedure, pass :: NCIX_FUNCNAME(int4,ter)
        procedure, pass :: NCIX_FUNCNAME(int4,qad)
        procedure, pass :: NCIX_FUNCNAME(epoch,vec)
        procedure, pass :: NCIX_FUNCNAME(epoch,mat)
        procedure, pass :: NCIX_FUNCNAME(epoch,ter)
        procedure, pass :: NCIX_FUNCNAME(epoch,qad)
        procedure, pass :: NCIX_FUNCNAME(epoch16,vec)
        procedure, pass :: NCIX_FUNCNAME(epoch16,mat)
        procedure, pass :: NCIX_FUNCNAME(epoch16,ter)
        procedure, pass :: NCIX_FUNCNAME(epoch16,qad)
#ifdef DATETIME_FORTRAN
        procedure, pass :: NCIX_FUNCNAME(datetime,vec)
        procedure, pass :: NCIX_FUNCNAME(datetime,mat)
        procedure, pass :: NCIX_FUNCNAME(datetime,ter)
        procedure, pass :: NCIX_FUNCNAME(datetime,qad)
#endif
        procedure, pass :: NCIX_FUNCNAME(detailepoch,vec)
        procedure, pass :: NCIX_FUNCNAME(detailepoch,mat)
        procedure, pass :: NCIX_FUNCNAME(detailepoch,ter)
        procedure, pass :: NCIX_FUNCNAME(detailepoch,qad)
#endif
        generic :: NCIX_IFX_NAME => NCIX_FUNCNAME(float,sca), &
                          NCIX_FUNCNAME(double,sca), &
                          NCIX_FUNCNAME(int2,sca), &
                          NCIX_FUNCNAME(int4,sca), &
                          NCIX_FUNCNAME(detailepoch,sca), &
                          NCIX_FUNCNAME(epoch,sca), &
#ifdef DATETIME_FORTRAN
                          NCIX_FUNCNAME(datetime,sca), &
#endif
#ifdef HAS_STR
                          NCIX_FUNCNAME(str,sca), &
#endif
#ifdef ONLY_SCA
                          NCIX_FUNCNAME(epoch16,sca)
#else
                          NCIX_FUNCNAME(epoch16,sca), &
                          NCIX_FUNCNAME(float,vec), &
                          NCIX_FUNCNAME(float,mat), &
                          NCIX_FUNCNAME(float,ter), &
                          NCIX_FUNCNAME(float,qad), &
                          NCIX_FUNCNAME(double,vec), &
                          NCIX_FUNCNAME(double,mat), &
                          NCIX_FUNCNAME(double,ter), &
                          NCIX_FUNCNAME(double,qad), &
                          NCIX_FUNCNAME(int2,vec), &
                          NCIX_FUNCNAME(int2,mat), &
                          NCIX_FUNCNAME(int2,ter), &
                          NCIX_FUNCNAME(int2,qad), &
                          NCIX_FUNCNAME(int4,vec), &
                          NCIX_FUNCNAME(int4,mat), &
                          NCIX_FUNCNAME(int4,ter), &
                          NCIX_FUNCNAME(int4,qad), &
                          NCIX_FUNCNAME(epoch,vec), &
                          NCIX_FUNCNAME(epoch,mat), &
                          NCIX_FUNCNAME(epoch,ter), &
                          NCIX_FUNCNAME(epoch,qad), &
                          NCIX_FUNCNAME(epoch16,vec), &
                          NCIX_FUNCNAME(epoch16,mat), &
                          NCIX_FUNCNAME(epoch16,ter), &
                          NCIX_FUNCNAME(epoch16,qad), &
#ifdef DATETIME_FORTRAN
                          NCIX_FUNCNAME(datetime,vec), &
                          NCIX_FUNCNAME(datetime,mat), &
                          NCIX_FUNCNAME(datetime,ter), &
                          NCIX_FUNCNAME(datetime,qad), &
#endif
                          NCIX_FUNCNAME(detailepoch,vec), &
                          NCIX_FUNCNAME(detailepoch,mat), &
                          NCIX_FUNCNAME(detailepoch,ter), &
                          NCIX_FUNCNAME(detailepoch,qad)
#endif

#undef NCIX_IFX_NAME
#undef NCIX_F1
#undef NCIX_F2
#undef ONLY_SCA
#undef HAS_STR
