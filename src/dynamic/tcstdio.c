#include <stdio.h>

#include "tcinternal.h"
#include <tclibc.h>

char * _typecheck_fgets(char * s, int n, FILE * stream)
{
  char * ret;
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
  if(!verifyAlloc(s, n)){
    /* target is unallocated; warn */
    _output_simple(TCFG_RIUM);
  }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
  ret = fgets(s, n, stream);
  if(ret)
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, s,strlen(s));
  return ret;
}

char * _typecheck_gets(char * s)
{
  char * ret = gets(s);
  if(ret){
    int slen = strlen(ret);
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
    if(!verifyAlloc(s, slen+1)){
      /* target is unallocated; warn */
      _output_simple(TCG_RIUM);
    }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, s,slen);
  }
  return ret;
}

size_t _typecheck_fread(void * ptr, size_t size, size_t nitems, FILE * stream)
{
  size_t ret;
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
  if(!verifyAlloc(ptr, size*nitems)){
    /* target is unallocated; warn */
    _output_simple(TCFR_RIUM);
  }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
  ret = fread(ptr, size, nitems, stream);

  if(ret){

/* NEW CODE: set to INI tag */
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
                ptr, size*nitems, _typetag_init, _typetag_uninit);

#if 0 /* { OLD CODE: */
    /* Assumption: this is in order of likelihood */
    if(size == sizeof(char)){
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,nitems-1);
    } else if(size == sizeof(int)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_int);
    } else if(size == sizeof(short)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_short);
    } else if(size == sizeof(long)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_long);
    } else if(size == sizeof(long long)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_longlong);
    } else if(size == sizeof(double)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_double);
    } else if(size == sizeof(float)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_float);
#ifdef LONGDOUBLE /* { */
    } else if(size == sizeof(long double)){
      int i; for(i = 0; i < nitems; ++i)
               _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,_ctype_longdouble);
#endif /* } LONGDOUBLE */
    } else { /* default: char */
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr,size*nitems-1);
    }
#endif /* } if 0  */
  }
  return ret;
}

