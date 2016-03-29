#include <string.h>
#include <limits.h> /* for INT_MAX */

#include "tcinternal.h"
#include <tclibc.h>

void *_typecheck_memcpy(void * s1, const void * s2, size_t n)
{
  if(n){
    if(!verifyAlloc(s2, n)){
      /* source is unallocated; warn */
      _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMC_CFU);

      if(!verifyAlloc(s1, n)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMC_CIU);
      }
    } else {
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		s1, s2, n, _ctype_aggregate);
    }
  }
  return memcpy(s1, s2, n);
}

void *_typecheck_memmove(void * s1, const void * s2, size_t n)
{
  if(n){
    if(!verifyAlloc(s2, n)){
      /* source is unallocated; warn */
      _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMM_CFU);

      if(!verifyAlloc(s1, n)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMM_CIU);
      }
    } else {
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		s1, s2, n, _ctype_aggregate);
    }
  }
  return memmove(s1, s2, n);
}

/* if unalloc, return 0 - number of allocated bytes
 * else, return number of bytes checked
 * e.g. return 0 => s[0] unallocated
 *      (assume lim > 0)
 */
static int checkCharAllocUpTo(const char * s, int c, size_t lim)
{
  int i = 0;
  _mirror_pos_t tagpos;

  GET_TAG_POS(tagpos, s);

  while(i < lim) {
    if(TAG_BITS(tagpos) == _typetag_unalloc) return -i;
    if(s[i] == c) return i+1;
    NEXT_POS(tagpos);
    i++;
  }

  return i;
}

char *_typecheck_strcpy(char * dst, const char * src)
{
    int i = checkCharAllocUpTo((const char *) src, 0, INT_MAX);
    if(i <= 0){
      /* source is not fully allocated; warn */
      _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "strcpy",0,0,0, -1,-i,0,0);

      i = strlen(src);
      if(!verifyAlloc(dst, i)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strcpy",0,0,0, i,0,0,0);
      }
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,i);
    } else {
      /* copy i bytes */
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		dst, src, i, _ctype_aggregate);
    }
  return strcpy(dst, src);
}

char *_typecheck_strncpy(char * dst, const char * src, size_t n)
{
  if(n){
    int i = checkCharAllocUpTo((const char *) src, 0, n);
    if(i <= 0){
      /* source is not fully allocated; warn */
      _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "strncpy",0,0,0, n,-i,0,0);

      if(!verifyAlloc(dst, n)){
        /* destination is unallocated; warn */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strncpy",0,0,0, n,0,0,0);
      }
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,n);
    } else {
      if(!verifyAlloc(dst, n)){
        /* destination is unallocated; warn */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strncpy",0,0,0, n,0,0,0);
      }
      /* copy i bytes */
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		dst, src, i, _ctype_aggregate);
      /* pad n-i bytes */
      if(n - i){
        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst+i,n-i-1);
      }
    }
  }
  return strncpy(dst, src, n);
}

char *_typecheck_strcat(char * dst, const char * src)
{
    int j;
    int i = checkCharAllocUpTo((const char *) src, 0, INT_MAX);
    if(i <= 0){
      /* source is not fully allocated; warn */
      _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "strcat",0,0,0, -1,-i,0,0);

      i = strlen(src);

      j = checkCharAllocUpTo((const char *) dst, 0, INT_MAX);
      if(j <= 0 || !verifyAlloc(dst+j, i)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strcat",0,0,0, i,0,0,0);
      }
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,strlen(dst)+i);
    } else {
      j = checkCharAllocUpTo((const char *) dst, 0, INT_MAX);
      if(j <= 0){
        /* first part of destination is unallocated; warn */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strcat",0,0,0, i,0,0,0);

        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,strlen(dst));
      }
      /* copy i bytes */
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		dst+strlen(dst), src, i, _ctype_aggregate);
    }
  return strcat(dst, src);
}

char *_typecheck_strncat(char * dst, const char * src, size_t n)
{
  if(n){
    int j;
    int i = checkCharAllocUpTo((const char *) src, 0, n);
    if(i <= 0){
      /* source is not fully allocated; warn */
      _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "strncat",0,0,0, n,-i,0,0);

      i = strlen(src);
      if(i > n) i = n;

      j = checkCharAllocUpTo((const char *) dst, 0, INT_MAX);
      if(j <= 0 || !verifyAlloc(dst+j, i)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strncat",0,0,0, i,0,0,0);
      }
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,strlen(dst)+i);
    } else {
      j = checkCharAllocUpTo((const char *) dst, 0, INT_MAX);
      if(j <= 0){
        /* first part of destination is unallocated; warn */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "strncat",0,0,0, i,0,0,0);

        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, dst,strlen(dst));
      }
      /* copy i bytes */
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		dst+strlen(dst), src, i, _ctype_aggregate);
      if(i == n){
        _verifyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			"_tc_strncat:dest[n]", dst+strlen(dst)+i, _ctype_char);
        _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			dst+strlen(dst)+i, _ctype_char);
      }
    }
  }
  return strncat(dst, src, n);
}

size_t _typecheck_strxfrm(char * s1, const char * s2, size_t n)
{
  return strxfrm(s1, s2, n);
}

void _typecheck_bzero(void * s, size_t n)
{
  if(!verifyAlloc(s, n)){
    /* target is unallocated; warn */
    _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMS_SUM);
  }
  _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		s, n, _typetag_init, _typetag_uninit);
  bzero(s, n);
}

void *_typecheck_memset(void * s, int c, size_t n)
{
  if(!verifyAlloc(s, n)){
    /* target is unallocated; warn */
    _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCMS_SUM);
  }
  if(c == 0)
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		s, n, _typetag_init, _typetag_uninit);
  else
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, s,n-1);
  return memset(s, c, n);
}

/* initialize type for returned string? */
/* char *_typecheck_strerror(int errnum); */

void *_typecheck_memccpy(void * s1, const void * s2, int c, size_t n)
{
  if(n){
    int i = checkCharAllocUpTo((const char *) s2, c, n);
    if(i <= 0){
      /* source is not fully allocated; warn */
      _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "memccpy",0,0,0, n,-i,0,0);

      if(!verifyAlloc(s1, i)){
        /* destination is unallocated; warn */
/* NOTE: this check is only done here since _copyTag does a verifyAlloc
	 on its destination. */
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CIU, "memccpy",0,0,0, i,0,0,0);
      }
    } else {
      /* copy i bytes */
      _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		s1, s2, i, _ctype_aggregate);
    }
  }
  return memccpy(s1, s2, c, n);
}

/* initialize type for returned string? */
/* char *_typecheck_strsignal(int sig); */

char *_typecheck_strdup(const char * s1)
{
  char * ret;
  int i = checkCharAllocUpTo((const char *) s1, 0, INT_MAX);
  if(i <= 0){
    /* source is not fully allocated; warn */
    _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCLIB_CFU, "strdup",0,0,0, -1,-i,0,0);
  }

  ret = strdup(s1);
  if(ret) _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ret,strlen(ret));
  return ret;
}

#ifdef STANDALONE

int foofoo()
{
  char * c = "abc";
  _setUninitTag("foo",0,0, c, 4);
  printf("Verify abc up to c,1 = %d\n",
	checkCharAllocUpTo(c,'c',1));
  printf("Verify abc up to c,10 = %d\n",
	checkCharAllocUpTo(c,'c',10));
  printf("Verify abc up to 0,10 = %d\n",
	checkCharAllocUpTo(c, 0 ,10));
  printf("Verify abc up to d,3 = %d\n",
	checkCharAllocUpTo(c,'d',3));
  printf("Verify abc up to d,10 = %d\n",
	checkCharAllocUpTo(c,'d',10));
}

#endif

