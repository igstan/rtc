#include <stdlib.h>
#include <string.h> /* bzero */

#define TC_MALLOC_PADDING 64

#include "tcptr_internal.h"
#include "tcmalloc-hash.h"

void * _typecheck_malloc(size_t size)
{   
  void * newmem;
  
  newmem = malloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
  } 

  return newmem;
}

void * _typecheck_malloc_zero(size_t size)
{   
  void * newmem;
  
  newmem = malloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
    bzero(newmem, size);
  } 

  return newmem;
}

void * malloc_zero(size_t size)
{   
  void * newmem;
  
  newmem = malloc(size);

  if(newmem){
    bzero(newmem, size);
  } 

  return newmem;
}

void * _typecheck_calloc(size_t nelem, size_t elsize)
{
  void * newmem;

  newmem = calloc(nelem + TC_MALLOC_PADDING/elsize, elsize);

  if(newmem){
    size_t size = nelem * elsize;
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
  }

  return newmem;
}

void   _typecheck_free(void * ptr)
{
  size_t siz = mhash_remove(ptr);

  if(siz){
    free(ptr);
    _tcptr_clearTags(ptr, siz);
  } else {
    _output_flce(__FILE__,__LINE__,0,"(_typecheck_free)","Freeing unallocated memory");
  }
}

/* don't output error if ptr target not allocated */
void   _typecheck_free_partial(void * ptr)
{
  size_t siz = mhash_remove(ptr);

  if(siz){
    free(ptr);
    _tcptr_clearTags(ptr, siz);
  }
}

void * _typecheck_memalign(size_t alignment, size_t size)
{
  void * newmem;

  newmem = memalign(alignment, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
  }

  return newmem;
}

void * _typecheck_memalign_zero(size_t alignment, size_t size)
{
  void * newmem;

  newmem = memalign(alignment, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
    bzero(newmem, size);
  }

  return newmem;
}

void * memalign_zero(size_t alignment, size_t size)
{
  void * newmem;

  newmem = memalign(alignment, size);

  if(newmem){
    bzero(newmem, size);
  }

  return newmem;
}

void * _typecheck_realloc(void * ptr, size_t size)
{
  void * newmem;
  size_t oldsiz;

  if(ptr == 0) /* behaves like malloc */
    return _typecheck_malloc(size);
  if(size == 0){ /* behaves like free */
    _typecheck_free(ptr);
    return 0;
  }

  oldsiz = mhash_remove(ptr);

  newmem = realloc(ptr, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(newmem == ptr){ /* memory block extended */
      if(size < oldsiz){ /* shrink */
        _tcptr_clearTags((char *)ptr+size, oldsiz-size);
      } else { /* extend */
        _tcptr_setTags((char *)ptr+oldsiz, size-oldsiz);
      }
    } else { /* new memory block created */
      /*NOTE: newmem and old block may overlap: must clear before set!! */
      _tcptr_clearTags(ptr, oldsiz);
      _tcptr_setTags(newmem, size);
    }
  }

  return newmem;
}

void * _typecheck_realloc_zero(void * ptr, size_t size)
{
  void * newmem;
  size_t oldsiz;

  if(ptr == 0) /* behaves like malloc */
    return _typecheck_malloc_zero(size);
  if(size == 0){ /* behaves like free */
    _typecheck_free(ptr);
    return 0;
  }

  oldsiz = mhash_remove(ptr);

  newmem = realloc(ptr, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(newmem == ptr){ /* memory block extended */
      if(size < oldsiz){ /* shrink */
        _tcptr_clearTags((char *)ptr+size, oldsiz-size);
      } else { /* extend */
        _tcptr_setTags((char *)ptr+oldsiz, size-oldsiz);
      }
    } else { /* new memory block created */
      /*NOTE: newmem and old block may overlap: must clear before set!! */
      _tcptr_clearTags(ptr, oldsiz);
      _tcptr_setTags(newmem, size);
    }
    if(oldsiz && (size > oldsiz)){
      bzero((char *)newmem+oldsiz, size-oldsiz);
    }
  }

  return newmem;
}

void * _typecheck_valloc(size_t size)
{
  void * newmem;

  newmem = valloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
  }

  return newmem;
}

void * _typecheck_valloc_zero(size_t size)
{
  void * newmem;

  newmem = valloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    _tcptr_setTags(newmem, size);
    bzero(newmem, size);
  }

  return newmem;
}

void * valloc_zero(size_t size)
{
  void * newmem;

  newmem = valloc(size);

  if(newmem){
    bzero(newmem, size);
  }

  return newmem;
}


