#include <stdlib.h>

#include "tcinternal.h"
#include "tcmalloc-hash.h"
#include <tclibc.h>

/* Define following to make *alloc functions all zero-init memory */
/* #define TC_SAFE_MALLOC */

/***********************************************/
/*********  trackfree data structures  *********/
/***********************************************/

struct free_rec
{
  void * ptr;
  size_t siz;
  struct free_rec * nxt;
  const char * file;
  int line, col;
  char * status;
};
struct free_rec * freed_list = 0; /* in reverse temporal order (always insert at head) */

static void trackfree_alloc(void * ptr, size_t siz)
{
  struct free_rec * q = (struct free_rec *) malloc(sizeof(struct free_rec));
  if(q){ /* in reverse temporal order */
    q->ptr = ptr;
    q->siz = siz;
    q->file = _globalErrlocFile;
    q->line = _globalErrlocLine;
    q->col  = _globalErrlocCol;
    q->status = "alloc'ed";
    q->nxt = freed_list;
    freed_list = q;
  } else {
    /* out of memory */
    _output_internal_error("trackfree_alloc", "malloc freed list out of memory");
  }
}

static void trackfree_free(void * ptr, size_t siz)
{
  struct free_rec * q = (struct free_rec *) malloc(sizeof(struct free_rec));
  if(q){ /* in reverse temporal order */
    q->ptr = ptr;
    q->siz = siz;
    q->file = _globalErrlocFile;
    q->line = _globalErrlocLine;
    q->col  = _globalErrlocCol;
    q->status = "freed";
    q->nxt = freed_list;
    freed_list = q;
  } else {
    /* out of memory */
    _output_internal_error("trackfree_free", "malloc free list out of memory");
  }
}

void _tcMallocStatus(void * ptr)
{
  struct free_rec * q;
  for(q = freed_list; q; q = q->nxt)
    if(ptr >= q->ptr &&
	((char *)ptr) < (char *)q->ptr + q->siz + TC_MALLOC_PADDING){
      if(((char *)ptr) < (char *)q->ptr + q->siz){
        fprintf(stderr, "Address %p in block {%p,%d}\n\t%s at [%s:%d.%d]\n",
		ptr, q->ptr, q->siz, q->status, q->file, q->line, q->col);
      } else {
        fprintf(stderr, "Address %p in padding of block {%p,%d}\n\t%s at [%s:%d.%d]\n",
		ptr, q->ptr, q->siz, q->status, q->file, q->line, q->col);
      }
    }
}

/**************************************/
/*********  malloc functions  *********/
/**************************************/
void * _typecheck_malloc(size_t size)
{
  void * newmem;

  newmem = malloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
#ifdef TC_SAFE_MALLOC /* { */
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
    _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, size);
#endif /* } ifndef TC_SAFE_MALLOC */
  }

  return newmem;
}

/* identical to _typecheck_malloc, except set initTag rather than setUninitTag */
void * _typecheck_malloc_init(size_t size)
{
  void * newmem;

  newmem = malloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
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
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
  }

  return newmem;
}

void _typecheck_free(void * ptr)
{
  size_t siz = mhash_remove(ptr);
  if(tc_flag_trackfree) trackfree_free(ptr, siz);

  if(siz){
    free(ptr);
    SET_UNALLOC_TAG("_typecheck_free",_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr, (char *)ptr+siz);
  } else {
    _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCF_FUM);
  }
}

/* don't output error if ptr target not allocated */
void _typecheck_free_partial(void * ptr)
{
  size_t siz = mhash_remove(ptr);
  if(tc_flag_trackfree) trackfree_free(ptr, siz);

  if(siz){
    free(ptr);
    SET_UNALLOC_TAG("_typecheck_free",_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr, (char *)ptr+siz);
  }
}

void * _typecheck_memalign(size_t alignment, size_t size)
{
  void * newmem;

  newmem = (void *) memalign(alignment, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
#ifdef TC_SAFE_MALLOC /* { */
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
    _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, size);
#endif /* } ifndef TC_SAFE_MALLOC */
  }

  return newmem;
}

void * _typecheck_memalign_init(size_t alignment, size_t size)
{
  void * newmem;

  newmem = (void *) memalign(alignment, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
  }

  return newmem;
}

void * _typecheck_realloc_body(void * ptr, size_t size, int init)
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
  if(tc_flag_trackfree) trackfree_free(ptr, oldsiz);

  newmem = realloc(ptr, size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
    if(newmem == ptr){ /* memory block extended */
      if(size < oldsiz){ /* shrink */
        SET_UNALLOC_TAG("_typecheck_realloc", _globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			(char *)ptr+size, (char *)ptr+oldsiz);
      } else { /* extend */
#ifdef TC_SAFE_MALLOC /* { */
        _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			(char *)ptr+oldsiz, size-oldsiz, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
        if(init) _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			(char *)ptr+oldsiz, size-oldsiz, _typetag_init, _typetag_unalloc);
        else _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			(char *)ptr+oldsiz, size-oldsiz);
#endif /* } ifndef TC_SAFE_MALLOC */
      }
    } else { /* new memory block created */
      if(((char *)ptr > (char *)newmem && (char *)ptr <= (char *)newmem+size) ||
	 ((char *)newmem > (char *)ptr && (char *)newmem <= (char *)ptr+oldsiz)){
	/* NOTE: newmem and old block may still overlap, in atypical fashion */
	/*       When this happens, instead of copying tags, we'll just set it to init */
        SET_UNALLOC_TAG("_typecheck_realloc", _globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr, (char *)ptr+oldsiz);
#ifdef TC_SAFE_MALLOC /* { */
        _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			newmem, size, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
        if(init) _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
				newmem, size, _typetag_init, _typetag_unalloc);
	else {
          if(size < oldsiz){ /* shrinkage: all "init" */
            _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, size, _typetag_init, _typetag_unalloc);
          } else { /* growth: "init"|"uninit" */
            _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, oldsiz, _typetag_init, _typetag_unalloc);
            _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, (char *)newmem+oldsiz, size-oldsiz);
          }
        }
#endif /* } ifndef TC_SAFE_MALLOC */
      } else {
#ifdef TC_SAFE_MALLOC /* { */
        _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			newmem, size, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
        if(init) _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			newmem, size, _typetag_init, _typetag_unalloc);
        else _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, size);
#endif /* } ifndef TC_SAFE_MALLOC */
        _copyTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, ptr, (size<oldsiz)?size:oldsiz, _ctype_aggregate);
        SET_UNALLOC_TAG("_typecheck_realloc", _globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ptr, (char *)ptr+oldsiz);
      }
    }
  }

  return newmem;
}

void * _typecheck_realloc(void * ptr, size_t size)
{
  return _typecheck_realloc_body(ptr, size, 0);
}

void * _typecheck_realloc_init(void * ptr, size_t size)
{
  return _typecheck_realloc_body(ptr, size, 1);
}

void * _typecheck_valloc(size_t size)
{
  void * newmem;

  newmem = valloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
#ifdef TC_SAFE_MALLOC /* { */
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
#else /* } ifndef TC_SAFE_MALLOC { */
    _setUninitTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, newmem, size);
#endif /* } ifndef TC_SAFE_MALLOC */
  }

  return newmem;
}

void * _typecheck_valloc_init(size_t size)
{
  void * newmem;

  newmem = valloc(size + TC_MALLOC_PADDING);

  if(newmem){
    mhash_insert(newmem, size);
    if(tc_flag_trackfree) trackfree_alloc(newmem, size);
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		newmem, size, _typetag_init, _typetag_unalloc);
  }

  return newmem;
}

/****************************************/
/*********  test main function  *********/
/****************************************/
#ifdef STANDALONE /* { */

int main()
{
  void * v, * w, * x;
  v = _typecheck_malloc(12);
  w = _typecheck_malloc(42);
  x = _typecheck_malloc(1);
  _typecheck_free(w);
  _typecheck_free(v);
  _typecheck_free(x);
  v = _typecheck_malloc(32);
  _typecheck_free(w);
  w = _typecheck_malloc(83);
  _typecheck_free(v);
  x = _typecheck_malloc(97);
  _typecheck_free(x);
  return 0;
}

#endif /* } STANDALONE */
