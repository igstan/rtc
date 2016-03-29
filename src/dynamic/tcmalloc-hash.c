#include <stdio.h>
#include <stdlib.h>

#include "tcmalloc-hash.h"

/*******************************************/
/*********  malloc info hashtable  *********/
/*******************************************/
/* #define MHASHSIZE 1021 */
#define MHASHSIZE 4093
/* #define MHASHSIZE 16381 */

#define MHASHCHUNK 10
#define MHASHMASK (~((-1)<<MHASHCHUNK))

/* old SY version:
#define mhash(v) ((						\
	(((unsigned long)(v)) & MHASHMASK)			\
	+((((unsigned long)(v)) >> MHASHCHUNK) & MHASHMASK)	\
	+((((unsigned long)(v)) >> (2*MHASHCHUNK)) & MHASHMASK)	\
	) % MHASHSIZE)
*/
int mhash(const void * v) /* Thomas Wang's 32 bit Mix */
{
  unsigned long h = (unsigned long) v;
  h += ~(h << 15);
  h ^=  (h >> 10);
  h +=  (h << 3);
  h ^=  (h >> 6);
  h += ~(h << 11);
  h ^=  (h >> 16);
  return h % MHASHSIZE;
}

struct ptr_siz
{
  void * ptr;
  size_t siz;
  struct ptr_siz * nxt;
};
struct ptr_siz * mhash_table[MHASHSIZE] = {0};

/*******************************************/

#if defined(TC_PRINT_STATS) /* { */
static unsigned long long
		tc_remove_sum = 0,	tc_getsize_sum = 0,	tc_getsize_notfound_sum = 0;
static int	tc_remove_count = 0,	tc_getsize_count = 0,	tc_getsize_notfound_count = 0;
static int	tc_remove_max = 0,	tc_getsize_max = 0,	tc_getsize_notfound_max = 0;
static int	tc_remove_max_len = 0,	tc_getsize_max_len = 0;
#define TC_STAT_DO(x) x
#else /* } !defined(TC_PRINT_STATS) { */
#define TC_STAT_DO(x)
#endif /* } !defined(TC_PRINT_STATS) */

/*******************************************/

void mhash_insert(void * ptr, size_t siz)
{
  int hash = mhash(ptr);
  struct ptr_siz * p = (struct ptr_siz *) malloc(sizeof(struct ptr_siz));
  if(p){
    p->ptr = ptr;
    p->siz = siz;
    p->nxt = mhash_table[hash];
    mhash_table[hash] = p;
  } else {
    fprintf(stderr, "mhash_insert: malloc hashtable out of memory\n");
  }
}

size_t mhash_remove(void * ptr)
{
  struct ptr_siz ** pp;
  TC_STAT_DO(int cnt = 0);
  for(pp = &mhash_table[mhash(ptr)]; *pp; pp = &(*pp)->nxt){
    TC_STAT_DO(cnt++);
    if((*pp)->ptr == ptr){
      struct ptr_siz * tmp = *pp;
      size_t ret = tmp->siz;
      *pp = tmp->nxt;
      free(tmp);
#if defined(TC_PRINT_STATS) /* { */
      tc_remove_sum += cnt;
      tc_remove_count++;
      if(tc_remove_max < cnt)
        tc_remove_max = cnt;
      while(*pp){
        cnt++;
        pp = &(*pp)->nxt;
      }
      if(tc_remove_max_len < cnt)
        tc_remove_max_len = cnt;
#endif /* } defined(TC_PRINT_STATS) */
      return ret;
    }
  }
  /* entry not found! */
  return 0;
}

size_t mhash_getsize(const void * ptr)
{
  struct ptr_siz * pp;
  TC_STAT_DO(int cnt = 0);
  for(pp = mhash_table[mhash(ptr)]; pp; pp = pp->nxt){
    TC_STAT_DO(cnt++);
    if(pp->ptr == ptr){
#if defined(TC_PRINT_STATS) /* { */
      struct ptr_siz * tmp = pp;
      tc_getsize_sum += cnt;
      tc_getsize_count++;
      if(tc_getsize_max < cnt)
        tc_getsize_max = cnt;
      while(tmp){
        cnt++;
        tmp = tmp->nxt;
      }
      if(tc_getsize_max_len < cnt)
        tc_getsize_max_len = cnt;
#endif /* } defined(TC_PRINT_STATS) */
      return pp->siz;
    }
  }
#if defined(TC_PRINT_STATS) /* { */
  tc_getsize_notfound_sum += cnt;
  tc_getsize_notfound_count++;
  if(tc_getsize_notfound_max < cnt)
    tc_getsize_notfound_max = cnt;
#endif /* } defined(TC_PRINT_STATS) */
  return 0;
}

void mhash_debugout(FILE * os)
{
#ifdef TC_PRINT_STATS /* { */
  fprintf(os, "mhash-remove sum:%llu/count:%d = avg:%.2f\n",
		tc_remove_sum, tc_remove_count, (float)tc_remove_sum/(float)tc_remove_count);
  fprintf(os, "mhash-remove max=%d, maxlen=%d\n",
		tc_remove_max, tc_remove_max_len);
  fprintf(os, "mhash-getsize sum:%llu/count:%d = avg:%.2f\n",
		tc_getsize_sum, tc_getsize_count, (float)tc_getsize_sum/(float)tc_getsize_count);
  fprintf(os, "mhash-getsize max=%d, maxlen=%d\n",
		tc_getsize_max, tc_getsize_max_len);
  fprintf(os, "mhash-gnotfound sum:%llu/count:%d = avg:%.2f\n",
		tc_getsize_notfound_sum, tc_getsize_notfound_count, (float)tc_getsize_notfound_sum/(float)tc_getsize_notfound_count);
  fprintf(os, "mhash-gnotfound max=%d\n", tc_getsize_notfound_max);
#endif /* } !defined TC_PRINT_STATS */
  {
    struct histo_node {
      int cnt;
      int count;
      struct histo_node * next;
    } * histo = 0;
    int i;
    for(i = 0; i < MHASHSIZE; ++i){
      struct ptr_siz * pp;
      int cnt = 0;
      for(pp = mhash_table[i]; pp; pp = pp->nxt){
        cnt++;
      }
      { /* add <cnt> to histo */
        struct histo_node ** npp;
        for(npp = &histo; (*npp) && ((*npp)->cnt < cnt); npp = &(*npp)->next)
          ;
        if((*npp) && (*npp)->cnt == cnt){
          (*npp)->count++;
        } else {
          struct histo_node * tmp = (struct histo_node *) malloc(sizeof(struct histo_node));
          if(tmp){
            tmp->cnt = cnt;
            tmp->count = 1;
            tmp->next = *npp;
            *npp = tmp;
          } else fprintf(stderr, "ERROR(mhash_debugout): malloc out of memory\n");
        }
      }
    }
    fprintf(os, "MALLOC-HASH-SUMMARY:\n");
    {
      struct histo_node * np;
      for(np = histo; np; np = np->next){
        fprintf(os, "mhash-histo: %d\t%d\n", np->cnt, np->count);
      }
    }
  }
}
