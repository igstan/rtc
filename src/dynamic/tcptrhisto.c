#include <stdio.h>
#include <stdlib.h>

/**************************************************/
/* Data Structures */
/**************************************************/

enum Histo {
  Histo_VP,
  Histo_ST,
  Histo_CT,
  Histo_MALLOC,
  Histo_FREE,
  Histo_MAX
};

static struct flc_node {
  const char * file;
  int line;
  int col;
  unsigned long long count;
  size_t size_min, size_max;
  struct flc_node * next;
} * histo[Histo_MAX] = {0};

/**************************************************/
/* insertion function */
/**************************************************/
static histo_insert(enum Histo entry, const char * file, int line, int col, size_t size)
{
  struct flc_node * np;
  for(np = histo[entry]; np; np = np->next){
    if(np->file == file &&
	np->line == line &&
	np->col == col){
      np->count++;
      if(np->size_min > size) np->size_min = size;
      if(np->size_max < size) np->size_max = size;
      break;
    }
  }
  if(!np){
    np = (struct flc_node *) malloc(sizeof(struct flc_node));
    np->file = file;
    np->line = line;
    np->col = col;
    np->count = 1;
    np->size_min = size;
    np->size_max = size;
    np->next = histo[entry];
    histo[entry] = np;
  }
}

/**************************************************/
/* Output */
/**************************************************/

static void print_histo(FILE * os, const char * descr, struct flc_node * list)
{
  /* collect and print top ten */
  {
    struct flc_node * topten[10] = {0};
    int num_topten = 0;
    int i;
    struct flc_node * np;

    /* collect top ten */
    for(np = list; np; np = np->next){
      if(num_topten < 10){
        topten[num_topten] = np;
        num_topten++;
      } else {
        int mindex = 0;
        for(i = 1; i < 10; ++i){
          if(topten[mindex]->count > topten[i]->count){
            mindex = i;
          }
        }
        if(topten[mindex]->count < np->count){
          topten[mindex] = np;
        }
      }
    }

    /* output top ten */
    for(i = 0; i < num_topten; ++i){
      fprintf(os, " TOPTEN-%s %llu [%d,%d] %s:%d.%d\n",
		descr, topten[i]->count,
		topten[i]->size_min, topten[i]->size_max,
		topten[i]->file, topten[i]->line, topten[i]->col);
    }
  }
  /* print full histogram */
  {
    struct flc_node * np;
    fprintf(os, "HISTO-%s\n", descr);
    for(np = list; np; np = np->next){
      fprintf(os, " %llu [%d,%d] %s:%d.%d\n",
		np->count, np->size_min, np->size_max,
		np->file, np->line, np->col);
    }
  }
}

void tcptr_print_histo(FILE * os)
{
  if(histo[Histo_VP]){
    print_histo(os, "VP", histo[Histo_VP]);
  }
  if(histo[Histo_ST]){
    print_histo(os, "ST", histo[Histo_ST]);
  }
  if(histo[Histo_CT]){
    print_histo(os, "CT", histo[Histo_CT]);
  }
  if(histo[Histo_MALLOC]){
    print_histo(os, "MALLOC", histo[Histo_MALLOC]);
  }
  if(histo[Histo_FREE]){
    print_histo(os, "FREE", histo[Histo_FREE]);
  }
}

/**************************************************/
/* The Functions */
/**************************************************/

void _tcptr_histo_verifyPtr(const char * file, int line, int col, const char * exp,
                                const void * addr, size_t size)
{
  histo_insert(Histo_VP, file, line, col, size);
}

void _tcptr_histo_setTags(const char * file, int line, int col,
                                const void * addr, size_t size)
{
  histo_insert(Histo_ST, file, line, col, size);
}

void _tcptr_histo_clearTags(const char * file, int line, int col,
                                const void * addr, size_t size)
{
  histo_insert(Histo_CT, file, line, col, size);
}

void * _tcptr_histo_malloc(const char * file, int line, size_t size)
{
  histo_insert(Histo_MALLOC, file, line, 0, size);
  return malloc(size);
}

void * _tcptr_histo_calloc(const char * file, int line, size_t nelem, size_t elsize)
{
  histo_insert(Histo_MALLOC, file, line, 0, nelem*elsize);
  return calloc(nelem, elsize);
}

void _tcptr_histo_free(const char * file, int line, void * ptr)
{
  histo_insert(Histo_FREE, file, line, 0, 0);
  free(ptr);
}

void * _tcptr_histo_realloc(const char * file, int line, void * ptr, size_t size)
{
  histo_insert(Histo_MALLOC, file, line, 0, size);
  return realloc(ptr, size);
}

void * _tcptr_histo_memalign(const char * file, int line, size_t align, size_t size)
{
  histo_insert(Histo_MALLOC, file, line, 0, size);
  return memalign(align, size);
}

void * _tcptr_histo_valloc(const char * file, int line, size_t size)
{
  histo_insert(Histo_MALLOC, file, line, 0, size);
  return valloc(size);
}

