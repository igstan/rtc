#ifndef TC_MALLOC_HASH_H /* { */
#define TC_MALLOC_HASH_H

#include <stdio.h> /* for FILE */

void mhash_insert(void * ptr, size_t siz);
size_t mhash_remove(void * ptr);
size_t mhash_getsize(const void * ptr);
void mhash_debugout(FILE * os);

#endif /* } ifndef TC_MALLOC_HASH_H */
