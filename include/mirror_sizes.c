/* automatically generates mirror_sizes.h 
   see ckit/src/c-util/sizes.c for more stuff */

#include <stdio.h>

typedef long long longlong;
typedef long double longdouble;
typedef char* pointer;

/* Should we do this with a typedef instead?
   What do we do with the last case?
   How about temps? */
#define MIRROR_TYPE(ty) (					\
	   (sizeof(char) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tchar\n")		\
	|| (sizeof(short) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tshort\n")	\
	|| (sizeof(int) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tint\n")		\
	|| (sizeof(long) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tlong\n")		\
	|| (sizeof(longlong) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tlong long\n")	\
	|| (sizeof(float) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tfloat\n")	\
	|| (sizeof(double) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tdouble\n")	\
	|| (sizeof(longdouble) * 2 == sizeof(ty))		\
	   && printf("#define _MIRROR_" #ty "\tlong double\n")	\
	|| (sizeof(pointer) * 2 == sizeof(ty))			\
	   && printf("#define _MIRROR_" #ty "\tchar *\n")	\
	|| (ty##_mt_found = 0))


/* First computes log of size of the argument type (also saves
   it in the <type>_ls variable).  Taken from function
   initStaticRep() of pmirror_newtags.c. */
#define LOGSIZE(ty)						\
	{							\
	  int i, ls;						\
	  unsigned long unum = (unsigned long) sizeof(ty);	\
	  for(ls = -1; unum; ++ls)				\
	    unum >>= 1;						\
	  if(ls == -1 || (1 << ls != sizeof(ty)))		\
	    fprintf(stderr,					\
		    "Error: incompatible scalar size "		\
		    "(sizeof(" #ty ") = %d)\n", sizeof(ty));	\
	  else							\
	    printf("#define _LOGSIZE_" #ty "\t%d\n",		\
		   ty##_ls = ls);				\
	 }

/* Creates unions containing the byte array of the uninit
   and init tag for each type's size.  These unions provide
   skeletons of the tags for all types, so we can write tags
   in one step by using the _MIRROR_<type> fields.
  union {
    unsigned char uninit_bytes = {0x08|_typetag_uninit|'<type>_ls', 0x88, 0x88,...};
    MIRROR_TYPE uninit_mt;
  } _<type>_uninit_tag_u;

  union {
    unsigned char init_bytes = {0x08|_typetag_<type>|'<type>_ls', 0x88, 0x88,...};
    MIRROR_TYPE init_mt;
  } _<type>_tag_u;
*/				    
#define TAG_UNIONS(ty, tagname)						\
	{								\
	  int i;							\
	  if (ty##_mt_found) {						\
	    printf("static union {\n  unsigned char uninit_bytes[%d];\n", \
		   sizeof(ty)/2);					\
	    printf("  _MIRROR_" #ty " uninit_mt;\n} _" #ty		\
		   "_uninit_tag_u = {{0x%x0|_typetag_uninit",		\
		    8 + ty##_ls);					\
	    for (i = 2; i < sizeof(ty); i += 2)				\
	      printf(", 0x88");						\
	    printf("}};\n");						\
	    printf("static union {\n  unsigned char init_bytes[%d];\n",	\
		   sizeof(ty)/2);					\
	    printf("  _MIRROR_" #ty " init_mt;\n} _" #ty		\
		   "_tag_u = {{0x%x0|" #tagname, 8 + ty##_ls);		\
	    for (i = 2; i < sizeof(ty); i += 2)				\
	      printf(", 0x88");						\
	    printf("}};\n\n");						\
	  }								\
	}

main ()
{
  /* Temporaries to carry logsize information between LOGSIZE
     and TAG_UNIONS. */
  int char_ls, short_ls, int_ls, long_ls, longlong_ls,
      float_ls, double_ls, longdouble_ls, pointer_ls;

  /* Temporaries to tell TAG_UNIONS whether MIRROR_TYPE was successful. */
  int char_mt_found = 1, short_mt_found = 1, int_mt_found = 1,
      long_mt_found = 1, longlong_mt_found = 1, float_mt_found = 1,
      double_mt_found = 1, longdouble_mt_found = 1, pointer_mt_found = 1;

  printf("#ifndef _MIRROR_SIZES_H\n");
  printf("#define _MIRROR_SIZES_H\n");
  printf("/* This file was automatically generated using mirror_sizes.c.\n");
  printf("   It assumes exactly four bit tags. */\n\n");

  /* For each type figure out what type is one half its size. */
  MIRROR_TYPE(char);
  MIRROR_TYPE(short);
  MIRROR_TYPE(int);
  MIRROR_TYPE(long);
  MIRROR_TYPE(longlong);
  MIRROR_TYPE(float);
  MIRROR_TYPE(double);
#ifdef LONGDOUBLE
  MIRROR_TYPE(longdouble);
#endif /* LONGDOUBLE */
  MIRROR_TYPE(pointer);
  printf("\n");

/* Compute logs of sizes of types (also saves them
   in the <type>_ls variables for use by TAG_UNIONS(). */
  LOGSIZE(char);
  LOGSIZE(short);
  LOGSIZE(int);
  LOGSIZE(long);
  LOGSIZE(longlong);
  LOGSIZE(float);
  LOGSIZE(double);
#ifdef LONGDOUBLE
  LOGSIZE(longdouble);
#endif /* LONGDOUBLE */
  LOGSIZE(pointer);
  printf("\n");

  /* Now create unions containing the byte array of the uninit
     and init tags for each type's size. */
  TAG_UNIONS(char, _typetag_int);
  TAG_UNIONS(short, _typetag_int);
  TAG_UNIONS(int, _typetag_int);
  TAG_UNIONS(long, _typetag_int);
  TAG_UNIONS(longlong, _typetag_int);
  TAG_UNIONS(float, _typetag_float);
  TAG_UNIONS(double, _typetag_float);
#ifdef LONGDOUBLE
  TAG_UNIONS(longdouble, _typetag_float);
#endif /* LONGDOUBLE */
  TAG_UNIONS(pointer, _typetag_ptr);

  printf("#endif /* _MIRROR_SIZES_H */\n");
}
