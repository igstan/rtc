#ifndef _TYPECHECK_API_POSTPROC_H_
#define _TYPECHECK_API_POSTPROC_H_

#define _TC_BYTE 8

typedef enum {_typetag_unalloc = 0, _typetag_uninit, _typetag_int,
	      _typetag_float, _typetag_ptr, _typetag_init, _typetag_bits} _typetag_t;

typedef long long longlong;
#ifdef LONGDOUBLE
typedef long double longdouble;
#endif /* LONGDOUBLE */
typedef char* pointer;

#include "mirror_sizes.h"

/*******************************************
  Modify/experiment with following #define
 *******************************************/
#define MIRRORPAGE_NUMBITS 20

/*******************************************/
#define MIRRORPAGE_NUMBYTES (1 << MIRRORPAGE_NUMBITS)
#define MIRRORPAGE_MASK (~((-1) << MIRRORPAGE_NUMBITS))
#define MIRRORMAP_NUMBITS ((sizeof(void *) << 3) - MIRRORPAGE_NUMBITS)
#define MIRRORMAP_NUMELEMENTS (1 << MIRRORMAP_NUMBITS)

/* Handy macros */
/* Note possible point of confusion: MIRRORPAGE_INDEX is the mirrorpage
   portion of the address *in program space*, not in mirror space.
   To get the latter, divide by two */
#define MIRRORMAP_INDEX(addr) ((unsigned long)(addr) >> MIRRORPAGE_NUMBITS)
#define MIRRORPAGE_INDEX(addr) ((unsigned long)(addr) & MIRRORPAGE_MASK)

#define SAME_MIRROR_PAGE(addr1, addr2) \
	(MIRRORMAP_INDEX(addr1) ==  MIRRORMAP_INDEX(addr2))
				      
extern void * mirrormap[];

/* Note, BITS_PER_TAG must be no larger than _TC_BYTE.  */

#define BITS_PER_TAG 4
#define LOG_BITS_PER_TAG 2

/*
   These bit patterns have to be in exact correspondence
   with the _type_t enum values above.

   The first (highest-order) of the four bits of a tag
   is used to encode size.  If the bit is 0, the byte
   represented by this tag is the first in a datum.
   Otherwise, it's a continuation of a datum whose first
   byte appears earlier.
*/
#define CONT_BIT_MASK (1 << (BITS_PER_TAG - 1))

/*
   The last three of the four bits of a tag are used
   to represent types as follows:

   000 - Unalloc, 001 - Uninit, 010 - Int,
   011 - Float, 100 - Ptr, 101 - Bits, others?
*/
#define TYPE_BITS_MASK (0xffu >> (_TC_BYTE - BITS_PER_TAG + 1))

#define TAG_BITS_MASK  (0xffu >> (_TC_BYTE - BITS_PER_TAG))

/**********************************/
/* mirror pos manipulating macros */
/**********************************/

/* Extract the tag indicated by pos. */
#define TAG_BITS(pos) \
	((*(pos).ptr >> (pos).bit) & TAG_BITS_MASK)

/* Extract the continuation bit (see above) from the tag indicated by pos. */
#define CONT_BIT(pos) \
	((*(pos).ptr >> (pos).bit) & CONT_BIT_MASK)

/* Extract the type bits (see above) from the tag indicated by pos. */
/* Could replace the RHS of & with 7 if BITS_PER_TAG is fixed at 4. */
#define TYPE_BITS(pos) \
	((*(pos).ptr >> (pos).bit) & TYPE_BITS_MASK)

/* Macro to set the tag indicated by pos. */
#define WRITE_TAG(pos,tag) ( \
		*(pos).ptr = (*(pos).ptr & \
			      ~((0xffu >> (_TC_BYTE - BITS_PER_TAG)) << (pos).bit)) | \
			     (tag << (pos).bit) \
			   );

/* Increment tag position pos:
   Increment the bit position first.  If the bit number did not
   roll over, do nothing.  If it did, increment the pointer.
   If the pointer stayed within the same mirror page, do nothing.
   Otherwise, find the position in the new mirror page.
   The last one is to make the expression non-void.  Alternatively,
   could make overflowTagPos return something. */
#define NEXT_POS(pos) \
		(((pos).bit = ((pos).bit + BITS_PER_TAG) % _TC_BYTE) || \
		  (((unsigned long)(++(pos).ptr)%(MIRRORPAGE_NUMBYTES/2)) || \
		    (--(pos).ptr, overflowTagPos(&(pos), 2), 1)))

/* Decrement tag position pos. */
#define PREV_POS(pos)  {\
		(pos).bit = ((pos).bit + BITS_PER_TAG) % _TC_BYTE; \
		if((pos).bit) \
		  if(!((unsigned long)((pos).ptr--)%(MIRRORPAGE_NUMBYTES/2))){ \
		    ++(pos).ptr; \
		    overflowTagPos(&(pos), -2); \
		  } \
	 }

#define EVEN_TAG_MASK  TAG_BITS_MASK
#define EVEN_TYPE_MASK TYPE_BITS_MASK
#define EVEN_CONT_MASK CONT_BIT_MASK
#define ODD_TAG_MASK  (TAG_BITS_MASK << BITS_PER_TAG)
#define ODD_TYPE_MASK (TYPE_BITS_MASK << BITS_PER_TAG)
#define ODD_CONT_MASK (CONT_BIT_MASK << BITS_PER_TAG)

#define ODD_TAG_BITS(byte)	(((byte) >> BITS_PER_TAG) & TAG_BITS_MASK)
#define ODD_TYPE_BITS(byte)	(((byte) >> BITS_PER_TAG) & TYPE_BITS_MASK)
#define EVEN_TAG_BITS(byte)	((byte) & TAG_BITS_MASK)
#define EVEN_TYPE_BITS(byte)	((byte) & TYPE_BITS_MASK)

/* The following macro must be an lvalue */
#define GET_TAG_BYTE(addr)	*(\
	 (mirrormap[MIRRORMAP_INDEX(addr)]) \
	    || (_touchMirrorPage(MIRRORMAP_INDEX(addr)),1), \
	    ((unsigned char *) mirrormap[MIRRORMAP_INDEX(addr)] \
		+ MIRRORPAGE_INDEX(addr)/2) \
	 )

/*
#define GET_CHAR_TAG(byte, isOdd) ((byte) >> BITS_PER_TAG * (isOdd) & TAG_BITS_MASK)
*/
#define GET_CHAR_TAG(byte, isOdd) \
		((byte) >> ((isOdd) << LOG_BITS_PER_TAG) & TAG_BITS_MASK)

/* rhs may need to be a tmp, so that it is not the same as lhs. */
/*
#define SET_CHAR_TAG(lhs, rhs, isOdd) ( \
	  (lhs) = (rhs) << BITS_PER_TAG * (isOdd) | \
		  (lhs) & ~(TAG_BITS_MASK << BITS_PER_TAG * (isOdd)) \
	)
*/
#define SET_CHAR_TAG(lhs, rhs, isOdd) ( \
	  (lhs) = ((rhs) << ((isOdd) << LOG_BITS_PER_TAG)) | \
		  ((lhs) & ~(TAG_BITS_MASK << ((isOdd) << LOG_BITS_PER_TAG))) \
	)

#endif /* _TYPECHECK_API_POSTPROC_H_ */

