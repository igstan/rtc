#ifndef _MIRROR_SIZES_H
#define _MIRROR_SIZES_H
/* This file was automatically generated using mirror_sizes.c.
   It assumes exactly four bit tags. */

#define _MIRROR_short	char
#define _MIRROR_int	short
#define _MIRROR_long	short
#define _MIRROR_longlong	int
#define _MIRROR_float	short
#define _MIRROR_double	int
#define _MIRROR_pointer	short

#define _LOGSIZE_char	0
#define _LOGSIZE_short	1
#define _LOGSIZE_int	2
#define _LOGSIZE_long	2
#define _LOGSIZE_longlong	3
#define _LOGSIZE_float	2
#define _LOGSIZE_double	3
#define _LOGSIZE_pointer	2

static union {
  unsigned char uninit_bytes[1];
  _MIRROR_short uninit_mt;
} _short_uninit_tag_u = {{0x90|_typetag_uninit}};
static union {
  unsigned char init_bytes[1];
  _MIRROR_short init_mt;
} _short_tag_u = {{0x90|_typetag_int}};

static union {
  unsigned char uninit_bytes[2];
  _MIRROR_int uninit_mt;
} _int_uninit_tag_u = {{0xa0|_typetag_uninit, 0x88}};
static union {
  unsigned char init_bytes[2];
  _MIRROR_int init_mt;
} _int_tag_u = {{0xa0|_typetag_int, 0x88}};

static union {
  unsigned char uninit_bytes[2];
  _MIRROR_long uninit_mt;
} _long_uninit_tag_u = {{0xa0|_typetag_uninit, 0x88}};
static union {
  unsigned char init_bytes[2];
  _MIRROR_long init_mt;
} _long_tag_u = {{0xa0|_typetag_int, 0x88}};

static union {
  unsigned char uninit_bytes[4];
  _MIRROR_longlong uninit_mt;
} _longlong_uninit_tag_u = {{0xb0|_typetag_uninit, 0x88, 0x88, 0x88}};
static union {
  unsigned char init_bytes[4];
  _MIRROR_longlong init_mt;
} _longlong_tag_u = {{0xb0|_typetag_int, 0x88, 0x88, 0x88}};

static union {
  unsigned char uninit_bytes[2];
  _MIRROR_float uninit_mt;
} _float_uninit_tag_u = {{0xa0|_typetag_uninit, 0x88}};
static union {
  unsigned char init_bytes[2];
  _MIRROR_float init_mt;
} _float_tag_u = {{0xa0|_typetag_float, 0x88}};

static union {
  unsigned char uninit_bytes[4];
  _MIRROR_double uninit_mt;
} _double_uninit_tag_u = {{0xb0|_typetag_uninit, 0x88, 0x88, 0x88}};
static union {
  unsigned char init_bytes[4];
  _MIRROR_double init_mt;
} _double_tag_u = {{0xb0|_typetag_float, 0x88, 0x88, 0x88}};

static union {
  unsigned char uninit_bytes[2];
  _MIRROR_pointer uninit_mt;
} _pointer_uninit_tag_u = {{0xa0|_typetag_uninit, 0x88}};
static union {
  unsigned char init_bytes[2];
  _MIRROR_pointer init_mt;
} _pointer_tag_u = {{0xa0|_typetag_ptr, 0x88}};

#endif /* _MIRROR_SIZES_H */
