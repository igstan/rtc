/* Note: code for interpreting the scanf format string is modified from
 *       the GNU C Library (version 2.1.2) source code, which was
 *       copyright (C) 1991,92,93,94,95,96,97,98,99 Free Software Foundation, Inc.
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "tcinternal.h"
#include <tclibc.h>

#define TCSC_LONG    0x001   /* l: long or double */
#define TCSC_LONGDBL 0x002   /* L: long long or long double */
#define TCSC_SHORT   0x004   /* h: short */
#define TCSC_MALLOC  0x100   /* a: malloc strings */
#define TCSC_CHAR    0x200   /* hh: char */

struct tcsc_arg_info {
  int posn; /* zero to mark end of array */
  int size;
  int sequence;
  void * address;
  _ctype_t ctype;
  int charstring:3,
      malloc:1,
      ignore:1;
};
/* charstring: */
#define TCSC_CHARS   1
#define TCSC_WCHARS  2
#define TCSC_STRING  3
#define TCSC_WSTRING 4

#define TC_SCANF_BUFFER_SIZE 20

static struct tcsc_arg_info * process_format_string(const char * format)
{
  const char * c;
  int sc_plain = 0;
  int sc_positional = 0;
  int curargindex, curposn;
  static struct tcsc_arg_info * argarray = 0;
  static int aasize = 0;

  /* allocate argarray */
  {
    int num_pcts = 0;
    for(c = format; *c; ++c) if(*c == '%') num_pcts++;
    if(num_pcts > aasize){
      if(!aasize) aasize = TC_SCANF_BUFFER_SIZE;
      while(num_pcts > aasize) aasize *= 2;
      if(!(argarray = (struct tcsc_arg_info *) realloc(argarray, aasize * sizeof(struct tcsc_arg_info)))){
        _output_internal_error("_typecheck_scanf:process_format_string", "realloc out of memory");
        return 0;
      }
    }
  }

  curargindex = 0;
  curposn = 0;
  c = format;
  while(*c){
    if(*c++ == '%'){
      int field_width = 0;
      int is_positional = 0;
      int mod_flags = 0;

      curposn++;
      /* Check for a positional parameter specification.  */
      if(isdigit(*c)){
        const char * d = c;
        int posn = 0;
        while(isdigit(*d)) posn = posn * 10 + (*d++ - '0');
        if(*d == '$'){
          sc_positional = is_positional = 1;
          curposn = posn;
          c = d+1;
        }
      }

      /* Check for the assignment-suppressing and the number grouping flag.  */
      if(*c == '*'){
        if(!is_positional) curposn--;
        continue;
      }
      if(*c == '\''){
        c++;
        if(*c == '*'){
          if(!is_positional) curposn--;
          continue;
        }
      }

      if(!is_positional) sc_plain = 1; /* non-positional parameter spec */

      /* field width. */
      while (isdigit (*c))
        field_width = field_width * 10 + (*c++ - '0');

      /* Check for type modifiers.  */
      switch (*c++) {
	case 'h': /* ints are short ints or chars.  */
	  if (*c == 'h') {
	      ++c;
	      mod_flags |= TCSC_CHAR;
	  } else
	    mod_flags |= TCSC_SHORT;
	  break;
	case 'l':
	  if (*c == 'l') { /* A double `l' is equivalent to an `L'.  */
	      ++c;
	      mod_flags |= TCSC_LONGDBL;
	  } else /* ints are long ints.  */
	    mod_flags |= TCSC_LONG;
	  break;
	case 'q':
	case 'L':
	  /* doubles are long doubles, and ints are long long ints.  */
	  mod_flags |= TCSC_LONGDBL;
	  break;
	case 'a':
	  /* The `a' is used as a flag only if followed by `s', `S' or
	     `['.  */
	  if (*c != 's' && *c != 'S' && *c != '[') {
	      --c;
	      break;
	  }
	  /* String conversions (%s, %[) take a `char **'
	     arg and fill it in with a malloc'd pointer.  */
	  mod_flags |= TCSC_MALLOC;
	  break;
	case 'z':
	  if (sizeof (size_t) > sizeof (unsigned long int))
	    mod_flags |= TCSC_LONGDBL;
	  else if (sizeof (size_t) > sizeof (unsigned int))
	    mod_flags |= TCSC_LONG;
	  break;
	case 'j':
	  if (sizeof (uintmax_t) > sizeof (unsigned long int))
	    mod_flags |= TCSC_LONGDBL;
	  else if (sizeof (uintmax_t) > sizeof (unsigned int))
	    mod_flags |= TCSC_LONG;
	  break;
	case 't':
/*
	  if (sizeof (ptrdiff_t) > sizeof (long int))
	    mod_flags |= TCSC_LONGDBL;
	  else if (sizeof (ptrdiff_t) > sizeof (int))
*/
	    mod_flags |= TCSC_LONG;
	  break;
	default:
	  /* Not a recognized modifier.  Backup.  */
	  --c;
	  break;
      }

      /* End of the format string?  */
      if(!*c){
	/* ERROR: malformed format string */
        _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCSC_MFS);

        return argarray;
      }

      /* initialize argarray entry */
      argarray[curargindex].posn = curposn;
      argarray[curargindex].size = 1;
      argarray[curargindex].sequence = 0;
      argarray[curargindex].address = 0;
      argarray[curargindex].ctype = 0;
      argarray[curargindex].charstring = 0;
      argarray[curargindex].malloc = 0;
      argarray[curargindex].ignore = 0;

      /* Find the conversion specifier.  */
      switch (*c++){
	case '%':
	  curargindex--;
	  break;

	case 'n':	/* Answer number of assignments done.  */
	  /* Corrigendum 1 to ISO C 1990 describes the allowed flags
	     with the 'n' conversion specifier.  */
          argarray[curargindex].ignore = 1;
	  if (mod_flags & TCSC_LONGDBL)
            argarray[curargindex].ctype = _ctype_longlong;
	  else if (mod_flags & TCSC_LONG)
            argarray[curargindex].ctype = _ctype_long;
	  else if (mod_flags & TCSC_SHORT)
            argarray[curargindex].ctype = _ctype_short;
	  else
            argarray[curargindex].ctype = _ctype_int;

#ifdef NO_BUG_IN_ISO_C_CORRIGENDUM_1
	      /* We have a severe problem here.  The ISO C standard
		 contradicts itself in explaining the effect of the %n
		 format in `scanf'.  While in ISO C:1990 and the ISO C
		 Amendement 1:1995 the result is described as

		   Execution of a %n directive does not effect the
		   assignment count returned at the completion of
		   execution of the f(w)scanf function.

		 in ISO C Corrigendum 1:1994 the following was added:

		   Subclause 7.9.6.2
		   Add the following fourth example:
		     In:
		       #include <stdio.h>
		       int d1, d2, n1, n2, i;
		       i = sscanf("123", "%d%n%n%d", &d1, &n1, &n2, &d2);
		     the value 123 is assigned to d1 and the value3 to n1.
		     Because %n can never get an input failure the value
		     of 3 is also assigned to n2.  The value of d2 is not
		     affected.  The value 3 is assigned to i.

		 We go for now with the historically correct code from ISO C,
		 i.e., we don't count the %n assignments.  When it ever
		 should proof to be wrong just remove the #ifdef above.  */
          argarray[curargindex].ignore = 0;
#endif
	  break;

	case '[':	/* Character class.  */
	case 'c':	/* Match characters.  */
	  if (!(mod_flags & TCSC_LONG)) {
            argarray[curargindex].ctype = _ctype_char;
            argarray[curargindex].charstring = TCSC_CHARS;
            if(!field_width) field_width = 1;
            argarray[curargindex].size = field_width;
            break;
	  }
	  /* FALLTHROUGH */
	case 'C':
          argarray[curargindex].ctype = /*_ctype_wchar;*/ _ctype_short;
          argarray[curargindex].charstring = TCSC_WCHARS;
          if(!field_width) field_width = 1;
          argarray[curargindex].size = field_width;
	  break;

	case 's':		/* Read a string.  */
	  if(!(mod_flags & TCSC_LONG)){
            argarray[curargindex].ctype = _ctype_char;
            argarray[curargindex].charstring = TCSC_STRING;
            argarray[curargindex].size = field_width;
            argarray[curargindex].malloc = (mod_flags & TCSC_MALLOC);
	    break;
          }
	  /* FALLTHROUGH */
	case 'S':
          argarray[curargindex].ctype = /*_ctype_wchar;*/ _ctype_short;
          argarray[curargindex].charstring = TCSC_WSTRING;
          argarray[curargindex].size = field_width;
          argarray[curargindex].malloc = (mod_flags & TCSC_MALLOC);
	  break;

	case 'x':	/* Hexadecimal integer.  */
	case 'X':	/* Ditto.  */
	case 'o':	/* Octal integer.  */
	case 'u':	/* Unsigned decimal integer.  */
	case 'd':	/* Signed decimal integer.  */
	case 'i':	/* Generic number.  */
	  if (mod_flags & TCSC_LONGDBL)
            argarray[curargindex].ctype = _ctype_longlong;
	  else if (mod_flags & TCSC_LONG)
            argarray[curargindex].ctype = _ctype_long;
	  else if (mod_flags & TCSC_SHORT)
            argarray[curargindex].ctype = _ctype_short;
	  else if (mod_flags & TCSC_CHAR)
            argarray[curargindex].ctype = _ctype_char;
	  else
            argarray[curargindex].ctype = _ctype_int;
	  break;

	case 'e':	/* Floating-point numbers.  */
	case 'E':
	case 'f':
	case 'g':
	case 'G':
	case 'a':
	case 'A':
	  if (mod_flags & TCSC_LONGDBL)
#ifdef LONGDOUBLE /* { */
	    argarray[curargindex].ctype = _ctype_longdouble;
#else
	    argarray[curargindex].ctype = _ctype_double;
#endif /* } LONGDOUBLE */
	  else if (mod_flags & TCSC_LONG)
	    argarray[curargindex].ctype = _ctype_double;
	  else
	    argarray[curargindex].ctype = _ctype_float;
	  break;

	case 'p':	/* Generic pointer.  */
          argarray[curargindex].ctype = _ctype_pointer;
          break;

	default:
	  /* If this is an unknown format character punt.  */
	  /* ERROR: malformed format string */
          _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCSC_MFS);

          return argarray;
      } /* end switch (*c++) */
      curargindex++;
      argarray[curargindex].posn = 0;
    } /* end if(*c++ == '%') */
  }
  if(sc_positional && sc_plain){
    /* WARNING: both positional and non-positional params given */
    _output_flc(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, TCSC_POSN);
  }
  return argarray;
}

int scanf_preprocess(va_list ap, struct tcsc_arg_info * argarray)
{
  int ai = 0, ri = 0;
  int last = 0;
  int out_of_order = 0;

  int num_args = 0;

  /* sequence arguments, skipping ignores */
  for(ai = 0; argarray[ai].posn; ++ai){
    if(argarray[ai].posn < last) out_of_order = 1;
    last = argarray[ai].posn;
    if(!argarray[ai].ignore) ++ri;
    argarray[ai].sequence = ri;
  }
  num_args = ai;

  if(out_of_order){ /* sort args - bubble should be adequate */
    int i, j;
    struct tcsc_arg_info tmp;
    for(i = 0; i < num_args; ++i){
      for(j = i+1; j < num_args; ++j){
        if(argarray[i].posn > argarray[j].posn){
          tmp = argarray[i];
          argarray[i] = argarray[j];
          argarray[j] = tmp;
        }
      }
    }
  }

  { /* collect arg addresses */
    int i, posn = 1;
    for(i = 0; i < num_args; ++i){
      while(posn++ < argarray[i].posn){
        int ignore = va_arg(ap, int);
      }
      argarray[i].address = va_arg(ap, void *);

      if((!verifyAlloc(argarray[i].address, 1))){
        _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCSC_UM, "pre-process",0,0,0, posn,i,0,0);
      }
    }
  }

  return num_args;
}

void scanf_postprocess(int num_args, int num_read, struct tcsc_arg_info * argarray)
{
  int i;
  for(i = 0; i < num_args; ++i){
    if(argarray[i].sequence > num_read) continue;

    switch(argarray[i].charstring){
      case TCSC_CHARS:
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
        if(!verifyAlloc(argarray[i].address, argarray[i].size)){
          /* target is unallocated; warn */
          _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCSC_UM, "chars",0,0,0, argarray[i].posn,i,0,0);
        }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			argarray[i].address, argarray[i].size-1);
        break;
      case TCSC_WCHARS:
        /* should be array of wchar_t */
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
        if(!verifyAlloc(argarray[i].address, argarray[i].size)){
          /* target is unallocated; warn */
          _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCSC_UM, "wchars",0,0,0, argarray[i].posn,i,0,0);
        }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			argarray[i].address, argarray[i].size-1);
        break;
      case TCSC_STRING:
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
        if(!verifyAlloc(argarray[i].address, strlen(argarray[i].address)+1)){
          /* target is unallocated; warn */
          _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCSC_UM, "string",0,0,0, argarray[i].posn,i,0,0);
        }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			argarray[i].address, strlen(argarray[i].address));
        break;
      case TCSC_WSTRING:
        /* should be array of wchar_t */
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
        if(!verifyAlloc(argarray[i].address, strlen(argarray[i].address)+1)){
          /* target is unallocated; warn */
          _output_si(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		TCSC_UM, "wstring",0,0,0, argarray[i].posn,i,0,0);
        }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
        _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			argarray[i].address, strlen(argarray[i].address));
        break;
      default:
        _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
			argarray[i].address, argarray[i].ctype);
        break;
    }
  }
}

int _typecheck_scanf(const char * format, ...)
{
  va_list ap;
  int ret;
  int num_args = 0;

  struct tcsc_arg_info * argarray;

  argarray = process_format_string(format);

  if(argarray){
    va_start(ap, format);
    num_args = scanf_preprocess(ap, argarray);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vscanf(format, ap);
  va_end(ap);

  if(argarray)
    scanf_postprocess(num_args, ret, argarray);

  return ret;
}

int _typecheck_fscanf(FILE * stream, const char * format, ...)
{
  va_list ap;
  int ret;
  int num_args = 0;

  struct tcsc_arg_info * argarray;
  argarray = process_format_string(format);

  if(argarray){
    va_start(ap, format);
    num_args = scanf_preprocess(ap, argarray);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vfscanf(stream, format, ap);
  va_end(ap);

  if(argarray)
    scanf_postprocess(num_args, ret, argarray);

  return ret;
}

int _typecheck_sscanf(const char * str, const char * format, ...)
{
  va_list ap;
  int ret;
  int num_args = 0;

  struct tcsc_arg_info * argarray;
  argarray = process_format_string(format);

  if(argarray){
    va_start(ap, format);
    num_args = scanf_preprocess(ap, argarray);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vsscanf(str, format, ap);
  va_end(ap);

  if(argarray)
    scanf_postprocess(num_args, ret, argarray);

  return ret;
}

int _typecheck_sprintf(char * str, const char * format, ...)
{
  va_list ap;
  int ret;

  /* actual call */
  va_start(ap, format);
  ret = vsprintf(str, format, ap);
  va_end(ap);

  if(ret > 0){
    int slen = strlen(str);
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
    if(!verifyAlloc(str, slen+1)){
      /* target is unallocated; warn */
      _output_simple(TCSP_RIUM);
    }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, str,slen);
  }

  return ret;
}

/* SY: did not verify correctness: in particular, assumes str always
 *     null terminated (may not be a requirement if truncated?)
 */
int _typecheck_snprintf(char * str, size_t size, const char * format, ...)
{
  va_list ap;
  int ret;

  /* actual call */
  va_start(ap, format);
  ret = vsnprintf(str, size, format, ap);
  va_end(ap);

  if(ret > 0){
    int slen = strlen(str);
#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
    if(!verifyAlloc(str, slen+1)){
      /* target is unallocated; warn */
      _output_simple(TCSNP_RIUM);
    }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, str,slen);
  }

  return ret;
}

