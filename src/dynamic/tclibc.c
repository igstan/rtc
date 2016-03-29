#include <langinfo.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <sys/uio.h>

#include "tcinternal.h"
#include <tclibc.h>

char * _typecheck_nl_langinfo(nl_item item)
{
  char * ret = nl_langinfo(item);
  _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ret,strlen(ret));
  return ret;
}


/* Note: this is approximate, and is the easy way out;
   proper way is to instrument a dummy global (for each platform/each compilation?) */
static struct stat * initbuf(struct stat * buf)
{
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_dev, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_ino, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_mode, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_nlink, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_uid, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_gid, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_rdev, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_size, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_blksize, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_blocks, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_atime, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_mtime, _ctype_long);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &buf->st_ctime, _ctype_long);

  return buf;
}
int _typecheck_stat(const char *path, struct stat *buf)
{
  return stat(path, initbuf(buf));
}
int _typecheck_lstat(const char *path, struct stat *buf)
{
  return lstat(path, initbuf(buf));
}
int _typecheck_fstat(int fildes, struct stat *buf)
{
  return fstat(fildes, initbuf(buf));
}


static struct tm * init_tm(struct tm * tmp)
{
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_sec, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_min, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_hour, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_mday, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_mon, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_year, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_wday, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_yday, _ctype_int);
  _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &tmp->tm_isdst, _ctype_int);
  return tmp;
}
char *_typecheck_ctime(const time_t *clock)
{
  char * ret = ctime(clock);
  _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ret,strlen(ret));
  return ret;
}
struct tm *_typecheck_localtime(const time_t *clock)
{
  return init_tm(localtime(clock));
}
struct tm *_typecheck_gmtime(const time_t *clock)
{
  return init_tm(gmtime(clock));
}
char *_typecheck_asctime(const struct tm *tm)
{
  char * ret = asctime(tm);
  _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ret,strlen(ret));
  return ret;
}

char * _typecheck_getpass(const char * prompt)
{
  char * ret = getpass(prompt);
  _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, ret,strlen(ret));
  return ret;
}
/*
int    _typecheck_brk(void *endds)
{
}
*/
/*
void * _typecheck_sbrk(int incr)
{
}
*/

ssize_t _typecheck_read(int fildes, void *buf, size_t nbyte)
{
  ssize_t ret;

#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
  if(!verifyAlloc(buf, nbyte)){
    /* target is unallocated; warn */
    _output_simple(TCFR_RIUM);
  }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
  ret = read(fildes, buf, nbyte);

  if(ret){
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
                buf, nbyte, _typetag_init, _typetag_uninit);
  }
  return ret;
}

ssize_t _typecheck_pread(int fildes, void *buf, size_t nbyte, off_t offset)
{
  ssize_t ret;

#ifdef TC_VERIFY_INPUT_BUFFER_ALLOC /* { */
  if(!verifyAlloc(buf, nbyte)){
    /* target is unallocated; warn */
    _output_simple(TCFR_RIUM);
  }
#endif /* } !TC_VERIFY_INPUT_BUFFER_ALLOC */
  ret = pread(fildes, buf, nbyte, offset);

  if(ret){
    _setByteTags(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
                buf, nbyte, _typetag_init, _typetag_uninit);
  }
  return ret;
}

ssize_t _typecheck_readv(int fildes, struct iovec *iov, int iovcnt)
{
/*TODO*/
  return readv(fildes, iov, iovcnt);
}

