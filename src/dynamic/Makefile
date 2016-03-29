TC_DIR = /p/wpis/people/students/suan/TypecheckDebugger
 # TC_DIR = /home/research/TypecheckDebugger
SRCDIR = $(TC_DIR)/src/dynamic
OBJDIR = ../obj
INCDIR = $(TC_DIR)/include
LIBDIR = $(TC_DIR)/lib

CC = gcc
CFLAGS = -I$(INCDIR)
AR = ar
# r = add to archive; c = suppress error message if archive doesn't exist
AR_FLAGS = rc
RANLIB = ranlib

SRCS =	pmirror_newtags.c \
	tclibc.c \
	tcmalloc-hash.c \
	tcmalloc.c \
	tcmalloc-safe.c \
	tcmir.c \
	tcnoop.c \
	tcnooplibc.c \
	tcptr.c \
	tcptrhisto.c \
	tcptrlibc.c \
	tcptrmalloc.c \
	tcptrscanf.c \
	tcptrprintf.c \
	tcscanf.c \
	tcsocket.c \
	tcstdio.c \
	tcstring.c

TCLIBC_OBJS = $(OBJDIR)/tcmalloc.o $(OBJDIR)/tcmalloc-hash.o \
		$(OBJDIR)/tcstdio.o $(OBJDIR)/tcscanf.o \
		$(OBJDIR)/tcstring.o $(OBJDIR)/tclibc.o $(OBJDIR)/tcsocket.o
TCLIBC_G_OBJS = $(OBJDIR)/tcmalloc-g.o $(OBJDIR)/tcmalloc-hash-g.o \
		$(OBJDIR)/tcstdio-g.o $(OBJDIR)/tcscanf-g.o \
		$(OBJDIR)/tcstring-g.o $(OBJDIR)/tclibc-g.o $(OBJDIR)/tcsocket-g.o
TCLIBC_PG_OBJS = $(OBJDIR)/tcmalloc-pg.o $(OBJDIR)/tcmalloc-hash-pg.o \
		$(OBJDIR)/tcstdio-pg.o $(OBJDIR)/tcscanf-pg.o \
		$(OBJDIR)/tcstring-pg.o $(OBJDIR)/tclibc-pg.o $(OBJDIR)/tcsocket-pg.o
TCLIBC_CC_OBJS = $(OBJDIR)/tcmalloc-cc.o $(OBJDIR)/tcmalloc-hash-cc.o \
		$(OBJDIR)/tcstdio-cc.o $(OBJDIR)/tcscanf-cc.o \
		$(OBJDIR)/tcstring-cc.o $(OBJDIR)/tclibc-cc.o $(OBJDIR)/tcsocket-cc.o

TCPTR_LIBC_OBJS = $(OBJDIR)/tcptrmalloc.o $(OBJDIR)/tcmalloc-hash.o \
		$(OBJDIR)/tcptrlibc.o $(OBJDIR)/tcptrscanf.o $(OBJDIR)/tcptrprintf.o
TCPTR_LIBC_G_OBJS = $(OBJDIR)/tcptrmalloc-g.o $(OBJDIR)/tcmalloc-hash-g.o \
		$(OBJDIR)/tcptrlibc-g.o $(OBJDIR)/tcptrscanf-g.o $(OBJDIR)/tcptrprintf-g.o \
		$(OBJDIR)/tcptrhisto-g.o
TCPTRW_LIBC_OBJS = $(OBJDIR)/tcptrmalloc.w.o $(OBJDIR)/tcmalloc-hash.o \
		$(OBJDIR)/tcptrlibc.w.o $(OBJDIR)/tcptrscanf.w.o $(OBJDIR)/tcptrprintf.w.o
TCPTRW_LIBC_G_OBJS = $(OBJDIR)/tcptrmalloc-g.w.o $(OBJDIR)/tcmalloc-hash-g.o \
		$(OBJDIR)/tcptrlibc-g.w.o $(OBJDIR)/tcptrscanf-g.w.o $(OBJDIR)/tcptrprintf-g.w.o \
		$(OBJDIR)/tcptrhisto-g.o
TCVULNW_LIBC_OBJS = $(OBJDIR)/tcptrmalloc.vw.o $(OBJDIR)/tcmalloc-hash.o \
		$(OBJDIR)/tcptrlibc.vw.o $(OBJDIR)/tcptrscanf.vw.o $(OBJDIR)/tcptrprintf.vw.o
TCVULNW_LIBC_G_OBJS = $(OBJDIR)/tcptrmalloc-g.vw.o $(OBJDIR)/tcmalloc-hash-g.o \
		$(OBJDIR)/tcptrlibc-g.vw.o $(OBJDIR)/tcptrscanf-g.vw.o $(OBJDIR)/tcptrprintf-g.vw.o \
		$(OBJDIR)/tcptrhisto-g.o

TCAPI_NEWTAGS_OBJS = $(OBJDIR)/pmirror_newtags.o $(TCLIBC_OBJS)
TCAPI_NEWTAGS_NS_OBJS = $(OBJDIR)/pmirror_newtags-ns.o $(TCLIBC_OBJS)
TCAPI_NEWTAGS_TRS_OBJS = $(OBJDIR)/pmirror_newtags-trs.o $(TCLIBC_OBJS)
TCAPI_NEWTAGS_G_OBJS = $(OBJDIR)/pmirror_newtags-g.o $(TCLIBC_G_OBJS)
TCAPI_NEWTAGS_PG_OBJS = $(OBJDIR)/pmirror_newtags-pg.o $(TCLIBC_PG_OBJS)
TCAPI_NEWTAGS_CC_OBJS = $(OBJDIR)/pmirror_newtags-cc.o $(TCLIBC_CC_OBJS)
TCAPI_NEWTAGS_CCTRS_OBJS = $(OBJDIR)/pmirror_newtags-cctrs.o $(TCLIBC_CC_OBJS)

TCPTR_OBJS = $(OBJDIR)/tcptr.o $(TCPTR_LIBC_OBJS)
TCPTR_G_OBJS = $(OBJDIR)/tcptr-g.o $(TCPTR_LIBC_G_OBJS)

TCPTR_STATICMIR_OBJS = $(OBJDIR)/tcptr-stm.o $(TCPTR_LIBC_OBJS)
TCPTR_STATICMIR_G_OBJS = $(OBJDIR)/tcptr-stm-g.o $(TCPTR_LIBC_G_OBJS)

TCPTRW_OBJS = $(OBJDIR)/tcptr.o $(TCPTRW_LIBC_OBJS)
TCPTRW_G_OBJS = $(OBJDIR)/tcptr-g.o $(TCPTRW_LIBC_G_OBJS)

TCPTRW_STATICMIR_OBJS = $(OBJDIR)/tcptr-stm.o $(TCPTRW_LIBC_OBJS)
TCPTRW_STATICMIR_G_OBJS = $(OBJDIR)/tcptr-stm-g.o $(TCPTRW_LIBC_G_OBJS)

TCVULNW_OBJS = $(OBJDIR)/tcptr.vw.o $(TCVULNW_LIBC_OBJS)
TCVULNW_G_OBJS = $(OBJDIR)/tcptr-g.vw.o $(TCVULNW_LIBC_G_OBJS)

TCVULNW_STATICMIR_OBJS = $(OBJDIR)/tcptr-stm.vw.o $(TCVULNW_LIBC_OBJS)
TCVULNW_STATICMIR_G_OBJS = $(OBJDIR)/tcptr-stm-g.vw.o $(TCVULNW_LIBC_G_OBJS)

TCNOOP_OBJS = $(OBJDIR)/tcnoop.o $(OBJDIR)/tcnooplibc.o

# Won't make use of verbose-asm without outputting assembly.  Also
# won't benefit from inling-functions if source is linked with mirror.o.
ifeq ($(CC), gcc)
	CFLAGS += -finline-functions -fverbose-asm
endif

regular: $(OBJDIR)/libtcapint.a

# obsolete targets include: pg cc cctrs terse
# (was: regular g sm smg ptr ptrg ptrw ptrwg nosig pg cc cctrs terse
#  )
all: regular g sm smg ptr ptrg ptrstm ptrstmg ptrw ptrwg vulnw vulnwg ptrwstm ptrwstmg vulnwstm vulnwstmg nosig noop

depend:
	makedepend -I$(INCDIR) -Y $(SRCS) -p$(OBJDIR)/

g: $(OBJDIR)/libtcapintg.a

sm: $(OBJDIR)/libtcapintsm.a

smg: $(OBJDIR)/libtcapintsmg.a

# verify-ptr version
ptr: $(OBJDIR)/libtcptr.a

# verify-ptr version, -g
ptrg: $(OBJDIR)/libtcptrg.a

# verify-ptr write-only version
ptrw: $(OBJDIR)/libtcptrw.a

# verify-ptr write-only version, -g
ptrwg: $(OBJDIR)/libtcptrwg.a

# vuln-verify-ptr write-only version
vulnw: $(OBJDIR)/libtcvulnw.a

# vuln-verify-ptr write-only version, -g
vulnwg: $(OBJDIR)/libtcvulnwg.a

# staticmir version of above
ptrstm: $(OBJDIR)/libtcptrstm.a

ptrstmg: $(OBJDIR)/libtcptrstmg.a

ptrwstm: $(OBJDIR)/libtcptrwstm.a

ptrwstmg: $(OBJDIR)/libtcptrwstmg.a

vulnwstm: $(OBJDIR)/libtcvulnwstm.a

vulnwstmg: $(OBJDIR)/libtcvulnwstmg.a

# "no signal" version
nosig: $(OBJDIR)/libtcapintns.a

# "terse output" version 
terse: $(OBJDIR)/libtcapinttrs.a

pg: $(OBJDIR)/libtcapintpg.a

cc: $(OBJDIR)/libtcapintcc.a

cctrs: $(OBJDIR)/libtcapintcctrs.a

newtags: $(OBJDIR)/libtcapint.a

mir: $(OBJDIR)/libtcapimir.a


$(OBJDIR)/libtcapint.a: $(TCAPI_NEWTAGS_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintg.a: $(TCAPI_NEWTAGS_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintsm.a: $(subst tcmalloc.o,tcmalloc-safe.o,$(TCAPI_NEWTAGS_OBJS))
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintsmg.a:  $(subst tcmalloc-g.o,tcmalloc-safe-g.o,$(TCAPI_NEWTAGS_G_OBJS))
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptr.a: $(TCPTR_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrg.a: $(TCPTR_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrw.a: $(TCPTRW_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrwg.a: $(TCPTRW_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcvulnw.a: $(TCVULNW_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcvulnwg.a: $(TCVULNW_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrstm.a: $(TCPTR_STATICMIR_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrstmg.a: $(TCPTR_STATICMIR_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrwstm.a: $(TCPTRW_STATICMIR_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcptrwstmg.a: $(TCPTRW_STATICMIR_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcvulnwstm.a: $(TCVULNW_STATICMIR_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcvulnwstmg.a: $(TCVULNW_STATICMIR_G_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapimir.a: tcmir.o
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapinttrs.a: $(TCAPI_NEWTAGS_TRS_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintns.a: $(TCAPI_NEWTAGS_NS_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintpg.a: $(TCAPI_NEWTAGS_PG_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintcc.a: $(TCAPI_NEWTAGS_CC_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

$(OBJDIR)/libtcapintcctrs.a: $(TCAPI_NEWTAGS_CCTRS_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

noop: $(OBJDIR)/libtcnoop.a

$(OBJDIR)/libtcnoop.a: $(TCNOOP_OBJS)
	rm -f $@
	$(AR) $(AR_FLAGS) $@ $^
	cp -f $@ $(LIBDIR)

pmirror_newtags: pmirror_newtags.c
	$(CC) -DSTANDALONE -g $(CFLAGS) $< -o $@

tcptr: tcptr.c
	$(CC) -DSTANDALONE -g $(CFLAGS) $< -o $@

############

$(OBJDIR)/%-ns.o: %.c
	$(CC) -c -o $@ -DTC_NOSIGNAL $(CFLAGS) $<

$(OBJDIR)/%-trs.o: %.c
	$(CC) -c -o $@ -DTC_TERSE_OUTPUT $(CFLAGS) $<

$(OBJDIR)/%-stm-g.w.o: %.c
	$(CC) -DTC_PTRW -DTC_STATIC_MIRROR -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-stm-g.vw.o: %.c
	$(CC) -DTC_PTRW -DTC_VULNERABLE_VP -DTC_STATIC_MIRROR -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-g.w.o: %.c
	$(CC) -DTC_PTRW -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-g.vw.o: %.c
	$(CC) -DTC_PTRW -DTC_VULNERABLE_VP -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-stm.w.o: %.c
	$(CC) -DTC_PTRW -DTC_STATIC_MIRROR -c -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-stm.vw.o: %.c
	$(CC) -DTC_PTRW -DTC_VULNERABLE_VP -DTC_STATIC_MIRROR -c -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%.w.o: %.c
	$(CC) -DTC_PTRW -c -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%.vw.o: %.c
	$(CC) -DTC_PTRW -DTC_VULNERABLE_VP -c -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-stm-g.o: %.c
	$(CC) -DTC_STATIC_MIRROR -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-g.o: %.c
	$(CC) -DTC_PRINT_STATS -c -g -o $@ $(CFLAGS) -O0 $<

$(OBJDIR)/%-pg.o: %.c
	$(CC) -c -pg -o $@ $(CFLAGS) $<

$(OBJDIR)/%-cc.o: %.c
	cc -c -o $@ $(CFLAGS) $<

$(OBJDIR)/%-cctrs.o: %.c
	cc -c -o $@ -DTC_TERSE_OUTPUT $(CFLAGS) $<

$(OBJDIR)/%-stm.o: %.c
	$(CC) -DTC_STATIC_MIRROR -c -o $@ $(CFLAGS) $<

$(OBJDIR)/%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

############

clean:
	rm -f $(OBJDIR)/*
clean_all: clean
	rm -f $(LIBDIR)/*

# DO NOT DELETE

../obj/pmirror_newtags.o: tcinternal.h
../obj/pmirror_newtags.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/pmirror_newtags.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/pmirror_newtags.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tclibc.o: tcinternal.h
../obj/tclibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tclibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tclibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tclibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcmalloc-hash.o: tcmalloc-hash.h
../obj/tcmalloc.o: tcinternal.h
../obj/tcmalloc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcmalloc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcmalloc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcmalloc.o: tcmalloc-hash.h
../obj/tcmalloc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcmalloc-safe.o: tcmalloc.c tcinternal.h
../obj/tcmalloc-safe.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcmalloc-safe.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcmalloc-safe.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcmalloc-safe.o: tcmalloc-hash.h
../obj/tcmalloc-safe.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcmir.o: tcinternal.h
../obj/tcmir.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcmir.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcmir.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcnoop.o: tcinternal.h
../obj/tcnoop.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcnoop.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcnoop.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcnooplibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcptr.o: tcptr_internal.h
../obj/tcptr.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h
../obj/tcptr.o: tcmalloc-hash.h
../obj/tcptrlibc.o: tcptr_internal.h
../obj/tcptrlibc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h
../obj/tcptrmalloc.o: tcptr_internal.h
../obj/tcptrmalloc.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h
../obj/tcptrmalloc.o: tcmalloc-hash.h
../obj/tcptrscanf.o: tcptr_internal.h
../obj/tcptrscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h
../obj/tcptrscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcptrprintf.o: tcptr_internal.h
../obj/tcptrprintf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h
../obj/tcptrprintf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcscanf.o: tcinternal.h
../obj/tcscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcscanf.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcsocket.o: tcinternal.h
../obj/tcsocket.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcsocket.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcsocket.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcsocket.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcstdio.o: tcinternal.h
../obj/tcstdio.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcstdio.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcstdio.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcstdio.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
../obj/tcstring.o: tcinternal.h
../obj/tcstring.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h
../obj/tcstring.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tcapi_postinstr.h
../obj/tcstring.o: /p/wpis/people/students/suan/TypecheckDebugger/include/mirror_sizes.h
../obj/tcstring.o: /p/wpis/people/students/suan/TypecheckDebugger/include/tclibc.h
