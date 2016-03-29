#
# -DSTRONGER_CPP changes and bypasses some code that
# used to work with gcc (3.2.1) but no longer works
# with (3.3.2):
#  - static const defined inline
#  - friend function no longer recognized!?
#
############################################################

CXX = g++
CXXFLAGS = -g -Wreturn-type
LDFLAGS =

STEMS = ao cfg dfa diag ecr edge id interval mbu pmbu pta ran rda rdmb red suco_usage tca ty

SRCS = $(addsuffix .cpp, $(STEMS))
OBJS = $(addsuffix .o, $(STEMS))

rtca-raw: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

pure: rtca-raw-pure

rtca-raw-pure: $(OBJS)
	purify -chain-length=12 $(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

condor: CXX=/usr/bin/g++
condor: $(OBJS)
	condor_compile $(CXX) $(CXXFLAGS) -o rtca-raw-condor $^ $(LDFLAGS)

libc: libc.c
	tc -assign -instr-no-printfPctN libc.c
	cp -f libc.tc_iasgs ../../scripts/libc.tc_asgs
	@echo '*******************************************************'
	@echo 'FOR CONDOR: COPY libc.tc_iasgs TO ~/CONDOR/libc.tc_asgs'
	@echo '*******************************************************'

depend:
	makedepend -Y $(SRCS)

clean:
	rm -f *.o

# DO NOT DELETE

ao.o: ao.h ty.h suco.h id.h edge.h flags.h ecr.h
cfg.o: ao.h ty.h suco.h id.h edge.h ecr.h cfg.h dfa.h mbu.h rda.h flags.h
cfg.o: rdmb.h ran.h red.h interval.h tca.h diag.h
dfa.o: ao.h ty.h suco.h id.h edge.h cfg.h dfa.h mbu.h rda.h flags.h rdmb.h
dfa.o: ran.h red.h interval.h diag.h
diag.o: flags.h diag.h ao.h ty.h suco.h id.h edge.h ecr.h cfg.h dfa.h mbu.h
diag.o: rda.h rdmb.h ran.h red.h interval.h
ecr.o: diag.h ao.h ty.h suco.h id.h edge.h ecr.h flags.h
edge.o: edge.h ao.h ty.h suco.h id.h
id.o: id.h ao.h ty.h suco.h edge.h
interval.o: ao.h ty.h suco.h id.h edge.h flags.h interval.h
mbu.o: ao.h ty.h suco.h id.h edge.h ecr.h cfg.h dfa.h mbu.h rda.h flags.h
mbu.o: rdmb.h ran.h red.h interval.h diag.h
pmbu.o: cfg.h suco.h ao.h ty.h id.h edge.h dfa.h mbu.h rda.h flags.h rdmb.h
pmbu.o: ran.h red.h interval.h ecr.h pmbu.h
pta.o: flags.h diag.h ao.h ty.h suco.h id.h edge.h ecr.h pta.h
ran.o: cfg.h suco.h ao.h ty.h id.h edge.h dfa.h mbu.h rda.h flags.h rdmb.h
ran.o: ran.h red.h interval.h diag.h ecr.h
rda.o: ao.h ty.h suco.h id.h edge.h ecr.h cfg.h dfa.h mbu.h rda.h flags.h
rda.o: rdmb.h ran.h red.h interval.h diag.h
rdmb.o: ecr.h ao.h ty.h suco.h id.h edge.h cfg.h dfa.h mbu.h rda.h flags.h
rdmb.o: rdmb.h ran.h red.h interval.h
red.o: cfg.h suco.h ao.h ty.h id.h edge.h dfa.h mbu.h rda.h flags.h rdmb.h
red.o: ran.h red.h interval.h ecr.h
suco_usage.o: suco.h suco.cpp
tca.o: flags.h diag.h ao.h ty.h suco.h id.h edge.h tca.h ecr.h cfg.h dfa.h
tca.o: mbu.h rda.h rdmb.h ran.h red.h interval.h pta.h pmbu.h
ty.o: flags.h diag.h ao.h ty.h suco.h id.h edge.h
