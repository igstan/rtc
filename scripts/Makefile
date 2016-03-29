#################################################################
# USER DEFINED PART #
#####################
CC = gcc
#CFLAGS +=
#TCFLAGS += -persist
#LDFLAGS += -lm
#RTCAFLAGS += -mbu -rda

## Uncomment this to send rtca jobs to condor
## (only when making targets tsl-tca, tsl-ptr, or tsl-ptrw
#RTCA_USE_CONDOR = 1

### STEMS: list of object file stems
### TARGET: target name
ifndef TARGET

usage:
	@echo Usage: make TARGET=tgt

else

STEMS = $(TARGET)

endif


### If a dummy_externs object file is needed, add it here
#DUMMY_EXTERNS = dummy_externs.o

#################################################################

include /p/wpis/people/students/suan/TypecheckDebugger/scripts/Makefile.template

