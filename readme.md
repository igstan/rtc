# Protecting C Programs from Attacks via Invalid Pointer Dereferences

Imported from here: http://pages.cs.wisc.edu/~suan/Projects/SecurityTool/. Original README follows:

RTC tool and Security Tool.

(Suan Yong, Alexey Loginov; Susan Horwitz, Thomas Reps)
(University of Wisconsin-Madison)

Skeletal how-to file written 3 May 2004.

Requires:
 - smlnj
 - ckit (included)
 - c/c++ compiler (assume gcc/g++)

Build instructions: ($RTCDIR is this directory)
1. To build instrumentation module (needs sml): (">" is shell prompt)

    > cd $RTCDIR/src/typecheck-instr
    > sml
    sml> CM.make();
       ...
      val it = () : unit
    sml> SMLofNJ.exportFn("filesToInstrCFiles", ParseToInstrAst.externFileToInstrCFile);
    > mv filesToInstrCFiles.* ../../bin

   This builds the SML module filesToInstrCFiles
   and moves it to the bin directory.
   The SML module instruments pre-processed C input
   files.

2. To build RTC libraries:

    > cd $RTCDIR/src/dynamic
    > make all

   This makes all the different libraries, and
   moves them to the lib directory.  Some of the
   libraries are:

    libtcapint.a  - RTC runtime library
    libtcapintg.a - Same, but with -g turned on
    libtcptrw.a   - Security tool runtime library

3. Update scripts: go to $RTCDIR/scripts and edit
   each of the following files, to set the appropriate
   directory environment (currently set to those on
   my machine): tc rtca Makefile

   Add $RTCDIR/scripts to your path.

4. To compile test.c with RTC:

    > tc test.c

   NOTE: ckit currently reports certain array-vs-pointer
   type mismatches as "errors".  These can be ignored
   using "tc -persist".

5. To compile test.c with security tool (checking
   writes only):

    > tc -ptrw test.c

   (Use -ptr to check both reads and writes.)

6. To build static analysis component:

    > cd $RTCDIR/src/analysis
    > make
    > mv rtca-raw ../../bin

   Also, to build models for library functions, do:

    > make libc

7. Use the provided makefile as a starting point:

    > mkdir /tmp/test
    > cd /tmp/test
    > echo 'int *p; int main() { *p++ = 0; }' > test.c
    > cp $/RTCDIR/scripts/Makefile Makefile

   Don't forget: per step 3 above, the last line of
   this Makefile should have been edited to reflect
   the path on your machine.

   a. To create the executable file 'test':

      > make TARGET=test exe

   b. To create the RTC-instrumented executable
      'test-nt.instr':

      > make TARGET=test nt

      Try running it.

   c. Do static analysis, then instrument with RTC.
      Output is test-nt-tca.instr.

      > make TARGET=test nt-tca

   d. Instrument with security tool; output is
      test-ptrw.pwinstr.

      > make TARGET=test ptrw

   e. Security tool, with static analysis, and also
      array range-analysis and _red_undant-checks analysis.

      > make TARGET=test ptrw-tca ADDL_RTCAFLAGS='-range -red'

   Run 'rtca' to see other options to the static analysis
   module.

8. Edit Makefile: set STEMS to a list of file stems to
   compile multiple files.

-----

ADVANCED NOTES:

Without static analysis, it should be possible to substitute
CC="tc" or CC="tc -ptrw" into an arbitrary Makefile.
With static analysis, however, the complication of whole-program
analysis makes it impossible to do so, so the included Makefile
and Makefile.template is the easiest way to build projects with
multiple source file (though it hasn't been tuned for multiple
source directories).

Here are the steps in compiling files foo and bar with RTC:

  1. tc -E foo.c bar.c
     : preprocess files (output: %.ppi.c).
  2. tc -assign foo.ppi.c bar.ppi.c
     : use SML module to output intermediate representation
       for static analysis (output: %.tc_iasgs).
  3. rtca -nt -o foobar.tc_tca foo.tc_iasgs bar.tc_iasgs
     : static analysis.  Run 'rtca' to see various options.
  4. tc -instr -tsl-foobar.tc_tca foo.ppi.c foo.ppi.c
     : use SML module to instrument source files, guided
       by static analysis output given by -tsl-* argument
       (output: %.opt.instr.c).
     4a. Note: omitting the -tsl-* flag will cause ALL
         expressions to be instrumented (output: %.instr.c).
  5. tc -c foo.opt.instr.c bar.opt.instr.c
     : compile the instrumented source files
       (output: %.opt.instr.o)
  6. tc -o foobar foo.opt.instr.o bar.opt.instr.o
     : link the files (output executable: *-nt-tca.instr).

and for the security tool (replace -ptrw with -ptr to
check both reads and writes):

  1. tc -ptrw -E foo.c bar.c
     : preprocess files (output: %.ppp.c).
  2. tc -ptrw -assign foo.ppp.c bar.ppp.c
     : use SML module to output intermediate representation
       for static analysis (output: %.tc_pasgs).
  3. rtca -ptrw -o foobar.tc_ptrw foo.tc_pasgs bar.tc_pasgs
     : static analysis.  Run 'rtca' to see various options.
  4. tc -ptrw -instr -tsl-foobar.tc_ptrw foo.ppp.c foo.ppp.c
     : use SML module to instrument source files, guided
       by static analysis output given by -tsl-* argument
       (output: %.opt.pwinstr.c).
     4a. Note: omitting the -tsl-* flag will cause ALL
         dereferences and locations to be instrumented
         (output: %.pwinstr.c).
  5. tc -ptrw -c foo.opt.pwinstr.c bar.opt.pwinstr.c
     : compile the instrumented source files
       (output: %.opt.pwinstr.o)
  6. tc -ptrw -o foobar foo.opt.pwinstr.o bar.opt.pwinstr.o
     : link the files (output executable: *-ptrw-tca.pwinstr).

NOTE: Do "tc -g" to compile and output execution-time stats;
      this will also cause each error to send a SIGUSR1
      signal that can be caught with gdb (for example).
NOTE: Do "tc -macros -stm" to compile "optimal" version that
      uses inlined macro checks and a fast statically-allocated
      mirror.
