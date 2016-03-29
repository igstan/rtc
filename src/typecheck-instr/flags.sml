(*
  Note, for each new flag:
   I.   add documentation
   II.  add variable declaration
   III. add string comparison cases for both -flag and -no-flag
   IV.  add usage help information

(**********************)
(* I. - documentation *)

-aid (default=off)
 - set execution mode to "aid": Dump aid-annotated source (to file.aids)
   (default execution mode is "instr": Instrument and output to file.instr.c)

-assign (default=off)
 - set execution mode to "assign": Dump assignment edges (to file.tc_asgs)
   (default execution mode is "instr": Instrument and output to file.instr.c)

-cacheAlias (default=true)
 - use cached version of alias lookup function: slower during first lookup,
   but amortized over long run.

-freeMallocOnly (default=true)
 - in optimized mode, when checking if free must be instrumented, ignore
   all non-Malloc objects in free's argument's points-to set.
   This is potentially unsafe, as it would elide instrumentation that might
   detect an erroneous free of a non-malloc object.

-indivClearTag (default=false)	(* default true for -ptr/-ptrw *)
 - (-ptr mode only) clear stack tags by calling clearTags on auto locals
   individually, rather than calling processReturn

-initGlobals (default=true)
 - initialize the tags of global and static variables, even when uninitialized
   (ansi-C will 0-initialize them; this assumes the initial-0 is "correctly typed")
	- instrDeclsStmts: if stClass = Ast.STATIC
	- instrExternalDecl: Ast.ExternalDecl - non-extern non-function ids

-initAll (default=false)
 => will also set initGlobals := true
 - initialize the tags of all variables, even when uninitialized
   ("bad" for auto variables, which could contain gibberish)
	- instrDeclsStmts: fallthrough (auto):
		case initExprOp of SOME Ast.Aggregate
		case initExprOp of NONE (2 cases)

-mayBeUninit (default=false for now, will set to true when working)
 - run may_be_uninit analysis, when -tslfile- is set.
 --> NO LONGER SUPPORTED--DO NOT USE <--

-nullifyMBUptrs (default=true)
 - zero-initialize safe pointers that may be uninitialized
   (autos and malloc'ed).

-nullifyAllAutos (default=false)
 - zero-initialize all auto and malloc variables.

-optimizeCopyTag (default=true)
 - if rhs of a would-be copyTag call (for an assignment) is a scalar static
   repptr, call setScalarTag instead.

-preserveQCrval (default=false)
 - requires preservation of rvalues for question-colon operator
 - turning off is non-ANSI compliant, but may optimize expressions where the
   rvalues of the ?: operator are never used, e.g.: flag ? doThis() : doThat();

-printfPctN (default=true)
 - if printf format string is a literal with no "%n", don't instrument.

-ptr (default=off)
 - set execution mode to "ptr": instrument to do only verifyPtr checks (on
   both reads and writes), for security tool
  -> also turns off skipClashingCalls

-ptrw (default=off)
 - set execution mode to "ptrw": instrument to do verifyPtr checks, only
   on writes, for security tool
  -> also turns off skipClashingCalls

-regular (default=on)
 - sets execution mode to "instr": regular instrumentation;
   overridden by other execution mode flags: -assign, -ptr, -ptrw

-reportStructAssigns (default=false)
 - reports every non-scalar-typed assignment found

-sizedUninit (default=true)
 - the "Uninit" tagging will now include a size component (if false, "Uninit" is
   treated like "Unalloc" which treats memory as a bunch of one-byte chunks)

-skipClashingCalls (default=false)
 - GCC's interpretation of C specs is such that if an expression has any clashing
     side effects, it balks on the *entire* expressions, without preserving the
     "expected" behavior of any non-clashing sub-expressions. This affects the way
     we handle function calls, where we pass information about actual arguments
     and the expected return type via a global pointer, so an expression like
       foo(a) + foo(b);
     is instrumented to become something like
       (global=info_a, foo(a)) + (global=info_b, foo(b));
     Since the entire expression has clashing side-effects, GCC does NOT preserve
     the "expected" behavior that global is info_a at the foo(a) callsite,
     and info_b at the foo(b) callsite.
   When this flag is turned on, we'll simply skip instrumenting calls whenever
     there's more than one function call within a non-sequence-pointed expression.

-strictPointer (default=false)
 - in this mode, a value is tagged "Ptr" iff it is a "true pointer", i.e. it comes
   from the address of operator, from malloc, or from interfaces with uninstrumented
   code (return values, formal arguments from a callback?).
   IOW, NULL pointers are treated as integers.
 * Note: this flag is forwarded to the runtime library.
	- true: initTag calls _setScalarTagPtrToInt instead of _setScalarTag
	- false: all casts are treated as conversion --> NO LONGER SO! changed 07sep02 <--

-strLitWritable (default=false)
 - for -ptrw mode: indicate whether we want to consider string literals writable.

-verifyArray (default=false)
 - verifyPtr by default is not called to verify an array type, mainly because we
	don't know how to handle incomplete array types. With this flag, we would
	verify that the entire array is allocated; if the type is incomplete, we
	would just verify one element of the array.

-vuln (default=false)
 - ptr/ptrw vulnerable mode:
   track only vulnerable locations, and don't check vulnerable dereferences.

-opti-<s> (<s>=[none|locaddrof|...], default=none)
 - flow-insensitive optimization type

-opts-<s> (<s>=[none|...], default=none)
 - flow-sensitive optimization type

-tsl-default-safe
-tsl-default-unsafe (default)
 - set the default ts-level

-tslfile-<filename> (default = "" = no ts-level-file)
 - read ts_levels from file

*)

structure Flags =
struct

(****************************************)
(* II. - reference variable declaration *)

  datatype execute_mode = EM_ASSIGN | EM_AID | EM_INSTR
  datatype instr_mode = IM_INSTR | IM_PTR | IM_PTRW

  val executeMode = ref EM_INSTR
  val instrMode = ref IM_INSTR
  val cacheAlias = ref true
  val freeMallocOnly = ref true
  val indivClearTag = ref false
  val initGlobals = ref true
  val initAll = ref false
  val mayBeUninit = ref false
  val optimizeCopyTag = ref true
  val nullifyMBUptrs = ref true
  val nullifyAllAutos = ref false
  val preserveQCrval = ref false
  val printfPctN = ref true
  val reportStructAssigns = ref false
  val sizedUninit = ref true
  val skipClashingCalls = ref false
  val strictPointer = ref false
  val strLitWritable = ref false
  val verifyArray = ref false
  val vuln = ref false
  val optiType = ref "none"
  val optsType = ref "none"
  val ts_default = ref Rtc.TSL_UNSAFE
  val ts_file = ref ""

(****************************)
(* III. - string comparison *)

  fun set (flag) =
      case flag
	of "-aid" => (executeMode := EM_AID)
	 | "-assign" => (executeMode := EM_ASSIGN)
	 | "-cacheAlias" => (cacheAlias := true)
	 | "-no-cacheAlias" => (cacheAlias := false)
	 | "-freeMallocOnly" => (freeMallocOnly := true)
	 | "-no-freeMallocOnly" => (freeMallocOnly := false)
	 | "-indivClearTag" => (indivClearTag:= true)
	 | "-no-indivClearTag" => (indivClearTag:= false)
	 | "-initGlobals" => (initGlobals := true)
	 | "-no-initGlobals" => (initGlobals := false)
	 | "-initAll" => ( initAll := true
			 ; initGlobals := true
			 )
	 | "-no-initAll" => (initAll := false)
	 | "-mayBeUninit" => (mayBeUninit := true)
	 | "-no-mayBeUninit" => (mayBeUninit := false)
	 | "-nullifyMBUptrs" => (nullifyMBUptrs := true)
	 | "-no-nullifyMBUptrs" => (nullifyMBUptrs := false)
	 | "-nullifyAllAutos" => (nullifyAllAutos := true)
	 | "-no-nullifyAllAutos" => (nullifyAllAutos := false)
	 | "-optimizeCopyTag" => (optimizeCopyTag := true)
	 | "-no-optimizeCopyTag" => (optimizeCopyTag := false)
	 | "-preserveQCrval" => (preserveQCrval := true)
	 | "-no-preserveQCrval" => (preserveQCrval := false)
	 | "-printfPctN" => (printfPctN := true)
	 | "-no-printfPctN" => (printfPctN := false)
	 | "-ptr" =>	( instrMode := IM_PTR
			; indivClearTag:= true
			; skipClashingCalls := false
			)
	 | "-ptrw" =>	( instrMode := IM_PTRW
			; indivClearTag:= true
			; skipClashingCalls := false
			)
	 | "-regular" => (instrMode := IM_INSTR)
	 | "-reportStructAssigns" => (reportStructAssigns := true)
	 | "-no-reportStructAssigns" => (reportStructAssigns := false)
	 | "-sizedUninit" => (sizedUninit := true)
	 | "-no-sizedUninit" => (sizedUninit := false)
	 | "-skipClashingCalls" => (skipClashingCalls := true)
	 | "-no-skipClashingCalls" => (skipClashingCalls := false)
	 | "-strictPointer" => (strictPointer := true)
	 | "-no-strictPointer" => (strictPointer := false)
	 | "-strLitWritable" => (strLitWritable := true)
	 | "-no-strLitWritable" => (strLitWritable := false)
	 | "-verifyArray" => (verifyArray := true)
	 | "-no-verifyArray" => (verifyArray := false)
	 | "-vuln" => (vuln := true)
	 | "-no-vuln" => (vuln := false)
	 | "-opti-none" => (optiType := "none")
	 | "-opti-locaddrof" => (optiType := "locaddrof")
	 | "-opts-none" => (optsType := "none")
	 | "-tsl-default-safe" => (ts_default := Rtc.TSL_SAFE)
	 | "-tsl-default-unsafe" => (ts_default := Rtc.TSL_UNSAFE)
	 | _ =>
	   if String.isPrefix "-tslfile-" flag
	   then (ts_file := (String.extract (flag,9,NONE)))
	   else print ("Unrecognized flag: " ^ flag ^ "\n")

(***************************)
(* IV. - usage information *)

  fun printUsage () =
      print(
	  "Command line flags (-flag or -no-flag) are (default value):\n"
	^ " -aid          Dump aid-annotated file - to file.aids\n"
	^ " -assign       Dump assignment edges - to file.tc_[ip]asgs\n"
	^ " -ptr          Do verifyPtr-only instrumentation - security tool\n"
	^ " -ptrw         Do verifyPtr-on-write only instrumentation - security tool\n"
	^ " -vuln         Do ptr/ptrw in vulnerable mode (false)\n"
	^ " -regular      Do ordinary instrumentation (default)\n"
	^ "\n"
	^ " -cacheAlias   Use cached version of alias lookup function (true)\n"
	^ " -freeMallocOnly\n"
	^ "               When optimizing free instrumentation, ignore potential\n"
	^ "               freeing of non-malloc objects (true)\n"
	^ " -indivClearTag\n"
	^ "               clear tags for locals individually (false; true if -ptr/ptrw)\n"
	^ " -initGlobals  Initialize global and static variables (true)\n"
	^ " -initAll      Initialize all variables (false)\n"
	^ " -mayBeUninit  *OBSOLETE* Run local version of may-be-uninit analysis (true)\n"
	^ "               (*this feature is no longer supported, and may not be accurate)\n"
	^ " -nullifyMBUptrs\n"
	^ "               zero-initialize pointers that may be uninitialized (true)\n"
	^ "               (if false, use pMBU unsafe/tracked info instead)\n"
	^ " -nullifyAllAutos\n"
	^ "               zero-initialize all auto and heap locations (false)\n"
	^ " -optimizeCopyTag\n"
	^ "               replace copyTag with setScalarTag if possible (true)\n"
	^ " -preserveQCrval\n"
	^ "               Preserve rvalues for ?: operator (false)\n"
	^ " -printfPctN   Don't instrument printf with %n-less stringlit (true)\n"
	^ " -reportStructAssigns\n"
	^ "               Report non-scalar assignments in source file (false)\n"
	^ " -sizedUninit  Uninit tags are sized (true)\n"
	^ " -skipClashingCalls\n"
	^ "               Don't instrument function calls when instrumentation\n"
	^ "               might cause side-effect clashes (false)\n"
	^ " -strictPointer\n"
	^ "               Ptr tag for `true pointers' only (false)\n"
	^ " -strLitWritable\n"
	^ "               -ptrw mode only: are string literals writable? (false)\n"
	^ " -verifyArray  verifyPtr will be called on array types (false)\n"
	^ "\n"
	^ " -opti-<s>     flow-insenitive optimization [(none)|locaddrof]\n"
	^ " -opts-<s>     flow-sensitive optimization [(none)]\n"
	^ " -tsl-default-safe\n"
	^ " -tsl-default-unsafe (default)\n"
	^ "               sets default type-safety level\n"
	^ " -tslfile-<filename>\n"
	^ "               read type-safety levels from file (default \"\")\n"
      )

end
