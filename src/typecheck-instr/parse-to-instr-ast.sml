(* parse-to-ast.sml *)

structure ParseToInstrAst : PARSE_TO_INSTR_AST =
struct

  fun fileToC infile = ParseToAst.fileToC infile

  (* given input "x.c", outputs "x.out.c" *)
  fun fileToCFile infile =
      let val outfile = String.substring(infile,0,(String.size infile)-2) ^ ".out.c"
	  val os = TextIO.openOut outfile
	  val {ast, tidtab, ...} = ParseToAst.fileToAst infile
	  val _ = PPLib.ppToStrm (PPAst.ppAst () tidtab) os ast
	  val _ = TextIO.closeOut os
      in  () end

  fun test fstem =
      let
	  val bundle as {tidtab=ttab,
			 auxiliaryInfo as {aidtab=atab,...},
			 ...} = ParseToAst.fileToAst (fstem ^ ".c")
	  val ((mbu_pids,mbu_aids), red_set, esc_set) = FlowSensitive.flowSensitiveAnalyses (bundle,fn _ => nil,fn aol => aol)
(*
	  val _ = FlowSensitive.aidset.app (fn aid => print (" " ^ (Int.toString aid))
					    ) mbu_aids
	  val _ = print "\n"
*)
      in  () end

  fun getFileStem (nil, infile) = infile
    | getFileStem (ext::tail, infile) =
	let val extlen = String.size ext
	    val infilelen = String.size infile
	in if (infilelen >= extlen)
		andalso (String.substring(infile,infilelen-extlen,extlen) = ext)
	    then String.substring(infile,0,infilelen-extlen)
	    else getFileStem(tail, infile)
	end

  (* given input ending in inexts, instrument with InstrEngine, and output with outext *)
  fun fileToInstrCFile InstrFunction (infile,inexts,outext) = 
      let

	  val filestem = getFileStem(inexts, infile)

	  val outfile = filestem ^ outext

	  val _ = print ("  -- Parsing " ^ infile ^ "\n")

	  val bundle = ParseToAst.fileToAst infile
	  val bundle = PreProcess.handlePrintfPctN bundle	(* Pass through printf %n pre-processor *)
	  val bundle = PreProcess.fixLibFnRetTys bundle		(* Fix pointer return types for undeclared lib-fns *)

	  val _ = print ("  -- Instrumenting AST\n")
	  val (bundle' as { ast,
			    tidtab (*Bindings.tidBinding Tidtab.uidtab*),
			    errorCount (*int*),
			    warningCount (*int*),
			    auxiliaryInfo as {aidtab, implicits (*opaidtab*), env}
			  }
		) = InstrFunction (bundle, filestem, TextIO.stdErr)

	  val os = TextIO.openOut outfile

	  val _ = print ("  -- Writing to file " ^ outfile ^ "\n")

	  val _ = PPLib.ppToStrm (PPAst.ppAst () tidtab) os ast
	  val _ = TextIO.closeOut os
      in  errorCount end

  fun externFileToC (arg0,nil) =
      let val _ = print ("Usage: " ^ arg0 ^ " file.c\n")
      in
	  OS.Process.success
      end
    | externFileToC (arg0,arg1::ignore) =
      let val _ = fileToInstrCFile (fn (x,_,_) => x) (arg1,[".tmp.c",".c"],".ckit.c")
      in
	  OS.Process.success
      end

  (* given input "x.c" or "x.tmp.c", outputs "typed" code in "x.typed.c" *)
  fun externFileToTypedCFile (arg0,nil) =
      let val _ = print ("Usage: " ^ arg0 ^ " file.c\n")
      in
	  OS.Process.success
      end
    | externFileToTypedCFile (arg0,arg1::ignore) =
      let val _ = fileToInstrCFile TypeExprs.instrAst (arg1,[".tmp.c",".c"],".typed.c")
      in
	  OS.Process.success
      end

  (* given input "x.c" or "x.tmp.c", outputs test code in "x.test.c" *)
  fun externFileToTestCFile (arg0,nil) =
      let val _ = print ("Usage: " ^ arg0 ^ " file.c\n")
      in
	  OS.Process.success
      end
    | externFileToTestCFile (arg0,arg1::ignore) =
      let val _ = fileToInstrCFile TestInstr.instrAst (arg1,[".tmp.c",".c"],".test.c")
      in
	  OS.Process.success
      end

  (* given input "x.c" or "x.tmp.c", outputs mirror-test code in "x.mir.c" *)
  fun externFileToMirCFile (arg0,nil) =
      let val _ = print ("Usage: " ^ arg0 ^ " file.c\n")
      in
	  OS.Process.success
      end
    | externFileToMirCFile (arg0,arg1::ignore) =
      let val _ = fileToInstrCFile MirrorTest.instrAst (arg1,[".tmp.c",".c"],".mir.c")
      in
	  OS.Process.success
      end

  (* given input "x.pp?.c", outputs "x.aids" *)
  fun outputAidFile (pp_sfx,aid_sfx) fname =
      let
	  val filestem = getFileStem([pp_sfx], fname)

	  val bundle = ParseToAst.fileToAst (fname)
	  val bundle = PreProcess.handlePrintfPctN bundle	(* Pass through printf %n pre-processor *)
	  val bundle = PreProcess.fixLibFnRetTys bundle		(* Fix pointer return types for undeclared lib-fns *)

	  val {ast,tidtab,errorCount,...} = bundle

	  val os = TextIO.openOut (filestem ^ aid_sfx)
	  val (no_tc_ast,tc_inc_aid) = OptInterface.stripTcInclude ast
	  val _ = PPLib.ppToStrm (PPAstDebugAid.ppAst tc_inc_aid tidtab) os no_tc_ast
	  val _ = TextIO.closeOut os

	  val _ = TextIO.output (TextIO.stdErr, "outputAidFile: tc_inc_aid = " ^ (Int.toString tc_inc_aid) ^ "\n")

      in  errorCount  end

  (* given input "x.pp?.c", dump assignment edges to "x.tc_?asgs" *)
  fun outputAssignEdges (pp_sfx,asg_sfx) fname =
      let
	  val filestem = getFileStem([pp_sfx], fname)

	  val bundle = ParseToAst.fileToAst (fname)
	  val bundle = PreProcess.handlePrintfPctN bundle	(* Pass through printf %n pre-processor *)
	  val bundle = PreProcess.fixLibFnRetTys bundle		(* Fix pointer return types for undeclared lib-fns *)

	  val { ast=edecls,
		tidtab=ttab,
		errorCount,
		warningCount,
		auxiliaryInfo as {aidtab, implicits, env}
	      } = bundle

	  val (no_tc_edecls,tc_inc_aid) = OptInterface.stripTcInclude edecls

	  val bundle = {ast=no_tc_edecls,
                         tidtab=ttab,
                         errorCount=errorCount,
                         warningCount=warningCount,
                         auxiliaryInfo=auxiliaryInfo}

	  fun expToCty (exp as Ast.EXPR (_,aid,_)) =
	      case Aidtab.find (aidtab,aid)
	        of SOME ct => ct
	         | NONE => (TextIO.output (TextIO.stdErr, "expToCty: no type for expression; aid = "
							  ^ (Int.toString aid)
							  ^ (PPLib.ppToString (PPAst.ppExpression () ttab) exp));
			    Ast.Void)

	  val os = TextIO.openOut (filestem ^ asg_sfx)
	  val (ahash,phash) = OptInterface.buildUidMaps (bundle,filestem ^ ".c")
	  val _ = OptInterface.outputAidPidAliases (ahash, phash, os)
	  val _ = OptInterface.genAssignEdges (bundle,os)
	  val _ = FlowSensitive.outputDataflowOperators
			(bundle, tc_inc_aid, expToCty, os)
	  val _ = TextIO.closeOut os
      in  errorCount  end

  (* given input "x.pp?.c", outputs instrumented code in "x.instr.c" or "x.opt.instr.c" *)
  (* or, if -assign is on, then dump assignment edges to "x.tc_asgs" *)
  fun externFileToInstrCFile (arg0,args) = 
      let fun execute fname =
	      let val opt_infix = if !Flags.ts_file = "" then "" else ".opt"
		  val (pp_sfx,asg_sfx,instr_sfx) =
		      case !Flags.instrMode
			of Flags.IM_INSTR => (".ppi.c", ".tc_iasgs", ".instr.c")
			 | Flags.IM_PTR =>   (".ppp.c", ".tc_pasgs", if !Flags.vuln then ".vinstr.c"
										    else ".pinstr.c")
			 | Flags.IM_PTRW =>  (".ppp.c", ".tc_pasgs", if !Flags.vuln then ".vwinstr.c"
										    else ".pwinstr.c")
	      in case !Flags.executeMode
		   of Flags.EM_AID => outputAidFile (pp_sfx,".aids") fname
		    | Flags.EM_ASSIGN => outputAssignEdges (pp_sfx,asg_sfx) fname
		    | Flags.EM_INSTR => fileToInstrCFile InstrAst.instrAst (fname, [pp_sfx], opt_infix ^ instr_sfx)
	      end

	  fun handleFlags nil = nil
	    | handleFlags (arg::tail) =
		if (String.sub(arg,0) = #"-")
		then let val _ = Flags.set arg
		     in handleFlags(tail) end
		else arg::(handleFlags(tail))
      in
	  case handleFlags (args)
	    of nil => let val _ = print ("Usage: " ^ arg0 ^ " file.pp?.c\n")
			  val _ = Flags.printUsage()
		      in
			  OS.Process.failure
		      end
	     | filenames =>
		if List.exists (fn fname => (execute fname) > 0) filenames
		then OS.Process.failure
		else OS.Process.success
      end

end (* structure ParseToInstrAst *)
