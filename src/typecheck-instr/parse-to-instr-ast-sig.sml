(* ast/parse-to-ast-sig.sml *)

(* This is the top-level interface to the C front-end.  It is 
 * implemented by the structures Ansic, FiveESSC, and D *)

signature PARSE_TO_INSTR_AST =
sig

  val fileToC : string -> unit

(**)val test : string -> unit

  val fileToCFile : string -> unit
  val externFileToC : string * string list -> OS.Process.status

  val fileToInstrCFile :
	(ParseToAst.astBundle * string * TextIO.outstream -> ParseToAst.astBundle) ->
	string * string list * string -> int

  val outputAssignEdges : string * string -> string -> int

  val externFileToTypedCFile : string * string list -> OS.Process.status
  val externFileToInstrCFile : string * string list -> OS.Process.status
  val externFileToTestCFile : string * string list -> OS.Process.status
  val externFileToMirCFile : string * string list -> OS.Process.status

end (* signature PARSE_TO_INSTR_AST *)
