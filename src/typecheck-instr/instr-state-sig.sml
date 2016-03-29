(* --------------------------------------------------------------------
 * InstrState: a local structure for operating on context state during
 * the instr-ast phase.
 * --------------------------------------------------------------------
 *)
      
signature INSTR_STATE =
sig

  (* tmpVariables: stack of lists of id's created during the
   instrumentation of a Compound statement. *)
  type tmpVariables =
    {tmpVars : (Ast.id list) list ref}

  (* undeclFunctions: list of ids of undeclared functions seen while instrumenting an AST *)
  type undeclFunctions =
    {undeclFuns : Ast.id list ref}

  (* staticFlagIds: list of ids created while processing a Compound's statics *)
  type staticFlagIds =
    {staticIds : Ast.id list ref}

  (* externVariables: list of ids of externs seen while instrumenting an AST *)
  type externVariables =
    {newExterns : Ast.id list ref}

  (* local instrumentation state components, holding temporary information *)
  type localInstrState =
    {tmpVariables : tmpVariables,
     undeclFunctions : undeclFunctions,
     staticFlagIds : staticFlagIds,
     externVariables : externVariables}

  (* packages of functions to manipulate state implicitly *)
  type instrStateFuns =
    {errorState : Error.errorState,

     tmpVarsFuns :
      {addTmpVar : Ast.id -> unit,
         (* records id for a new temporary *)
       getTmpVars : unit -> Ast.id list,
         (* returns top id list on the stack of lists *)
       pushTmpVarList : unit -> unit,
         (* pushes a new list onto the stack of lists of tmp vars *)
       popTmpVarList : unit -> unit},
         (* pops the stack of lists of tmp vars *)

     undeclFunsFuns :
      {pushUndeclFuns : Ast.id -> unit,
         (* records id of an undeclared function *)
       resetUndeclFuns : unit -> Ast.id list},
         (* returns list of recently generated undeclared function ids
	  (since last resetUndeclFuns call) *)

     staticFlagsFuns :
      {pushStatic : Ast.id -> unit,
         (* records id for temporary introduced while processing a Compound's decls *)
       popStatic : unit -> unit,
         (* pops static from the staticIds list *)
       getStatic : unit -> Ast.id},
         (* returns top static from the staticIds list *)

     externVarsFuns :
      {pushExterns : Ast.id -> unit,
         (* records id of an extern *)
       resetExterns : unit -> Ast.id list}}
         (* returns list of recently generated extern ids (since last resetExterns call) *)

  (* state initialization function *)
  val initLocal : unit -> localInstrState

  (* returns a collection of state functions *)
  val instrStateFuns : localInstrState * Error.errorState -> instrStateFuns

end (* sigature INSTR_STATE *)
