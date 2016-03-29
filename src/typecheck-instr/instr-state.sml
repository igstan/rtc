structure InstrState : INSTR_STATE =
struct

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

  fun initLocal () : localInstrState =
	     {tmpVariables = {tmpVars = ref []},
	      undeclFunctions = {undeclFuns = ref []},
	      staticFlagIds = {staticIds = ref []},
	      externVariables = {newExterns = ref []}}

  (* provide packages of implicit state manipulation functions *)
  fun instrStateFuns(localInstrState as
		     {tmpVariables, undeclFunctions, staticFlagIds, externVariables}: localInstrState,
		     errorState: Error.errorState) : instrStateFuns =
     let val bug = Error.bug errorState

	 (* tmpVariables functions ******************************************)
	 local val {tmpVars} = tmpVariables
	 in

	     fun addTmpVar id =
		 case !tmpVars of
		     head::tail => tmpVars := (id :: head) :: tail
	       | nil => (bug "addTmpVar: empty tmpVars stack")

	     fun getTmpVars () =
		 case !tmpVars of
		     head::tail => rev(head) before tmpVars := nil :: tail
		   | nil => (bug "getTmpVars: empty tmpVars stack"; nil)
	     (* id's are pushed onto newExterns as encountered; need to reverse list to
	      * give original program order*)

	     fun pushTmpVarList () = tmpVars := nil :: !tmpVars

	     fun popTmpVarList () =
		 case !tmpVars of
		     head::tail => tmpVars := tail
		   | nil => (bug "popTmpVarList: empty tmpVars stack")
	 end

	 (* undeclFunctions functions ******************************************)
	 local val {undeclFuns} = undeclFunctions
	 in

	     fun pushUndeclFuns id =
		 undeclFuns := id :: !undeclFuns

	     fun resetUndeclFuns () =
		 rev(!undeclFuns) before (undeclFuns := [])
	     (* id's are pushed onto undeclFuns as encountered; need to reverse list to
	      * give original program order*)
	 end

	 (* staticFlagIds functions ***********************************************)
	 local val {staticIds} = staticFlagIds
	     val bogusStatic =
		 {name = Symbol.object "static_flag", uid = Pid.new(),
		  location = SourceMap.UNKNOWN, ctype = Ast.Error,
		  stClass = Ast.DEFAULT, status = Ast.IMPLICIT,
		  global = false, kind = Ast.NONFUN}
	 in
	     (* get "current" static *)
	     (* accesses: staticIds *)
	     fun getStatic () =
		 case !staticIds
		     of id :: _ => id
		   | nil => (bug "getStatic: empty staticIds stack"; bogusStatic)

	     (* push the staticIds stack *)
	     (* affects: staticIds *)
	     fun pushStatic id =
		 staticIds := id :: !staticIds

	     (* pop the staticIds stack *)
	     (* affects: locStack *)
	     fun popStatic () =
		 case !staticIds
		     of _ :: rest => staticIds := rest
		   | nil => bug "popLoc: empty staticIds stack"

	 end (* staticFlagIds *)

	 (* externVariables functions ******************************************)
	 local val {newExterns} = externVariables
	 in

	     fun pushExterns id =
		 newExterns := id :: !newExterns

	     fun resetExterns () =
		 rev(!newExterns) before (newExterns := [])
	     (* id's are pushed onto newExterns as encountered; need to reverse list to
	      * give original program order*)
	 end

     in (* state function package *)
	 {errorState = errorState,

	  tmpVarsFuns =
	  {addTmpVar = addTmpVar,
	   getTmpVars = getTmpVars,
	   pushTmpVarList = pushTmpVarList,
	   popTmpVarList = popTmpVarList},

	  undeclFunsFuns =
	  {pushUndeclFuns = pushUndeclFuns,
	   resetUndeclFuns = resetUndeclFuns},

	  staticFlagsFuns =
	  {pushStatic = pushStatic,
	   popStatic = popStatic,
	   getStatic = getStatic},

	  externVarsFuns =
	  {pushExterns = pushExterns,
	   resetExterns = resetExterns}}
     end (* fun instrStateFuns *)

end (* structure State *)
