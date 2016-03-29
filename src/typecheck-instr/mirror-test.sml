structure MirrorTest : (*MIRROR_TEST*) INSTR_AST = struct 

(* Top level function. *)
fun instrAst (bundle as {ast as edecls,
			 tidtab,
			 errorCount,
			 warningCount,
			 auxiliaryInfo as {aidtab, implicits, env as symtab}},
	      filestem, errStrm) =
let
  (* If BuildAst.makeAst starts returning errorState, we may want to reuse it. *)
  val errorState = Error.mkErrState errStrm
  val bug = Error.bug errorState

  (* Reconstruct the state out of the AST bundle. *)
  val stateInfo = ParseToAst.progToState bundle

  val globalState as {uidTables={ttab,atab,implicits},...} =
      State.initGlobal(stateInfo, errorState)

  val localState = State.initLocal ()

  val stateFuns = State.stateFuns(globalState, localState)

  val {locFuns =
       {pushLoc, popLoc, getLoc, error, warn},
       tidsFuns =
       {pushTids, resetTids},
       tmpVarsFuns =
       {pushTmpVars, resetTmpVars},
       envFuns =
       {topLevel, pushLocalEnv, popLocalEnv, lookSym, bindSym,
	lookSymGlobal, bindSymGlobal, lookLocalScope, getGlobalEnv},
       uidTabFuns =
       {bindAid, lookAid, bindTid, lookTid},
       funFuns =
       {newFunction, getReturnTy, checkLabels, addLabel, addGoto},
       switchFuns =
       {pushSwitchLabels, popSwitchLabels, addSwitchLabel, addDefaultLabel},
       ...}
      = stateFuns

  (* lookup type of expr in aidtab *)
  fun lookupExprType(Ast.EXPR(_, adorn, _)) =
      case lookAid adorn
        of SOME ct => ct
	 | NONE => (bug ("lookupExprType: no type for expression; aid = "
			 ^ Int.toString adorn);
		    Ast.Void)

  fun getGlobalFun name =
      let val funSym = Symbol.func (name)
      in
	  case lookSymGlobal funSym
	    of SOME (Bindings.ID id) => id
	     | _ =>
		  (* if ANSI C then this should be an error... *)
		  let val id = {name = funSym, uid = Pid.new(), location = getLoc(),
				ctype = Ast.Error, stClass = Ast.EXTERN,
				status = Ast.IMPLICIT, global = true,
				kind = Ast.FUNCTION{hasFunctionDef=false}}
		      val binding = Bindings.ID id
		  in
		      (print ("getGlobalFun: function " ^ name ^ " not declared\n\t" ^
				"or symbol is not a function, created a bogus id:\n\t");
		       (* Insert the id into the global symbol table to prevent further msgs. *)
		       bindSymGlobal(funSym, binding);
		       id)
		  end
      end

  fun addNewVariable (varName,stmt_loc) =
      (* Construct an id for the variable. *)
      let
	  val pid = Pid.new()
	  val varSym = Symbol.object varName

	  (* NOTE: build-ast.sml suggests that status if DEFINED if initialized,
		   and DECLARED otherwise. I don't think this distinction is
		   useful (or correct). Since we don't really use status anyway,
		   and for simplicity, status is set to DEFINED. *)
	  val id = {name = varSym, uid = pid, location = stmt_loc, ctype = Ast.Pointer Ast.Void,
		    stClass = Ast.DEFAULT, status = Ast.DEFINED, global = false, kind = Ast.NONFUN}

	  (* Insert the id into the local symbol table. *)
	  val binding = Bindings.ID id
	  val _ = bindSym(varSym, binding)
      in
	  id
      end

(*
  fun lookupExprImplicitType(Ast.EXPR(_,adorn,_)) = Aidtab.find (implicits,adorn)
*)
  (**************************  END OF HELPER FUNCTIONS *************************)
  (************************** START OF INSTR FUNCTIONS *************************)

  fun instrFunctionBody (stmt as Ast.STMT(_,stmt_aid,stmt_loc)) =
      let
	  (* - ADD VAR DECL: void * _mirrortest_dummy; - *)
	  val mirrortest_dummy_id = addNewVariable ("_mirrortest_dummy",stmt_loc)
	  val zero_exp = Ast.EXPR(Ast.IntConst 0, bindAid Ast.Void, stmt_loc)
	  val mirrortest_dummy_decl = Ast.VarDecl(mirrortest_dummy_id, SOME (Ast.Simple zero_exp))

	  (* - ADD FN CALL: _mirrortest_function(&_mirrortest_dummy); - *)
	  val mirrortest_function_id = getGlobalFun("_mirrortest_function")
	  val mirrortest_function_exp = Ast.EXPR(Ast.Id mirrortest_function_id, bindAid Ast.Void, stmt_loc)
	  val mirrortest_dummy_exp = Ast.EXPR(Ast.Id mirrortest_dummy_id, bindAid Ast.Void, stmt_loc)
	  val addrof_exp = Ast.EXPR(Ast.AddrOf mirrortest_dummy_exp, bindAid Ast.Void, stmt_loc)
	  val call_exp = Ast.EXPR(Ast.Call (mirrortest_function_exp, [addrof_exp]), bindAid Ast.Void, stmt_loc)
	  val call_stmt = Ast.STMT(Ast.Expr (SOME call_exp), Aid.new(), stmt_loc)
      in
          Ast.STMT(Ast.Compound ([mirrortest_dummy_decl],[call_stmt,stmt]), Aid.new(), stmt_loc)
      end

  fun instrExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
      (case coreEdecl
	 of Ast.ExternalDecl (Ast.TypeDecl _) => edecl
	  | Ast.ExternalDecl (Ast.VarDecl (id,initExprOp)) => edecl
	  | Ast.FunctionDef (id,ids,stmt) =>
		Ast.DECL (Ast.FunctionDef (id,ids,instrFunctionBody stmt),edecl_aid,edecl_loc)
	  | Ast.ExternalDeclExt _ => edecl
      )

  fun instrExternalDecls nil = nil
  |   instrExternalDecls (ed::edecls) =
		(instrExternalDecl ed)::(instrExternalDecls edecls)

  fun instrAst' edecls =
      let val new_edecls = instrExternalDecls (edecls)
	  val ret_bundle =
		{ ast=new_edecls,
		  tidtab=ttab,
		  errorCount=errorCount,
		  warningCount=warningCount,
		  auxiliaryInfo = { aidtab=atab,
				    implicits=implicits,
				    env=getGlobalEnv()}}
      in
	  (* Return AstBundle *)
          ret_bundle
      end

in
    instrAst' edecls
end (* fun instrAst *)


end (* Structure TypeExprs *)
