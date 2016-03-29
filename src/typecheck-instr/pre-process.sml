structure PreProcess : PRE_PROCESS =
struct 

  (* translate bundle by applying func to each expression, bottom-up in AST *)
  (* - similar to OptInterface.applyToExp, only that doesn't mutate the ast *)
  fun mapExprs func {ast,
		     tidtab,
		     errorCount,
		     warningCount,
		     auxiliaryInfo as {aidtab, implicits, env}} =
  (let 
      fun instrExpr (exp as (Ast.EXPR (cexp,exp_aid,exp_loc))) =
	 func	(* apply func here *)
	 (case cexp
	    of Ast.IntConst li => exp
	     | Ast.RealConst r => exp
	     | Ast.StringConst s => exp
	     | Ast.Call (fexp,exps) =>
		Ast.EXPR (Ast.Call (instrExpr fexp, List.map instrExpr exps)
			 ,exp_aid,exp_loc)
	     | Ast.QuestionColon (e1,e2,e3) =>
		Ast.EXPR (Ast.QuestionColon (instrExpr e1, instrExpr e2, instrExpr e3)
			 ,exp_aid,exp_loc)
	     | Ast.Assign (e1,e2) =>
		Ast.EXPR (Ast.Assign (instrExpr e1, instrExpr e2)
			 ,exp_aid,exp_loc)
	     | Ast.Comma (e1,e2) =>
		Ast.EXPR (Ast.Comma (instrExpr e1, instrExpr e2)
			 ,exp_aid,exp_loc)
	     | Ast.Sub (e1,e2) =>
		Ast.EXPR (Ast.Sub (instrExpr e1, instrExpr e2)
			 ,exp_aid,exp_loc)
	     | Ast.Member (e,mem) =>
		Ast.EXPR (Ast.Member (instrExpr e, mem)
			 ,exp_aid,exp_loc)
	     | Ast.Arrow (e,mem) =>
		Ast.EXPR (Ast.Arrow (instrExpr e, mem)
			 ,exp_aid,exp_loc)
	     | Ast.Deref e =>
		Ast.EXPR (Ast.Deref (instrExpr e)
			 ,exp_aid,exp_loc)
	     | Ast.AddrOf e =>
		Ast.EXPR (Ast.AddrOf (instrExpr e)
			 ,exp_aid,exp_loc)
	     | Ast.Binop (binop,e1,e2) =>
		Ast.EXPR (Ast.Binop (binop, instrExpr e1, instrExpr e2)
			 ,exp_aid,exp_loc)
	     | Ast.Unop (unop,e) =>
		Ast.EXPR (Ast.Unop (unop, instrExpr e)
			 ,exp_aid,exp_loc)
	     | Ast.SizeOf ty => exp
	     | Ast.Cast (ctype,e) =>
		Ast.EXPR (Ast.Cast (ctype, instrExpr e)
			 ,exp_aid,exp_loc)
	     | Ast.Id pid => exp
	     | Ast.EnumId (pid,li) => exp
	     | Ast.ExprExt ee => exp
	     | Ast.ErrorExpr => exp
	 )

      fun instrInitExpr iexpr =
      (case iexpr of Ast.Simple exp => Ast.Simple (instrExpr exp)
		   | Ast.Aggregate iexps => Ast.Aggregate (List.map instrInitExpr iexps)
      )

      fun instrDecl decl =
      ( case decl
	  of Ast.TypeDecl _ => decl
	   | Ast.VarDecl (id, initExprOp) =>
	     ( case initExprOp
		 of SOME iexp => Ast.VarDecl (id, SOME (instrInitExpr iexp))
		  | NONE => decl
	     )
      )

      fun instrStmt (outer_stmt as (Ast.STMT(coreStmt,stmt_aid,stmt_loc))) =
      (case coreStmt
	 of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => Ast.STMT (Ast.Expr (SOME (instrExpr exp)),stmt_aid,stmt_loc)
		 | NONE => outer_stmt
	    )

	  | Ast.Compound (decls, stmts) =>
		Ast.STMT(Ast.Compound (List.map instrDecl decls, List.map instrStmt stmts),stmt_aid,stmt_loc)

	  | Ast.While (exp,stmt) =>
		Ast.STMT(Ast.While ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)

	  | Ast.Do (exp,stmt) => 
		Ast.STMT(Ast.Do ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)

	  | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1op' = case e1op of SOME exp1 => SOME (instrExpr exp1)
					   | NONE => NONE
		    val e2op' = case e2op of SOME exp2 => SOME (instrExpr exp2)
					   | NONE => NONE
		    val e3op' = case e3op of SOME exp3 => SOME (instrExpr exp3)
					   | NONE => NONE
		    val stmt' = instrStmt stmt
		in
		    Ast.STMT(Ast.For (e1op',e2op',e3op',stmt'),stmt_aid,stmt_loc)
		end

	  | Ast.Labeled (label,stmt) =>
		Ast.STMT(Ast.Labeled (label,(instrStmt stmt)),stmt_aid,stmt_loc)
	  | Ast.CaseLabel (li,exp,stmt) => 
		Ast.STMT(Ast.CaseLabel (li,exp,(instrStmt stmt)),stmt_aid,stmt_loc)
	  | Ast.DefaultLabel stmt =>
		Ast.STMT(Ast.DefaultLabel (instrStmt stmt),stmt_aid,stmt_loc)
	  | Ast.Goto label => outer_stmt
	  | Ast.Break => outer_stmt
	  | Ast.Continue => outer_stmt
	  | Ast.Return expOp => 
		(case expOp
		   of NONE => outer_stmt
		    | SOME exp => Ast.STMT(Ast.Return (SOME (instrExpr exp)),stmt_aid,stmt_loc)
		)
	  | Ast.IfThen (exp,stmt) => 
		Ast.STMT(Ast.IfThen ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)
	  | Ast.IfThenElse (exp,stmt1,stmt2) => 
		Ast.STMT(Ast.IfThenElse ((instrExpr exp),(instrStmt stmt1),(instrStmt stmt2)),
			 stmt_aid,stmt_loc)
	  | Ast.Switch (exp,stmt) =>
		Ast.STMT(Ast.Switch ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)
	  | Ast.StatExt se => outer_stmt
	  | Ast.ErrorStmt => outer_stmt
      )

      fun instrExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
      (case coreEdecl
	 of Ast.ExternalDecl (Ast.TypeDecl _) => edecl
	  | Ast.ExternalDecl (Ast.VarDecl (id,initExprOp)) => edecl
	  | Ast.FunctionDef (id,ids,stmt) =>
		Ast.DECL (Ast.FunctionDef (id,ids,instrStmt stmt),edecl_aid,edecl_loc)
	  | Ast.ExternalDeclExt _ => edecl
      )

  in {	ast = List.map instrExternalDecl ast,
	tidtab = tidtab,
	errorCount = errorCount,
	warningCount = warningCount,
	auxiliaryInfo = auxiliaryInfo }
  end
  )

  val num_cleared = ref 0
  val num_pctn = ref 0

  fun checkPrintfPctN (printf_id,fprintf_id) (exp as (Ast.EXPR (cexp,exp_aid,exp_loc))) =
     (case cexp
	of Ast.Call (fexp as (Ast.EXPR (fcexp,_,_)),exps) =>
	   let val fexp' =
		   (case fexp
		      of (Ast.EXPR (Ast.Id fnid,fn_aid,fn_loc)) =>
			 let fun PctNsafe _ nil = false
			       | PctNsafe 0 ((Ast.EXPR (fmt_cexp,_,_))::_) =
				 (case fmt_cexp
				    of Ast.StringConst s =>
					let fun isNotPct c = (c <> #"%")
					    fun isNotConvChar c = not (Char.contains "cdeEfgGinopsuwxX%" c)
					    fun traverseSubstring ss =
						let val pct_ss = Substring.dropl isNotPct ss
						in  if Substring.isEmpty pct_ss then true       (* no %n found *)
						    else let val cc_ss = Substring.dropl isNotConvChar (Substring.triml 1 pct_ss)
							 in  case (Substring.first cc_ss)
							       of SOME #"n" => false    (* found potential %n *)
								| _ => traverseSubstring (Substring.triml 1 cc_ss)
							 end
						end
					in  traverseSubstring (Substring.all s)  end
				     | _ => false
				 )
			       | PctNsafe argno (_::tail) = PctNsafe (argno-1) tail
			 in (case (Symbol.name (#name fnid))
			       of "_typecheck_pctn_printf" => if PctNsafe 0 exps
							      then ( num_cleared := (!num_cleared) + 1
								   ; Ast.EXPR (Ast.Id printf_id,fn_aid,fn_loc) )
							      else ( num_pctn := (!num_pctn) + 1
								   ; fexp )
				| "_typecheck_pctn_fprintf" => if PctNsafe 1 exps
							       then ( num_cleared := (!num_cleared) + 1
								    ; Ast.EXPR (Ast.Id fprintf_id,fn_aid,fn_loc) )
							       else ( num_pctn := (!num_pctn) + 1
								    ; fexp )
				| _ => fexp
			 ) end
		       | _ => fexp
		   )
	   in Ast.EXPR (Ast.Call (fexp', exps),exp_aid,exp_loc) end
	 | _ => exp
     )

  fun handlePrintfPctN (bundle as {auxiliaryInfo as {aidtab, implicits, env},...}) =
  (
    if !Flags.printfPctN
    then let val printf_id_op = case State.ST.find (env, Symbol.func "printf")
				  of SOME (Bindings.ID id) => SOME id | _ => NONE
	     val fprintf_id_op = case State.ST.find (env, Symbol.func "fprintf")
				   of SOME (Bindings.ID id) => SOME id | _ => NONE
	 in  case (printf_id_op,fprintf_id_op)
	       of (SOME printf_id, SOME fprintf_id) =>
		  let val _ = (num_cleared := 0)
		      val _ = (num_pctn := 0)

		      val bundle' = mapExprs (checkPrintfPctN (printf_id,fprintf_id)) bundle

		      val _ = print ("Printf %n-check: eliminated " ^ (Int.toString (!num_cleared))
					^ " remaining " ^ (Int.toString (!num_pctn)) ^ "\n")
		  in  bundle'  end
		| _ => ( print "WARNING(handlePrintfPctN): id for printf/fprintf not found, skipping...\n"
		       ; bundle )
	 end
    else bundle

  ) (* end fun handlePrintfPctN (bundle) *)

  fun fixLibFnRetTys (bundle as {tidtab, auxiliaryInfo as {aidtab, implicits, env},...}) =
  (let
      val changelist = ref nil
      fun count_update (fnname,cty) =
	  let fun update nil (fnname,cty) = [(fnname,cty,1)]
		| update ((id',ty',ct)::tail) (fnname,cty) =
		  if (id' = fnname) andalso TypeUtil.equalType tidtab (ty',cty)
		  then (id',ty',ct+1)::tail
		  else (id',ty',ct)::(update tail (fnname,cty))
	  in  (changelist := (update (!changelist) (fnname,cty)))  end

      fun checkForPtr (fnname,aid) =
	 (case Aidtab.find (aidtab,aid)
	    of SOME cty => if TypeUtil.isPointer tidtab cty
			   then ()
			   else ( count_update (fnname,cty)
				; Aidtab.insert (aidtab, aid, Ast.Pointer Ast.Void))
	     | NONE => (print ("ERROR(fixLibFnRetTys): no aidtab entry for aid " ^ (Aid.toString aid) ^ "\n");
			Aidtab.insert (aidtab, aid, Ast.Pointer Ast.Void))
	 )

      fun checkLibFnRetTy (exp as (Ast.EXPR (cexp,exp_aid,_))) =
	 (case cexp
	    of Ast.Call (Ast.EXPR (Ast.Id fnid,_,_), exps) =>
	       let val fnname = (Symbol.name (#name fnid))
	       in
	       (case fnname
		  of "_typecheck_fgets" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_gets" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_malloc" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_calloc" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_memalign" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_realloc" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_valloc" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_memcpy" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_memmove" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strcpy" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strncpy" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strcat" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strncat" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_memset" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strerror" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_memccpy" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strsignal" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_strdup" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_nl_langinfo" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_ctime" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_localtime" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_gmtime" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_asctime" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_getpass" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_sbrk" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_gethostbyname" => checkForPtr (fnname,exp_aid)
		   | "_typecheck_gethostbyaddr" => checkForPtr (fnname,exp_aid)
		   | _ => ()
		)end
	     | _ => ()
	 ; exp)

       val bundle' = mapExprs checkLibFnRetTy bundle

       val _ = List.app (fn (fnname,retty,count) =>
			print ("WARNING(fixLibFnRetTy): return type for " ^ fnname ^ " changed from "
				^ (PPLib.ppToString (PPAst.ppCtype () tidtab) retty) ^ " "
				^ (Int.toString count) ^ " times\n")
			) (!changelist)
   in bundle' end
  ) (* end fun fixLibFnRetTys (bundle as {tidtab, auxiliaryInfo as {aidtab, implicits, env},...}) *)


end (* Structure PreProcess *)
