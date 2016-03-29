(* Copyright (c) 1998 by Lucent Technologies *)

(* Pending changes and future work:
   UTU - lots of useful type utilities are defined in type-util-fn.sml.
         We already have access to them here (TU).  Should use them
         instead of reinventing the wheel (e.g. in isConversion,
         getTypeString, and reducing typedefs).

   RPM - Renaming to _prog_main.  Currently only renaming the definition
         of main.  To be able to rename all uses of main (e.g. calls and
         identifier copying), we'll need to create a prototype of _prog_main
         at the top of the file but we may not know the type of main() until
         we get to the end of the file.  Put off the hacks.

   OFS - Prototype function declarations result in Object symbols instead of
         Func.  We lose some information this way.  Should mention to Lucent.
         After a quick test it looks like all VarDecl's are result in insertion
         of Object symbols into the symbol tables.
         Note that this is not an issue for code that looks up functions by
         using the symbol table's find function (or anything that calls it,
         e.g. the state's lookSymGlobal) because the Func and Object symbols
         are placed in the same namespace and are considered equal if their
         strings are (see symbol.sml).

   OBS - Rewriting of instrExternalDecl(s), instrFunctionDef(s), and instrStmt(s)
         have rendered the following functions obsolete:
          - processDecl
          - insertAfterDecl(s)
         there may be others.

   NEEDED - we've been lazy about making sure we have the correct adornment
	types for expressions, and so have been passing Ast.Void to virtually
	all wrapEXPR calls. However, I think we should, by stages (when it's
	convenient), fix them; often the type of the expression is already
	available, and it's simply a matter of figuring out what it is.
	Anyhow, I've started doing some (very little) of this; but these
	are nonessential. There are a few cases (cast, function return),
	however, where it is vital that the types are correct. These I've
	marked with "(*NEEDED!*)" so that in case we find some reason to
	do away with the types, these cases would need to be looked at
	carefully.

  INITONDECL - marks changes made to initialize variables upon declaration.

*)

structure InstrAst : INSTR_AST = struct 

  structure PTA = ParseToAst

  structure S = State
  structure IS = InstrState
  structure B = Bindings
  structure Sym = Symbol
  structure PPL = PPLib
  structure PPA = PPAst

  structure TU = TypeUtil

  structure Pid = Pid
  structure Tid = Tid
  structure Aid = Aid
  structure TT = Tidtab
  structure AT = Aidtab

  structure OPT = OptInterface

  (* If defined to be true, diagnostic output will be produced
   at the entry into each major function. *)
  val trace = ref false

  fun warning f msg = (print f; print ": "; print msg)

  (* idset data structure: used by collectLocalVarInfo *)
  structure idset = ListSetFn ( struct
				  type ord_key = Ast.id
				  val compare = fn (id1:Ast.id,id2:Ast.id) => Pid.compare (#uid id1, #uid id2)
				end)

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
  val stateInfo = PTA.progToState bundle

  val globalState as {uidTables={ttab,atab,implicits},...} =
      S.initGlobal(stateInfo, errorState)

  val localState = S.initLocal ()

  val stateFuns = S.stateFuns(globalState, localState)

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

  val localInstrState = IS.initLocal ()

  val instrStateFuns = IS.instrStateFuns(localInstrState, errorState)

  val {tmpVarsFuns = {addTmpVar, getTmpVars, pushTmpVarList, popTmpVarList},
       undeclFunsFuns = {pushUndeclFuns, resetUndeclFuns},
       staticFlagsFuns = {pushStatic, popStatic, getStatic},
       externVarsFuns = {pushExterns, resetExterns},...} = instrStateFuns

  fun ppId id = PPL.ppToStrm PPA.ppId TextIO.stdOut id

  fun ppToStdOut pp v =
      ( PPL.ppToStrm (pp () ttab) TextIO.stdOut v
      ; print "\n"
      ) 

  (* String output version of the above. *)
  fun ppToString pp v = PPL.ppToString (pp () ttab) v

  (* lookup type of expr in aidtab *)
  fun lookupExprType(Ast.EXPR(_, adorn, _)) =
      case lookAid adorn
        of SOME ct => ct
	 | NONE => (bug ("lookupExprType: no type for expression; aid = "
			 ^ Int.toString adorn);
		    Ast.Void)

  val expToAO = OPT.expToAbsObjects (lookupExprType, ttab, {deref=true})

  (* Strip out all declarations from typecheck.h by searching for
   the special marker (int _tc_h_end;).  This allows us to replace
   function declarations with macros while getting typechecking and
   avoid instrumenting our external declarations. *)
  val (no_tc_edecls,tc_incl_aid) = OPT.stripTcInclude edecls

  (* optimization type-safety level, alias lookup function, flow-sensitive results *)
  val { ts_lookup_fn
      , ts_lookup_children_fn
      , freearg_lookup_fn
      , noins_libfn_lookup
      , may_be_uninit_aid
      , may_be_uninit_pid
      , vp_may_be_uninit_ao
      , vp_vuln_loc_ao
      , vp_vuln_enclosing_loc_ao
      , vp_vuln_deref_ao
      , vp_redundant_aid
      , vt_redundant_aid
      , array_inbounds_aid
      } : OPT.lookup_bundle =
	if !Flags.ts_file <> ""
	then let val shash = OPT.buildStringMap (bundle,filestem ^ ".c") (* assume .c extension *)
		 val (lookup_bundle,aliasf) = OPT.readTSlevels (shash, !Flags.ts_file, filestem, tc_incl_aid)
	     in  if !Flags.mayBeUninit
		 then let val ((is_mbu_pid,is_mbu_aid),is_red_aid,is_mbi_pid)
				= FlowSensitive.flowSensitiveAnalyses (bundle, expToAO, aliasf)
		      in { ts_lookup_fn = (#ts_lookup_fn lookup_bundle)
			 , ts_lookup_children_fn = (#ts_lookup_children_fn lookup_bundle)
			 , freearg_lookup_fn = (#freearg_lookup_fn lookup_bundle)
			 , noins_libfn_lookup = (#noins_libfn_lookup lookup_bundle)
			 , may_be_uninit_aid = (is_mbu_aid o #1)
			 , may_be_uninit_pid = (is_mbu_pid)
			 , vp_may_be_uninit_ao = (#vp_may_be_uninit_ao lookup_bundle)
			 , vp_vuln_loc_ao = (#vp_vuln_loc_ao lookup_bundle)
			 , vp_vuln_enclosing_loc_ao = (#vp_vuln_enclosing_loc_ao lookup_bundle)
			 , vp_vuln_deref_ao = (#vp_vuln_deref_ao lookup_bundle)
			 , vp_redundant_aid = (#vp_redundant_aid lookup_bundle)
			 , vt_redundant_aid = (#vt_redundant_aid lookup_bundle)
			 , array_inbounds_aid = (#array_inbounds_aid lookup_bundle)
			 }
		      end
		 else lookup_bundle
	     end
	else let val (tslf,tslcf) = case !Flags.optiType
				      of "locaddrof" => OPT.addrTakenAnalysis bundle
				       | _ => (OPT.simple_lookup, OPT.simple_lookup)
	     in { ts_lookup_fn = tslf
		, ts_lookup_children_fn = tslcf
		, freearg_lookup_fn = (fn _ => Rtc.ES_ALL)
		, noins_libfn_lookup = (fn _ => Rtc.False)
		, may_be_uninit_aid = Rtc.True
		, may_be_uninit_pid = Rtc.True
		, vp_may_be_uninit_ao = Rtc.True
		, vp_vuln_loc_ao = Rtc.True
		, vp_vuln_enclosing_loc_ao = Rtc.True
		, vp_vuln_deref_ao = Rtc.True
		, vp_redundant_aid = fn _ => Rtc.False
		, vt_redundant_aid = fn _ => Rtc.False
		, array_inbounds_aid = Rtc.False
		}
	     end

  (* These are the "accessor" functions to check the classifications,
     and encapsulates the main type-safety-category lookup functions
     ts_lookup_fn and ts_lookup_children_fn.
     The extra layer allows us to adjust the behavior for vuln-ptr(w) mode.
  *)
  fun aos_le_exposed aos =
	if !Flags.vuln
	then List.exists vp_vuln_loc_ao aos
	else Rtc.ts_le (ts_lookup_fn aos, Rtc.TSC_EXPOSED)
  fun aos_child_le_exposed aos =
	if !Flags.vuln
	then List.exists vp_vuln_enclosing_loc_ao aos
	else Rtc.ts_le (ts_lookup_children_fn aos, Rtc.TSC_EXPOSED)
  fun aos_eq_exposed aos =
	if !Flags.vuln
	then List.exists vp_vuln_loc_ao aos
	else ts_lookup_fn aos = Rtc.TSC_EXPOSED
  fun aos_child_eq_exposed aos =
	if !Flags.vuln
	then List.exists vp_vuln_enclosing_loc_ao aos
	else ts_lookup_children_fn aos = Rtc.TSC_EXPOSED
  fun aos_eq_safe aos =
	if !Flags.vuln
	then not (aos_le_exposed aos)
	else (ts_lookup_fn aos) = Rtc.TSC_SAFE
  fun aos_child_eq_safe aos =
	if !Flags.vuln
	then not (aos_child_le_exposed aos)
	else (ts_lookup_children_fn aos) = Rtc.TSC_SAFE
  fun aos_poss_invalid aos =
	if (!Flags.vuln)
	andalso (List.exists vp_vuln_deref_ao aos)
	then false
	else Rtc.ts_le (ts_lookup_fn aos, Rtc.TSC_POSS_INVALID)
  (* used by RTC only *)
  fun aos_badly_typed aos = Rtc.ts_le (ts_lookup_fn aos, Rtc.TSC_BADLY_TYPED)
  fun aos_child_badly_typed aos = Rtc.ts_le (ts_lookup_children_fn aos, Rtc.TSC_BADLY_TYPED)
  fun aos_influential aos = Rtc.ts_le (ts_lookup_fn aos, Rtc.TSC_INFLUENTIAL)
  
(***********************************************************************)
(* DEBUG CODE: to dump each lookup performed *)
(*
  val (may_be_uninit_pid, may_be_uninit_aid) =
      let fun mm_to_str mm = ( case mm of Rtc.MBU_Assign => "Assign"
				(*	| Rtc.MBU_Call => "Call"    *)
					| Rtc.MBU_Verify => "Verify"
					| Rtc.MBU_Decl => "Decl"
					| Rtc.MBU_Arg => "Arg"
					| Rtc.MBU_Return => "Return"
				)
	  fun mbup pid =
	      let val ret = may_be_uninit_pid pid
		  val _ = print ("MBU pid(" ^(Int.toString pid)^ ") = " ^(Bool.toString ret)^ "\n")
	      in  ret  end
	  fun mbua (aid,mm) =
	      let val ret = may_be_uninit_aid (aid,mm)
		  val _ = print ("MBU aid(" ^(Int.toString aid)^ "," ^(mm_to_str mm)^ ") = " ^(Bool.toString ret)^ "\n")
	      in  ret  end
	  
      in  (mbup,mbua) end
*)
(***********************************************************************)

  (* type utils *)
  val stdInt = TU.stdInt
  val getCoreType = TU.getCoreType ttab
  val isStructOrUnion = TU.isStructOrUnion ttab
  val isArray = TU.isArray ttab
  val isPointer = TU.isPointer ttab
  val isIntegral = TU.isIntegral ttab
  val cnvFunctionToPointer2Function = TU.cnvFunctionToPointer2Function ttab
  (* obsolete?: using isNonPointerFunction instead *)
  (* Note: isFunctionPrototype returns true only if argument types are provided;
	i.e. it returns false for int foo(); and true for int foo(void); *)
  val isFunctionPrototype = TU.isFunctionPrototype ttab
  val isNonPointerFunction = TU.isNonPointerFunction ttab
  val hasKnownStorageSize = TU.hasKnownStorageSize ttab

  fun isFunctionOrFunctionPointer otype =
      case getCoreType otype
	of Ast.Pointer ty => isNonPointerFunction ty
	 | Ast.Function _ => true
	 | _ => false

  (* duplicated from sizeof.sml *)
  fun equalMember({uid=uid1,...}: Ast.member, {uid=uid2,...}: Ast.member) =
      Pid.equal(uid1,uid2)

  fun isBitField (ctype, mem) =
      case (getCoreType ctype)
	of Ast.StructRef tid =>
	   ( case lookTid tid
	       of SOME {ntype = SOME (B.Struct (tid, fields)),...} =>
		  let fun checkMem (nil) = false
			| checkMem ((_,SOME mem',sizeOp)::tail) =
				if equalMember(mem,mem') then (isSome sizeOp)
							 else checkMem(tail)
			| checkMem (_::tail) = checkMem(tail)
		  in  checkMem(fields)
		  end
		| _ => (warning "isBitField" ("encountered non-struct tid: " ^ Tid.toString tid ^ "\n");
			false)
	   )

	 | Ast.UnionRef tid => false
	   (* Currently, ckit doesn't support sized union members, so union members
	      are automatically non-bitfields. *)

	 | _ => (warning "isBitField" "Type is neither struct nor union\n";
		 print "ctype: "; ppToStdOut PPA.ppCtype ctype; false)

  fun lookupEnum (ty,mem as {name,...}) = case (TU.lookupEnum ttab (ty,mem)) of
      SOME num => num
    | NONE => (warning "lookupEnum" ("enum const " ^ Sym.name name ^ " not found\n");
	       LargeInt.fromInt 0)

  val equalType = TU.equalType ttab

  (* version that doesn't waste type digging through tidtabs;
     plus, TypeUtil.isPointer seems to return true for arrays
     and functions also! *)
  fun isImmediatePointer (Ast.Pointer _) = true
    | isImmediatePointer _ = false

  (* Can't just introduce id of type ty: may not be legal to do assignment (e.g. for arrays).
     So, first convert arrays to pointers, functions to pointers, and eliminate qualifiers.
     Potential problem: elimination of volatile qualifiers on temporary variables?
     From SimplifyAssignOps. *)
  fun niceTy ty =
    let val cty = getCoreType ty
    in (case cty of Ast.Array (_, elty) => Ast.Pointer elty
		  | Ast.Function _ => Ast.Pointer ty
		  | _ => cty)
    end

  (* If type is an array of unknown size, return the type of a single element;
     else return none to signify that the type is complete *)
  fun deArrayify ty =
    (case getCoreType ty
       of Ast.Array (NONE, elty) => SOME elty
        | _ => NONE)

  fun derefOneLevel ty =
      case getCoreType ty
	of Ast.Pointer rty => rty
	 | Ast.Array (_,elty) => elty
	 | _ => (warning "derefOneLevel" ("Not a pointer: " ^ (ppToString PPA.ppCtype ty) ^ "\n"); ty)

  (* Returns ctype option *)
  (* Note: Formerly expOpAidType *)
  fun lookupExprImplicitType(Ast.EXPR(_,adorn,_)) = AT.find (implicits,adorn)

  fun wrapSTMT(coreStmt: Ast.coreStatement) : Ast.statement =
      Ast.STMT (coreStmt, Aid.new (), getLoc())

  fun wrapDECL(coreExtDecl: Ast.coreExternalDecl) : Ast.externalDecl =
      Ast.DECL(coreExtDecl, Aid.new (), getLoc())

  fun wrapEXPR (ty, coreExpr) =
    let val ty = cnvFunctionToPointer2Function ty
      (* all expressions of type Function are promoted to Pointer(Function)
       * exceptions (&, sizeof) are handled in unops *)
      (* Strictly speaking, arrays should also be converted to pointers here;
         however code using array expressions deal with the array case directly (e.g. Sub, Deref);
         Caution: if we were to make this change, we still need to know it was an array!
         Where is the right place to do this conversion? *)
      val adorn = bindAid ty
    in Ast.EXPR (coreExpr, adorn, getLoc())
    end

  val bogusTid = Tid.new()
  val bogusPid = Pid.new()
  fun bogusMember sym =
      {name = sym, uid = Pid.new(), location = getLoc(),
       ctype = Ast.Error, kind = Ast.STRUCTmem}  (* AAL: is this kind ok? *)

  (* Returns true iff the cast (ctype) exp involves a conversion
   (as opposed to a no-op cast).  For now this function does not
   take into account information loss due to overflow/underflow,
   loss of const and other qualifiers, etc.
   Note: float<->double and array->pointer are considered conversions. *)
  fun isConversion (ctype, exp_type) =
      (* Note that ctype cannot be an array since it is a cast type. *)
      not (((isIntegral ctype) orelse (isPointer ctype))
	    andalso
	    ((isIntegral exp_type) orelse ((isPointer exp_type)
					   andalso not (isArray exp_type))))

  (* Given string, look up type in global symtab and return associated ctype *)
  fun lookupTypeDefType typeName =
      case lookSymGlobal (Sym.typedef typeName) of
	   SOME (B.TYPEDEF{ctype=ty,...}) => ty
	 | _ =>
	      (warning "lookupTypeDefType" (typeName ^ " not declared\n\t" ^
					    "or symbol is not a typedef, returning void\n");
	       Ast.Void)

  (* Cast if exp's type is different from type *)
  fun makeCastExpr (exp,cast_type) =
      if equalType(lookupExprType exp, cast_type)
      then exp
      else let
	       (* resolve typedefs, remove qualifiers, convert arrays to pointer *)
	       val cast_type' = niceTy cast_type
	   in
	       wrapEXPR ((*NEEDED!*)cast_type', Ast.Cast (cast_type', exp))
	   end

  (* If exp has an implicit cast (to a different type), make the cast explicit. *)
  fun makeCastExplicit exp =
      case lookupExprImplicitType exp
        of SOME cast_type => makeCastExpr (exp,cast_type)
         | NONE => exp

  (* if ID has "register" storage class, replace with "default" storage class *)
  fun de_register(id as {name=nn,uid=uu,location=ll,ctype=ct,stClass=sc,status=ss,global=gg,kind=kk}) =
      if sc = Ast.REGISTER then {stClass=Ast.DEFAULT,
				 name=nn,uid=uu,location=ll,ctype=ct,status=ss,global=gg,kind=kk}
			   else id

  (* Create an expression for offsetof(struct S, f) = (size_t)(&((struct S * )0)->f) *)
  fun newOffsetofExpr (str_ty, member) =
      let val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
	  val strptr_ty = Ast.Pointer str_ty
	  val cast1_exp = wrapEXPR(Ast.Void, Ast.Cast (strptr_ty,zero_exp))
	  val arrow_exp = wrapEXPR(Ast.Void, Ast.Arrow (cast1_exp,member))
	  val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf arrow_exp)
	  val sizet_ty = lookupTypeDefType "size_t"
      in
	  wrapEXPR(Ast.Void, Ast.Cast (sizet_ty,addrof_exp))
      end

  (* returns a core expr for a[i].f *)
  fun newAsubIdotFcexpr (a : Ast.id, i : LargeInt.int, f : Ast.member) =
      let
	  val a_exp = wrapEXPR(Ast.Void, Ast.Id a)
	  val i_exp = wrapEXPR(Ast.Void, Ast.IntConst i)
	  val a_i_exp = wrapEXPR(Ast.Void, Ast.Sub (a_exp, i_exp))
	  val a_i_f_cexp = Ast.Member (a_i_exp, f)
      in
	  a_i_f_cexp
      end

  (* Given variable name suffix, ctype, storage class, and init expression, create
   a variable, insert into local symtab, add it to list of tmps to be processed at
   the enclosing Compound stmt, and return its id. *)
  fun addNewVariable (varName,{isTemp:bool},varTy,stClass) =
      (* Construct an id for the variable. *)
      let
	  val pid = Pid.new()
	  val varSym = if isTemp
		       then Sym.object ("tmp" ^ (Pid.toString pid) ^ varName)
		       else Sym.object varName

	  (* NOTE: build-ast.sml suggests that status if DEFINED if initialized,
		   and DECLARED otherwise. I don't think this distinction is
		   useful (or correct). Since we don't really use status anyway,
		   and for simplicity, status is set to DEFINED. *)
	  val id = {name = varSym, uid = pid, location = getLoc(), ctype = varTy,
		    stClass = stClass, status = Ast.DEFINED, global = false, kind = Ast.NONFUN}

	  (* Insert the id into the local symbol table. *)
	  val binding = B.ID id
	  val _ = bindSym(varSym, binding) 
      in
	  id
      end

  (* Creates a new temporary tagptr (const void * ), inserts it into the local symbol table, adds it to
   list of tmps to be processed at the enclosing Compound stmt, and returns its id. *)
  fun addTmpTagPtr () =
      let val const_void_ty = Ast.Pointer (Ast.Qual (Ast.CONST, Ast.Void))
	  val id = addNewVariable ("_tagptr", {isTemp=true}, const_void_ty, Ast.DEFAULT)
	  (* Save the id, so we can process it when get back up to Compound stmt. *)
	  val _ = addTmpVar id
      in 
	  id
      end

  (* Creates a new temporary _addr_and_size_t array of input size.  Such
   arrays are mainly for use in function call instrumentation.  The array
   is inserted into the local symbol table, and added to list of tmps to
   be processed at the enclosing Compound stmt.  Array's id is returned. *)
  fun addTmpAddrSizeArray nelem =
      let val addr_size_ty = lookupTypeDefType "_addr_and_size_t"
	  val expr = wrapEXPR(stdInt, Ast.IntConst nelem)
	  val arr_ty = Ast.Array (SOME (nelem,expr), addr_size_ty)
	  val id = addNewVariable ("_args",{isTemp=true},arr_ty,Ast.DEFAULT)
	  (* Save the id, so we can process it when get back up to Compound stmt. *)
	  val _ = addTmpVar id
      in
	  id
      end

  (* Creates a new temporary variable.
     If pointer is true, create a temp of type pointer to exp's type.
     If pointer is false, create a temp of exp's type UNLESS exp's type is an array or
     function, in which case, it is converted to a pointer.
     The new temp is inserted into the local symbol table, and added to list of tmps to
     be processed at the enclosing Compound stmt. The new temp's id is returned. *)
  (* Note: tmp_val variables cannot be constant (can't assign to them): niceTy takes care of that *)
  fun addTmpValVarByType cty =
      let val tmp_ty = niceTy cty
 	  val id = addNewVariable ("_val",{isTemp=true},tmp_ty,Ast.DEFAULT)
	  (* Save the id, so we can process it when get back up to Compound stmt. *)
	  val _ = addTmpVar id
      in
	  id
      end

  fun addTmpValVar (exp, {pointer:bool}) =
      let val exp_ty = lookupExprType exp
	  (* pointer=true means we want a tmp of type pointer to exp_ty *)
	  (* pointer=false means we want exp_ty, but if it is Array or Function, we want a pointer. *)
      in
	  addTmpValVarByType (if pointer then Ast.Pointer exp_ty else exp_ty)
      end

  fun getGlobalObj name =
      let val varSym = Sym.object (name)
      in
	  case lookSymGlobal varSym
	    of SOME (B.ID id) => id
	     | _ => let val id = {name = varSym, uid = bogusPid, location = getLoc(),
				  ctype = Ast.Error, stClass = Ast.EXTERN,
				  status = Ast.IMPLICIT, global = true, kind = Ast.NONFUN}
			val binding = B.ID id
			val _ = warning "getGlobalObj"
					("object " ^ name ^ " not declared\n\t" ^
					 "or symbol is not an object, created a bogus id.\n")
		       (* Insert the id into the global  symbol table to prevent further msgs. *)
			val _ = bindSymGlobal(varSym, binding)
		    in
			id
		    end
      end

  fun getLocalObj name =
      let val varSym = Sym.object (name)
      in
	  case lookSym varSym
	    of SOME (B.ID id) => id
	     | _ => let val id = {name = varSym, uid = bogusPid, location = getLoc(),
				  ctype = Ast.Error, stClass = Ast.AUTO,
				  status = Ast.IMPLICIT, global = false, kind = Ast.NONFUN}
			val binding = B.ID id
			val _ = warning "getLocalObj"
					("object " ^ name ^ " not declared\n\t" ^
					 "or symbol is not an object, created a bogus id.\n");
		       (* Insert the id into the global  symbol table to prevent further msgs. *)
			val _ = bindSym(varSym, binding)
		    in
			id
		    end
      end

  (* Given ctype which is a struct (or union), return the member
      (type Ast.member) with the given name.
     NOTE: the member is looked up only in the global symbol table,
      i.e. the struct (or union) must be globally defined!
     This function is now named "getStructMember" simply because we
      only call it on structures. The function works equally well with
      unions; rename the function if we ever use it to look up a member
      of a union
   *)
  fun getStructMember (ctype, name) =
      case isStructOrUnion ctype of
	  SOME tid =>
	      let val memSym = Sym.member (tid, name)
	      in
		  case lookSymGlobal memSym
		    of SOME (B.MEMBER mem) => mem
		     | _ => let val mem = bogusMember memSym
				val binding = B.MEMBER mem

				val _ = (warning "getStructMember"
						 ("member " ^ name ^ " not declared in ctype\n\t" ^
						  "or symbol is not a member, created a bogus member.\n\t");
					 print "ctype: "; ppToStdOut PPA.ppCtype ctype)
				(* Insert the member into the global symbol table to prevent further msgs. *)
				val _ = bindSymGlobal(memSym, binding);
			    in
				mem
			    end
	      end
	| NONE =>
	      let val memSym = Sym.member(bogusTid,name)
		  val mem = bogusMember memSym
		  val binding = B.MEMBER mem
		  val _ = (warning "getStructMember"
				   ("ctype not an aggregate, created a bogus member.\n\t");
			   print "ctype: "; ppToStdOut PPA.ppCtype ctype)
		  (* Insert the member into the global symbol table to prevent further msgs. *)
		  val _ =  bindSymGlobal(memSym, binding);
	      in
		  mem
	      end

  fun getGlobalFun name =
      let val funSym = Sym.func (name)
      in
	  case lookSymGlobal funSym of
	      SOME (B.ID id) => id
	    | _ => 
		  (* if ANSI C then this should be an error... *)
		  let val id = {name = funSym, uid = bogusPid, location = getLoc(),
				ctype = Ast.Error, stClass = Ast.EXTERN,
				status = Ast.IMPLICIT, global = true,
				kind = Ast.FUNCTION{hasFunctionDef=false}}
		      val binding = B.ID id
		  in
		      (warning "getGlobalFun" ("function " ^ name ^ " not declared\n\t" ^
					       "or symbol is not a function, created a bogus id:\n\t");
		       (* Insert the id into the global symbol table to prevent further msgs. *)
		       bindSymGlobal(funSym, binding);
		       id)
		  end
      end

  (* Mutually recursive with getTidString. *)
  fun getTypeString ctype =
      case ctype
        of Ast.Void            => "void"
	 | Ast.Qual (_,ty)     => getTypeString ty
	 | Ast.Numeric (_,_,_, kind,_) =>
	      (case kind of
		   Ast.CHAR       => "char"
		 | Ast.SHORT      => "short"
		 | Ast.INT        => "int"
		 | Ast.LONG       => "long"
		 | Ast.LONGLONG   => "longlong"
		 | Ast.FLOAT      => "float"
		 | Ast.DOUBLE     => "double"
		 | Ast.LONGDOUBLE => "longdouble")
	 | Ast.Array _         => "aggregate"
	 | Ast.Pointer _       => "pointer"
	 | Ast.Function _      => "function"  (* temporary - should never happen. *)
	 | Ast.StructRef (tid) => "aggregate"
	 | Ast.UnionRef (tid)  => "aggregate"  (* AAL: Could we want to do smth else? *)
	 | Ast.EnumRef _       => "int"
	 | Ast.TypeRef (tid)   => getTidString tid
	 | _                   => "error"  (* Ellipsis should never occur here. *)

  (* Only called by getTypeString. *)
  and getTidString tid =
      case lookTid tid of
	  SOME{ntype=SOME(B.Typedef (_,ty)),...} => getTypeString ty
	(* Should never match anything below! *)
	| SOME{ntype=SOME(B.Struct _),...} => "aggregate"
	| SOME{ntype=SOME(B.Union _),...} => "aggregate"
	| SOME{ntype=SOME(B.Enum _),...} => "int"
	| _ => (warning "getTidString"
		("tid " ^ Tid.toString tid ^ " not found or invalid!\n");
		getTypeString Ast.Void)

  (* returns true if both are pointers, or if their coreTypes
     (de-qualified and de-typedefed) are equal *)
  (* current version: scalars handled correctly, but structs
     may be too strict? Really doesn't matter in contexts where
     this function is used. *)
  fun tcEqualType (t1,t2) =
      let val t1ts = getTypeString t1
	  val t2ts = getTypeString t2
      in  if t1ts = "aggregate"
	  then equalType (t1,t2)
	  else t1ts = t2ts
      end

  (* Converts an expression to a string *)
  fun exprToString exp =
      let val str = ppToString PPA.ppExpression exp
	  val strlen = String.size str
      in  if (strlen > 2)
	  andalso (String.sub(str,0) = #"\n")
	  andalso (String.sub(str,strlen - 1) = #"\n")
	  then String.substring (str, 1, strlen - 2)
	  else str
      end

  (* Count number of instrumentation calls inserted *)
  val count_tot_inst = ref 0
  val count_set_tag = ref 0
  val count_verify_ptr = ref 0
  val count_verify_ptr_elided = ref 0
  val count_vp_array_direct = ref 0
  val count_vp_array_typed = ref 0
  val count_vp_array_sub = ref 0
  val count_libfn = ref 0
  val count_libfn_elided = ref 0
  val count_malloc = ref 0
  val count_malloc_elided = ref 0
  val count_free = ref 0
  val count_free_partial = ref 0
  val count_free_elided = ref 0

  fun newInstCallExpr (fun_exp,arglist_exps) =
      let
	  val _ = (count_tot_inst := (!count_tot_inst) + 1)
	  val (fname,lineno,colno) =
	      case getLoc()
		of SourceMap.LOC{srcFile,beginLine,beginCol,...} =>
		    (srcFile,beginLine,beginCol)
		 | SourceMap.UNKNOWN => ("unknown",0,0)
	  val fname_exp = wrapEXPR(Ast.Void, Ast.StringConst fname)
	  val lineno_exp = wrapEXPR(Ast.Void, Ast.IntConst (LargeInt.fromInt lineno))
	  val colno_exp = wrapEXPR(Ast.Void, Ast.IntConst (LargeInt.fromInt colno))
      in
	  wrapEXPR(Ast.Void, Ast.Call (fun_exp,fname_exp::lineno_exp::colno_exp::arglist_exps))
      end

  (* Return an EXPR for ctype's enum constant. *)
  fun newCtypeEnumExpr ctype =
      let
	  val enum_ty = lookupTypeDefType "_ctype_t"
	  val enum_str = "_ctype_" ^ getTypeString ctype

	  val enum_const_cexp =
	      case lookSymGlobal (Sym.enumConst enum_str) of
		  SOME (B.MEMBER enum_mem) => let val enum_const = lookupEnum(enum_ty,enum_mem)
					      in Ast.EnumId (enum_mem,enum_const) end
		| SOME _ => (warning "newCtypeEnumExpr" ("wrong symbol returned for enum const "
							 ^ enum_str ^ "\n");
			     Ast.IntConst 0)
		| NONE => (warning "newCtypeEnumExpr" ("enum const " ^ enum_str ^ " not declared\n");
			   Ast.IntConst 0)
      in
	  wrapEXPR(Ast.Void, enum_const_cexp)
      end

  val setUninitTag = "_setUninitTag"
  val setInitTag = "_setInitTag"
  val clearTag = "_clearTag"

  (* fnname can be "_setUninitTag", "_extern_setUninitTag", "_setInitTag", "_clearTag" *)
  fun newSetTagExpr_id fnname (id as {ctype=id_type,location=id_loc,...}) =
      let val _ = pushLoc id_loc
	  (* Create EXPR for &id *)
	  val id_exp = wrapEXPR(Ast.Void, Ast.Id id)
	  val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf id_exp)
	  (* Create EXPR for _setUninitTag(&id,sizeof id) *)
	  val sut_id = getGlobalFun fnname
	  val sut_exp = wrapEXPR(Ast.Void, Ast.Id sut_id)
	  val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf id_type)
	  val sut_call_exp = newInstCallExpr (sut_exp,[addrof_exp,sizeof_exp])
      in
	  sut_call_exp before popLoc()
      end

  fun newSetTagExpr fnname (addr_exp,size_exp,loc) =
      let val _ = pushLoc loc
	  (* Create EXPR for _setUninitTag(&id,sizeof id) *)
	  val sut_id = getGlobalFun fnname
	  val sut_exp = wrapEXPR(Ast.Void, Ast.Id sut_id)
	  val sut_call_exp = newInstCallExpr (sut_exp,[addr_exp,size_exp])
      in
	  sut_call_exp before popLoc()
      end

  fun newSetStringTagExpr(exp, str) =
      let
	  val sst_id = getGlobalFun ("_setStringTag")
	  val sst_exp = wrapEXPR(Ast.Void, Ast.Id sst_id)
	  (* todo: compute strlen: must interpret str according to C rules? *)
	  val strlen_li = Int.toLarge (String.size str)
	  val strlen_exp = wrapEXPR(Ast.Void, Ast.IntConst strlen_li)
      in  newInstCallExpr(sst_exp,[exp,strlen_exp])
      end

  fun newProcessReturnExpr(scaf_start_id, scaf_end_id, agrf_start_id, agrf_end_id,
			   argAddrs_id, tagptr_cexp, sizeof_cexp) =
      let
	  val scaf_start_exp = wrapEXPR(Ast.Void, Ast.Id scaf_start_id)
	  val scaf_end_exp = wrapEXPR(Ast.Void, Ast.Id scaf_end_id)
	  val agrf_start_exp = wrapEXPR(Ast.Void, Ast.Id agrf_start_id)
	  val agrf_end_exp = wrapEXPR(Ast.Void, Ast.Id agrf_end_id)
	  val argAddrs_exp = wrapEXPR(Ast.Void, Ast.Id argAddrs_id)
	  val addr_argAddrs_exp = wrapEXPR(Ast.Void, Ast.AddrOf argAddrs_exp)
	  val tagptr_exp = wrapEXPR(Ast.Void, tagptr_cexp)
	  val sizeof_exp = wrapEXPR(Ast.Void, sizeof_cexp)

	  val pr_id = getGlobalFun "_processReturn"
	  val pr_exp = wrapEXPR(Ast.Void, Ast.Id pr_id)

	  val pr_call_exp = newInstCallExpr
		(pr_exp,[scaf_start_exp,scaf_end_exp,agrf_start_exp,agrf_end_exp,addr_argAddrs_exp,tagptr_exp,sizeof_exp])
      in
	  pr_call_exp
      end

  fun newVerifyTagCallExpr (outer_exp, addr_cexp, exp_type) =
      let
	  val cty_str = getTypeString exp_type
	  val vt_id = getGlobalFun ("_verifyTag_" ^ cty_str)
	  val vt_exp = wrapEXPR(Ast.Void, Ast.Id vt_id)
	  val oex_str = exprToString outer_exp
	  val oex_exp = wrapEXPR(Ast.Void, Ast.StringConst oex_str)
	  val addr_exp = wrapEXPR(Ast.Void, addr_cexp)
      in
	  newInstCallExpr (vt_exp,[oex_exp,addr_exp(*,ctype_exp*)])
      end

  (* Create new EXPR for a "_verifyPtr" call *)
  fun newVerifyPtrCallExpr (outer_exp, addr_cexp, exp_type) =
      let
	  val cty_str = getTypeString exp_type
	  val vp_id = getGlobalFun ("_verifyPtr_" ^ cty_str)
	  val vp_exp = wrapEXPR(Ast.Void, Ast.Id vp_id)
	  val oex_str = exprToString outer_exp
	  val oex_exp = wrapEXPR(Ast.Void, Ast.StringConst oex_str)
	  val addr_exp = wrapEXPR(Ast.Void, addr_cexp)
	  val size_exp = wrapEXPR(Ast.Void, Ast.SizeOf exp_type)
      in
	  newInstCallExpr (vp_exp,[oex_exp,addr_exp,size_exp])
      end

  (* Creates coreExpr for the staticpos corresponding to ty. *)
  fun newStaticRepPtrId ty =
      let (* Compose the name of the static_repptr and create EXPR for it. *)
	  val static_repptr_str = "_" ^ (getTypeString ty) ^ "_static_repptr"
      in  getGlobalObj static_repptr_str
      end

  (* Creates EXPR for the staticpos corresponding to ty. *)
  fun newStaticRepPtrExpr ty =
      let val id as {ctype=str_ty,...} = newStaticRepPtrId ty
      in  wrapEXPR(str_ty, Ast.Id id)
      end

  fun newStaticTypeAssign (tagptr_cexp,ctype,{is_zero=iszero}) =
      let
	  (* Create EXPR for tagptr *)
	  val tagptr_exp = wrapEXPR(Ast.Void, tagptr_cexp)

	  (* Create EXPR for static_repptr *)
	  val ctype_static_repptr_exp =
		if iszero
		then let val id as {ctype=str_ty,...} = getGlobalObj "_ini_static_repptr"
			 in  wrapEXPR(str_ty, Ast.Id id)
			 end
		else newStaticRepPtrExpr ctype

	  (* Create EXPR for tagptr = static_repptr. *)
	  val assign_cexp = Ast.Assign(tagptr_exp, ctype_static_repptr_exp)
	  val assign_exp = wrapEXPR(Ast.Void, assign_cexp)
      in
	  assign_exp
      end

  (* given list of expressions, returns a comma expression *)
  (* currently used by initTag *)
  fun commafyExprs(prev, nil) = prev
    | commafyExprs(prev, head::tail) =
	let val comma_exp = wrapEXPR(Ast.Void, Ast.Comma (prev,head))
	in  commafyExprs(comma_exp, tail)
	end

  datatype initmode = MODE_DEFAULT | MODE_EXTERN | MODE_INTEGER | MODE_UNINIT | MODE_INIT

  (********************************************
   initTag(addr_exp_builder	: () -> Ast.expression,
	   ctype		: Ast.ctype
	   loc			: SourceMap.location
	   imode		: initmode)
	-> init_exprs : Ast.expression
   ********************************************)
  (* prefix is either "" (normal case) or "_extern", for registerExtern initialization *)
  fun initTag (addr_exp_builder, ctype, loc, imode : initmode) =
     (let val _ = pushLoc loc

	  (* shortcut here: if -ptr, then always use MODE_INIT (other modes may init component types individually) *)
	  val imode = if (!Flags.instrMode = Flags.IM_PTR)
		      orelse (!Flags.instrMode = Flags.IM_PTRW)
		      then MODE_INIT else imode

	  val prefix = if (imode = MODE_EXTERN) then "_extern"
						else ""

	  fun initTagCtype (exp_builder,ctype) =
	      case ctype
	       of Ast.Qual (_,ty) => initTagCtype (exp_builder,ty)
		| Ast.StructRef (tid) => initTagTid (exp_builder,tid)
		| Ast.UnionRef (tid) => initTagTid (exp_builder,tid)
		| Ast.TypeRef (tid) => initTagTid (exp_builder,tid)
		| Ast.Array (opt,ty) =>
		    let
			val init_exps = initTagCtype(exp_builder,ty)

			(* -- replicateTag(exp,sizeof(ty),siz) -- *)
			(* Create EXPR for _replicateTag function. *)
			val rt_id = getGlobalFun (prefix ^ "_replicateTag")
			val rt_exp = wrapEXPR(Ast.Void, Ast.Id rt_id)

			(* Create EXPRs for exp', sizeof(ty), sizOp *)
			val exp' = exp_builder ()
			val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf ty)
			val nelem = case opt of SOME (siz,_) => siz
					      | NONE => 0
			val nelem_exp = wrapEXPR(Ast.Void, Ast.IntConst nelem)

			(* Create the call to _replicateTag(exp', sizeof(ty), nelem). *)
			val rt_call_exp = newInstCallExpr (rt_exp,[exp',sizeof_exp,nelem_exp])
		    in
			(init_exps @ [rt_call_exp])
		    end
		| _  => 
		    let
			(* -- setScalarTag_ctype(exp) -- *)

			val cty_str = getTypeString ctype

			(* if MODE_UNINIT, call setScalarUninitTag instead *)
			val sst_id = if (imode = MODE_UNINIT)
				     then getGlobalFun ("_setScalarUninitTag_" ^ cty_str)
				     else if (imode = MODE_INTEGER) andalso (!Flags.strictPointer)
					  then getGlobalFun ("_setScalarTagPtrToInt_" ^ cty_str)
					  else getGlobalFun (prefix ^ "_setScalarTag_" ^ cty_str)
			(* Create EXPR for _setScalarTag_ctype function. *)
			val sst_exp = wrapEXPR(Ast.Void, Ast.Id sst_id)

			(* Create the call to _setScalarTag_ctype(exp). *)
			val sst_call_exp = newInstCallExpr (sst_exp,[exp_builder ()])
		    in
			[sst_call_exp]
		    end

	  and initTagTid(exp_builder,tid) =
	      case lookTid tid
		of SOME {ntype = SOME nty,...} =>
		   (case nty
		       of B.Struct (tid, fields) =>
			  let fun processFields(nil) = nil
				(* AAL: Anonymous bitfield.  Padding?  Should we set its type? *)
			        | processFields((_,NONE,_)::tail) = processFields(tail)
			        | processFields((_,_,SOME size)::tail) = (* bitfields: skip *)
								processFields(tail)
			        | processFields((fty,SOME mem,_)::tail) =
				    let
					(* build new_exp = exp + offsetof(nty,fpid) *)
					fun new_builder () =
					    let val exp' = exp_builder ()
						val off_exp = newOffsetofExpr(Ast.StructRef tid, mem)
					    in  wrapEXPR(Ast.Void, Ast.Binop (Ast.Plus,exp',off_exp))
					    end
					val init_exps = initTagCtype(new_builder,fty)
				    in
					init_exps @ processFields(tail)
				    end
			  in
			     processFields (fields)
			  end
			| B.Union (tid, nil) => nil
			| B.Union (tid, (field1 as (fty,mem))::_) => initTagCtype(exp_builder,fty)
			| B.Enum _ => initTagCtype(exp_builder,stdInt)
			| B.Typedef (tid,ty) => initTagCtype(exp_builder,ty)
                   )
		 | _ => (warning "initTagTid"
			 ("tid " ^ Tid.toString tid ^ " not found or type is partial!\n");
			 nil)

	  val init_exp =
		if (imode = MODE_INIT)
		then newSetTagExpr setInitTag (addr_exp_builder (), wrapEXPR (Ast.Void, Ast.SizeOf ctype), loc)
		else if (imode = MODE_UNINIT) andalso not (!Flags.sizedUninit)
		then (* -- if uninits are unsized, then we just need to call setUninitTag *)
		     newSetTagExpr setUninitTag (addr_exp_builder (), wrapEXPR (Ast.Void, Ast.SizeOf ctype), loc)
		else let
			 (* -- collect initTag (setScalarTag and replicateTag) expressions -- *)
			 (* the "seed" expression is of the form "( char * ) addr_exp" *)
			 fun cast_builder () =
			     let val char_type = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,
							      Ast.CHAR,Ast.SIGNASSUMED)
			     in wrapEXPR(Ast.Void, Ast.Cast(Ast.Pointer char_type,addr_exp_builder ())) end

			 val init_exps = initTagCtype(cast_builder, ctype)

			 (* -- call setUninitTag(&id, sizeof id) if id is aggregate -- *)
			 val sut_exps = if (isSome (isStructOrUnion ctype)) orelse (isArray ctype)
					then [newSetTagExpr (prefix ^ setUninitTag) ( addr_exp_builder ()
										    , wrapEXPR (Ast.Void, Ast.SizeOf ctype)
										    , loc)]
					else nil

			 (* -- prepare to commafy; default is a 0-expression which should never happen *)
			 val (head,tail) = case (sut_exps @ init_exps)
					     of nil  => (wrapEXPR(Ast.Void, Ast.IntConst 0),nil)
					      | h::t => (h,t)
		     in
			 commafyExprs(head, tail)
		     end
      in
	  init_exp before popLoc()
      end) (* end fun initTag (addr_exp_builder, ctype, loc, imode : initmode) *)

  (* call initTag, seeding it with "&id" expression builder *)
  fun initTag_id (id as {ctype=id_type,location=id_loc,...}, imode : initmode) =
      let fun addrof_builder () = wrapEXPR (Ast.Void, Ast.AddrOf (wrapEXPR (Ast.Void, Ast.Id id)))
      in  initTag (addrof_builder, id_type, id_loc, imode) end

  (* Returns list of "setStringTag" statements for initializing pointers
     in id that are initialized to string literals *)
  fun doSetStringTag (lhs_expbuilder, rhs_exp as Ast.EXPR (rhs_cexp,aid,_)) =
      case rhs_cexp
	of Ast.StringConst s =>
	   if aos_le_exposed [Rtc.aoStringLit aid]
	   andalso ((!Flags.instrMode <> Flags.IM_PTRW) orelse (!Flags.strLitWritable))
	   then let
		    val _ = (count_set_tag := (!count_set_tag) + 1)
		    val lhs_exp = lhs_expbuilder()
		    val sst_exp = newSetStringTagExpr (lhs_exp,s)
		in  case (getCoreType (lookupExprType lhs_exp))
		      of Ast.Pointer _ => [wrapSTMT(Ast.Expr (SOME sst_exp))]
		       | _ => (warning "doSetStringTag" "non-pointer LHS!\n"; nil)
		end
	   else nil
	 | _ => nil

  (* returns "setScalarTag" statements for initializing non-zero values.
     In all cases, prior to calling initNonZeroConsts, mirror
     has been set to INIT *)
  fun doSetScalarTag (lhs_expbuilder, rhs_exp) =
	if not (OPT.isZero rhs_exp)
	then let val lhs_exp as Ast.EXPR(lhs_cexp,_,_) = lhs_expbuilder()
		 val is_bitfield =
			case lhs_cexp
			  of Ast.Member (rhs_exp,mem) =>
				isBitField (lookupExprType rhs_exp, mem)
			   | Ast.Arrow (rhs_exp,mem) =>
				isBitField (derefOneLevel (lookupExprType rhs_exp), mem)
			   | _ => false
	     in  if aos_le_exposed (expToAO lhs_exp)
		 andalso not is_bitfield
		 then let (* -- setScalarTag_ctype(lhs_exp) -- *)
			  val _ = (count_set_tag := (!count_set_tag) + 1)
			  val lhs_exp_type = lookupExprType lhs_exp
			  val cty_str = getTypeString lhs_exp_type
			  val ssct_id = getGlobalFun ("_setScalarTag_" ^ cty_str)
			  val ssct_exp = wrapEXPR(Ast.Void, Ast.Id ssct_id)
			  val addr_exp = wrapEXPR(Ast.Void, Ast.AddrOf lhs_exp)
			  (* Create the call to _setScalarTag_ctype(lhs_exp). *)
			  val ssct_call_exp = newInstCallExpr (ssct_exp,[addr_exp])
		      in  [wrapSTMT(Ast.Expr (SOME ssct_call_exp))] end
		 else nil
	     end
	else nil

  (* Applies function (one of doSetStringTag and doSetScalarTag, above) to an initExpr
     of *constant* initializers, and collects the returned list into one big list *)
  fun mapInitExpr processLRpair (id as {ctype=id_ty,...}:Ast.id, initExpr) =
      let
	  (***********************************************)
	  (* process a single initialization expression  *)
	  (***********************************************)
	  fun checkInitExpr (ebuilder, ty, initexpr) =
	      (case initexpr
		 of Ast.Aggregate initexprs =>
			(case getCoreType ty
			   of Ast.Array (_, ety) =>
				  checkArray(ebuilder,ety,0,initexprs)
			    | Ast.StructRef tid =>
				 (case lookTid tid
				    of SOME {ntype=SOME(B.Struct (_,fields)),...} =>
					  checkStructFields(ebuilder,fields,initexprs)
				     | _ => nil) (* error! *)
			    | Ast.UnionRef tid =>
				 (case lookTid tid
				    of SOME {ntype=SOME(B.Union (_,fields)),...} =>
					  checkUnionFields(ebuilder,fields,initexprs)
				     | _ => nil) (* error! *)
			    | _ => nil)
		  | Ast.Simple exp => processLRpair (ebuilder, exp)
	      )

	  (********************)
	  (* process an array *)
	  (********************)
	  and checkArray (ebuilder, ety, index, iehead::ietail) =
		let fun sub_builder () =
			let val idx_exp = wrapEXPR(Ast.Void, Ast.IntConst index)
			in  wrapEXPR ((*NEEDED*)ety, Ast.Sub (ebuilder(), idx_exp))
			end
		in 
		    checkInitExpr(sub_builder, ety, iehead)
		    @ checkArray(ebuilder, ety, index + 1, ietail)
		end
	    | checkArray (_, _, _, nil) = nil

	  (***********************)
	  (* process a structure *)
	  (***********************)
	  and checkStructFields(ebuilder, (fty,SOME mem,_)::ftail,iehead::ietail) =
		let fun mem_builder () = wrapEXPR ((*NEEDED*)fty, Ast.Member (ebuilder(), mem))
		in
		    checkInitExpr(mem_builder,fty,iehead)
		    @ checkStructFields(ebuilder,ftail,ietail)
		end
	    | checkStructFields(ebuilder, (_,NONE,_)::ftail,iexprs) =
		(*-- skip unnamed fields *)
		checkStructFields(ebuilder,ftail,iexprs)
	    | checkStructFields(_,_,_) = nil

	  (*******************)
	  (* process a union *)
	  (*******************)
	  and checkUnionFields(ebuilder,(fty,mem)::_,iehead::_) =
		let fun mem_builder () = wrapEXPR ((*NEEDED*)fty, Ast.Member (ebuilder(), mem))
		in  checkInitExpr(mem_builder,fty,iehead)
		end
	    | checkUnionFields(_,_,_) = nil
      in
	  checkInitExpr((fn () => wrapEXPR((*NEEDED*)id_ty, Ast.Id id)), id_ty, initExpr)
      end

  (* Creates a function definition (FunctionDef) containing stmts, external-decl
     instrumentation, and touched-function instrumentation.
     (External-decl comes from global, retrieved via resetExterns().)
     Arguments tfuncset and tglobset are sets of funcs and globs "touched"
     in current file, and may be used to filter out unnecessary instrumentation.
     Returns function's id, prototype declaration, and function definition. *)
  fun makeStmtFun(init_stmts, tfuncset, tglobset) =
      let
	  fun filterList (set,list) =
	      let val lset = idset.addList (idset.empty, list)
		  val rset = idset.intersection (lset,set)
	      in  idset.listItems rset  end

	  (* Create extern decls and "registerExtern" calls *)
	  fun processExternVars nil = (nil,nil)
	    | processExternVars ((e_id as {uid=pid,ctype=e_type,location=e_loc,...})::tail) =
		let val _ = pushLoc e_loc
		    (* is e_type aggregate (struct/union/array)? *)
		    val e_type_is_aggregate = (getTypeString e_type = "aggregate")
		    (* is e_type complete? *)
		    val e_type_is_complete = hasKnownStorageSize e_type

		    (* Create extern declaration statement *)
		    val e_decl = Ast.VarDecl (e_id, NONE)


		    (* create (_extern_)initTag stmts for aggregate type, but only if e_type is complete *)
		    (* SY: I don't remember why we do this: could be a hack for ctype functions that use
			   an extern array?  For now, disabling for ptr/ptrw-mode *)
		    val inittag_stmts = if (!Flags.instrMode = Flags.IM_PTR)
					orelse (!Flags.instrMode = Flags.IM_PTRW)
					then nil
					else if e_type_is_aggregate andalso e_type_is_complete
					     andalso aos_child_le_exposed [Rtc.aoId pid]
					     then let
						      val _ = (count_set_tag := (!count_set_tag) + 1)
						      val init_exp = initTag_id (e_id,MODE_EXTERN)
						  in  [wrapSTMT(Ast.Expr (SOME init_exp))] end
					     else nil

		    (* Create call to registerExtern(&e, e_type, e_size) *)
		    val e_exp = wrapEXPR(Ast.Void, Ast.Id e_id)
		    val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf e_exp)
		    val e_type_exp = newCtypeEnumExpr e_type

		    val re_id = getGlobalFun "_registerExtern"
		    val re_exp = wrapEXPR(Ast.Void, Ast.Id re_id)

		    val e_size_exp = if e_type_is_complete
				     then wrapEXPR(Ast.Void, Ast.SizeOf e_type)
				     else wrapEXPR(Ast.Void, Ast.IntConst 0)

		    val call_exp = newInstCallExpr (re_exp,[addrof_exp,e_type_exp,e_size_exp])
		    val regis_stmt = wrapSTMT(Ast.Expr (SOME call_exp))

		    val (tail_decls,tail_stmts) = processExternVars tail
		in
		    (e_decl::tail_decls, inittag_stmts @ (regis_stmt::tail_stmts)) before popLoc()
		end

	  (* filter externs by tglobset, then generate registerExtern stuff *)
	  val (exvar_decls,regis_stmts) = processExternVars (filterList (tglobset, resetExterns()))

	  (* Create extern decls and "registerFunction" calls *)
	  fun processFunctions nil = (nil,nil)
	    | processFunctions (f_id::tail) =
		let val _ = pushLoc (#location f_id)

		    (* Create extern declaration statement *)
		    val f_decls = if ((#stClass f_id) = Ast.STATIC)
				  then nil (* HACK: don't redeclare static functions *)
				  else [Ast.VarDecl (f_id, NONE)]

		    (* Create call to registerFunction(&e) *)
		    val f_exp = wrapEXPR(Ast.Void, Ast.Id f_id)
		    val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf f_exp)

		    val rf_id = getGlobalFun "_registerFunction"
		    val rf_exp = wrapEXPR(Ast.Void, Ast.Id rf_id)

		    val call_exp = newInstCallExpr (rf_exp,[addrof_exp])
		    val regis_stmt = wrapSTMT(Ast.Expr (SOME call_exp))

		    val (tail_decls,tail_stmts) = processFunctions tail
		in
		    (f_decls @ tail_decls, regis_stmt::tail_stmts) before popLoc()
		end

	  fun notBuiltin (funid:Ast.id) = not (String.isPrefix "__builtin_" (Sym.name (#name funid)))

	  (* filter out builtin functions, then generate registerFunction calls *)
	  val (fun_decls,regfun_stmts) = processFunctions (List.filter notBuiltin (idset.listItems tfuncset))

	  (* Forward compile time flags to the runtime library *)
	  (* - currently only one flag (strictPointer) is forwarded *)
	  val flaginit_stmts =
		if (!Flags.strictPointer)
		then let val flag_id = getGlobalObj "strictPointer"
			 val flag_exp = wrapEXPR(Ast.Void, Ast.Id flag_id)
			 val one_exp = wrapEXPR(Ast.Void, Ast.IntConst 1)
			 val assign_exp = wrapEXPR(Ast.Void, Ast.Assign (flag_exp, one_exp))
		     in
			 [wrapSTMT(Ast.Expr (SOME assign_exp))]
		     end
		else nil

	  (* Report static counts: _reportStaticCounts(fname, descr, count_tot_inst) *)
	  fun reportStaticCountsStmt descr count =
		let val rscfn_id = getGlobalObj "_reportStaticCounts"
		    val rscfn_exp = wrapEXPR(Ast.Void, Ast.Id rscfn_id)
		    val fname_exp = wrapEXPR(Ast.Void, Ast.StringConst filestem)
		    val descr_exp = wrapEXPR(Ast.Void, Ast.StringConst descr)
		    val icount_exp = wrapEXPR(Ast.Void, Ast.IntConst count)
		    val call_exp = wrapEXPR(Ast.Void, Ast.Call (rscfn_exp, [fname_exp, descr_exp, icount_exp]))
		in
		    wrapSTMT(Ast.Expr (SOME call_exp))
		end

	  val rsc_stmts = [reportStaticCountsStmt "tot_inst" (!count_tot_inst)
			  ,reportStaticCountsStmt "st_inst" (!count_set_tag)
			  ,reportStaticCountsStmt "vp_inst" (!count_verify_ptr)
			  ,reportStaticCountsStmt "vp_elided" (!count_verify_ptr_elided)
			  ,reportStaticCountsStmt "vp_array_direct" (!count_vp_array_direct)
			  ,reportStaticCountsStmt "vp_array_typed" (!count_vp_array_typed)
			  ,reportStaticCountsStmt "vp_array_sub" (!count_vp_array_sub)
			  ,reportStaticCountsStmt "malloc_inst" (!count_malloc)
			  ,reportStaticCountsStmt "malloc_elided" (!count_malloc_elided)
			  ,reportStaticCountsStmt "libfn_inst" (!count_libfn)
			  ,reportStaticCountsStmt "libfn_elided" (!count_libfn_elided)
			  ,reportStaticCountsStmt "free_inst" (!count_free)
			  ,reportStaticCountsStmt "free_partial" (!count_free_partial)
			  ,reportStaticCountsStmt "free_elided" (!count_free_elided)
			  ]

	  (* Function body cosists of extern declarations,
	     flag initialization statements, register statements, and init statements *)
	  val coreStmt = Ast.Compound (exvar_decls @ fun_decls,
				rsc_stmts @ flaginit_stmts @ regis_stmts @ regfun_stmts @ init_stmts)
	  val stmt = wrapSTMT(coreStmt)

	  (* Function name is _TypeErrorDebugger_init_<tmpid>_<fname>.  Type is (void foo()). *)
	  val pid = Pid.new()
	  val fname = (* Convert all non-alnum characters in filename to _ *)
		      let fun cvtchar x = if (Char.isAlphaNum x) then x else #"_"
		      in  String.map (cvtchar) filestem
		      end
	  val funName = "_TypeErrorDebugger_init_" ^ (Pid.toString pid) ^ "_" ^ fname
	  val funSym = Sym.func funName
	  val funTy = Ast.Function (Ast.Void, nil)

	  (* Construct an id for the function. *)
	  val fun_id = {name = funSym, uid = pid, location = SourceMap.UNKNOWN,
			ctype = funTy, stClass = Ast.DEFAULT,
			status = Ast.DEFINED, global = true,
			kind = Ast.FUNCTION {hasFunctionDef=true}}

	  (* Insert the id into the global symbol table. *)
	  val binding = B.ID fun_id
	  val _ = bindSymGlobal(funSym, binding)

	  (* Create function prototype and definition. *)
	  val proto = wrapDECL (Ast.ExternalDecl (Ast.VarDecl (fun_id,NONE)))
	  val func_def = wrapDECL (Ast.FunctionDef(fun_id, nil, stmt))
      in
	  (* Return the id of the 0-argument function, function prototype,
	  and definition (external decl). *)
	  (fun_id, proto, func_def)
      end

  (* Reports if exp contains more than one function call at the same "level"
     (with no intervening sequence point).
     In these cases, if we are pessimistic we must skip instrumenting those
     function calls, because the instrumentation assignment to _globalArgAddr
     would result in undefined behavior.
     (At least gcc decided to interpret the C specs so.) *)
  fun hasClashingCalls (exp as (Ast.EXPR (_,_,exp_loc))) =
      if !Flags.skipClashingCalls
      then let fun countCallsInExpr (exp as Ast.EXPR (coreExpr,_,_)) =
		   let fun maxCallsInExprs nil = 0
			 | maxCallsInExprs (head::tail) =
			   let val maxhead = countCallsInExpr head
			       val maxtail = maxCallsInExprs tail
			   in  if   maxhead   >  maxtail
			       then maxhead else maxtail
			   end
		   in case coreExpr
			of Ast.IntConst li => 0
			 | Ast.RealConst r => 0
			 | Ast.StringConst s => 0
			 | Ast.Call (fexp,exps) => (maxCallsInExprs (fexp::exps)) + 1
			 | Ast.QuestionColon (e1,e2,e3) => maxCallsInExprs [e1,e2,e3]
			 | Ast.Assign (e1,e2) => maxCallsInExprs [e1,e2]
			 | Ast.Comma (e1,e2) => maxCallsInExprs [e1,e2]
			 | Ast.Sub (e1,e2) => (countCallsInExpr e1) + (countCallsInExpr e2)
			 | Ast.Member (exp,mem) => countCallsInExpr exp
			 | Ast.Arrow (exp,mem) => countCallsInExpr exp
			 | Ast.Deref exp => countCallsInExpr exp
			 | Ast.AddrOf exp => countCallsInExpr exp
			 | Ast.Binop (binop,e1,e2) =>
				(case binop of Ast.And => maxCallsInExprs [e1,e2]
					     | Ast.Or => maxCallsInExprs [e1,e2]
					     | _ => (countCallsInExpr e1) + (countCallsInExpr e2)
				)
			 | Ast.Unop (unop,exp) => countCallsInExpr exp
			 | Ast.SizeOf ty => 0
			 | Ast.Cast (ctype,exp) => countCallsInExpr exp
			 | Ast.Id id => 0
			 | Ast.EnumId (pid,li) => 0
			 | Ast.ExprExt ee => 0
			 | Ast.ErrorExpr => 0
		   end

	       fun countCallsInExprs nil = 0
		 | countCallsInExprs (exp::tail) =
		   let val thiscount = countCallsInExpr exp
		   in  if thiscount > 1 then thiscount
					else thiscount + (countCallsInExprs tail)
		   end

	       val ret = (countCallsInExprs [exp]) > 1

	       val _ = if ret then print ("Clashing Calls in [" ^ (SourceMap.locToString exp_loc)
							^ "]:" ^ (exprToString exp) ^ "\n" )
			      else ()
	   in  ret  end
      else false

  (**********************************************)
  (* Count number of scalars in ctype		*)
  (* UNUSED: keep around? may be useful later?	*)
  fun countScalars ttab ctype =
     (case TypeUtil.getCoreType ttab ctype
	of Ast.Array (sizeop, ety) =>
	   (case sizeop of SOME (sz,_) => sz * (countScalars ttab ety)
			 | _ => 1 (*error!*)
	   )
	 | Ast.StructRef tid =>
	  (case Tidtab.find (ttab,tid)
	     of SOME {ntype=SOME(Bindings.Struct (_, fields)),name,global,location} =>
		List.foldl (fn (field,cnt) =>
				let val fty = (#1 field)
				in  (countScalars ttab fty) + cnt
				end
			   ) 0 fields
	      | _ => 1 (* error! *)
	  )
	 | Ast.UnionRef tid =>
	  (case Tidtab.find (ttab,tid)
	     of SOME {ntype=SOME(Bindings.Union (_, field::tail)),name,global,location} =>
		let val fty = (#1 field)
		in  countScalars ttab fty
		end
	      | _ => 1 (* empty union, or error *)
	  )
	 | _ => 1
      )

  (**************************************************************)
  (* builds an initExpr that initializes the first scalar in	*)
  (* ctype to 0 - the hard part is getting the right number of	*)
  (* {braces}.							*)
  fun zeroInitFirstScalar ttab ctype =
     (case TypeUtil.getCoreType ttab ctype
	of Ast.Array (sizeop, ety) =>
	   Ast.Aggregate [zeroInitFirstScalar ttab ety]
	 | Ast.StructRef tid =>
	  (case Tidtab.find (ttab,tid)
	     of SOME {ntype=SOME(Bindings.Struct (_, (fty,_,_)::tail)),name,global,location} =>
		Ast.Aggregate [zeroInitFirstScalar ttab fty]
	      | _ => (* error! *)
		Ast.Simple (wrapEXPR (stdInt, Ast.IntConst 0))
	  )
	 | Ast.UnionRef tid =>
	  (case Tidtab.find (ttab,tid)
	     of SOME {ntype=SOME(Bindings.Union (_, (fty,_)::tail)),name,global,location} =>
		Ast.Aggregate [zeroInitFirstScalar ttab fty]
	      | _ => (* empty union, or error *)
		Ast.Simple (wrapEXPR (stdInt, Ast.IntConst 0))
	  )
	 | _ => Ast.Simple (wrapEXPR (stdInt, Ast.IntConst 0))
      )

  (**************************  END OF HELPER FUNCTIONS *************************)
  (************************** START OF INSTR FUNCTIONS *************************)

  (* ALLOCA: for -indivClearTag mode, must store each alloca block's
	pointer and size in function-scoped temporaries.
	- At alloca callsite, collect these temporaries into alloca_tmps;
	  create cleartag expressions and put into alloca_ct_exps
	- At function def, after processing body, declare temporaries
	  in alloca_tmps, and call alloca_ct_exps at the end
  *)
  val alloca_tmps = ref nil
  val alloca_ct_exps = ref nil

  fun instrLValueExpr (outer_exp as (Ast.EXPR (coreExpr,exp_aid,exp_loc)),
			{enforce:bool,verify_ptr:bool,rval:bool,skip_call:bool,is_write:bool},
			tagptr_cexpOp) = 
      ( if !trace then ppToStdOut PPA.ppExpression outer_exp else ();
	pushLoc exp_loc;

	let
	    (* enforce_local is for local consumption; if instrumenting sub-exprs, pass enforce *)
	    val outer_exp_aos = expToAO outer_exp
	    val enforce_local = if not (!Flags.instrMode = Flags.IM_PTR)
				andalso not (!Flags.instrMode = Flags.IM_PTRW)
				andalso ((aos_badly_typed outer_exp_aos)
					 orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify))
				andalso not (vt_redundant_aid Rtc.RED_Verify exp_aid)
				then enforce
				else false
	    val verify_ptr_local = if aos_poss_invalid outer_exp_aos
				   then if (!Flags.instrMode = Flags.IM_PTRW) andalso not is_write
					then false
					else if vp_redundant_aid (!Flags.instrMode = Flags.IM_PTRW) exp_aid
					     orelse array_inbounds_aid exp_aid
					     then false
					     else verify_ptr
				   else false

	    (* --- 1: preamble *)
	    (*  -- adjust flag: if outer_exp is statically an array, then do not enforce. *)
	    val exp_type = lookupExprType outer_exp
	    val isarray = isArray exp_type

	    (*  -- Helper function: allocates a new tmp_ptr cexp if none yet allocated *)
	    (*	Second return value is the fall-through state storing the allocated tmp_ptr *)
	    fun getTmpPtrCexp (tp_cexpOp) =
		(case tp_cexpOp
		   of SOME tp_cexp => (tp_cexp, SOME tp_cexp)
		    | NONE =>	let val tp_cexp = Ast.Id (addTmpValVar (outer_exp,{pointer=true}))
				in (tp_cexp, SOME tp_cexp)
				end
		)

	    (* --- 2: the big case statement: set appropriate flags and functions *)
	    (*	The get___PtrCexp functions need to be isomorphic, and conform to
	    	the signature of getTmpPtrCexp above. *)
	    val {
		  functionHackCexp: (unit -> Ast.coreExpression) option, (* HACK: needed for Id and Deref cases
									    to properly handle functions *)
		  bitfieldHackPair: (Ast.expression * Ast.member) option,
				(* HACK: needed for Deref and Arrow cases to sidestep bitfields problem *)
		  doVerifyPtr : bool,			(* flag: do verifyPtr? *)
		  doVerifyTag : bool,			(* flag: do verifyTag? *)
		  getValuePtrCexp :
			Ast.coreExpression option -> (Ast.coreExpression * Ast.coreExpression option),
							(* function to generate (L-)value representation *)
		  getTagPtrCexp :
			Ast.coreExpression option -> (Ast.coreExpression * Ast.coreExpression option),
							(* function to generate tag representation *)
		  outerInstCexp: Ast.coreExpression	(* the instrumented expression *)
	        } = (case coreExpr
		       of Ast.Sub (e1,e2) =>
				{
				  functionHackCexp = NONE,
				  bitfieldHackPair = NONE,
				  doVerifyPtr = true,	getValuePtrCexp = getTmpPtrCexp,
				  doVerifyTag = true,	getTagPtrCexp = getTmpPtrCexp,
				  outerInstCexp = (* -- <e1,T,none>[<e2,T,none>] *)
					let val (e1',_) = instrRValueExpr (e1,{enforce=true,needrval=true,skip_call=skip_call},
										NONE)
					    val (e2',_) = instrRValueExpr (e2,{enforce=true,needrval=true,skip_call=skip_call},
										NONE)
					in  Ast.Sub (e1',e2')
					end
				}
			| Ast.Member (exp,mem) =>
				{
				  functionHackCexp = NONE,
				  bitfieldHackPair = (
				    if isBitField (lookupExprType exp, mem)
				    then SOME (wrapEXPR (Ast.Void, Ast.AddrOf exp), mem)
				    else NONE
				  ),
				  doVerifyPtr = OPT.isDerefLvalExpr exp,
							getValuePtrCexp = getTmpPtrCexp,
				  doVerifyTag = true,	getTagPtrCexp = getTmpPtrCexp,
				  outerInstCexp = (* -- [exp,F,none].mem *)
					let val exp' = instrLValueExpr (exp,{enforce=false,verify_ptr=false,rval=false,
									     skip_call=skip_call,is_write=is_write},
									NONE)
					in  Ast.Member (exp',mem)
					end
				}
			| Ast.Arrow (exp,mem) =>
				{
				  functionHackCexp = NONE,
				  bitfieldHackPair = (
				    if isBitField (derefOneLevel (lookupExprType exp), mem)
				    then SOME (exp, mem)
				    else NONE
				  ),
				  doVerifyPtr = true,	getValuePtrCexp = getTmpPtrCexp,
				  doVerifyTag = true,	getTagPtrCexp = getTmpPtrCexp,
				  outerInstCexp = (* -- [exp,T,none]->mem *)
				  	let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=skip_call},
										NONE)
					in  Ast.Arrow (exp',mem)
					end
				}
			| Ast.Deref exp => (* -- *[exp,T,none] *)
				if (isFunctionOrFunctionPointer (lookupExprType exp))
				then (* if exp is a function or function pointer *)
				{    (* don't verify anything, and set tagptr (if any) to _pointer_ctype *)
				  functionHackCexp = SOME (fn () => (Ast.Id (newStaticRepPtrId(Ast.Pointer Ast.Void)))),
				  bitfieldHackPair = NONE,
				  doVerifyPtr = false,
				  doVerifyTag = false,
				  getValuePtrCexp = getTmpPtrCexp,
				  (* tag representative = _pointer_static_repptr *)
				  getTagPtrCexp = ( fn ceo =>
						    (Ast.Id (newStaticRepPtrId (Ast.Pointer Ast.Void)), ceo)
						  ),
				  outerInstCexp = (* -- *[exp,T,none] *)
					Ast.Deref (#1 (instrRValueExpr (exp,
									{enforce=true,needrval=true,skip_call=skip_call},NONE)))
				} else {
				  functionHackCexp = NONE,
				  bitfieldHackPair = NONE,
				  doVerifyPtr = true,	getValuePtrCexp = getTmpPtrCexp,
				  doVerifyTag = true,	getTagPtrCexp = getTmpPtrCexp,
				  outerInstCexp = (* -- *[exp,T,none] *)
					Ast.Deref (#1 (instrRValueExpr (exp,
									{enforce=true,needrval=true,skip_call=skip_call},NONE)))
				}
			| Ast.Id (pid as {ctype=pid_type,...}) =>
				let val isfunc = isNonPointerFunction pid_type
				    val addridfunc = ( fn ceo =>
							(Ast.AddrOf (wrapEXPR(Ast.Void, Ast.Id pid)), ceo)
						     )
				in {
				  functionHackCexp =
					   if isfunc
					   then SOME (fn () => (Ast.Id (newStaticRepPtrId(Ast.Pointer Ast.Void))))
					   else NONE,
				  bitfieldHackPair = NONE,
				  doVerifyPtr = false,
				  doVerifyTag = not isfunc, (* -- don't verify tag if we're a function *)
				  getValuePtrCexp = addridfunc, (* -- use "&id" inst of creating a tmp_ptr *) 
				  getTagPtrCexp = (* -- if we're a function, use _pointer_static_repptr... *)
					if isfunc
					then ( fn ceo =>
						(Ast.Id (newStaticRepPtrId (Ast.Pointer Ast.Void)), ceo)
					     )
					else addridfunc, (* -- else use "&id" *)
				  outerInstCexp = Ast.Id pid	(* -- id *)
				} end
			| _ =>  let val _ = warning "instrLValueExpr"
					     ("non-Lvalue expression encountered at " ^ (SourceMap.locToString exp_loc) ^ ":")
				    val _ = ppToStdOut PPA.ppExpression outer_exp
				in {
				     functionHackCexp = NONE,
				     bitfieldHackPair = NONE,
				     doVerifyPtr = false,	getValuePtrCexp = getTmpPtrCexp,
				     doVerifyTag = false,	getTagPtrCexp = getTmpPtrCexp,
				     outerInstCexp = coreExpr
				} end
		    )

	    (* HACK: for now, stick this here to trick bitfield cases into not generating
	       "intermediate" instrumentation.
	       Long term solution: restructure so that the case statement is bottom-most,
	       and the shared stuff (greating verifyPtr/verifyTag, etc) are put into
	       helper functions called from the appropriate cases *)
	    val (doVerifyPtr, doVerifyTag) =
		(case bitfieldHackPair
		   of SOME _ => (false, false)
		    | NONE => (doVerifyPtr, doVerifyTag)
		)

	    (* --- 3: start creating "intermediate" instrumentation statements:
	    	currently processed backwards for convenience of "cons"-ing only;
	    	for a fact, these could be output in arbitrary order *)

(*--HACK: when ts_level is safe (not tracked or unsafe), tagptr_cexp cannot be assigned to &exp!--*)
(**)(*    Better solution: rewrite instrLvalue to return static_rep option*)
(**)(*    Better yet: merge instrL and instrR into one! *)
(**)	    val (inter_inst_exps,tp_cexpOp) =
(**)		if aos_eq_safe (expToAO outer_exp)
(**)		andalso not (may_be_uninit_aid (exp_aid,Rtc.MBU_Verify))
(**)		then case tagptr_cexpOp
(**)		       of SOME tagptr_cexp =>
(**)			  let (*- add "tagptr_cexp = static_rep" *)
(**)			      val tp_assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
(**)			  in
(**)			      ([tp_assign_exp],NONE)
(**)			  end
(**)			| NONE => (nil, NONE)
(**)		else (nil,tagptr_cexpOp)
(*--END HACK--*)
(*
	    val (inter_inst_exps,tp_cexpOp) = (nil,tagptr_cexpOp)
*)

	    (*  -- 3a: _verifyTag(oex, tmp_ptr, typeof(outer_exp)):
	    	only if enforce_local = true and not array *)
	    (*  Also, if we DO create a doVerifyTag, set doVerifyPtr to false *)
	    val (inter_inst_exps, tp_cexpOp, doVerifyPtr) =
		    if doVerifyTag andalso enforce_local andalso (not isarray)
		    then let val (tp_cexp, tp_cexpOp) = getTagPtrCexp(tp_cexpOp)
			     val vt_call_exp = newVerifyTagCallExpr (outer_exp, tp_cexp, exp_type)
			 in  (vt_call_exp::inter_inst_exps , tp_cexpOp, false)
			 end
		    else (inter_inst_exps, tp_cexpOp, doVerifyPtr)

	    (*  -- 3b: verifyPtr exp: _verifyPtr(oex,tmp_ptr,sizeof(outer_exp)) *)
	    (* 	   - unless enforce_local=false && verify_ptr_local=false:
			     Note that we verify even if it's an array *)
			(*    -> not sure if this is true; we can't verify incomplete type *)
	    val (inter_inst_exps, tp_cexpOp) =
		    if doVerifyPtr andalso (!Flags.verifyArray orelse not isarray)
		    then if (enforce_local orelse verify_ptr_local)
			 then let val _ = (count_verify_ptr := (!count_verify_ptr) + 1)

				  (**********************************************)
				  (* Count instrumented array indexing exprs	*)
				  (*  - direct: direct indexing of array object	*)
				  (*  - typed: indexing of array-typed object	*)
				  (*	(incl. arrays in heap-allocated struct) *)
				  (*  - sub: use of [] notation			*)
				  fun classifyDeref (exp as Ast.EXPR (cexp,_,_)) =
				      let fun isLocExpr (exp as Ast.EXPR(cexp,_,_)) =
					     (case cexp
						of Ast.Call (fexp,exps) => true
						 | Ast.QuestionColon (e1,e2,e3) => (isLocExpr e2) andalso (isLocExpr e3)
						 | Ast.Assign (e1,e2) => isLocExpr e1
						 | Ast.Comma (e1,e2) => isLocExpr e2
						 | Ast.Sub (e1,e2) => ((isArray (lookupExprType e1)) andalso (isLocExpr e1))
									orelse ((isArray (lookupExprType e2)) andalso (isLocExpr e2))
						 | Ast.Member (e,mem) => isLocExpr e
						 | Ast.Arrow (e,mem) => false
						 | Ast.Deref e => (isArray (lookupExprType e)) andalso (isLocExpr e)
						 | Ast.AddrOf e => false
						 | Ast.Cast (ty,e) => isLocExpr e
						 | Ast.Id id => true
						 | _ => false
					      )
				      in(case cexp
					   of Ast.Sub (e1,e2) => ( isLocExpr exp
								 , isArray (lookupExprType e1) orelse isArray (lookupExprType e2)
								 , true)
					    | Ast.Deref e => ( isLocExpr exp
							     , isArray (lookupExprType e)
							     , false)
					    | Ast.Member (e,_) => classifyDeref e
					    | Ast.Arrow (e,_) => (false,false,false)
					    | _ => (false,false,false)
				      )end


				  val (is_array_direct,is_array_typed,is_array_sub) = classifyDeref outer_exp

				  val _ = if is_array_direct then (count_vp_array_direct := (!count_vp_array_direct) + 1)
							     else ()
				  val _ = if is_array_typed then (count_vp_array_typed := (!count_vp_array_typed) + 1)
							    else ()
				  val _ = if is_array_sub then (count_vp_array_sub := (!count_vp_array_sub) + 1)
							  else ()
				  (**********************************************)

				  val exp_type' = (case deArrayify exp_type
						of SOME elt_type => elt_type
						 | NONE => exp_type)
				  val (tp_cexp, tp_cexpOp) = getValuePtrCexp(tp_cexpOp)
				  val vp_call_exp = newVerifyPtrCallExpr(outer_exp, tp_cexp, exp_type')
			      in  (vp_call_exp::inter_inst_exps, tp_cexpOp)
			      end
			 else let val _ = if enforce orelse verify_ptr
					  then (count_verify_ptr_elided := (!count_verify_ptr_elided) + 1)
					  else ()
			      in  (inter_inst_exps, tp_cexpOp)  end
		    else (inter_inst_exps, tp_cexpOp)

	    (* --- 4: Done with "intermediate" instrumentation  *)
	    val final_cexp = (*   - Did we create any "intermediate" instrumentation? *)
		(case bitfieldHackPair
		   of SOME (addr_exp, mem) =>
			(* Bitfield case: if tagptr, do (tagptr=&dummyInt, setScalarTag(), addr_exp)->mem *)
			(case tp_cexpOp
			   of SOME tp_cexp =>
				let val tp_exp = wrapEXPR (Ast.Void, tp_cexp)
				    val dummy_id = getGlobalObj "_dummyInt"
				    val dummy_exp = wrapEXPR (Ast.Void, Ast.Id dummy_id)
				    val addrof_exp = wrapEXPR (Ast.Void, Ast.AddrOf dummy_exp)
				    val tp_assign_exp = wrapEXPR (Ast.Void, Ast.Assign (tp_exp, addrof_exp))

				    val tp_exp2 = wrapEXPR (Ast.Void, tp_cexp)
				    val sst_id = getGlobalFun ("_setScalarTag_" ^ (getTypeString exp_type))
				    val sst_exp = wrapEXPR(Ast.Void, Ast.Id sst_id)
				    val sst_call_exp = newInstCallExpr (sst_exp,[tp_exp2])

				    val comma1_exp = wrapEXPR (Ast.Void,
							Ast.Comma (tp_assign_exp, sst_call_exp))

				    val comma2_exp = wrapEXPR(Ast.Void, Ast.Comma (comma1_exp, addr_exp))
				in
				    Ast.Arrow (comma2_exp, mem)
				end

			      (* -- if no tagptr, revert to F,none case -- *)
			    | NONE => outerInstCexp
			)
		    | NONE =>
			(case inter_inst_exps
			   of nil => (*  -- 4a: no intermediate instrumentation *)
				(case tp_cexpOp
				   of NONE => (* 4a1: No tmp created: just do [e] *)
					outerInstCexp
				    | SOME tp_cexp => (*  -- 4a2: do *(exp_type * )(t=&[e]) *)
					(* HACK: when dealing with a function (in the Id and Deref cases)
					   functionHackCexp will be SOME, verifyPtr/Tag will be false,
					   so we'll always fall through to this case (so we only check it
					   here). If ever our lvalue instrumentation grows, be sure to
					   see if we can still do this here.
					   Anyhow, when dealing with a function, do the following:
						*(t = <static rep>, &[e])
					*)
					case functionHackCexp
					  of SOME staticCexpGen =>
					     let
						 val static_exp = wrapEXPR(Ast.Void, staticCexpGen ())
						 val tp_exp = wrapEXPR(Ast.Void, tp_cexp)
						 val tp_assign_exp = wrapEXPR(Ast.Void,
									Ast.Assign (tp_exp,static_exp))
						 val outer_inst_exp = wrapEXPR(Ast.Void, outerInstCexp)
						 val addrof_exp = wrapEXPR(Ast.Void,
									Ast.AddrOf (outer_inst_exp))
						 val comma_exp = wrapEXPR(Ast.Void,
									Ast.Comma(tp_assign_exp, addrof_exp))
					     in
						 Ast.Deref comma_exp
					     end
					   | NONE =>
					     let
						 val tp_exp = wrapEXPR(Ast.Void, tp_cexp)
						 val outer_inst_exp = wrapEXPR(Ast.Void, outerInstCexp)
						 val addrof_exp = wrapEXPR(Ast.Void,
									Ast.AddrOf (outer_inst_exp))
						 val tp_assign_exp = wrapEXPR(Ast.Void,
									Ast.Assign (tp_exp,addrof_exp))
						 val tp_cast_exp = wrapEXPR(Ast.Void,
									Ast.Cast(Ast.Pointer exp_type,
										 tp_assign_exp))
					     in
						 Ast.Deref tp_cast_exp
					     end
				)
			    | first_exp::remaining_exps =>
				let val (first_exp,remaining_exps,doderef) =
					(case tp_cexpOp
					   of NONE => (* 4b: No tmp created: just do *(...,&[e]) *)
						if rval
						(* -- 4b1: if rval=true, then do (...,[e]) *)
						then let val outerInstExp = wrapEXPR(Ast.Void,outerInstCexp)
						     in (first_exp, remaining_exps @ [outerInstExp],false)
						     end
						(* -- 4b2: if rval=false, then do *(...,&[e]) *)
						else let val outerInstExp = wrapEXPR(Ast.Void, outerInstCexp)
							 val addrof_exp = wrapEXPR(Ast.Void,
										Ast.AddrOf (outerInstExp))
						     in (first_exp, remaining_exps @ [addrof_exp],true)
						     end
					    | SOME tp_cexp => (*  -- 4c: do the *(t=&[e],...,t) thing *)
					   	let (*   - 4c1: assign the instrumented expr to tmp_ptr *)
						    val tp_exp = wrapEXPR(Ast.Void, tp_cexp)
						    val outer_inst_exp = wrapEXPR(Ast.Void, outerInstCexp)
						    val addrof_exp = wrapEXPR(Ast.Void,
									Ast.AddrOf (outer_inst_exp))
						    val tp_assign_exp = wrapEXPR(Ast.Void,
									Ast.Assign (tp_exp,addrof_exp))

						    (*   - 4c2: final tmp_ptr *)
						    val tp_exp = wrapEXPR(Ast.Void, tp_cexp)
						    val tp_cast_exp = wrapEXPR(Ast.Void,
									Ast.Cast(Ast.Pointer exp_type,
										 tp_exp))
						in
						    (tp_assign_exp,
						     (first_exp::remaining_exps) @ [tp_cast_exp], true)
						end
					)
				    (*   - commafy *)
				    val commafied_exp = commafyExprs(first_exp, remaining_exps)
				    (* --- if doderef, dereference *)
				    val final_cexp = if doderef
						     then Ast.Deref commafied_exp
						     (* Note: this is a hackish way to do this *)
						     else let val Ast.EXPR (cexp,_,_) = commafied_exp
							  in  cexp
							  end
				in  final_cexp
				end
			)
		)
	in
	    (* Return the constructed EXPR. *)
	    Ast.EXPR (final_cexp,exp_aid,exp_loc)
	end
	before popLoc())

  and instrRValueExpr (outer_exp as (Ast.EXPR (coreExpr,exp_aid,exp_loc)),
			{enforce:bool,needrval:bool,skip_call:bool}, tagptr_cexpOp) = 
      ( if !trace then ppToStdOut PPA.ppExpression outer_exp else ();
 	pushLoc exp_loc;
	let
	    val isptrptrw = (!Flags.instrMode = Flags.IM_PTR) orelse (!Flags.instrMode = Flags.IM_PTRW)

	in case coreExpr
	     of Ast.IntConst li =>
		(case tagptr_cexpOp
		   of SOME tagptr_cexp =>
		      if !Flags.optimizeCopyTag
		      then (Ast.EXPR (Ast.IntConst li,exp_aid,exp_loc), {is_zero=(li=0),
									 stype_op=SOME (lookupExprType outer_exp),
									 ctrvlist=nil})
		      else let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=(li=0)})
			       val const_exp = wrapEXPR(Ast.Void, Ast.IntConst li)
			       val comma_cexp = Ast.Comma(assign_exp, const_exp)
			   in
			       (Ast.EXPR (comma_cexp,exp_aid,exp_loc), {is_zero=(li=0),stype_op=NONE,ctrvlist=nil})
			   end
		    | NONE =>
			(Ast.EXPR (Ast.IntConst li,exp_aid,exp_loc), {is_zero=(li=0),stype_op=NONE,ctrvlist=nil})
		)
	      | Ast.RealConst r =>
		(case tagptr_cexpOp
		   of SOME tagptr_cexp =>
		      if !Flags.optimizeCopyTag
		      then (Ast.EXPR (Ast.RealConst r,exp_aid,exp_loc), {is_zero=false,
									 stype_op=SOME (lookupExprType outer_exp),
									 ctrvlist=nil})
		      else let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
			       val const_exp = wrapEXPR(Ast.Void, Ast.RealConst r)
			       val comma_cexp = Ast.Comma(assign_exp, const_exp)
			   in
			       (Ast.EXPR (comma_cexp,exp_aid,exp_loc), {is_zero=false,stype_op=NONE,ctrvlist=nil})
			   end
		    | NONE =>
			(Ast.EXPR (Ast.RealConst r,exp_aid,exp_loc), {is_zero=false,stype_op=NONE,ctrvlist=nil})
		)
	      | Ast.StringConst s =>
		let
		    (* if tagptr, assign pointer_static_rep_ptr *)
		    val (collected_exps,stypeOp) =
			(case tagptr_cexpOp
			    of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (nil, SOME (lookupExprType outer_exp))
				else ([newStaticTypeAssign (tagptr_cexp,
						lookupExprType outer_exp, {is_zero=false})], NONE)
			     | NONE => (nil, NONE)
			)

		    val (collected_exps,rval_cexp) =
			if aos_le_exposed (expToAO outer_exp)
			andalso ((!Flags.instrMode <> Flags.IM_PTRW) orelse (!Flags.strLitWritable))
			then let
				 val _ = (count_set_tag := (!count_set_tag) + 1)
				 (* Create tmp_val of type char * *)
				 val tv_id = addTmpValVar (outer_exp,{pointer=false})
				 (* Assign tmp_val = "StringConst" *)
				 val tv_exp = wrapEXPR(Ast.Void, Ast.Id tv_id)
				 val sc_exp = wrapEXPR(Ast.Void, Ast.StringConst s)
				 val tv_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(tv_exp,sc_exp))
				 (* Call _setStringTag *)
				 val tv_exp = wrapEXPR(Ast.Void, Ast.Id tv_id)
				 val sst_exp = newSetStringTagExpr(tv_exp, s)
				 (* rval_cexp = tmp_ptr *)
			     in  (collected_exps @ [tv_assign_exp, sst_exp],
				  Ast.Id tv_id)
			     end
			else (collected_exps, Ast.StringConst s)

		    val final_cexp = 
			case collected_exps
			  of nil => rval_cexp
			   | head::tail => Ast.Comma (commafyExprs(head, tail), wrapEXPR(Ast.Void, rval_cexp))

		in
		    (Ast.EXPR (final_cexp,exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		end

	      | Ast.Call (fun_exp as (Ast.EXPR (fun_cexp,fun_aid,fun_loc)),exps) =>
		(* Is this call to a builtin function? *)
		let
		    val skipCall = skip_call orelse (hasClashingCalls outer_exp) (* just to be ultra conservative *)

		    datatype function_class =
			FC_ALLOCA | FC_SCANF | FC_XSCANF | FC_BUILTIN | FC_TYPECHECK | FC_NORMAL

		    val fclass = case fun_cexp
				   of Ast.Id (id as {name=fun_sym,...}) =>
					 let val fun_name = Sym.name fun_sym
					 in
					   if (fun_name = "alloca")
					      orelse (fun_name = "__builtin_alloca") then FC_ALLOCA
					   else if (fun_name = "scanf") then FC_SCANF
					   else if (fun_name = "fscanf")
						   orelse (fun_name = "sscanf") then FC_XSCANF
					   else if (String.isPrefix "__builtin_" fun_name) then FC_BUILTIN
					   else if (String.isPrefix "_typecheck_" fun_name) then FC_TYPECHECK
					   else FC_NORMAL
					 end
				    | _ => FC_NORMAL

		    (*************************************************)
		    (* function used by non-instrumented cases below *)
		    (*************************************************)
		    fun instrArgExprs(nil) = nil
		      | instrArgExprs(exp1::tail) = 
			let val (exp1',_) = instrRValueExpr
						(exp1, {enforce=false,needrval=true,skip_call=skipCall},NONE)
			in
			    exp1'::instrArgExprs(tail)
			end

		in (case fclass
		 of FC_ALLOCA =>
		    (* -- t1 = alloca(t2 = [size]), setUninitTag(t1,t2), (tagptr = static_type), t1 *)
		    if aos_le_exposed [Rtc.aoMalloc exp_aid]
		    then(let
			     val _ = (count_set_tag := (!count_set_tag) + 1)
			     (* hackery: push tmpvar list, then pop, so that t1 and t2 are not declared in innermost
				containing scope.  Instead, store t1 and t2 onto alloca_tmps list, which will be
				processed by instrFunctionDef at the function scope level *)
			     val _ = pushTmpVarList ()
			     val t1_id = addTmpValVarByType (Ast.Pointer Ast.Void)
			     val t2_id = addTmpValVarByType stdInt
			     val _ = popTmpVarList ()
			     val _ = alloca_tmps := [t1_id,t2_id] @ (!alloca_tmps)
			     val _ = alloca_ct_exps := (if (!Flags.indivClearTag)
							then [newSetTagExpr clearTag ( wrapEXPR(Ast.Void, Ast.Id t1_id)
										     , wrapEXPR(Ast.Void, Ast.Id t2_id)
										     , exp_loc)]
							else nil) @ (!alloca_ct_exps)

			     val exps' = (case exps
					    of exp1::tail =>
						let val _ = case tail  of nil => ()
									| _ => (warning "instrRValueExpr:Call"
										"alloca called with >1 arguments!\n")
						    val t2_exp = wrapEXPR(Ast.Void, Ast.Id t2_id)
						    val (exp1',_) = instrRValueExpr
									(exp1, {enforce=false,needrval=true,skip_call=skipCall},NONE)
						    val assign_exp = wrapEXPR(Ast.Void, Ast.Assign(t2_exp, exp1'))
						in
						    [assign_exp]
						end
					     | _ => ( warning "instrRValueExpr:Call" "alloca called with no arguments!\n"
						    ; exps)
					 )
			     val t1_exp = wrapEXPR(Ast.Void, Ast.Id t1_id)
			     val call_exp = wrapEXPR(Ast.Void, Ast.Call (fun_exp,exps'))
			     val assign_exp = wrapEXPR(Ast.Void, Ast.Assign(t1_exp, call_exp))

			     val sut_id = getGlobalFun setUninitTag
			     val sut_exp = wrapEXPR(Ast.Void, Ast.Id sut_id)
			     val t1_exp = wrapEXPR(Ast.Void, Ast.Id t1_id)
			     val t2_exp = wrapEXPR(Ast.Void, Ast.Id t2_id)
			     val sut_call_exp = newInstCallExpr (sut_exp,[t1_exp,t2_exp])

			     val comma1_exp = wrapEXPR(Ast.Void, Ast.Comma(assign_exp, sut_call_exp))
			     val (comma2_exp,stypeOp) = 
				 case tagptr_cexpOp
				   of SOME tagptr_cexp =>
				      if !Flags.optimizeCopyTag
				      then (comma1_exp, SOME (lookupExprType outer_exp))	(*TODO: instead of looking up type,	*)
				      else (wrapEXPR ( Ast.Void				(*	why not just set to void *?	*)
						     , Ast.Comma ( comma1_exp
								 , newStaticTypeAssign ( tagptr_cexp
											, lookupExprType outer_exp
											, {is_zero=false})
								 )), NONE)
				    | NONE => (comma1_exp,NONE)

			     val t1_exp = wrapEXPR(Ast.Void, Ast.Id t1_id)
			 in
			    (Ast.EXPR (Ast.Comma (comma2_exp,t1_exp),exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
			 end
			)
		    else(case tagptr_cexpOp
			   of SOME tagptr_cexp =>
			      if !Flags.optimizeCopyTag
			      then (Ast.EXPR ( Ast.Call (fun_exp,instrArgExprs exps)
					     , exp_aid, exp_loc)
				   , {is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
			      else let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
				       val call_exp = wrapEXPR(Ast.Void, Ast.Call (fun_exp,instrArgExprs exps))
				   in
				       (Ast.EXPR ( Ast.Comma(assign_exp, call_exp)
						 , exp_aid, exp_loc)
				       , {is_zero=false,stype_op=NONE,ctrvlist=nil})
				   end
			    | NONE => (Ast.EXPR ( Ast.Call (fun_exp, instrArgExprs exps),exp_aid,exp_loc)
						, {is_zero=false,stype_op=NONE,ctrvlist=nil})
			)

		  | FC_BUILTIN =>
		    (* If this call is to a builtin function, don't instrument it
		       because the function's address can't be taken. *)
		     (case tagptr_cexpOp
			of SOME tagptr_cexp =>
			   if !Flags.optimizeCopyTag
			   then (Ast.EXPR ( Ast.Call (fun_exp,instrArgExprs exps)
					  , exp_aid, exp_loc)
				, {is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
			   else let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
				    val call_exp = wrapEXPR(Ast.Void, Ast.Call (fun_exp,instrArgExprs exps))
				in
				    (Ast.EXPR ( Ast.Comma(assign_exp, call_exp)
					      , exp_aid, exp_loc)
				    , {is_zero=false,stype_op=NONE,ctrvlist=nil})
				end
			 | NONE =>
			    (Ast.EXPR (Ast.Call (fun_exp, instrArgExprs exps),exp_aid,exp_loc),
						{is_zero=false,stype_op=NONE,ctrvlist=nil})
		     )

		  | FC_TYPECHECK =>
		    (* One of our wrapper library function: set global errlocs, and skip the argtag stuff *)
		    let
			(* RTC malloc calls: if tsl is exposed, replace *alloc with *alloc_init *)
			(* ptr/w malloc calls: if pmbu, replace *alloc with *alloc_zero *)
			(* BOTH malloc calls: if tsl is safe, strip out _typecheck_ prefix *)
			(* free: lookup freearg status, choose one of free,_typecheck_free,_typecheck_free_partial *)
			(* general: strip _typecheck_ if noins_libfn_lookup *)
			val (fun_cexp',is_inst) =
			    case fun_cexp
			      of Ast.Id funid =>
				 let fun cvtInitIfExposed (name,id,isinst) =
					 if isptrptrw
					 then if (!Flags.nullifyAllAutos)
					      orelse ((!Flags.nullifyMBUptrs)
						      andalso (vp_may_be_uninit_ao (Rtc.aoMalloc exp_aid)))
					      then (name ^ "_zero",getGlobalFun (name ^ "_zero"),isinst)
					      else (name,id,isinst)
					 else if aos_eq_exposed [Rtc.aoMalloc exp_aid]
					      then (name,getGlobalFun (name ^ "_init"),isinst)
					      else (name,id,isinst)

				     fun stripTCifSafe (name,id,_) =
					 if aos_eq_safe [Rtc.aoMalloc exp_aid]
					 then ( (count_malloc_elided := (!count_malloc_elided) + 1)
					      ; (name,getGlobalFun (String.extract (name, 11, NONE)),false) )
					 else ( (count_malloc := (!count_malloc) + 1)
					      ; (count_set_tag := (!count_set_tag) + 1)
					      ; (name,id,true) )

				     val (_,newfunid,is_inst) =
					 (case (Sym.name (#name (funid)))
					     (* *alloc *)
					    of "_typecheck_malloc" =>
						stripTCifSafe (cvtInitIfExposed ("_typecheck_malloc",funid,true))
					     | "_typecheck_calloc" =>
						stripTCifSafe ("_typecheck_calloc",funid,true)
					     | "_typecheck_memalign" =>
						stripTCifSafe (cvtInitIfExposed ("_typecheck_memalign",funid,true))
					     | "_typecheck_realloc" =>
						stripTCifSafe (cvtInitIfExposed ("_typecheck_realloc",funid,true))
					     | "_typecheck_valloc" =>
						stripTCifSafe (cvtInitIfExposed ("_typecheck_valloc",funid,true))

					     (* free *)
					     | "_typecheck_free" => (* free case is special: must lookup aliases of *arg1 *)
						(case exps
						   of exp1::tail =>
							(case (freearg_lookup_fn (expToAO exp1))
							   of Rtc.ES_ALL =>
								( (count_free := (!count_free) + 1)
								; ("",funid,true) )
							    | Rtc.ES_SOME =>
								( (count_free_partial := (!count_free_partial) + 1)
								; ("",getGlobalFun "_typecheck_free_partial",true) )
							    | Rtc.ES_NONE =>
								( (count_free_elided := (!count_free_elided) + 1)
								; ("",getGlobalFun "free",false) )
							)
						    | nil =>
							( warning "instrRValueExpr:Call:free" "calling free with no arguments\n"
							;  ("",funid,true) )
						)

					     | fnname =>
						if noins_libfn_lookup (!Flags.vuln) exp_aid
						then (* STRIP _typecheck_ *)
						     ( (count_libfn_elided := (!count_libfn_elided) + 1)
						     ; (fnname,getGlobalFun (String.extract (fnname, 11, NONE)),true) )
						else ( (count_libfn := (!count_libfn) + 1)
						     ; (fnname,funid,true) )
					 )
				 in  (Ast.Id newfunid,is_inst)  end
			       | _ => (fun_cexp,true)
			val fun_exp' = Ast.EXPR (fun_cexp',fun_aid,fun_loc)
			val arg_exp' = instrArgExprs exps

			val (fncall_cexp,stypeOp) =
			     (case tagptr_cexpOp
				of SOME tagptr_cexp =>
				   if !Flags.optimizeCopyTag
				   then ( Ast.Call(fun_exp', arg_exp'), SOME (lookupExprType outer_exp))
				   else ( Ast.Comma( newStaticTypeAssign (tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
						   , wrapEXPR(Ast.Void, Ast.Call (fun_exp', arg_exp'))
						   ), NONE)
				 | NONE =>
				    ( Ast.Call(fun_exp', arg_exp'), NONE)
			     )

			(* Assign to globalErrLoc, only if instrumentation function is called, and not in ptr/ptrw mode *)
			val gErrLoc_expOp =
			    if isptrptrw 
			    orelse not is_inst
			    then NONE
			    else let val (fname, lineno, colno) =
					    case exp_loc
					      of SourceMap.LOC{srcFile,beginLine,beginCol,...} =>
						(srcFile, beginLine, beginCol)
					       | SourceMap.UNKNOWN => ("unknown",0,0)

					(* -- _globalErrlocFile = file -- *)
				     val gef_id = getGlobalObj "_globalErrlocFile"
				     val gef_exp = wrapEXPR(Ast.Void, Ast.Id gef_id)
				     val fname_exp = wrapEXPR(Ast.Void, Ast.StringConst fname)
				     val gef_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gef_exp, fname_exp))
				     (* -- _globalErrlocLine = line -- *)
				     val gel_id = getGlobalObj "_globalErrlocLine"
				     val gel_exp = wrapEXPR(Ast.Void, Ast.Id gel_id)
				     val line_exp = wrapEXPR(Ast.Void, Ast.IntConst (LargeInt.fromInt lineno))
				     val gel_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gel_exp, line_exp))
				     (* -- _globalErrlocCol = col -- *)
				     val gec_id = getGlobalObj "_globalErrlocCol"
				     val gec_exp = wrapEXPR(Ast.Void, Ast.Id gec_id)
				     val col_exp = wrapEXPR(Ast.Void, Ast.IntConst (LargeInt.fromInt colno))
				     val gec_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gec_exp, col_exp))

				     val comma1_exp = wrapEXPR(Ast.Void, Ast.Comma(gef_assign_exp, gel_assign_exp))
				     val comma2_exp = wrapEXPR(Ast.Void, Ast.Comma(comma1_exp, gec_assign_exp))
				 in  SOME comma2_exp  end

		    in
			(Ast.EXPR (case gErrLoc_expOp
				     of SOME gErrLoc_exp =>
					Ast.Comma (gErrLoc_exp, wrapEXPR (Ast.Void, fncall_cexp))
				      | NONE => fncall_cexp
				  ,exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		    end
(*
		  | FC_SCANF =>
		  | FC_XSCANF =>
		  | FC_NORMAL =>
*)
		  | _ =>
		   (( case fun_cexp
			of (* Is the call to a (so far) undeclared function? *)
			   Ast.Id (id as {status=Ast.IMPLICIT,...}) => pushUndeclFuns(id)
				(* Insert this id into the table of undeclared functions.
				   Later it will have its prototype inserted in the file
				   so that its address can be taken. *)
			 | _ => ()
		    ) ; (
		    if isptrptrw
		    then	(* -->  do verifyFnPtr; skip for now <-- *)
		      let val (fun_exp',_) = instrRValueExpr (fun_exp,{enforce=true,needrval=true,skip_call=false},NONE)

			  fun processArgs nil = nil
			    | processArgs (exp::tail) =
			      let val (exp',_) = instrRValueExpr (exp, {enforce=true,needrval=true,skip_call=skipCall}, NONE)
			      in  exp'::(processArgs tail)
			      end

			  val exps' = processArgs exps

			  val call_cexp = Ast.Call (fun_exp', exps')
		      in
			  (Ast.EXPR (call_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
		      end

		    else if skip_call (* hack to get around GCC "strict C specs compliance" *)
		    then
		      let val (fun_exp',_) = instrRValueExpr (fun_exp,{enforce=true,needrval=true,skip_call=false},NONE)

			  fun processArgs nil = nil
			    | processArgs (exp::tail) =
			      let val (exp',_) = instrRValueExpr (exp, {enforce=true,needrval=true,skip_call=skipCall}, NONE)
			      in  exp'::(processArgs tail)
			      end

			  val exps' = processArgs exps

(**)			  val call_cexp = Ast.Call (fun_exp', exps')
(**) val comma_cexp = call_cexp
(*
			  val call_exp = wrapEXPR(Ast.Void, Ast.Call (fun_exp', exps'))

			  (* -- _globalCallTarget = 0 -- *)
			  val gct_id = getGlobalObj "_globalCallTarget"
			  val gct_exp = wrapEXPR(Ast.Void, Ast.Id gct_id)
			  val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			  val gct_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gct_exp, zero_exp))

			  val comma_cexp = Ast.Comma (gct_assign_exp, call_exp)
*)
			  val (final_cexp,stypeOp) =
			      case tagptr_cexpOp
				of SOME tagptr_cexp =>
				   if !Flags.optimizeCopyTag
				   then (comma_cexp, SOME (lookupExprType outer_exp))
				   else (Ast.Comma (wrapEXPR(Ast.Void, comma_cexp),
						    newStaticTypeAssign (tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
						   ), NONE)
				 | NONE => (comma_cexp, NONE)
		      in
			  (Ast.EXPR (final_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		      end
		    else
		      let

			(* -- tmp_fptr = [fun_exp,T,none] -- *)
			val tfptr_id = addTmpValVar (fun_exp,{pointer=false})
			val tfptr_exp1 = wrapEXPR(Ast.Void, Ast.Id tfptr_id)
			val (fun_exp',_) = instrRValueExpr (fun_exp,{enforce=true,needrval=true,skip_call=false},NONE)
			val tfptr_assign_exp = wrapEXPR(Ast.Void, Ast.Assign (tfptr_exp1, fun_exp'))

			val first_exp = tfptr_assign_exp

			(* -- tmp variable: _addr_and_size_t tmp_args[num_args + 1] -- *)
			val arr_size = LargeInt.fromInt(length(exps) + 1)
			val tmp_args_id = addTmpAddrSizeArray arr_size

			(* ctype to be used by getStructMember *)
			val addr_size_ty = lookupTypeDefType "_addr_and_size_t"

			(********************************************
			 processArgs(index     : int
				     args      : Ast.expression list)
			    -> ( inst_args     : Ast.expression list
			         tmpvals       : Ast.expression list
			         clear_tmpvals : Ast.expression list )
			 ********************************************
			  generates the ( tmp_vali = [<ei>,F,some tmp_args[i].addr],
			  tmp_args[i].size = sizeof(tmp_vali) )
			  part (inst_args) and a list of tmp_vals
			  (tmpvals) as arguments for the upcoming
			  instrumented function call.
			 ********************************************)
			fun processArgs(n,nil) = (nil, nil,nil)
			  | processArgs(i,(exp::tail)) =
			    let
				(* -- precomputation -- *)
				(* If exp is cast implicitly, make it explicit. *)
				val exp' = makeCastExplicit exp

				(* HACK: ckit does not currently implicitly cast arrays into
				   pointers, so do it ourselves *)
				val exp'_type = lookupExprType exp'
				val exp' = if isArray exp'_type
					   then let val ptrified_type = Ast.Pointer (derefOneLevel exp'_type)
						in  makeCastExpr (exp',ptrified_type) end
					   else exp'

				(* tmp_val for this argument *)
			        val tmpval_id = addTmpValVar (exp',{pointer=false})

				(* -- tmp_val = [<exp>,F,some tmp_args[i].addr] -- *)
				val tmpval_exp = wrapEXPR(Ast.Void, Ast.Id tmpval_id)
				val tagptr_cexp = newAsubIdotFcexpr (tmp_args_id, i,
								     getStructMember (addr_size_ty,"addr"))

				val (inst_exp',{stype_op=stypeOp,is_zero=iszero,ctrvlist=arg_ctrvs}) =
						instrRValueExpr (exp', {enforce=false,needrval=true,skip_call=skipCall},
									SOME tagptr_cexp)

				val tmpval_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(tmpval_exp,inst_exp'))

				val (sofar_exp,clear_tmpvals) =
				    (case stypeOp
				       of SOME stype =>
					  if !Flags.optimizeCopyTag
					  then
					       if (getTypeString stype) = "aggregate"
					       then (* -- tmpval = [<exp>], initTag(tmpval), args[i].ptr = &tmpval -- *)
						    let val inittag_exp = initTag_id(tmpval_id, MODE_DEFAULT)
							val comma1_exp = wrapEXPR(Ast.Void, Ast.Comma(tmpval_assign_exp, inittag_exp))

							val tmpval_exp = wrapEXPR(Ast.Void, Ast.Id tmpval_id)
							val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf tmpval_exp)
							val tagptr_exp = wrapEXPR(Ast.Void, tagptr_cexp)
							val tagptr_assign_exp = wrapEXPR(Ast.Void, Ast.Assign (tagptr_exp,addrof_exp))

							(* in -indivClearTags, must clear tmpval's tag; prebuild the clearTag expr *)
							val clear_tmpval_exps = if (!Flags.indivClearTag)
										then [newSetTagExpr_id clearTag tmpval_id]
										else nil
						    in
							( wrapEXPR(Ast.Void, Ast.Comma(comma1_exp, tagptr_assign_exp))
							, clear_tmpval_exps )
						    end
					       else (* -- args[i].ptr = static_repptr, tmpval = [<exp>] -- *)
						    let val argptr_assign_exp = newStaticTypeAssign(tagptr_cexp,stype, {is_zero=iszero})
						    in  ( wrapEXPR(Ast.Void, Ast.Comma(argptr_assign_exp, tmpval_assign_exp))
							, nil )
						    end
					  else ( tmpval_assign_exp, nil )
				        | NONE => ( tmpval_assign_exp, nil )
				    )

				(* -- tmp_args[i].size = sizeof(typeof exp') -- *)
				val tmp_i_size_cexp = newAsubIdotFcexpr (tmp_args_id, i,
								getStructMember (addr_size_ty,"size"))
				val tmp_i_size_exp = wrapEXPR(Ast.Void, tmp_i_size_cexp)
				val exp'_type = lookupExprType exp'
				val sizeof_exp = wrapEXPR (Ast.Void, Ast.SizeOf exp'_type)
				val size_assign_exp = wrapEXPR(Ast.Void,
								Ast.Assign(tmp_i_size_exp,sizeof_exp))

				(* -- (sofar_exp,size_assign_exp) -- *)
				val comma_exp = wrapEXPR(Ast.Void,
							 Ast.Comma(sofar_exp,size_assign_exp))

				(* -- Process tail -- *)
				val (inst_tail,tail_tmpvals,tail_clears) = processArgs((i+1),tail)
				(* -- Create exp for tmpval to return -- *)
				val tmpval_exp = wrapEXPR(Ast.Void, Ast.Id tmpval_id)
			    in
				(comma_exp::inst_tail, tmpval_exp::tail_tmpvals, arg_ctrvs @ clear_tmpvals @ tail_clears)
			    end

			(* -- process arguments -- *)
			val (inst_exps,tmpval_exps,clear_tmpval_exps) = processArgs(1,exps)

			val assembled_exps = inst_exps

			(* -- _globalArgAddrs = tmpargs -- *)
			val gat_id = getGlobalObj "_globalArgAddrs"
			val gat_exp = wrapEXPR(Ast.Void, Ast.Id gat_id)
			val tmpargs_exp = wrapEXPR(Ast.Void, Ast.Id tmp_args_id)
			val gat_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gat_exp, tmpargs_exp))
			(* -- _globalArgCount = num_args -- *)
			val gac_id = getGlobalObj "_globalArgCount"
			val gac_exp = wrapEXPR(Ast.Void, Ast.Id gac_id)
			val num_args = LargeInt.fromInt(length(exps))
			val num_args_exp = wrapEXPR(Ast.Void, Ast.IntConst num_args)
			val gac_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gac_exp, num_args_exp))
			(* -- _globalCallTarget = (void * ) tfptr -- *)
			val gct_id = getGlobalObj "_globalCallTarget"
			val gct_exp = wrapEXPR(Ast.Void, Ast.Id gct_id)
			val tfptr_exp2 = wrapEXPR(Ast.Void, Ast.Id tfptr_id)
			val cast_exp = wrapEXPR(Ast.Void, Ast.Cast(Ast.Pointer Ast.Void, tfptr_exp2))
			val gct_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gct_exp, cast_exp))

			val assembled_exps = assembled_exps @ [gat_assign_exp,gac_assign_exp,gct_assign_exp]

			(* -- tfptr(...tmpvals...) -- *)
			val tfptr_exp3 = wrapEXPR(Ast.Void, Ast.Id tfptr_id)
			val funcall_exp = wrapEXPR(Ast.Void, Ast.Call (tfptr_exp3,tmpval_exps))

			val enforce_local = enforce andalso not isptrptrw
					    andalso (aos_badly_typed (expToAO outer_exp)
						     orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify))

			val (remaining_exps,last_exp,ctrvs) =
			    if not enforce_local
			    andalso not (isSome tagptr_cexpOp)
			    andalso (null clear_tmpval_exps (* non-null only if indivClearTags with aggr argument *)
				     orelse not needrval)
			    then (* trivial case: [F,NONE] *)
			       let
				 (* -- tmp_args[0].size = 0 -- *)
				 val tmp0_size_cexp = newAsubIdotFcexpr (tmp_args_id, 0,
								getStructMember (addr_size_ty,"size"))
				 val tmp0_size_exp = wrapEXPR(Ast.Void, tmp0_size_cexp)
				 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
				 val size_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(tmp0_size_exp,zero_exp))

				 fun split_tail (head,nil) = (nil, head)
				   | split_tail (head,mid::tail) =
				     let val (mid',tail') = split_tail (mid,tail)
				     in  (head::mid', tail')  end

				 val (exps,last_exp) = split_tail (funcall_exp, clear_tmpval_exps)
			       in
				 (size_assign_exp::exps, last_exp, nil)
			       end
			    else
			       let
				 (* -- create tmp_retval -- *)
				 val trv_id as {
					   ctype=trv_type,...
					} = addTmpValVar (outer_exp,{pointer=false})

				 (* -- tmp_args[0].addr = &tmp_retval -- *)
				 val tmp0_addr_cexp = newAsubIdotFcexpr (tmp_args_id, 0,
								getStructMember (addr_size_ty,"addr"))
				 val tmp0_addr_exp = wrapEXPR(Ast.Void, tmp0_addr_cexp)
				 (* &tmp_retval *)
				 val trv_exp = wrapEXPR(Ast.Void, Ast.Id trv_id)
				 val addr_trv_exp = wrapEXPR(Ast.Void, Ast.AddrOf (trv_exp))
				 val addr_assign_exp = wrapEXPR(Ast.Void,
								Ast.Assign(tmp0_addr_exp,addr_trv_exp))

				 val common_exps = [addr_assign_exp]

				 (* -- tmp_args[0].size = sizeof(tmp_retval) -- *)
				 val tmp0_size_cexp = newAsubIdotFcexpr (tmp_args_id, 0,
								getStructMember (addr_size_ty,"size"))
				 val tmp0_size_exp = wrapEXPR(Ast.Void, tmp0_size_cexp)
				 val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf trv_type)
				 val size_assign_exp = wrapEXPR(Ast.Void,
								Ast.Assign(tmp0_size_exp,sizeof_exp))

				 val common_exps = common_exps @ [size_assign_exp]

				 (* -- initTag(&tmp_retval) -- *)
				 val inittag_exp = initTag_id(trv_id, MODE_DEFAULT)
				 (* -- prebuild clearTags(&tmp_retval) -- *)
							
				 val cleartrv_exps = if (!Flags.indivClearTag)
						     then [newSetTagExpr_id clearTag trv_id]
						     else nil

				 val common_exps = common_exps @ [inittag_exp]

				 (* -- tmp_retval = tfptr(tmpvals) -- *)
				 val trv_exp = wrapEXPR(Ast.Void, Ast.Id trv_id)
				 val funcall_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(trv_exp,funcall_exp))

				 val common_exps = common_exps @ [funcall_assign_exp]

				 (* -- append clear_tmpval_exps, from above (aggregate arguments) *)
				 val common_exps = common_exps @ clear_tmpval_exps

				 (* -- &tmp_retval -- *)
				 val trv_exp = wrapEXPR(Ast.Void, Ast.Id trv_id)
				 val addr_trv_cexp = Ast.AddrOf (trv_exp)

				 (* -- (assign_exps, addr_trv_cexp) -- *)
				 (*    if SOME then ([tagptr = &trv], tagptr) *)
				 (*    if NONE then ([], &trv)                *)
				 val (assign_exps,addr_trv_cexp) =
				     case tagptr_cexpOp
				       of SOME tagptr_cexp =>
					       let (* -- tagptr = &trv -- *)
						   val tagptr_exp = wrapEXPR(Ast.Void, tagptr_cexp)
						   val addr_trv_exp = wrapEXPR(Ast.Void, addr_trv_cexp)
						   val assign_exp = wrapEXPR(Ast.Void,
									Ast.Assign (tagptr_exp,addr_trv_exp))
					       in
						   ([assign_exp],tagptr_cexp)
					       end
					| NONE => (nil, addr_trv_cexp)

				 (* -- verifyTag(oex, addrof_trv, ret_ty) -- *)
				 val verify_exps =
				     if enforce_local		(* SY: not redundant-elidable? *)
				     then [newVerifyTagCallExpr (outer_exp, addr_trv_cexp, trv_type)]
				     else nil

				 val (ctrv_now,ctrv_defer) = if isSome tagptr_cexpOp then (nil, cleartrv_exps)
										     else (cleartrv_exps, nil)
				 (* -- tmp_retval -- *)
				 val trv_exp = wrapEXPR(Ast.Void, Ast.Id trv_id)
			       in
				 ((common_exps @ assign_exps @ verify_exps @ ctrv_now),
				  trv_exp, ctrv_defer)
			       end

			val commafied_exp = commafyExprs(first_exp, assembled_exps @ remaining_exps)
			val last_comma_cexp = Ast.Comma (commafied_exp,last_exp)
		      in
			(Ast.EXPR (last_comma_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=ctrvs})
		      end
		    ))
	        ) end
	      | Ast.QuestionColon (e1,e2,e3) =>
		let
		   (* Instrument e1: [e1,T,none] *)
	           val (e1',_) = instrRValueExpr (e1,{enforce=true,needrval=true,skip_call=false},NONE)

		   (* If e2 is cast implicitly, make it explicit *)
		   val e2_cast_exp = makeCastExplicit e2

		   (* Instrument e2: [e2,enforce,tagptr] *)
		   val (e2',{stype_op=e2stypeOp,is_zero=is2zero,ctrvlist=e2ctrvs}) =
					instrRValueExpr (e2_cast_exp,
							  {enforce=enforce,
							   needrval=(if !Flags.preserveQCrval
								     then true else needrval),
							  skip_call=false},
							  tagptr_cexpOp)

		   (* If e3 is cast implicitly, make it explicit *)
		   val e3_cast_exp = makeCastExplicit e3

		   (* Instrument e3: [e3,enforce,tagptr] *)
		   val (e3',{stype_op=e3stypeOp,is_zero=is3zero,ctrvlist=e3ctrvs}) =
					instrRValueExpr (e3_cast_exp,
							  {enforce=enforce,
							   needrval=(if !Flags.preserveQCrval
								     then true else needrval),
							  skip_call=false},
							  tagptr_cexpOp)

		   val this_ctrvs = e2ctrvs @ e3ctrvs

		   val (e2',e3',synth_attrs) =
			(case tagptr_cexpOp
			   of SOME tagptr_cexp =>
			      (case e2stypeOp
				 of SOME e2stype =>
				    (case e3stypeOp
				       of SOME e3stype =>
					  if (tcEqualType (e2stype, e3stype) andalso (is2zero andalso is3zero))
					  then (e2',e3',{stype_op=SOME e2stype, is_zero=true, ctrvlist=this_ctrvs})
					  else let val e2'' = wrapEXPR(Ast.Void, Ast.Comma(
								newStaticTypeAssign(tagptr_cexp,e2stype,{is_zero=is2zero}),
											   e2'))
						   val e3'' = wrapEXPR(Ast.Void, Ast.Comma(
								newStaticTypeAssign(tagptr_cexp,e3stype,{is_zero=is3zero}),
											   e3'))
					       in  (e2'',e3'',{stype_op=NONE, is_zero=false, ctrvlist=this_ctrvs})
					       end
					| NONE =>
					  let val e2'' = wrapEXPR(Ast.Void, Ast.Comma(
								newStaticTypeAssign(tagptr_cexp,e2stype,{is_zero=is2zero}),
											   e2'))
					  in  (e2'',e3',{stype_op=NONE, is_zero=false, ctrvlist=this_ctrvs})
					  end
				    )
				  | NONE =>
				    (case e3stypeOp
				       of SOME e3stype =>
					  let val e3'' = wrapEXPR(Ast.Void, Ast.Comma(
								newStaticTypeAssign(tagptr_cexp,e3stype,{is_zero=is3zero}),
											   e3'))
					  in  (e2',e3'',{stype_op=NONE, is_zero=false, ctrvlist=this_ctrvs})
					  end
					| NONE => (e2',e3',{stype_op=NONE, is_zero=false, ctrvlist=this_ctrvs})
				    )
			      )
			    | NONE => (e2',e3',{stype_op=NONE, is_zero=false, ctrvlist=this_ctrvs})
			)
		in
		   (Ast.EXPR (Ast.QuestionColon (e1',e2',e3'),exp_aid,exp_loc), synth_attrs)
		end
	      | Ast.Assign (e1,e2) =>
		if isptrptrw
		orelse ((not (aos_influential (expToAO e1))
			 orelse (vt_redundant_aid Rtc.RED_Assign exp_aid)
			) andalso not (may_be_uninit_aid (exp_aid,Rtc.MBU_Assign)))
		then let (* e1: [e1,F,NONE] *)
			 val inst_e1 = instrLValueExpr (e1,{enforce=false,verify_ptr=true,rval=false,skip_call=false,is_write=true},NONE)
			 (* e2: [<e2>,enforce,tagptr_cexpOp] *)
			 val e2_cast_exp = makeCastExplicit e2
			 val (inst_e2,{stype_op=stypeOp,is_zero=iszero,ctrvlist=e2ctrvs}) =
					instrRValueExpr (e2_cast_exp,{enforce=enforce,needrval=true,skip_call=false},tagptr_cexpOp)
		     in
			 (Ast.EXPR(Ast.Assign (inst_e1, inst_e2),exp_aid,exp_loc),
				{is_zero=iszero,stype_op=stypeOp,ctrvlist=e2ctrvs})
		     end
		else
		 let
		   val _ = if !Flags.reportStructAssigns
			      andalso isSome (isStructOrUnion (lookupExprType outer_exp))
			   then print ("STRUCT OR UNION ASSIGNMENT: " ^ (SourceMap.locToString exp_loc) ^ "\n")
			   else ()

	           (* Get the tagptr for instrumenting e1.
		      Use tagptr if it was passed in, else create a new tagptr temp. *)
		   val tagptr1_cexp = case tagptr_cexpOp
			of SOME tagptr_cexp => tagptr_cexp
			 | NONE => let val tagptr1_id = addTmpTagPtr()
				   in (Ast.Id tagptr1_id) end

		   (* Instrument e1: [e1,F,tagptr1] *)
		   val inst_e1 = instrLValueExpr (e1,{enforce=false,verify_ptr=true,rval=false,skip_call=false,is_write=true},
						     SOME tagptr1_cexp)

		   (* If e2 is cast implicitly, make it explicit *)
		   val e2_cast_exp = makeCastExplicit e2

		   (* Create a new tagptr temp for instrumenting e2. *)
		   val tagptr2_id = addTmpTagPtr()
		   val tagptr2_cexp = Ast.Id tagptr2_id

		   (* Instrument e2: [<e2>,enforce,tagptr2] *)
		   val (inst_e2,{stype_op=stypeOp,is_zero=iszero,ctrvlist=e2ctrvs}) =
					instrRValueExpr (e2_cast_exp,{enforce=enforce,needrval=true,skip_call=false},
							    SOME tagptr2_cexp)

		   (* Create EXPR for inst_e1 = inst_e2. *)
		   val e1_assign_cexp = Ast.Assign(inst_e1,inst_e2)
		   val e1_assign_exp = wrapEXPR(Ast.Void, e1_assign_cexp)

		   (* If needrval, do (tmp_val = assign,copyTag,tmp_val) *)
		   (* Else, do (assign,copyTag) *)
		   val (tv_assign_exp, tv_idOp) =
		       if needrval
		       then (* Create tmp_val of e1's type. *)
			    let val tv_id = addTmpValVar (e1,{pointer=false})

				(* Create EXPR for tmp_val. *)
				val tv_exp1 = wrapEXPR(Ast.Void, Ast.Id tv_id)

				(* Create EXPR for tmp_val = (inst_e1 = inst_e2). *)
				val tv_assign_cexp = Ast.Assign(tv_exp1,e1_assign_exp)
				val tv_assign_exp = wrapEXPR(Ast.Void, tv_assign_cexp)
			    in  (tv_assign_exp, SOME tv_id)
			    end
		       else (e1_assign_exp, NONE)

		   (* Create the call to _copyTag(tagptr1, tagptr2, sizeof e1). *)
		   val copytag_exp =
		      (case stypeOp
			 of SOME stype =>
			    (* optimize: call setScalarTag (or initTag, for aggregates) instead of copyTag *)
			    if (getTypeString stype) = "aggregate"
			    then (* initTag(tagptr1) *)
				 let fun tpbuilder () = wrapEXPR (Ast.Void, tagptr1_cexp)
				 in  initTag (tpbuilder, stype, exp_loc, MODE_DEFAULT)  end
			    else (* setScalarTag(tagptr1) *)
				 let val tagptr1_exp = wrapEXPR (Ast.Void, tagptr1_cexp)
				     val sst_id = getGlobalFun ("_setScalar" ^ (if iszero then "Init" else "")
								^ "Tag_" ^ (getTypeString stype))
				     val sst_exp = wrapEXPR(Ast.Void, Ast.Id sst_id)
				 in
				     newInstCallExpr (sst_exp,[tagptr1_exp])
				 end
			  | NONE =>
			    let
				(* Create EXPRs for _copyTag's arguments (tagptr1, tagptr2, sizeof e1). *)
				val tagptr1_exp = wrapEXPR (Ast.Void, tagptr1_cexp)
				val tagptr2_exp = wrapEXPR (Ast.Void, tagptr2_cexp)
				val e1_ty = lookupExprType e1
				val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf e1_ty)

				(* Create EXPR for _copyTag function. *)
				val e1ty_str = getTypeString e1_ty
				val ct_id = getGlobalFun ("_copyTag_" ^ e1ty_str)
				val ct_exp = wrapEXPR(Ast.Void, Ast.Id ct_id)
			    in
				newInstCallExpr (ct_exp,[tagptr1_exp,tagptr2_exp,sizeof_exp])
			    end
		      )

		   (* Append e2ctrvs after copytag *)
		    val copytag_ctrv_exp = commafyExprs(copytag_exp, e2ctrvs)

		   (* Create EXPR for (tv_assign_exp, copytag_exp). *)
		   val call_comma_cexp = Ast.Comma (tv_assign_exp, copytag_ctrv_exp)

		   (* Add trailing tmp_val only if needrval (i.e. if isSome tv_idOp) *)
		   val final_cexp =
		       case tv_idOp
			 of SOME tv_id =>
				let val call_comma_exp = wrapEXPR(Ast.Void, call_comma_cexp)
				    (* Create a new EXPR for tmp_val. *)
				    val tv_exp2 = wrapEXPR(Ast.Void, Ast.Id tv_id)
				    (* Create EXPR for (call_comma_exp, tv_exp2). *)
				    val tv_comma_cexp = Ast.Comma (call_comma_exp, tv_exp2)
				in  tv_comma_cexp
				end
			  | NONE => call_comma_cexp
		in
		   (Ast.EXPR(final_cexp,exp_aid,exp_loc),{is_zero=iszero,stype_op=stypeOp,ctrvlist=nil})
		end

	      | Ast.Comma (e1,e2) =>
		let
		    (* intercept va_arg calls, which the tc preprocessor tags
		       with a "tc_varg_dummy" id on the LHS of a comma expression *)
		in  if OPT.isVargDummy e1
		    then (* If tagptr, add tagptr = e2_statictype *)
			 (* For now, don't bother instrumenting e2 (the expanded va_arg macro) *)
			 case tagptr_cexpOp
			   of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (e2, {is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
				else let val tp_assign_exp = newStaticTypeAssign(tagptr_cexp,
								  lookupExprType outer_exp, {is_zero=false})
				     in
					(Ast.EXPR (Ast.Comma (tp_assign_exp,e2),exp_aid,exp_loc),
					 {is_zero=false,stype_op=NONE,ctrvlist=nil})
				     end
			    | NONE => (e2, {is_zero=false,stype_op=NONE,ctrvlist=nil})
		    else
			 let val (e1',_) = instrRValueExpr (e1,{enforce=false,needrval=false,skip_call=false},NONE)
			     val (e2',{stype_op=e2stypeOp,is_zero=e2iszero,ctrvlist=e2ctrvs}) =
					instrRValueExpr (e2,{enforce=enforce,needrval=needrval,skip_call=false},
								tagptr_cexpOp)
			 in
			     (Ast.EXPR (Ast.Comma (e1',e2'),exp_aid,exp_loc),
			      {is_zero=e2iszero,stype_op=e2stypeOp,ctrvlist=e2ctrvs})
			 end
		end
	      | Ast.Sub (e1,e2) =>
		let val skipCall = skip_call orelse (hasClashingCalls outer_exp)
		in if aos_badly_typed (expToAO outer_exp)
		   orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify)
		   then (instrLValueExpr (outer_exp,{enforce=enforce,verify_ptr=enforce,rval=true,skip_call=skipCall,is_write=false},
						    tagptr_cexpOp),
		      {is_zero=false,stype_op=NONE,ctrvlist=nil})
		   else let val (e1',_) = instrRValueExpr (e1,{enforce=true,needrval=true,skip_call=skipCall}, NONE)
			    val (e2',_) = instrRValueExpr (e2,{enforce=true,needrval=true,skip_call=skipCall}, NONE)
			in
			   case tagptr_cexpOp
			     of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (Ast.EXPR (Ast.Sub (e1',e2'),exp_aid,exp_loc),
					{is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
				else let val tp_assign_exp = newStaticTypeAssign(tagptr_cexp,
								  lookupExprType outer_exp, {is_zero=false})
					 val sub_exp = wrapEXPR(Ast.Void, Ast.Sub (e1',e2'))
				     in
					(Ast.EXPR (Ast.Comma (tp_assign_exp,sub_exp),exp_aid,exp_loc),
					 {is_zero=false,stype_op=NONE,ctrvlist=nil})
				     end
			      | NONE => (Ast.EXPR (Ast.Sub (e1',e2'),exp_aid,exp_loc),
					{is_zero=false,stype_op=NONE,ctrvlist=nil})
			end
		end

		(* For Member, if exp is not an lvalue, then do not forward, to instrLValueExpr;
		   this is the only case where LValue and RValue are handled differently *)
	      | Ast.Member (exp,mem) =>
		let
		    (* If exp.mem is statically an array, then do not enforce. *)
		    val exp_type = lookupExprType outer_exp
		    val enforce' = enforce andalso not (isArray exp_type)
					   andalso not isptrptrw
					   andalso (aos_badly_typed (expToAO outer_exp)
						    orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify))
		in
		    if not enforce' andalso not (isSome tagptr_cexpOp)
		    then (* trivial "false, none" case *)
			let val (inst_exp,_) =
				if (isArray exp_type)
				then (instrLValueExpr (exp,{enforce=true,verify_ptr=false,rval=true,skip_call=skip_call,is_write=false},
							   NONE),
					{stype_op=NONE,is_zero=false,ctrvlist=nil})
				else instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=skip_call},NONE)
			    val mem_cexp = Ast.Member (inst_exp, mem)
			in
			    (Ast.EXPR (mem_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
			end
		    else if isBitField (lookupExprType exp, mem)
		    then (* bitfield case: if tagptr, do tagptr = exp_type_statictype *)
			let val (inst_exp,_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=skip_call},NONE)
			    val mem_cexp = Ast.Member (inst_exp, mem)
			in
			    case tagptr_cexpOp
			      of SOME tagptr_cexp =>
				 let val assign_exp = newStaticTypeAssign(tagptr_cexp, exp_type, {is_zero=false})
				     val mem_exp = wrapEXPR(Ast.Void, mem_cexp)
				 in
				     (Ast.EXPR (Ast.Comma (assign_exp,mem_exp),exp_aid,exp_loc),
				      {is_zero=false,stype_op=NONE,ctrvlist=nil})
				 end
			       | NONE => (Ast.EXPR (mem_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
			end
		    else if OPT.isLvalExpr exp	(* exp is an lvalue --> forward to instrLValueExpr *)
		    then (instrLValueExpr (outer_exp,{enforce=enforce,verify_ptr=true,rval=true,skip_call=skip_call,is_write=false},
						      tagptr_cexpOp),
					{is_zero=false,stype_op=NONE,ctrvlist=nil})
		    else
			let val tagptr_cexp = case tagptr_cexpOp
						of SOME tagptr_cexp => tagptr_cexp
						 | NONE => Ast.Id (addTmpTagPtr ())

			    (* -- [exp,T,SOME tagptr] -- *)
			    val (inst_exp,{stype_op=stypeOp,is_zero=iszero,ctrvlist=exp_ctrvs}) =
					instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=skip_call},SOME tagptr_cexp)

			    val (ctrv_now,ctrv_defer) = if isSome tagptr_cexpOp
							then (nil,exp_ctrvs)
							else (exp_ctrvs,nil)

			    (* -- [exp,T,SOME tagptr].mem -- *)
			    val dot_exp = wrapEXPR(Ast.Void, Ast.Member (inst_exp,mem))
			in
			    case stypeOp
			      of SOME stype =>
				 (*NOTE: if tagptr_cexp is NONE, then tagptr_cexp refers to an unused temporary;
				   TODO: remove it from the tmp var stack? *)
				 (dot_exp, {stype_op=SOME exp_type,is_zero=false,ctrvlist=exp_ctrvs (*should be nil*)})
			       | NONE =>
				 let (* -- tmp_val = [exp,T,SOME tagptr].mem -- *)
				     val tv_id = addTmpValVar (outer_exp,{pointer=false})
				     val tv_exp1 = wrapEXPR(Ast.Void, Ast.Id tv_id)
				     val assign_exp = wrapEXPR(Ast.Void, Ast.Assign (tv_exp1,dot_exp))

				     (* -- tagptr = (char * ) tagptr + offsetof(exp_type,mem) -- *)
				     val tagptr_exp1 = wrapEXPR(Ast.Void, tagptr_cexp)
				     val char_type = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,
							 Ast.SIGNED,Ast.CHAR,Ast.SIGNASSUMED)
				     val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer char_type, tagptr_exp1))
				     val offsetof_exp = newOffsetofExpr(lookupExprType exp, mem)
				     val add_exp = wrapEXPR(Ast.Void, Ast.Binop (Ast.Plus,cast_exp,offsetof_exp))
				     val tagptr_exp2 = wrapEXPR(Ast.Void, tagptr_cexp)
				     val inx_assign_exp = wrapEXPR(Ast.Void, Ast.Assign (tagptr_exp2, add_exp))

				     (* -- verifyTag(), if enforce -- *)
				     val verifyTag_exps =
					 if enforce' (* -- verifyTag(outer_exp, tagptr, typeof(e.id)) -- *)
					 andalso not (vt_redundant_aid Rtc.RED_Verify exp_aid)
					 then [ newVerifyTagCallExpr (outer_exp,tagptr_cexp,exp_type) ]
					 else nil

				     (* -- (assign_exp,inx_assign_exp,verifyTag_exps,ctrv_now) -- *)
				     val comma_exp = commafyExprs (assign_exp, inx_assign_exp :: (verifyTag_exps @ ctrv_now))

				     val tv_exp = wrapEXPR(Ast.Void, Ast.Id tv_id)
				 in
				     (* -- (comma2_exp,tmp_val) -- *)
				     (Ast.EXPR (Ast.Comma (comma_exp,tv_exp),exp_aid,exp_loc),
				      {is_zero=false,stype_op=NONE,ctrvlist=ctrv_defer})
				 end
			end
		end

	      | Ast.Arrow (e,mem) =>
		if isBitField (derefOneLevel (lookupExprType e), mem)
		then (* bitfield case: if tagptr, do tagptr = exp_type_statictype *)
		    let val (inst_exp,_) = instrRValueExpr (e,{enforce=true,needrval=true,skip_call=skip_call},NONE)
			val arrow_cexp = Ast.Arrow (inst_exp, mem)
		    in  (case tagptr_cexpOp
			   of SOME tagptr_cexp =>
			      let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=false})
				  val arrow_exp = wrapEXPR(Ast.Void, arrow_cexp)
			      in
				  (Ast.EXPR (Ast.Comma (assign_exp,arrow_exp),exp_aid,exp_loc),
				   {is_zero=false,stype_op=NONE,ctrvlist=nil})
			      end
			    | NONE => (Ast.EXPR (arrow_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
			)
		    end
		else if aos_badly_typed (expToAO outer_exp)
		     orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify)
		then (instrLValueExpr (outer_exp,{enforce=enforce,verify_ptr=true,rval=true,skip_call=skip_call,is_write=false},
						 tagptr_cexpOp),
		      {is_zero=false,stype_op=NONE,ctrvlist=nil})
		else let val (e',_) = instrRValueExpr (e,{enforce=true,needrval=true,skip_call=skip_call}, NONE)
		     in
			 case tagptr_cexpOp
			   of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (Ast.EXPR (Ast.Arrow (e',mem),exp_aid,exp_loc),
					{is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
				else let val tp_assign_exp = newStaticTypeAssign(tagptr_cexp,
								  lookupExprType outer_exp, {is_zero=false})
					 val arr_exp = wrapEXPR(Ast.Void, Ast.Arrow (e',mem))
				     in
					(Ast.EXPR (Ast.Comma (tp_assign_exp,arr_exp),exp_aid,exp_loc),
					 {is_zero=false,stype_op=NONE,ctrvlist=nil})
				     end
			    | NONE => (Ast.EXPR (Ast.Arrow (e',mem),exp_aid,exp_loc),
					{is_zero=false,stype_op=NONE,ctrvlist=nil})
		     end

	      | Ast.Deref e =>
		if aos_badly_typed (expToAO outer_exp)
		orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify)
		then (instrLValueExpr (outer_exp,{enforce=enforce,verify_ptr=true,rval=true,skip_call=skip_call,is_write=false},
						 tagptr_cexpOp),
		      {is_zero=false,stype_op=NONE,ctrvlist=nil})
		else let val (e',_) = instrRValueExpr (e,{enforce=true,needrval=true,skip_call=skip_call}, NONE)
		     in
			 case tagptr_cexpOp
			   of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (Ast.EXPR (Ast.Deref e',exp_aid,exp_loc),
					{is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
				else let val tp_assign_exp = newStaticTypeAssign(tagptr_cexp,
								  lookupExprType outer_exp, {is_zero=false})
					 val der_exp = wrapEXPR(Ast.Void, Ast.Deref e')
				     in
					(Ast.EXPR (Ast.Comma (tp_assign_exp,der_exp),exp_aid,exp_loc),
					 {is_zero=false,stype_op=NONE,ctrvlist=nil})
				     end
			    | NONE => (Ast.EXPR (Ast.Deref e',exp_aid,exp_loc),
					{is_zero=false,stype_op=NONE,ctrvlist=nil})
		     end

	      | Ast.AddrOf exp =>
		let
		   (* Instrument exp: [exp,F,none] *)
		   val inst_exp = instrLValueExpr (exp,{enforce=false,verify_ptr=false,rval=false,skip_call=skip_call,is_write=false},
						       NONE)

		   (* Create instrumented cexp = &[exp,F,none] *)
		   val addr_cexp = Ast.AddrOf inst_exp

		   val (final_cexp,stypeOp) =
		      case tagptr_cexpOp
			of SOME tagptr_cexp =>
			    (* case SOME tagptr: return (tagptr=[staticpos]),addr_cexp *)
			   if !Flags.optimizeCopyTag
			   then (addr_cexp, SOME (lookupExprType outer_exp))
			   else let val assign_exp = newStaticTypeAssign(tagptr_cexp,
									 lookupExprType outer_exp, {is_zero=false})
				    val addr_exp = wrapEXPR(Ast.Void, addr_cexp)
				in
				    (Ast.Comma (assign_exp, addr_exp), NONE)
				end
			 | NONE => (* case NONE: return addr_cexp *)
			    (addr_cexp, NONE)
		in
		   (Ast.EXPR (final_cexp,exp_aid,exp_loc),{is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		end
	      | Ast.Binop (binop,e1,e2) =>
		let
		   val skipCall = case binop of Ast.And => false
					      | Ast.Or => false
					      | _ => skip_call orelse (hasClashingCalls outer_exp)
		   (* Instrument e1 *)
		   val e1' = if (OPT.isAssignBinop binop)
			     then (* e1' = [e1,T,tagptr] - Lvalue *)
				  instrLValueExpr (e1,{enforce=true,verify_ptr=true,rval=false,skip_call=skipCall,is_write=true},
						      tagptr_cexpOp)
			     else (* e1' = [e1,T,none]   - Rvalue *)
				  (#1 (instrRValueExpr (e1,{enforce=true,needrval=true,skip_call=skipCall},NONE)))

		   (* Instrument e2: [e2,T,none] *)
		   val (e2',_) = instrRValueExpr (e2,{enforce=true,needrval=true,skip_call=skipCall},NONE)

		   (* Create instrumented cexp = [e1,T,...] binop [e2,T,none] *)
		   val inst_coreExp = Ast.Binop (binop,e1',e2')

		   val (final_cexp, stypeOp) =
		      case (tagptr_cexpOp, OPT.isAssignBinop binop)
			of (SOME tagptr_cexp,false) =>
			    (* case SOME tagptr/non-assign binop: return (tagptr=[staticpos]),inst_coreExp *)
			    if !Flags.optimizeCopyTag
			    then (inst_coreExp, SOME (lookupExprType outer_exp))
			    else let val assign_exp = newStaticTypeAssign(tagptr_cexp,
									  lookupExprType outer_exp, {is_zero=false})
				     val rhs_exp = wrapEXPR(Ast.Void, inst_coreExp)
				 in
				     (Ast.Comma (assign_exp, rhs_exp), NONE)
				 end
			 | _ => (* case NONE: return inst_coreExp *)
			    (inst_coreExp, NONE)
		in
		   (Ast.EXPR (final_cexp,exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		end
	      | Ast.Unop (unop,exp) =>
		let
		   (* Instrument exp *)
		   val exp' = if (OPT.isAssignUnop unop)
			      then (* exp' = [exp,T,tagptr] - Lvalue *)
				   instrLValueExpr (exp,{enforce=true,verify_ptr=true,rval=false,skip_call=skip_call,is_write=true},
							tagptr_cexpOp)
			      else (* exp' = [exp,T,none]   - Rvalue *)
				   (#1 (instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=skip_call},NONE)))

		   (* Create instrumented cexp = unop [exp,T,...] *)
		   val inst_coreExp = Ast.Unop (unop,exp')

		   val (final_cexp, stypeOp) =
		      case (tagptr_cexpOp, OPT.isAssignUnop unop)
			of (SOME tagptr_cexp,false) =>
			    (* case SOME tagptr/not-assign unop: return (tagptr=[staticpos]),inst_coreExp *)
			    if !Flags.optimizeCopyTag
			    then (inst_coreExp, SOME (lookupExprType outer_exp))
			    else let val assign_exp = newStaticTypeAssign(tagptr_cexp,
									  lookupExprType outer_exp, {is_zero=false})
				     val rhs_exp = wrapEXPR(Ast.Void, inst_coreExp)
				 in
				     (Ast.Comma (assign_exp, rhs_exp), NONE)
				 end
			 | _ =>
			    (* case NONE: return inst_coreExp *)
			    (inst_coreExp, NONE)
		in
		   (Ast.EXPR (final_cexp,exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		end
	      | Ast.SizeOf ty =>
		let
		   (* Create instrumented cexp = sizeof ty *)
		   val inst_coreExp = Ast.SizeOf ty

		   val (final_cexp, stypeOp) =
		      case tagptr_cexpOp
			of SOME tagptr_cexp =>
			    (* case SOME tagptr: return (tagptr=[staticpos]),inst_coreExp *)
			    if !Flags.optimizeCopyTag
			    then (inst_coreExp, SOME (lookupExprType outer_exp))
			    else let val assign_exp = newStaticTypeAssign(tagptr_cexp,
									  lookupExprType outer_exp, {is_zero=false})
				     val rhs_exp = wrapEXPR(Ast.Void, inst_coreExp)
				 in
				     (Ast.Comma (assign_exp, rhs_exp), NONE)
				 end
			 | NONE => (* case NONE: return inst_coreExp *)
			    (inst_coreExp, NONE)
		in
		   (Ast.EXPR (final_cexp,exp_aid,exp_loc), {is_zero=false,stype_op=stypeOp,ctrvlist=nil})
		end

	      | Ast.Cast (ctype,exp) =>
		(* Get the type of exp. *)
		let val exp_type = lookupExprType exp
		in if tcEqualType (ctype,exp_type) (* "useless" cast *)
		   then let val (exp',{stype_op=stypeOp,is_zero=iszero,ctrvlist=exp_ctrvs}) =
					instrRValueExpr (exp,{enforce=enforce,needrval=true,skip_call=skip_call},
								      tagptr_cexpOp)
			in  (Ast.EXPR (Ast.Cast (ctype,exp'),exp_aid,exp_loc), {is_zero=iszero,stype_op=stypeOp,ctrvlist=exp_ctrvs})
			end
		   else if (* not (!Flags.strictPointer) orelse *) (* OBSOLETE? see flags.sml/strictPointer *)
			   isConversion (ctype, exp_type) (* conversion: enforce *)
		   then let
			    (* Instrument exp: [exp,T,none] *)
			    val (inst_exp,{is_zero=iszero,...}) = instrRValueExpr (exp,
										   {enforce=true,needrval=true,skip_call=skip_call},
										   NONE)

			    (* Create instrumented cexp = (ctype) [exp,T,none] *)
			    val cast_cexp = Ast.Cast (ctype,inst_exp)

			    val (final_cexp, stypeOp) =
				   case tagptr_cexpOp
				       of SOME tagptr_cexp =>
					   (* case SOME tagptr: return (tagptr=[staticpos]),cast_cexp *)
					   if !Flags.optimizeCopyTag
					   then (cast_cexp, SOME ctype)
					   else let (* NOTE: following is OK because ctype can never be aggregate
							     (though exp_type may be array) *)
						    val assign_exp = newStaticTypeAssign(tagptr_cexp,ctype,{is_zero=iszero})
						    val cast_exp = wrapEXPR(Ast.Void, cast_cexp)
						in
						    (Ast.Comma (assign_exp, cast_exp), NONE)
						end
				     | NONE => (* case NONE: return cast_cexp *)
					   (cast_cexp, NONE)
			in
			    (Ast.EXPR (final_cexp,exp_aid,exp_loc), {is_zero=iszero,stype_op=stypeOp,ctrvlist=nil})
			end
		   else (* Non-conversion (memcopy/expand/truncate) case: call promoteTag *)
			let
			    (* Instrument exp: [exp,enforce,tagptr_cexpOp] *)
			    val (inst_exp,{stype_op=stypeOp,is_zero=iszero,ctrvlist=exp_ctrvs}) =
					instrRValueExpr (exp,{enforce=enforce,needrval=true,skip_call=skip_call},
									tagptr_cexpOp)

			    val (final_cexp,iszero,stypeOp) =
				if iszero orelse isSome stypeOp (* implies !Flags.optimizeCopyTag *)
				then let (* Create instrumented cexp = (ctype) [exp,T,none] *)
					 val cast_cexp = Ast.Cast (ctype,inst_exp)
				     in  (cast_cexp,iszero,SOME (lookupExprType outer_exp))  end
				else case tagptr_cexpOp
				       of SOME tagptr_cexp =>
					    let
						(* Create a tmp_val of the type of outer_exp. *)
						val tv_id = addTmpValVar (outer_exp,{pointer=false})

						(* Create EXPR for tmp_val = (ctype) inst_exp. *)
						val tv_exp1 = wrapEXPR(Ast.Void, Ast.Id tv_id)
						val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (ctype, inst_exp))
						val assign_exp = wrapEXPR(Ast.Void,
									  Ast.Assign(tv_exp1,cast_exp))

						val assign_exp =
						     (case stypeOp
							of SOME stype => (* NOTE: OK because stype cannot be aggregate *)
							   wrapEXPR(Ast.Void,
							    Ast.Comma(newStaticTypeAssign(tagptr_cexp,stype,{is_zero=false}),
									assign_exp))
							 | NONE => assign_exp
						     )

						(* Create EXPR for function _promoteTag(). *)
						val pt_id = getGlobalFun "_promoteTag"
						val pt_exp = wrapEXPR(Ast.Void, Ast.Id pt_id)
						(* Create EXPR for &tagptr. *)
						val tagptr_exp = wrapEXPR(Ast.Void, tagptr_cexp)
						val addr_tp_exp = wrapEXPR(Ast.Void, Ast.AddrOf tagptr_exp)
						(* Create EXPR for ctype enum constants *)
						val exp_type_exp = newCtypeEnumExpr exp_type
						val cast_type_exp = newCtypeEnumExpr ctype
						(* Create EXPR for &tmp_val. *)
						val tv_exp2 = wrapEXPR(Ast.Void, Ast.Id tv_id)
						val addr_tv_exp = wrapEXPR(Ast.Void, Ast.AddrOf tv_exp2)
						(* Create EXPR for _promoteTag(args). *)
						val args = [addr_tp_exp, exp_type_exp,
							    cast_type_exp, addr_tv_exp]
						val pt_call_exp = newInstCallExpr (pt_exp,args)

					       (* Create EXPR for (assign_exp, tp_call_exp). *)
					       val pt_comma_exp = wrapEXPR(Ast.Void,
									Ast.Comma (assign_exp, pt_call_exp))

					       (* Create EXPR for tmp_val. *)
					       val tv_exp3 = wrapEXPR(Ast.Void, Ast.Id tv_id)
					   in
					       (* Return core_exp for (pt_comma_exp, tv_exp3). *)
					       (Ast.Comma (pt_comma_exp, tv_exp3), iszero, NONE)
					   end

					  (* tagptr_cexpOp = NONE, enforce = false: return (ctype) inst_exp. *)
					| NONE => (Ast.Cast (ctype, inst_exp), iszero, NONE)
			in
			    (Ast.EXPR (final_cexp,exp_aid,exp_loc),
			     {is_zero=iszero,stype_op=stypeOp,ctrvlist=exp_ctrvs})
					(* SY/TODO: when proper promoteTag is written, will need to add tv_id to
						    ctrvlist; note: since exp_ctrv may or may not have been
						    consumed, we'll let it be deferred also. *)
			end
		end

	      | Ast.Id pid =>
		if aos_badly_typed (expToAO outer_exp)
		orelse may_be_uninit_aid (exp_aid,Rtc.MBU_Verify)
		then (instrLValueExpr (outer_exp,{enforce=enforce,verify_ptr=enforce,rval=true,skip_call=skip_call,is_write=false},
						 tagptr_cexpOp),
		      {is_zero=false,stype_op=NONE,ctrvlist=nil})
		else (case tagptr_cexpOp
			of SOME tagptr_cexp =>
				if !Flags.optimizeCopyTag
				then (Ast.EXPR (Ast.Id pid,exp_aid,exp_loc),
					{is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
				else let val tp_assign_exp = newStaticTypeAssign(tagptr_cexp,
								  lookupExprType outer_exp, {is_zero=false})
					 val id_exp = wrapEXPR(Ast.Void, Ast.Id pid)
				     in
					(Ast.EXPR (Ast.Comma (tp_assign_exp,id_exp),exp_aid,exp_loc),
					 {is_zero=false,stype_op=NONE,ctrvlist=nil})
				     end
			 | NONE => (Ast.EXPR (Ast.Id pid,exp_aid,exp_loc), {is_zero=false,stype_op=NONE,ctrvlist=nil})
		     )

	      | Ast.EnumId (pid,li) => (* same is Ast.IntConst case*)
		(case tagptr_cexpOp
		   of SOME tagptr_cexp =>
		      if !Flags.optimizeCopyTag
		      then (Ast.EXPR (coreExpr,exp_aid,exp_loc), {is_zero=false,stype_op=SOME (lookupExprType outer_exp),ctrvlist=nil})
		      else let val assign_exp = newStaticTypeAssign(tagptr_cexp, lookupExprType outer_exp, {is_zero=(li=0)})
			       val const_exp = wrapEXPR(Ast.Void, Ast.EnumId (pid,li))
			       val comma_cexp = Ast.Comma(assign_exp, const_exp)
			   in
			       (Ast.EXPR (comma_cexp,exp_aid,exp_loc), {is_zero=(li=0),stype_op=NONE,ctrvlist=nil})
			   end
		    | NONE =>
			(Ast.EXPR (coreExpr,exp_aid,exp_loc), {is_zero=(li=0),stype_op=NONE,ctrvlist=nil})
		)
	      | Ast.ExprExt ee =>
		(Ast.EXPR (coreExpr,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
	      | Ast.ErrorExpr =>
		(Ast.EXPR (coreExpr,exp_aid,exp_loc),{is_zero=false,stype_op=NONE,ctrvlist=nil})
	end before popLoc()
      )

  fun newClearTagExprs NONE = NONE (* possible if label is never goto'ed *)
    | newClearTagExprs (SOME idlist) =
      let val setTagExprs = List.map (newSetTagExpr_id clearTag) idlist
      in  case setTagExprs
	    of nil => NONE
	     | head::tail => SOME (commafyExprs (head,tail))
      end

  fun do_initTag_op (id as {uid=pid,...}) =
      let val (do_init,init_mode) = if aos_child_eq_safe [Rtc.aoId pid]
				    then (may_be_uninit_pid pid, MODE_UNINIT)
				    else if aos_child_eq_exposed [Rtc.aoId pid]
				    then (true, if (may_be_uninit_pid pid)
						then MODE_UNINIT
						else MODE_INIT)
				    else (true, MODE_UNINIT)

      in  if do_init then SOME (initTag_id (id,  if (!Flags.initAll)
						   then MODE_INTEGER
						   else init_mode))
		     else NONE
      end

  (********************************************
     instrDeclsStmts
       (decls : Ast.declaration list,
        stmts : Ast.statement list,
	cfAO  : Rtc.absObject,			//- current function AO, used to lookup return tsl
        ct_stmts : Ast.statement list,				//- append after inner-scope stmts
        idhash: (Aid.uid, Pid.uid list) HashTable.hash_table)	//- used in -ptr mode, for alternative stack clearing
        -> (instr_decls : Ast.declaration list,
            instr_stmts : Ast.statement list,
            has_static  : bool):
   ********************************************)
  fun instrDeclsStmts (nil, stmts, cfAO, ct_stmts, idhash) = (nil, (instrStmts (stmts, cfAO, idhash)) @ ct_stmts, false)
    | instrDeclsStmts (head::tail, stmts, cfAO, ct_stmts, idhash) =

      ( if !trace then ppToStdOut PPA.ppDeclaration head else ()
      ; case head
	  of Ast.TypeDecl _ => 
	     (* -- typedef declarations: do nothing *)
	     (*  - except insert new tmps in front of tail' *)
		 let val (tail', stmts', has_static') = instrDeclsStmts(tail,stmts, cfAO,ct_stmts,idhash)

		     (* Obtain the tmps accumulated during the instrumentation of stmts;
		        stick them in front of tail' *)
		     val new_tmps = getTmpVars()
		     val tmpdecls = List.map (fn id => Ast.VarDecl(id, NONE)) new_tmps

		     val tail' =  tmpdecls @ tail'

		     (* HACK: there is no "clean" way to do this *)
		     val _ = (popTmpVarList(); pushTmpVarList())
		 in
		    (head::tail',stmts',has_static')
		 end

	   | Ast.VarDecl (id as {ctype=id_type,stClass=stClass,location=id_loc,uid=pid,...}, initExprOp) =>
	    (let val _ = pushLoc id_loc

		 val id_ao = Rtc.aoId pid

		 (* -- first, instrument tail *)
		 val (tail'', stmts', has_static') =
				instrDeclsStmts(tail,stmts, cfAO,ct_stmts,idhash)

		 (* Obtain the tmps accumulated during the instrumentation of stmts;
		    stick them in front of tail' *)
		 val new_tmps = getTmpVars()
		 val tmpdecls = List.map (fn id => Ast.VarDecl(id, NONE)) new_tmps

		 val tail' =  tmpdecls @ tail''

		 (* HACK: there is no "clean" way to do this *)
		 val _ = (popTmpVarList(); pushTmpVarList())

		 val initExprOp = OPT.simplifyIfScalar ttab (id_type, initExprOp)
	     in(
		 (* -- if id is a function (prototype): do nothing *)
		 if isNonPointerFunction id_type
		 then (head::tail',stmts',has_static')

		 (* -- if id is extern: add to extern_vars *)
		 else if (stClass = Ast.EXTERN)
		 then (head::tail',stmts',has_static') before pushExterns(id)

		 (* -- if id is static: do static_flag stuff, scopify tail, return has_static=true *)
		 else if (stClass = Ast.STATIC)
		 then let val (setStrStmts,setScaStmts) =
					case initExprOp
					  of SOME initExpr =>
						(mapInitExpr doSetStringTag (id, initExpr),
						 if (!Flags.instrMode = Flags.IM_PTR)
						 orelse (!Flags.instrMode = Flags.IM_PTRW)
						 then nil
						 else mapInitExpr doSetScalarTag (id, initExpr))
					   | _ => (nil,nil)
		      in( (* if SAFE and no string literals or setScaStmts: do nothing *) 
			  if (null setStrStmts)
			  andalso (null setScaStmts)
			  andalso (not (aos_child_le_exposed [id_ao]))	(* safe or influential? should never be influential!? *)
			  then (head::tail',stmts',has_static')
			  else let (* get the static_id (id of static_flag) to be used by static var decls *)
				   val static_id = getStatic()

				   (* -- predicate: static_flag *)
				   val static_exp = wrapEXPR(Ast.Void, Ast.Id static_id)

				   (* -- body: { initTag } or setUninitTag() *)
				   val body_stmts =
				       (case initExprOp
					  of SOME initExpr =>
					     (case initExpr
						of Ast.Simple exp =>
							if aos_le_exposed [id_ao]
							then
							   ( (count_set_tag := (!count_set_tag) + 1)
							   ; wrapSTMT (Ast.Expr (SOME (initTag_id(id, if (OPT.isZero exp)
												      then MODE_INIT
												      else MODE_INTEGER))))
							     :: setScaStmts (* SY: not sure when this would be non-nil *)
							   )
							else nil
						 | _ => if not (null setScaStmts)
							orelse aos_child_le_exposed [id_ao]
							then ( (count_set_tag := (!count_set_tag) + 1)
							     ; wrapSTMT (Ast.Expr (SOME (initTag_id (id, MODE_INIT))))
							       :: setScaStmts
							     )
							else nil
					     )
					   | NONE =>
						if aos_child_le_exposed [id_ao]
						then ( (count_set_tag := (!count_set_tag) + 1)
						     ; [wrapSTMT(Ast.Expr(SOME (initTag_id (id, if (!Flags.initGlobals)
											(*     (orelse id_ao is exposed)	*)
											        then MODE_INIT
											        else MODE_UNINIT))))]
						     )
						else nil
				       ) @ setStrStmts (* precomputed above *)

				   val body_stmt = case body_stmts
						     of stmt::nil => stmt
						      | _ => wrapSTMT (Ast.Compound (nil, body_stmts))

				   (* -- if statement *)
				   val if_stmt = wrapSTMT(Ast.IfThen (static_exp,body_stmt))
				   (* -- scopified tail *)
				   val ctail_stmt = wrapSTMT(Ast.Compound (tail', stmts'))
			       in
				   ([head], [if_stmt,ctail_stmt], true)
			       end
		      ) end

		     (* -- if id is auto *)
		 else let (* -- if storage class is "register", change it to "default" *)
			  val id = de_register(id)
			  val initExprOp' = if isSome initExprOp
					    then initExprOp
					    else if ((!Flags.instrMode = Flags.IM_PTR) orelse (!Flags.instrMode = Flags.IM_PTRW))
						 andalso ((!Flags.nullifyAllAutos)
							  orelse (!Flags.nullifyMBUptrs)
								  andalso (vp_may_be_uninit_ao id_ao))
						 then let	(* zero-initialize id *)
							  val iexp = zeroInitFirstScalar ttab id_type
						(*
								(* old version: borrow functionality from ckit *)
								(* problem: ran out of memory for very large objects *)
							  val iexp = InitializerNormalizer.normalize
									{lookTid = lookTid,
									 bindAid = bindAid,
									initType = id_type,
									initExpr = Ast.Aggregate nil}
						*)
(*
							 val _ = print ("INITIALIZING " ^ (PPL.ppToString PPA.ppId id) ^ " to "
									^ (PPL.ppToString (PPA.ppInitExpression () tidtab) iexp) ^ "\n")
*)
						      in SOME iexp end
						 else NONE
		      in(case initExprOp' (* AAL: Should we use id's status here? *)
			   of SOME initExpr =>
			     (case initExpr
				of Ast.Simple (exp as Ast.EXPR(_,exp_aid,exp_loc)) =>
				  (if not (aos_child_le_exposed [id_ao])
				   andalso not (may_be_uninit_pid pid) (* SY: this is possible in pathological case, *)
									     (*     goto may jump over init-declaration!   *)
				   then (* must still instrRvalue *)
					 let val _ = pushLoc exp_loc
					     val (exp',_) = instrRValueExpr (exp, {enforce=false,needrval=true,skip_call=false}, NONE)
					 in
					     ((Ast.VarDecl(id, SOME (Ast.Simple exp')))::tail', stmts', has_static')
					     before popLoc()
					 end
				   else if (!Flags.instrMode = Flags.IM_PTR)
					orelse (!Flags.instrMode = Flags.IM_PTRW)
				   then (* -- do only setUninitTag *)
					let val _ = pushLoc exp_loc
					    (* -- setUninitTag(id, sizeof(id)) -- *)
					    val _ = (count_set_tag := (!count_set_tag) + 1)
					    val sut_exp = initTag_id (id,MODE_UNINIT)
					    val (exp',_) = instrRValueExpr (exp, {enforce=false,needrval=true,skip_call=false}, NONE)

					    val collected_exp = wrapEXPR(Ast.Void, Ast.Comma (sut_exp, exp'))
					 in
					    ((Ast.VarDecl(id, SOME (Ast.Simple collected_exp)))::tail', stmts', has_static')
					    before popLoc()
					 end
				   else (* -- if exp is normal expression *)
				       (let val _ = pushLoc exp_loc

					    (* -- setUninitTag(id, sizeof(id)) -- *)
					    val sut_exp = initTag_id (id,MODE_UNINIT)

					    (* -- tmp_val = [<exp>, F, some tp] -- *)
					    (* If exp is cast implicitly, make it explicit *)
					    (* Note: ckit does not track implicits for
					       initialization declarations, so we'll just
					       cast it to id's type *)
					    val cast_exp = makeCastExpr (exp,id_type)
					    (* Create a new tagptr temp for instrumenting exp *)
					    val tagptr_cexp = Ast.Id (addTmpTagPtr())
					    (* Instrument exp: [<exp>,F,some tagptr] *)
					    val (inst_exp,{stype_op=stypeOp,is_zero=iszero,ctrvlist=exp_ctrvs}) =
								instrRValueExpr (cast_exp,
									  {enforce=false,needrval=true,skip_call=false},
									  SOME tagptr_cexp)
					    (* Create tmp_val of <exp>'s type. *)
					    (* Note: this works out well, since we want id's
						     type with consts removed *)
					    val tv_id = addTmpValVar (cast_exp,{pointer=false})
					    (* Assign tmp_val = inst_exp *)
					    val tv_exp = wrapEXPR(Ast.Void, Ast.Id tv_id)
					    val tv_assign_exp = wrapEXPR(Ast.Void,
									Ast.Assign(tv_exp,inst_exp))

					    val collected_exp = wrapEXPR(Ast.Void,
									Ast.Comma (sut_exp, tv_assign_exp))

					    (* Create the call to
						     _copyTag_<typeof id>(&id, tagptr, sizeof id) *)
					    val copytag_exp =
						(case stypeOp
						   of SOME stype =>
							(* optimize: call setScalarTag (or initTag, for aggregates) inst of copyTag *)
						     (if (getTypeString stype) = "aggregate"
						      then (* initTag(&id) *)
							   let fun addr_id_builder () =
									let val id_exp = wrapEXPR (Ast.Void, Ast.Id id)
									in wrapEXPR (Ast.Void, Ast.AddrOf id_exp) end
							   in  initTag (addr_id_builder, stype, exp_loc, MODE_DEFAULT)  end
						      else (* setScalarTag_stype(&id) *)
							   let val id_exp = wrapEXPR (Ast.Void, Ast.Id id)
							       val addr_id_exp = wrapEXPR (Ast.Void, Ast.AddrOf id_exp)
							       val sst_id = getGlobalFun ( "_setScalar"
											^ (if iszero then "Init" else "")
											^ "Tag_" ^ (getTypeString stype))
							       val sst_exp = wrapEXPR(Ast.Void, Ast.Id sst_id)
							   in
							       newInstCallExpr (sst_exp,[addr_id_exp])
							   end
						     )
						    | NONE =>
						     (let
							  (* Create EXPRs for _copyTag's arguments
								  (&id, tagptr, sizeof id). *)
							  val id_exp = wrapEXPR (Ast.Void, Ast.Id id)
							  val addr_id_exp = wrapEXPR (Ast.Void, Ast.AddrOf id_exp)
							  val tagptr_exp = wrapEXPR (Ast.Void,tagptr_cexp)
							  val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf id_type)

							  (* Create EXPR for _copyTag function. *)
							  val idty_str = getTypeString id_type
							  val ct_id = getGlobalFun("_copyTag_" ^ idty_str)
							  val ct_exp = wrapEXPR(Ast.Void, Ast.Id ct_id)
						      in
							  newInstCallExpr (ct_exp,
									[addr_id_exp,tagptr_exp,sizeof_exp])
						      end)
						)

					    (* collect copytag along with exp_ctrvs *)
					    val collected_exp = commafyExprs (collected_exp,copytag_exp::exp_ctrvs)

					    (* Create a new EXPR for tmp_val. *)
					    val tv_exp = wrapEXPR(Ast.Void, Ast.Id tv_id)

					    val collected_exp = wrapEXPR(Ast.Void,
									Ast.Comma (collected_exp, tv_exp))
				       in
					    ((Ast.VarDecl(id, SOME (Ast.Simple collected_exp)))::tail',
					     stmts', has_static')
					    before popLoc()
				       end)
				   )
				 | Ast.Aggregate initExprs =>
				    (* -- if exp is initlist (initExpression list) *)
				   (let
					val sst_stmts = if (!Flags.instrMode = Flags.IM_PTR)
							orelse (!Flags.instrMode = Flags.IM_PTRW)
							then nil
							else mapInitExpr doSetScalarTag (id, initExpr)
					val init_stmts = if not (null sst_stmts)
							 orelse aos_child_le_exposed [id_ao]
							 orelse may_be_uninit_pid pid (* SY: See pathological comment above. *)
							 then ( (count_set_tag := (!count_set_tag) + 1)
							      ; wrapSTMT (Ast.Expr (SOME (initTag_id (id,MODE_INIT))))
								:: sst_stmts
							      )
							 else nil

					val str_init_stmts = mapInitExpr doSetStringTag (id, initExpr)

					val ctail_stmt = wrapSTMT(Ast.Compound (tail', stmts'))
				    in
					([Ast.VarDecl (id, initExprOp')], init_stmts @ str_init_stmts @ [ctail_stmt],
					 has_static')
				    end
				   )
			     )
			    | NONE => (* -- if no init exp *)
			     (if aos_child_le_exposed [id_ao]
			      orelse may_be_uninit_pid pid
			      then(if (isArray id_type)
				   then let
					    val _ = (count_set_tag := (!count_set_tag) + 1)
					    val init_exp = initTag_id (id, if (!Flags.initAll)
									   then MODE_INTEGER
									(*SY: "in principle", this should be OK for array, I think...  *)
									(*    not sure if this might have any unexpected consequencese *)
									   else if aos_child_eq_exposed [id_ao]
										andalso not (may_be_uninit_pid pid)
									   then MODE_INIT
									   else MODE_UNINIT
									)
					    val init_stmt = wrapSTMT(Ast.Expr (SOME init_exp))
					    val ctail_stmt = wrapSTMT(Ast.Compound (tail',stmts'))
					in
					    ([head], [init_stmt, ctail_stmt], has_static')
					end

				   else let (* -- if id_type is not array *)
					    val _ = (count_set_tag := (!count_set_tag) + 1)
					    val init_exp = initTag_id (id, if (!Flags.initAll)
									   then MODE_INTEGER
									(*SY: what if some children are exposed, but others are worse? *)
									(*    must handle! for now, unsafe simplification! *)
									   else if aos_child_eq_exposed [id_ao]
										andalso not (may_be_uninit_pid pid)
									   then MODE_INIT
									   else MODE_UNINIT
									)
					    val id_exp = wrapEXPR(id_type, Ast.Id id)
					    val comma_exp = wrapEXPR(id_type, Ast.Comma (init_exp,id_exp))
					in
					    ((Ast.VarDecl(id, SOME (Ast.Simple comma_exp)))::tail',
					     stmts', has_static')
					end
				  )
			      else (head::tail',stmts',has_static') (* if safe: do nothing *)
			     )
			) end
		) before popLoc()
		end
	    )
      )

  (********************************************
     instrStmts
       (stmts : Ast.statement list,
	cfAO  : Rtc.absObject,			//- current function AO, used to lookup return tsl
	idhash: (Aid.uid, Pid.uid list) HashTable.hash_table)	//- used in -ptr mode, for alternative stack clearing
	-> (instr_stmts : Ast.statement list)
   ********************************************)
  and instrStmts (nil, cfAO, idhash) = nil
    | instrStmts (head::tail, cfAO, idhash) = (instrStmt (head,cfAO,idhash))::(instrStmts (tail,cfAO,idhash))

  (********************************************
     instrStmt
       (stmt : Ast.statement,
	cfAO  : Rtc.absObject,			//- current function AO, used to lookup return tsl
	idhash: (Aid.uid, Pid.uid list) HashTable.hash_table)	//- used in -ptr mode, for alternative stack clearing
	-> (instr_stmt : Ast.statement)
   ********************************************)
  and instrStmt (outer_stmt as (Ast.STMT(coreStmt,stmt_aid,stmt_loc)), cfAO, idhash) =

    (let val _ = if !trace then ppToStdOut PPA.ppStatement outer_stmt else ()
	 val _ = pushLoc stmt_loc
     in case coreStmt
	  of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp =>
		       let val (exp',_) = instrRValueExpr (exp,{enforce=false,needrval=false,skip_call=false},NONE)
		       in
			   Ast.STMT (Ast.Expr (SOME exp'),stmt_aid,stmt_loc)
		       end
		 | NONE => outer_stmt (* -- no change -- *)
	    )

	   | Ast.Compound (decls, stmts) =>
		let
		    (* NOTE: new symtab is pushed here! *)
		    val _ = pushLocalEnv()
		    (* Entering new scope: push tmp var list *)
		    val _ = pushTmpVarList()

		    (* create and push static_id *)
		    val static_id = addNewVariable ("_static_flag", {isTemp=false}, stdInt, Ast.STATIC)
		    val _ = pushStatic static_id

		    (* For -ptr -indivClearTag, collect clearTag exprs *)
		    (* These must go at the end of the innermost "newly introduced" statement *)
		    val ct_stmts = if (!Flags.indivClearTag)
				   then case (newClearTagExprs (HashTable.find idhash stmt_aid))
					  of SOME cts => [ wrapSTMT (Ast.Expr (SOME cts)) ]
					   | NONE => nil
				   else nil

		    (* instrument decls *)
		    val (decls', stmts', has_static)
				= instrDeclsStmts (decls, stmts, cfAO, ct_stmts, idhash) before popStatic()

		    val (new_decls, new_stmts) =
			   if has_static
			   then let
				    (* create decl/initialization of static_flag = 2 *)
				    val const_2_exp = wrapEXPR (stdInt, Ast.IntConst 2)
				    val sf_decl = Ast.VarDecl(static_id, SOME (Ast.Simple const_2_exp))

				    (* create STMT: if(static_flag) static_flag--; *)
				    val sf_exp = wrapEXPR(Ast.Void, Ast.Id static_id)
				    val decr_exp = wrapEXPR(Ast.Void, Ast.Unop (Ast.PostDec,sf_exp))
				    val decr_stmt = wrapSTMT(Ast.Expr (SOME decr_exp))
				    val sf_exp = wrapEXPR(Ast.Void, Ast.Id static_id)
				    val if_stmt = wrapSTMT(Ast.IfThen (sf_exp,decr_stmt))

				    (* scopified instrumented decls and stmts *)
				    val cinst_stmt = wrapSTMT(Ast.Compound (decls',stmts'))
				in
				    ([sf_decl], [if_stmt,cinst_stmt])
				end
			   else (decls', stmts')

		    (* Obtain the tmps accumulated during the instrumentation of stmts, 
		       and create declarations for them. *)
		    val new_tmps = getTmpVars()
		    val tmpdecls = List.map (fn id => Ast.VarDecl(id, NONE)) new_tmps
		in
		    Ast.STMT(Ast.Compound (tmpdecls @ new_decls, new_stmts),stmt_aid,stmt_loc)
 		    before (
			     popTmpVarList();
			     popLocalEnv()
			   )
		end

	   | Ast.While (exp,stmt) =>
		let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=false},NONE)
		    val stmt' = instrStmt (stmt, cfAO, idhash)
		in
		    Ast.STMT(Ast.While (exp',stmt'),stmt_aid,stmt_loc)
		end

	   | Ast.Do (exp,stmt) => 
		let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=false},NONE)
		    val stmt' = instrStmt (stmt, cfAO, idhash)
		in
		    Ast.STMT(Ast.Do (exp',stmt'),stmt_aid,stmt_loc)
		end

	   | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1op' = case e1op
				  of SOME exp1 =>
				     SOME (#1 (instrRValueExpr (exp1,{enforce=false,needrval=false,skip_call=false},NONE)))
				   | NONE => NONE
		    val e2op' = case e2op
				  of SOME exp2 =>
				     SOME (#1 (instrRValueExpr (exp2,{enforce=true,needrval=true,skip_call=false} ,NONE)))
				   | NONE => NONE
		    val e3op' = case e3op
				  of SOME exp3 =>
				     SOME (#1 (instrRValueExpr (exp3,{enforce=false,needrval=false,skip_call=false},NONE)))
				   | NONE => NONE
		    val stmt' = instrStmt (stmt, cfAO, idhash)
		in
		    Ast.STMT(Ast.For (e1op',e2op',e3op',stmt'),stmt_aid,stmt_loc)
		end

	   | Ast.Labeled (label,stmt) =>
		let val stmt' = instrStmt (stmt, cfAO, idhash)
		    val stmt'' = case (HashTable.find idhash stmt_aid)
				   of SOME idlist =>
				      (case (List.mapPartial do_initTag_op idlist)
					 of nil => stmt'
					  | head::tail =>
					    let val initexp = (commafyExprs (head,tail))
						val initstmt = wrapSTMT (Ast.Expr (SOME initexp))
					    in  wrapSTMT (Ast.Compound (nil, [ initstmt , stmt' ]))  end
				      )
				    | NONE => stmt'
		in
		    Ast.STMT(Ast.Labeled (label,stmt''),stmt_aid,stmt_loc)
		end
	   | Ast.CaseLabel (li,exp,stmt) => 
		let val stmt' = instrStmt (stmt, cfAO, idhash)
		    val stmt'' = case (HashTable.find idhash stmt_aid)
				   of SOME idlist =>
				      (case (List.mapPartial do_initTag_op idlist)
					 of nil => stmt'
					  | head::tail =>
					    let val initstmt = wrapSTMT (Ast.Expr (SOME (commafyExprs (head,tail))))
					    in  wrapSTMT (Ast.Compound (nil, [ initstmt , stmt' ]))  end
				      )
				    | NONE => stmt'
		in
		    Ast.STMT(Ast.CaseLabel (li,exp,stmt''),stmt_aid,stmt_loc)
		end
	   | Ast.DefaultLabel stmt =>
		let val stmt' = instrStmt (stmt, cfAO, idhash)
		    val stmt'' = case (HashTable.find idhash stmt_aid)
				   of SOME idlist =>
				      (case (List.mapPartial do_initTag_op idlist)
					 of nil => stmt'
					  | head::tail =>
					    let val initstmt = wrapSTMT (Ast.Expr (SOME (commafyExprs (head,tail))))
					    in  wrapSTMT (Ast.Compound (nil, [ initstmt , stmt' ]))  end
				      )
				    | NONE => stmt'
		in
		    Ast.STMT(Ast.DefaultLabel stmt'',stmt_aid,stmt_loc)
		end
	   | Ast.Goto label =>
		if (!Flags.indivClearTag)
		then case (newClearTagExprs (HashTable.find idhash stmt_aid))
		       of SOME cts => Ast.STMT(Ast.Compound (nil, [ wrapSTMT (Ast.Expr (SOME cts))
								  , wrapSTMT (Ast.Goto label)
								  ]),stmt_aid,stmt_loc)
			| NONE => outer_stmt (* -- no change -- *)
		else outer_stmt (* -- no change -- *)
	   | Ast.Break =>
		if (!Flags.indivClearTag)
		then case (newClearTagExprs (HashTable.find idhash stmt_aid))
		       of SOME cts => Ast.STMT(Ast.Compound (nil, [ wrapSTMT (Ast.Expr (SOME cts))
								  , wrapSTMT (Ast.Break)
								  ]),stmt_aid,stmt_loc)
			| NONE => outer_stmt (* -- no change -- *)
		else outer_stmt (* -- no change -- *)
	   | Ast.Continue =>
		if (!Flags.indivClearTag)
		then case (newClearTagExprs (HashTable.find idhash stmt_aid))
		       of SOME cts => Ast.STMT(Ast.Compound (nil, [ wrapSTMT (Ast.Expr (SOME cts))
								  , wrapSTMT (Ast.Continue)
								  ]),stmt_aid,stmt_loc)
			| NONE => outer_stmt (* -- no change -- *)
		else outer_stmt (* -- no change -- *)
	   | Ast.Return expOp => 
	    (case expOp
	       of NONE => (* void return: new scope *)
		  let
		     (* -- processReturn(sf_start, argAddrs, 0, 0) -- *)

		     val pr_expOp = if (!Flags.indivClearTag)
				    then newClearTagExprs (HashTable.find idhash stmt_aid)
				    else let val scaf_start_id = getLocalObj "_scaf_start"
					     val scaf_end_id   = getLocalObj "_scaf_end"
					     val agrf_start_id = getLocalObj "_agrf_start"
					     val agrf_end_id   = getLocalObj "_agrf_end"
					     val argAddrs_id   = getLocalObj "_argAddrs"
					 in  SOME (newProcessReturnExpr
							(scaf_start_id, scaf_end_id, agrf_start_id, agrf_end_id,
							 argAddrs_id, Ast.IntConst 0, Ast.IntConst 0))
					 end

		     val car_expOp = if !Flags.vuln
				     then SOME (wrapEXPR (Ast.Void, Ast.Id (getGlobalObj ("_tcvuln_clearARretval"))))
				     else NONE

		     val alloca_ct_stmts = case (!alloca_ct_exps)
					     of e1::tail => [ wrapSTMT (Ast.Expr (SOME (commafyExprs (e1, tail)))) ]
					      | nil => nil

		     val ret_cstmt = case (pr_expOp,car_expOp,alloca_ct_stmts)
				       of (NONE,NONE,nil) => Ast.Return NONE
					| _ =>
					  let val pr_stmt = wrapSTMT (Ast.Expr pr_expOp)
					      val car_stmt = wrapSTMT (Ast.Expr car_expOp)
					      val ret_stmt = wrapSTMT (Ast.Return NONE)
					  in  Ast.Compound (nil, alloca_ct_stmts @ [pr_stmt,car_stmt,ret_stmt])  end
		  in
		     Ast.STMT(ret_cstmt,stmt_aid,stmt_loc)
		  end
		| SOME exp => (* non-void return: instrument *)
		  let val isptrptrw = (!Flags.instrMode = Flags.IM_PTR) orelse (!Flags.instrMode = Flags.IM_PTRW)

		      (* -- copy retval tag only if AOReturn is safe and not may-be-uninit *)
		      val do_copy_rv =	aos_child_badly_typed [Rtc.aoReturn cfAO]
					orelse may_be_uninit_aid (stmt_aid,Rtc.MBU_Return)

		      (* -- tmp_tagptr, tmp_val, sf_start, argAddrs --*)
		      val retval_id as {ctype=retval_type,...} = getLocalObj "_retval"
		      val tmptagptr_id = if isptrptrw orelse not do_copy_rv
					 then retval_id (*dummy placeholder only*)
					 else addTmpTagPtr ()

		      (* -- tmp_val = [<e>,F,some tmp_tagptr] -- *)
		      val exp_type = lookupExprType exp
		      (* make cast explicit; ckit doesn't have entry in implicits
		         table for return statement *)
		      val exp' = if equalType(exp_type, retval_type)
				 then exp
				 else wrapEXPR((*NEEDED!*)retval_type, Ast.Cast (retval_type, exp))
		      val (inst_exp,{stype_op=stypeOp,is_zero=iszero,ctrvlist=exp_ctrvs}) =
						instrRValueExpr (exp', {enforce=false,needrval=true,skip_call=false},
									if isptrptrw orelse not do_copy_rv
									then NONE else SOME (Ast.Id tmptagptr_id))
		      fun getTagPtrCexp () =
			  if isptrptrw orelse not do_copy_rv
			  then Ast.IntConst 0
			  else case stypeOp
				 of SOME stype =>
				    if (getTypeString stype) = "aggregate"
				    then (* -- tmptagptr = &retval, initTag(tmptagptr), tmptagptr -- *)
					 let val tmptagptr_exp = wrapEXPR(Ast.Void, Ast.Id tmptagptr_id)
					     val retval_exp = wrapEXPR(Ast.Void, Ast.Id retval_id)
					     val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf retval_exp)
					     val tagptr_assign_exp = wrapEXPR(Ast.Void, Ast.Assign (tmptagptr_exp,addrof_exp))

					     fun ttp_builder () = wrapEXPR (Ast.Void, Ast.Id tmptagptr_id)
					     val inittag_exp = initTag(ttp_builder, stype, stmt_loc, MODE_DEFAULT)
					     val comma1_exp = wrapEXPR(Ast.Void, Ast.Comma(tagptr_assign_exp, inittag_exp))

					     val tmptagptr_exp = wrapEXPR(Ast.Void, Ast.Id tmptagptr_id)
					 in
					     Ast.Comma(comma1_exp, tmptagptr_exp)
					 end
				    else (* -- tmptagptr = static_repptr -- *)
					 let val Ast.EXPR (assign_cexp,_,_) =
							newStaticTypeAssign (Ast.Id tmptagptr_id,stype,{is_zero=iszero})
					 in  assign_cexp  end
				  | NONE => Ast.Id tmptagptr_id

		      val prct_exps =
			  if (!Flags.indivClearTag)
			  then let val proc_ret_exps =
				       if isptrptrw
				       then nil
				       else let val argAddrs_id   = getLocalObj "_argAddrs"
						val argAddrs_exp = wrapEXPR(Ast.Void, Ast.Id argAddrs_id)
						val tagptr_exp = wrapEXPR (Ast.Void, getTagPtrCexp ())

						val sizeof_exp = wrapEXPR (Ast.Void, Ast.SizeOf retval_type)

						val pr_id = getGlobalFun "_processReturnNoClear"
						val pr_exp = wrapEXPR(Ast.Void, Ast.Id pr_id)

						val pr_call_exp = newInstCallExpr (pr_exp,[argAddrs_exp,tagptr_exp,sizeof_exp])
					    in  [pr_call_exp]  end

				   val ct_exps = case (newClearTagExprs (HashTable.find idhash stmt_aid))
						   of SOME ct_exp => [ct_exp]
						    | NONE => nil

			       in  proc_ret_exps @ exp_ctrvs @ ct_exps  end
						   (* note: ctrvs appended here, after proc_ret *)
			  else let (* -- processReturn(sf_start, argAddrs, tmp_tagptr, sizeof(retval_type)) -- *)
				   val scaf_start_id = getLocalObj "_scaf_start"
				   val scaf_end_id   = getLocalObj "_scaf_end"
				   val agrf_start_id = getLocalObj "_agrf_start"
				   val agrf_end_id   = getLocalObj "_agrf_end"
				   val argAddrs_id   = getLocalObj "_argAddrs"
				   val tmptptr_cexp  = getTagPtrCexp ()
				   val sizeof_cexp   = Ast.SizeOf retval_type

			       in [ newProcessReturnExpr (scaf_start_id, scaf_end_id, agrf_start_id, agrf_end_id,
								argAddrs_id, tmptptr_cexp, sizeof_cexp) ]
			       end

		      val car_exps = if !Flags.vuln
				     then [ wrapEXPR (Ast.Void, Ast.Id (getGlobalObj ("_tcvuln_clearARretval"))) ]
				     else nil

		      val ret_exp =
			  case (prct_exps @ (!alloca_ct_exps) @ car_exps)
			    of nil => inst_exp
			     | (head::tail) =>
			       let (* -- return (retval = [<e>,F,NONE], processReturn/clearTags, retval) -- *)
				   val retval_exp = wrapEXPR(Ast.Void, Ast.Id retval_id)
				   val assign_exp = wrapEXPR(Ast.Void, Ast.Assign (retval_exp,inst_exp))

				   val prct_exp = commafyExprs (head, tail)
				   val comma_exp = wrapEXPR(Ast.Void, Ast.Comma (assign_exp, prct_exp))

				   val retval_exp = wrapEXPR(Ast.Void, Ast.Id retval_id)
				   val comma_exp = wrapEXPR(Ast.Void, Ast.Comma (comma_exp, retval_exp))
			       in  comma_exp  end
		       in
			  Ast.STMT(Ast.Return (SOME ret_exp),stmt_aid,stmt_loc)
		       end
	    )
	   | Ast.IfThen (exp,stmt) => 
		let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=false},NONE)
		    val stmt' = instrStmt (stmt, cfAO, idhash)
		in
		    Ast.STMT(Ast.IfThen (exp',stmt'),stmt_aid,stmt_loc)
		end
	   | Ast.IfThenElse (exp,stmt1,stmt2) => 
		let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=false},NONE)
		    val stmt1' = instrStmt (stmt1, cfAO, idhash)
		    val stmt2' = instrStmt (stmt2, cfAO, idhash)
		in
		    Ast.STMT(Ast.IfThenElse (exp',stmt1',stmt2'),stmt_aid,stmt_loc)
		end
	   | Ast.Switch (exp,stmt) =>
		let val (exp',_) = instrRValueExpr (exp,{enforce=true,needrval=true,skip_call=false},NONE)
		    val stmt' = instrStmt (stmt, cfAO, idhash)
		in
		    Ast.STMT(Ast.Switch (exp',stmt'),stmt_aid,stmt_loc)
		end
	   | Ast.StatExt se => outer_stmt (* -- no change -- *)
	   | Ast.ErrorStmt => outer_stmt (* -- no change -- *)
     end
     before popLoc()
     )

  (********************************************)

  fun collectLocalVarInfo (ids, stmt) =
     (let
	  val aidhash = HashTable.mkTable (Aid.toWord,Aid.equal) (25,Rtc.exTC "collectLocalVarInfo:aidhash")
	  val gotolist = ref nil
	  val labelhash = HashTable.mkTable (Pid.toWord,Pid.equal) (25,Rtc.exTC "collectLocalVarInfo:labelhash")

	  fun clvStmt (stmt as (Ast.STMT(coreStmt,stmt_aid,stmt_loc)), varset, breakset, contset, switchset) =
	     (case coreStmt
		of Ast.Expr expOp => ()
		 | Ast.Compound (decls, stmts) =>
		   (let fun clvDecl decl =
			    (case decl
			       of Ast.TypeDecl _ => NONE
				| Ast.VarDecl (id as {ctype,stClass,uid,...}, _) =>
				  let val id_ao = Rtc.aoId uid
				  in  if (stClass = Ast.EXTERN)
				      orelse isNonPointerFunction ctype	(* ckit doesn't automatically recog fns as extern! *)
				      orelse (stClass = Ast.STATIC)
				      orelse aos_child_eq_safe [id_ao]
				      then NONE
				      else SOME id
				  end
			    )
			val thisvarlist = List.mapPartial clvDecl decls
			val varset' = idset.addList (varset,thisvarlist)
			val _ = List.app (fn stmt => clvStmt (stmt, varset', breakset, contset, switchset)) stmts
			val _ = HashTable.insert aidhash (stmt_aid, thisvarlist)
		    in  ()  end
		   )
		 | Ast.While (exp,stmt) => clvStmt (stmt, varset, varset, varset, switchset)
		 | Ast.Do (exp,stmt) => clvStmt (stmt, varset, varset, varset, switchset)
		 | Ast.For (e1op,e2op,e3op,stmt) => clvStmt (stmt, varset, varset, varset, switchset)
		 | Ast.Labeled (label,stmt) =>
			( HashTable.insert labelhash (#uid label, (stmt_aid, varset))
			; clvStmt (stmt, varset, breakset, contset, switchset)
			)
		 | Ast.CaseLabel (li,exp,stmt) =>
			( HashTable.insert aidhash (stmt_aid, idset.listItems (idset.difference (varset, switchset)))
			; clvStmt (stmt, varset, breakset, contset, switchset)
			)
		 | Ast.DefaultLabel stmt =>
			( HashTable.insert aidhash (stmt_aid, idset.listItems (idset.difference (varset, switchset)))
			; clvStmt (stmt, varset, breakset, contset, switchset)
			)
		 | Ast.Goto label =>
			(gotolist := ((#uid label, stmt_aid, varset)::(!gotolist)))
		 | Ast.Break =>
			HashTable.insert aidhash (stmt_aid, idset.listItems (idset.difference (varset, breakset)))
		 | Ast.Continue =>
			HashTable.insert aidhash (stmt_aid, idset.listItems (idset.difference (varset, contset)))
		 | Ast.Return expOp =>
			HashTable.insert aidhash (stmt_aid, idset.listItems varset)
		 | Ast.IfThen (exp,stmt) => clvStmt (stmt, varset, breakset, contset, switchset)
		 | Ast.IfThenElse (exp,stmt1,stmt2) =>
			( clvStmt (stmt1, varset, breakset, contset, switchset)
			; clvStmt (stmt2, varset, breakset, contset, switchset)
			)
		 | Ast.Switch (exp,stmt) => clvStmt (stmt, varset, varset, contset, varset)
		 | Ast.StatExt se => ()
		 | Ast.ErrorStmt => ()
	     )

	  (* -- 1. do formals *)
	  fun clvFormal (id : Ast.id) =
	     (case (#ctype id)
		of Ast.Ellipses => NONE
		 | _ => if aos_child_eq_safe [Rtc.aoId (#uid id)]
	                then NONE
			else SOME id
	     )

	  val instformals = List.mapPartial clvFormal ids

	  (* -- 2. do body *)
	  val _ = clvStmt (stmt, idset.addList (idset.empty, instformals), idset.empty, idset.empty, idset.empty)

	  (* -- 3. process gotos/labels *)
	  fun clvGoto (label_uid, goto_aid, goto_varset) =
	      case (HashTable.find labelhash label_uid)
		of SOME (label_aid, label_varset) =>
			let val _ = HashTable.insert aidhash (goto_aid, idset.listItems (idset.difference (goto_varset, label_varset)))
			    val _ = HashTable.insert aidhash (label_aid, idset.listItems (idset.difference (label_varset, goto_varset)))
			in  ()  end
		 | NONE => ( print "collectLocalVarInfo: unconnected goto encountered\n"
			   ; HashTable.insert aidhash (goto_aid, nil)
			   )
	  val _ = List.app clvGoto (!gotolist)
      in
	  (instformals, aidhash)
      end
     )
  (********************************************)

  (********************************************
     instrFunctionDef
       (func_id : Ast.id,
	formals : Ast.id list,
	body    : Ast.statement)
	-> (func_id : Ast.id,
	    formals : Ast.id list,
	    body    : Ast.statement)
   ********************************************)
  fun instrFunctionDef (fnid, formal_ids, stmt as Ast.STMT(_,_,stmt_loc)) =
     (let val _ = pushLoc (#location fnid)
	  (* Entering new scope: push (create) a new symbol table (scope) here *)
	  val _ = pushLocalEnv ()

	  (* ----- LOCAL VARIABLES ----- *)

	  (* -- addr_and_size_t * _argAddrs *)
	  val addr_siz_ty = lookupTypeDefType "_addr_and_size_t"
	  val addr_siz_ptr_ty = Ast.Pointer (addr_siz_ty)
	  val argAddrs_id = addNewVariable ("_argAddrs",{isTemp=false},addr_siz_ptr_ty,Ast.DEFAULT)
	  (* - suppress if -ptr/ptrw and -indivClearTag - *)
	  val argAddrs_decls =
		if ((!Flags.instrMode = Flags.IM_PTR) orelse (!Flags.instrMode = Flags.IM_PTRW))
		andalso (!Flags.indivClearTag)
		then nil
		else [ Ast.VarDecl (argAddrs_id, SOME (Ast.Simple (wrapEXPR (addr_siz_ptr_ty, Ast.IntConst 0)))) ]

	  val scaf_start_id = addNewVariable ("_scaf_start",{isTemp=false},Ast.Pointer Ast.Void,Ast.DEFAULT)
	  val scaf_end_id = addNewVariable ("_scaf_end",{isTemp=false},Ast.Pointer Ast.Void,Ast.DEFAULT)
	  val agrf_start_id = addNewVariable ("_agrf_start",{isTemp=false},Ast.Pointer Ast.Void,Ast.DEFAULT)
	  val agrf_end_id = addNewVariable ("_agrf_end",{isTemp=false},Ast.Pointer Ast.Void,Ast.DEFAULT)
	  (*  - if -indivClearTag, suppress decl of scaf_start, etc... *)
	  val sca_agr_decls =
		if (!Flags.indivClearTag)
		then nil
		else let (* -- void * _scaf_start = 0, _scaf_end = 0 *)
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val scaf_start_decl = Ast.VarDecl(scaf_start_id, SOME (Ast.Simple zero_exp))
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val scaf_end_decl = Ast.VarDecl(scaf_end_id, SOME (Ast.Simple zero_exp))
			 (* -- void * _agrf_start = 0, _agrf_end = 0 *)
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val agrf_start_decl = Ast.VarDecl(agrf_start_id, SOME (Ast.Simple zero_exp))
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val agrf_end_decl = Ast.VarDecl(agrf_end_id, SOME (Ast.Simple zero_exp))
		     in  [scaf_start_decl,scaf_end_decl,agrf_start_decl,agrf_end_decl]  end

	  (* -- ctype_retval _retval - only if non-void return type *)
	  val retval_decls =
		case (#ctype fnid) (* Function type *)
		  of Ast.Function (ret_type,arg_types) =>
		     ( case (getCoreType ret_type)
			 of Ast.Void => nil
			  | _ => let val retval_id = addNewVariable ("_retval",
								     {isTemp=false},
								     ret_type,
								     Ast.DEFAULT)
				     val retval_decl = Ast.VarDecl(retval_id, NONE)
				 in
				     [retval_decl]
				 end
		     )
		   | _ => nil

	  (* -- local decls *)
	  val local_decls = argAddrs_decls @ sca_agr_decls @ retval_decls

	  (* ----- VULN-MODE: TAG RETVAL ----- *)
	  val (tag_ar_retval_stmts, clear_ar_retval_stmts) =
		if !Flags.vuln
		then ( [ wrapSTMT(Ast.Expr (SOME (wrapEXPR (Ast.Void,
							    Ast.Id (getGlobalObj ("_tcvuln_tagARretval")))))) ]
		     , [ wrapSTMT(Ast.Expr (SOME (wrapEXPR (Ast.Void,
							    Ast.Id (getGlobalObj ("_tcvuln_clearARretval")))))) ]
		     )
		else (nil,nil)

	  (**********************************************************
	     processFormals
	       (i       : int,			//-- counter
		formals : Ast.id list,		//-- list of formals
		sca_1st : bool,			//-- true if no previous scalar
		agr_1st : bool)			//-- true if no previous struct
		-> (processArgTags : Ast.statement list,
		    initTags       : Ast.statement list,
		    instVars       : Ast.id list,	//-- instrumented ids; -ptr mode only
		    formalptr_ifs  : Ast.statement list)
	   **********************************************************)
	  fun processFormals(_,nil,_,_) = (nil,nil,nil)
	    | processFormals(i, (formali_id as { uid=formali_pid,
						 ctype=formali_type,
						 location=formali_loc, ...
					       })::tail_ids, sca_1st, agr_1st) =
	      (case formali_type
		 of (* if formal is ellipses, don't do anything *)
		    Ast.Ellipses => (nil,nil,nil)
		  | _ =>
		      let val _ = pushLoc formali_loc

			  (* -- processArgTag(argAddrs, i, &formali, formali_type, sizeof(formali)) -- *)
			  val proc_stmts =
				if not (!Flags.instrMode = Flags.IM_PTR)
				andalso not (!Flags.instrMode = Flags.IM_PTRW)
				andalso aos_child_badly_typed [Rtc.aoId formali_pid]
					orelse may_be_uninit_pid formali_pid
				then let val argAddrs_exp = wrapEXPR(Ast.Void, Ast.Id argAddrs_id)
					 val i_exp = wrapEXPR(Ast.Void, Ast.IntConst i)
					 (* Create EXPR for &formali *)
					 val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
					 val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf formali_exp)
					 val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
					 val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf formali_type)

					 val cty_str = getTypeString formali_type
					 val pat_id = getGlobalFun ("_processArgTag_" ^ cty_str)
					 val pat_exp = wrapEXPR(Ast.Void, Ast.Id pat_id)
					 val call_exp = newInstCallExpr (pat_exp,[argAddrs_exp,i_exp,addrof_exp,sizeof_exp])
				     in  [ wrapSTMT(Ast.Expr (SOME call_exp)) ]  end
				else nil

			  (* -- initTag("&formali") -- *)
			  val init_stmts =
				if aos_child_eq_safe [Rtc.aoId formali_pid]
				then nil
				else let
					 val _ = (count_set_tag := (!count_set_tag) + 1)
					 val init_exp = initTag_id(formali_id,  if aos_child_eq_exposed [Rtc.aoId formali_pid]
										andalso not (may_be_uninit_pid formali_pid)
										then MODE_INIT
										else MODE_DEFAULT)
				     in  [ wrapSTMT(Ast.Expr (SOME init_exp)) ]  end

			  val (if_stmts, sca_1st, agr_1st) = 
			      if (!Flags.indivClearTag)
			      then (nil, sca_1st, agr_1st)
			      else (let
(*SY:TODO(FIX)?: if_stmts should use < or > depending on decreasing-ness of stack???*)
			           val (start_id, end_id, is1st, sca_1st, agr_1st) =
					if isSome (isStructOrUnion formali_type)
					then (agrf_start_id, agrf_end_id, agr_1st, sca_1st, false)
					else (scaf_start_id, scaf_end_id, sca_1st, false, agr_1st)

				   (* -- _s??_start = &formali *)
				   val start_exp = wrapEXPR(Ast.Void, Ast.Id start_id)
				   val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
				   val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf formali_exp)
				   val assign_exp = wrapEXPR(Ast.Void, Ast.Assign (start_exp, addrof_exp))
				   val start_stmt = wrapSTMT(Ast.Expr (SOME assign_exp))

				   (* -- _s??_end = (char * ) &formali + sizeof(formali_type) *)
				   val end_exp = wrapEXPR(Ast.Void, Ast.Id end_id)
				   val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
				   val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf formali_exp)
				   val char_type = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,
								Ast.CHAR,Ast.SIGNASSUMED)
				   val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer char_type, addrof_exp))
				   val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf formali_type)
				   val add_exp = wrapEXPR(Ast.Void, Ast.Binop (Ast.Plus, cast_exp, sizeof_exp))
				   val assign_exp = wrapEXPR(Ast.Void, Ast.Assign (end_exp, add_exp))
				   val end_stmt = wrapSTMT(Ast.Expr (SOME assign_exp))

				   val (start_stmt', end_stmt') =
					if is1st
					then (start_stmt, end_stmt)
					else let
					       (* -- if(_s??_start > (void * ) &formali) start_stmt; *)
						val start_exp = wrapEXPR(Ast.Void, Ast.Id start_id)
						val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
						val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf formali_exp)
						val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer Ast.Void, addrof_exp))
						val compare_exp = wrapEXPR(Ast.Void, Ast.Binop (Ast.Gt, start_exp, cast_exp))
						val start_if_stmt = wrapSTMT(Ast.IfThen (compare_exp, start_stmt))

					       (* -- if(_s??_end < (char * ) &formali + sizeof(formali_type)) end_stmt; *)
						val end_exp = wrapEXPR(Ast.Void, Ast.Id end_id)
						val formali_exp = wrapEXPR(Ast.Void, Ast.Id formali_id)
						val addrof_exp = wrapEXPR(Ast.Void, Ast.AddrOf formali_exp)
						val char_type = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,
									      Ast.CHAR,Ast.SIGNASSUMED)
						val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer char_type, addrof_exp))
						val sizeof_exp = wrapEXPR(Ast.Void, Ast.SizeOf formali_type)
						val add_exp = wrapEXPR(Ast.Void, Ast.Binop (Ast.Plus, cast_exp, sizeof_exp))
						val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer Ast.Void, add_exp))
						val compare_exp = wrapEXPR(Ast.Void, Ast.Binop (Ast.Lt, end_exp, cast_exp))
						val end_if_stmt = wrapSTMT(Ast.IfThen (compare_exp, end_stmt))
					   in
						(start_if_stmt, end_if_stmt)
					   end
			      in
				 ([start_stmt', end_stmt'], sca_1st, agr_1st)
			      end )

		          val (tproc_stmts,tinit_stmts,tif_stmts)
			      = processFormals ((i+1),tail_ids, sca_1st, agr_1st)
		      in
			  (proc_stmts @ tproc_stmts,
			   init_stmts @ tinit_stmts,
			     if_stmts @ tif_stmts
			  ) before popLoc()
		      end
	      )

	  (* ----- PROCESSARGISLOCS, INITTAGS, FORMAL1, FORMALN ----- *)

	  (* -- if any id is a register, remove the register keyword *)
	  val formal_ids = List.map de_register formal_ids
	  (* -- process formals to generate processArgTags, initTags; find formal1, formaln -- *)
	  val (proc_stmts,init_stmts,sfif_stmts) = processFormals (1,formal_ids,true,true)

(* -- DEBUGGING: add a call to _tcdebug_processCall at the head of each function -- *)
(*
val pc_id = getGlobalFun "_tcdebug_processCall"
val pc_exp = wrapEXPR(Ast.Void, Ast.Id pc_id)
val fnname_str = PPL.ppToString PPA.ppId fnid
val fnname_exp = wrapEXPR(Ast.Void, Ast.StringConst fnname_str)
val pc_call_exp = wrapEXPR(Ast.Void, Ast.Call (pc_exp,[fnname_exp]))
val pc_call_stmt = wrapSTMT(Ast.Expr (SOME pc_call_exp))
val sfif_stmts = pc_call_stmt::sfif_stmts
*)

	  (* ----- IF STATEMENT ----- *)
	  val if_and_gct_assign_stmts =
		if (!Flags.instrMode = Flags.IM_PTR)
		orelse (!Flags.instrMode = Flags.IM_PTRW)
		then nil
		else let (* ----- IF STATEMENT ----- *)
			 (* -- "if" predicate: _globalCallTarget == (void * ) &func *)
			 val gct_id = getGlobalObj "_globalCallTarget"
			 val gct_exp = wrapEXPR(Ast.Void, Ast.Id gct_id)
			 val func_exp = wrapEXPR(Ast.Void, Ast.Id fnid)
			 val addr_func_exp = wrapEXPR(Ast.Void, Ast.AddrOf func_exp)
			 val cast_exp = wrapEXPR(Ast.Void, Ast.Cast (Ast.Pointer Ast.Void, func_exp))
			 val compare_exp = wrapEXPR(Ast.Void, Ast.Binop(Ast.Eq, gct_exp, cast_exp))

			 (* -- "then" case -- *)
			 (* -- argAddrs = _globalArgAddrs *)
			 val gat_id = getGlobalObj "_globalArgAddrs"
			 val gat_exp = wrapEXPR(Ast.Void, Ast.Id gat_id)
			 val argAddrs_exp = wrapEXPR(Ast.Void, Ast.Id argAddrs_id)
			 val gat_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(argAddrs_exp, gat_exp))
			 val gat_assign_stmt = wrapSTMT(Ast.Expr (SOME gat_assign_exp))

			 (* -- Create STMT for "then" case *)
			 val then_stmts = gat_assign_stmt::proc_stmts
			 val then_stmt = wrapSTMT(Ast.Compound (nil,then_stmts))

			 (* TODO: add optional "if(globalArgCount != n) warn;" statement? *)

			 (* -- "else" case -- *)
			 (* -- argAddrs = 0 *)
			 val argAddrs_exp = wrapEXPR(Ast.Void, Ast.Id argAddrs_id)
(* TODO: add <zero_exp,pointer> to implicits? *)
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val at_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(argAddrs_exp, zero_exp))
			 val at_assign_stmt = wrapSTMT(Ast.Expr (SOME at_assign_exp))

			 (* TODO: add optional "warning" function here? *)

			 (* -- Create STMT for "else" case *)
			 val else_stmt = wrapSTMT(Ast.Compound (nil,[at_assign_stmt]))

			 (* -- collect if statement -- *)
			 val if_stmt = wrapSTMT(Ast.IfThenElse (compare_exp, then_stmt, else_stmt))

			 (* -- GLOBAL_CALL_TARGET STATEMENT -- *)
			 (* -- _globalCallTarget = 0 -- *)
			 val gct_exp = wrapEXPR(Ast.Void, Ast.Id gct_id)
			 val zero_exp = wrapEXPR(Ast.Void, Ast.IntConst 0)
			 val gct_assign_exp = wrapEXPR(Ast.Void, Ast.Assign(gct_exp, zero_exp))
			 val gct_assign_stmt = wrapSTMT(Ast.Expr (SOME gct_assign_exp))

		     in  [if_stmt, gct_assign_stmt]  end

	  (* ----- FUNCTION BODY ----- *)
	  val cfAO = Rtc.aoId (#uid fnid)
	  val _ = alloca_tmps := nil
	  val _ = alloca_ct_exps := nil
	  val (instformals, idhash) = collectLocalVarInfo (formal_ids, stmt)
	  val body_stmt = (pushLoc stmt_loc; instrStmt (stmt, cfAO, idhash) before popLoc())

	  (* ----- DECLARE ALLOCA TMPS ----- *)
	  val alloca_tmp_decls = List.map (fn id => Ast.VarDecl (id, SOME (Ast.Simple (wrapEXPR (Ast.Void, Ast.IntConst 0)))))
					  (!alloca_tmps)
	  val alloca_ct_stmts = case (!alloca_ct_exps)
				  of e1::tail => [ wrapSTMT (Ast.Expr (SOME (commafyExprs (e1, tail)))) ]
				   | nil => nil
	  val _ = alloca_tmps := nil
	  val _ = alloca_ct_exps := nil

	  (* ----- PROCESSRETURN ----- *)
	  val csf_call_expOp = if (!Flags.indivClearTag)
			       then newClearTagExprs (SOME instformals)
			       else SOME (newProcessReturnExpr (scaf_start_id, scaf_end_id, agrf_start_id, agrf_end_id,
								argAddrs_id, Ast.IntConst 0, Ast.IntConst 0))
	  val csf_call_stmt = wrapSTMT(Ast.Expr (csf_call_expOp))

	  (* ----- COLLECT THE PIECES ----- *)

	  (* -- new_stmt: the new outermost compound stmt of the function *)
	  val new_stmts = tag_ar_retval_stmts
			@ sfif_stmts
			@ init_stmts
			@ if_and_gct_assign_stmts
			@ [body_stmt, csf_call_stmt]
			@ alloca_ct_stmts
			@ clear_ar_retval_stmts
	  val new_stmt = wrapSTMT(Ast.Compound (local_decls @ alloca_tmp_decls,new_stmts))
      in
	  (fnid,formal_ids,new_stmt) before (popLoc(); popLocalEnv())
      end
     )

  fun instrExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc), init_stmts) =
      (if !trace then ppToStdOut PPA.ppExternalDecl edecl else ();
       pushLoc edecl_loc;
       (case coreEdecl
	  of Ast.ExternalDecl (Ast.TypeDecl _) => (edecl, init_stmts)
	   | Ast.ExternalDecl (Ast.VarDecl (id,initExprOp)) =>
	      let val {location=id_loc,ctype=id_type,stClass=stClass,status=status,uid=pid,...} = id
		  val _ = pushLoc id_loc

		  val id_ao = Rtc.aoId pid

		  val new_init_stmts =
		     (* -- if function prototype: do nothing -- *)
		     if isNonPointerFunction id_type
		     then nil
		     (* -- if extern: add to extern_vars -- *)
		     else if (stClass = Ast.EXTERN) then nil before pushExterns(id)
                     (* -- else: add _setUninitTag() or initTag and _setStringTag()s to init_stmts -- *)
		     else case (OPT.simplifyIfScalar ttab (id_type, initExprOp))
			    of SOME initExpr =>
				let val setStrStmts = mapInitExpr doSetStringTag (id, initExpr)
				in (case initExpr
				      of Ast.Simple exp =>
					 ( if aos_child_le_exposed [id_ao]
					   then ( (count_set_tag := (!count_set_tag) + 1)
						; [wrapSTMT (Ast.Expr (SOME (initTag_id(id, if (OPT.isZero exp)
											    then MODE_INIT
											    else MODE_INTEGER))))]
						)
					   else nil
					 ) @ setStrStmts
				       | _ => 
					 ( let val sst_stmts =  if (!Flags.instrMode = Flags.IM_PTR)
								orelse (!Flags.instrMode = Flags.IM_PTRW)
								then nil
								else mapInitExpr doSetScalarTag (id, initExpr)
					   in   if not (null sst_stmts)
						orelse aos_child_le_exposed [id_ao]
						then ( (count_set_tag := (!count_set_tag) + 1)
						     ; wrapSTMT (Ast.Expr (SOME (initTag_id(id,MODE_INIT))))
						       :: sst_stmts
						     )
						else nil
					   end
					 ) @ setStrStmts
				) end
			     | NONE =>
				if aos_child_le_exposed [id_ao]
				then ( (count_set_tag := (!count_set_tag) + 1)
				     ; [wrapSTMT (Ast.Expr (SOME (initTag_id (id, if (!Flags.initGlobals)
									(*	  orelse tsl is exposed	*)
										  then MODE_INIT
										  else MODE_UNINIT))))]
				     )
				else nil (* if ts-level is SAFE: do nothing *)
	      in
		  (edecl, init_stmts @ new_init_stmts) before popLoc()
	      end
	   | Ast.FunctionDef (id,ids,stmt) =>
		 (* instrument the function. *)
		 let val (id',ids',stmt') = instrFunctionDef (id,ids,stmt)
		 in
		     (Ast.DECL (Ast.FunctionDef (id',ids',stmt'),edecl_aid,edecl_loc), init_stmts)
		 end
	   | Ast.ExternalDeclExt _ => (edecl, init_stmts)
	) before popLoc())

  fun instrExternalDecls (nil,init_stmts) = (nil,init_stmts)
  |   instrExternalDecls ((ed::edecls),init_stmts) =
      let val (inst_decl,init_stmts) = instrExternalDecl (ed,init_stmts)
	  val (inst_decls,init_stmts) = instrExternalDecls (edecls,init_stmts)
      in
	  (inst_decl::inst_decls,init_stmts)
      end


  fun instrAst' edecls =
      let
          (********************************************************)
          (* Collect all "touched" global variables and functions *)
          (*                                                      *)
          val tfuncset = ref idset.empty
          val tglobset = ref idset.empty

          fun collectTouchedGlobals (exp as Ast.EXPR (coreExpr,_,_)) =
	     (case coreExpr
		of Ast.Id id =>
		   if isNonPointerFunction (#ctype id)
		   then (tfuncset := idset.add (!tfuncset, id))
		   else if ((#stClass id) = Ast.STATIC)
			orelse ((#stClass id) = Ast.EXTERN)
		   then (tglobset := idset.add (!tglobset, id))
		   else ()
		 | _ => ()
	     )

          val _ = OPT.applyToExp collectTouchedGlobals bundle
          (*                                                      *)
          (*                                                      *)
          (********************************************************)

	  val (edecls',init_stmts) = instrExternalDecls (edecls,nil)

	  (* Now create an initialization function containing init_stmts
	     and add the function to the end of edecls. *)
	  val (initfun_id,initfun_proto,initfun_def) = makeStmtFun(init_stmts, !tfuncset, !tglobset)

	  val _ = print ("instrAst: current AST's initialization function: "
			 ^ (PPL.ppToString PPA.ppId initfun_id) ^ "\n")

	  (* Add prototypes for all the undeclared functions encountered in the file,
	   so their addresses can be taken.  We no longer look up the id again here
	   because the default prototype (extern int foo()) (stored in undeclFuns)
	   obeys the correct semantics.  A new lookup could result in the prototype
	   using typedefs declared lower than even the call itself.  Alternatively,
	   we could resolve typedefs but we'd need a version of getCoreType that
	   descends into function argument and return types. *)
	  val undeclFuns = resetUndeclFuns()
	  val undeclfun_protos = List.map
	      (fn id => wrapDECL(Ast.ExternalDecl(Ast.VarDecl(id, NONE)))) undeclFuns

	  val new_edecls = (initfun_proto::undeclfun_protos) @ edecls' @ [initfun_def]
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
    instrAst' no_tc_edecls
end (* fun instrAst *)


end (* Structure InstrAst *)
