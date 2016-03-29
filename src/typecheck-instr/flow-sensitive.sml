(********************************************************
 Intermediate file format (line based):
 All flow-sensitive lines preceded by !:
    assign -> rtca:
	see outputDataflowOperators below
    rtca -> assign: (consumption not yet written)
	filestem alias:
	    ! @ N S	N=filestem_id, S=filestem
	interesting-fn
	    ! N f p [A|S|N] A   (library arg tracked status, ptr/ptrw mode)
	    ! N f v [A|S|N] A   (library arg tracked status, vuln mode)
				(see also "/": freearg status)
	may-be-uninit:
	    ! N u g A		assign, A=aid
(*	    ! N u c A		call, A=aid ->OBSOLETE?<-	*)
	    ! N u v A # comm	verifyTag|verifyRHS, A=aid
	    ! N u d <AO>	decl
	    ! N u a A		fn argument, A=aid
	    ! N u r A		fn return, A=aid
	redundant:
	    ! N r p [aw] A	verify-ptr: [a]ll/[w]rite, A=aid
	    ! N r t [gv] A	verify-tag: assi[g]n/[v]erify, A=aid
	range/bounds-check:
	    ! N b A		(verify-ptr) A=aid
 ********************************************************)
(********************************************************
 Expression String Representation (estr)
 e ::=	
     A(e)		: AddrOf
     B(e1,e2,<type>)	: Sub	# note: int[ptr] normalized to ptr[int]
     D(e)		: Deref
     I(<int>)		: IntConst
     M(e1,<typelist>)	: Member (struct or union)
     Q(e1,e2,e3)	: QuestionColon
     R(<real>)		: RealConst
     S			: Atom: Member (struct or union)
     T(<type>,e)	: Cast
     V			: Atom: Id, Malloc, StrLit, FnRet
     W(e1,<typelist>)	: Arrow (struct or union)
     Z(<type>)		: SizeOf

     o*(e1,e2)		: Times
     o/(e1,e2)		: Divide
     o%(e1,e2)		: Mod
     o>(e1,e2)		: Gt
     o<(e1,e2)		: Lt
     o|(e1,e2)		: BitOr
     o&(e1,e2)		: BitAnd
     o^(e1,e2)		: BitXor

     O>=(e1,e2)		: Gte
     O<=(e1,e2)		: Lte
     O==(e1,e2)		: Eq
     O!=(e1,e2)		: Neq
     O&&(e1,e2)		: And
     O||(e1,e2)		: Or
     O<<(e1,e2)		: Lshift
     O>>(e1,e2)		: Rshift
     O*=(e1,e2)		: TimesAssign
     O/=(e1,e2)		: DivAssign
     O%=(e1,e2)		: ModAssign
     O^=(e1,e2)		: XorAssign
     O|=(e1,e2)		: OrAssign	
     O&=(e1,e2)		: AndAssign
     O=<(e1,e2)		: LshiftAssign
     O=>(e1,e2)		: RshiftAssign

	# NOTE: type is pointer type for ptr-arith
	#       for plain arith, type is tcVoid
	# NOTE: p+ must normalize int+ptr => ptr+int!
     p+(e1,e2,<type>)	: Plus  (ptr+int, or num+num)
     p-(e1,e2,<type>)	: Minus (ptr-int, or num-num)
     p_(e1,e2,<type>)	: Minus (ptr-ptr)
     p#(e1,e2,<type>)	: PlusAssign
     p=(e1,e2,<type>)	: MinusAssign

     u+(e)		: Uplus
     u!(e)		: Not
     u-(e)		: Negate
     u~(e)		: BitNot

     U+<(e,<type>)	: PreInc
     U+>(e,<type>)	: PostInc
     U-<(e,<type>)	: PreDec
     U->(e,<type>)	: PostDec

 ********************************************************)

structure FlowSensitive : FLOW_SENSITIVE =
struct 

  structure pidset = SplaySetFn (struct
				  type ord_key = Pid.uid
				  val compare = Pid.compare
				 end)  

  structure pidaidmap = BinaryMapFn (struct
					type ord_key = Pid.uid
					val compare = Pid.compare
				     end)

  structure aidset = ListSetFn (struct
				  type ord_key = Aid.uid
				  val compare = Aid.compare
				 end)  

  val pIntersect = pidset.intersection
  val pUnion = pidset.union
  val pUnion' = List.foldl pUnion pidset.empty

  val aUnion = aidset.union
  val aUnion' = List.foldl aUnion aidset.empty

  val pamUnion = pidaidmap.unionWith (fn (l1:Aid.uid list,l2) => l1 @ l2)
  val pamEmpty = pidaidmap.empty

  fun isAndOr binop =
      case binop of Ast.And => true
		  | Ast.Or => true
		  | _ => false

  fun isPostIncDec unop =
      case unop of Ast.PostInc => true
		 | Ast.PostDec => true
		 | _ => false

  (************************** EXPRESSION STRING REPRESENTATION ***************************)
  fun expToStringAndAffLocs (expToAOs,expToCty,tidtab) convert_assignop (exp : Ast.expression) =
  (
      let val tyStrRep = Rtc.tcTypeToString o (OptInterface.ctypeToTcType tidtab)

	  exception exNotRedundant

	  fun expStrRepFn cvt_asgop (oexp as Ast.EXPR (cexp,_,_)) =
	     (let val expStrRep = expStrRepFn false
		  fun estr_affl (opstr, arglist) =
		      let fun processList nil = ("", nil)
			    | processList ((estr,affl)::tail) =
			      let val (estr',affl') = processList tail
			      in  ( if (estr = "") orelse (estr' = "")
				    then estr ^ estr'
				    else estr ^ "," ^ estr'
				  , affl @ affl'
				  )
			      end
			  val (arg_estr,arg_affl) = processList arglist
		      in  (opstr ^ "(" ^ arg_estr ^ ")", arg_affl)  end

		  (* to handle new behavior of expToAOs, which converts array-typed lvalues to &ao(e) *)
		  fun atom_estr_affl opstr =
		      ( case (expToAOs oexp)
			  of [Rtc.aoAddrOf ao] =>
			     ( if TypeUtil.isArray tidtab (expToCty oexp)
			       orelse (case ao of Rtc.aoMalloc _ => true
						| Rtc.aoStringLit _ => true
						| _ => false)
			       then () else print ("ERROR(atom_estr_affl): addrof ao from non-array!\n")
			     ; ("A(" ^ opstr ^ ")", [ao]))
			   | [ao] => (opstr, [ao])
			   | aolist =>
			     ( print ("ERROR(atom_estr_affl): non-singleton aolist (size=" ^ (Int.toString (List.length aolist)) ^ ")!\n")
			     ; ("V-ERROR", nil) )
		      )

		  (* Must convert each array-typed exp to addr-of(exp).	*)
		  (* Call this for non-atom expressions that may be	*)
		  (* array-typed; namely sub,mem,arrow,deref.		*)
		  (* (Atomic exprs that may be arrays are mem and id;	*)
		  (*  those are taken care of above in atom_estr_affl.	*)
		  fun addrofIfArray (opstr, arg_affl) =
			if TypeUtil.isArray tidtab (expToCty oexp)
			then ("A(" ^ opstr ^ ")", arg_affl)
			else (opstr, arg_affl)

		  fun tgtCty exp = case TypeUtil.deref tidtab (expToCty exp)
				     of SOME dty => dty
				      | NONE => Ast.Void
	      in case cexp
		   of Ast.IntConst li => estr_affl ("I", [(Rtc.largeIntToString li, nil)])
		    | Ast.RealConst r => estr_affl ("R", [(Real.toString r, nil)])
		    | Ast.StringConst s => atom_estr_affl "V"
		    | Ast.Call (fexp,exps) => atom_estr_affl "V"
		    | Ast.QuestionColon (e1,e2,e3) => estr_affl ("Q", [expStrRep e1, expStrRep e2, expStrRep e3])
		    | Ast.Assign (e1,e2) => expStrRep e2
		    | Ast.Comma (e1,e2) =>
		      if OptInterface.isVargDummy e1
		      then atom_estr_affl "V"
		      else expStrRep e2
		    | Ast.Sub (e1,e2) =>
			if TypeUtil.isPointer tidtab (expToCty e2)	(* int[pointer]: must flip *)
			then (* NOTE: should properly assert (tgtCty e1) is not pointer, etc.?	*)
			     addrofIfArray (estr_affl ("B", [expStrRep e2, expStrRep e1, (tyStrRep (expToCty oexp), nil)]))
			else addrofIfArray (estr_affl ("B", [expStrRep e1, expStrRep e2, (tyStrRep (expToCty oexp), nil)]))
		    | Ast.Member (exp,mem) =>
			(case expStrRep exp	(* treat var(.mem)* as the "atoms" in the expr encoding *)
			   of ("V",_) => atom_estr_affl "S"
			    | ("S",_) => atom_estr_affl "S"
			    | exp_str_rep =>
			      let val tylist = case OptInterface.getFieldSig tidtab (expToCty exp, mem, {deref=false})
						 of OptInterface.fsStruct tctylist => tctylist
						  | OptInterface.fsUnion tcty => [tcty]
			      in addrofIfArray (estr_affl ("M", [exp_str_rep, ((Rtc.tcTypeListToString tylist) ^ "; ", nil)])) end
			)

		    | Ast.Id id => atom_estr_affl "V"
		    | Ast.Arrow (exp,mem) =>
		      let val tylist = case OptInterface.getFieldSig tidtab (expToCty exp, mem, {deref=true})
					 of OptInterface.fsStruct tctylist => tctylist
					  | OptInterface.fsUnion tcty => [tcty]
		      in addrofIfArray (estr_affl ("W", [expStrRep exp, ((Rtc.tcTypeListToString tylist) ^ "; ", nil)])) end
		    | Ast.Deref exp => addrofIfArray (estr_affl ("D", [expStrRep exp]))
		    | Ast.AddrOf exp =>
			if TypeUtil.isArray tidtab (expToCty exp)	(* HACK: to handle &a, which apparently equals a or &a[0]. *)
			then let val exp_strafl = expStrRep exp		(* NOTE: this will incorrectly(?) treat *&a as equal to *a. *)
			     in if (String.sub (#1 exp_strafl,0)) = #"A"
				then exp_strafl
				else ( print ("WARNING: AddrOf array RHS is not A: " ^ (#1 exp_strafl) ^ "\n")
				     ; estr_affl ("A", [expStrRep exp]))
			     end
			else estr_affl ("A", [expStrRep exp])
		    | Ast.Binop (binop,e1,e2) =>
		     (case binop
		 	of Ast.Plus	=>
			   if TypeUtil.isPointer tidtab (expToCty e2)	(* int plus pointer: must flip *)
			   then (* NOTE: should properly assert (tgtCty e1) is not pointer, etc.?	*)
				estr_affl ("p+",  [expStrRep e2, expStrRep e1, (tyStrRep (tgtCty oexp), nil)])
			   else estr_affl ("p+",  [expStrRep e1, expStrRep e2, (tyStrRep (tgtCty oexp), nil)])
			 | Ast.Minus	=>
			   if TypeUtil.isPointer tidtab (expToCty e2)	(* subtraction of two pointers *)
			   then (* NOTE: should properly assert (tgtCty e1 = tgtCty e2).  Instead, I assume that	*)
				(*	  if e2 is a pointer, e1 must be a pointer of the same type; even if not,	*)
				(*	  e1's type should be the tie-breaker (left-associativity).			*)
				estr_affl ("p_",  [expStrRep e1, expStrRep e2, (tyStrRep (tgtCty e1), nil)])
			   else estr_affl ("p-",  [expStrRep e1, expStrRep e2, (tyStrRep (tgtCty oexp), nil)])
			 | Ast.Times	=> estr_affl ("o*",  [expStrRep e1, expStrRep e2])
			 | Ast.Divide	=> estr_affl ("o/",  [expStrRep e1, expStrRep e2])
			 | Ast.Mod	=> estr_affl ("o%",  [expStrRep e1, expStrRep e2])
			 | Ast.Gt	=> estr_affl ("o>",  [expStrRep e1, expStrRep e2])
			 | Ast.Lt	=> estr_affl ("o<",  [expStrRep e1, expStrRep e2])
			 | Ast.Gte	=> estr_affl ("O>=", [expStrRep e1, expStrRep e2])
			 | Ast.Lte	=> estr_affl ("O<=", [expStrRep e1, expStrRep e2])
			 | Ast.Eq	=> estr_affl ("O==", [expStrRep e1, expStrRep e2])
			 | Ast.Neq	=> estr_affl ("O!=", [expStrRep e1, expStrRep e2])
			 | Ast.And	=> estr_affl ("O&&", [expStrRep e1, expStrRep e2])
			 | Ast.Or	=> estr_affl ("O||", [expStrRep e1, expStrRep e2])
			 | Ast.BitOr	=> estr_affl ("o|",  [expStrRep e1, expStrRep e2])
			 | Ast.BitAnd	=> estr_affl ("o&",  [expStrRep e1, expStrRep e2])
			 | Ast.BitXor	=> estr_affl ("o^",  [expStrRep e1, expStrRep e2])
			 | Ast.Lshift	=> estr_affl ("O<<", [expStrRep e1, expStrRep e2])
			 | Ast.Rshift	=> estr_affl ("O>>", [expStrRep e1, expStrRep e2])
			 | Ast.PlusAssign	=>
			   if cvt_asgop then estr_affl ("p#", [expStrRep e1, expStrRep e2, (tyStrRep (tgtCty oexp), nil)])
					else (expStrRep e1)
			 | Ast.MinusAssign	=>
			   if cvt_asgop then estr_affl ("p=", [expStrRep e1, expStrRep e2, (tyStrRep (tgtCty oexp), nil)])
					else (expStrRep e1)
			 | Ast.TimesAssign	=> if cvt_asgop then estr_affl ("O*=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.DivAssign	=> if cvt_asgop then estr_affl ("O/=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.ModAssign	=> if cvt_asgop then estr_affl ("O%=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.XorAssign	=> if cvt_asgop then estr_affl ("O^=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.OrAssign		=> if cvt_asgop then estr_affl ("O|=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.AndAssign	=> if cvt_asgop then estr_affl ("O&=", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.LshiftAssign	=> if cvt_asgop then estr_affl ("O=<", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.RshiftAssign	=> if cvt_asgop then estr_affl ("O=>", [expStrRep e1, expStrRep e2]) else (expStrRep e1)
			 | Ast.BinopExt _	=> raise exNotRedundant
		     )
		    | Ast.Unop (unop,exp) =>
		     (case unop
			of Ast.Uplus	=> estr_affl ("u+", [expStrRep exp])
			 | Ast.Not	=> estr_affl ("u!", [expStrRep exp])
			 | Ast.Negate	=> estr_affl ("u-", [expStrRep exp])
			 | Ast.BitNot	=> estr_affl ("u~", [expStrRep exp])
			 | Ast.PreInc	=> if cvt_asgop then estr_affl ("U+<", [expStrRep exp, (tyStrRep (tgtCty oexp), nil)])
							else (expStrRep exp)
			 | Ast.PostInc	=> if cvt_asgop then estr_affl ("U+>", [expStrRep exp, (tyStrRep (tgtCty oexp), nil)])
							else (expStrRep exp)
			 | Ast.PreDec	=> if cvt_asgop then estr_affl ("U-<", [expStrRep exp, (tyStrRep (tgtCty oexp), nil)])
							else (expStrRep exp)
			 | Ast.PostDec	=> if cvt_asgop then estr_affl ("U->", [expStrRep exp, (tyStrRep (tgtCty oexp), nil)])
							else (expStrRep exp)
			 | Ast.UnopExt _ => raise exNotRedundant
		     )
		    | Ast.SizeOf ty => estr_affl ("Z", [(tyStrRep ty, nil)])
		    | Ast.Cast (ctype,exp) => estr_affl ("T", [(tyStrRep ctype, nil), expStrRep exp])
		    | Ast.EnumId (pid,li) => estr_affl ("I", [(Rtc.largeIntToString li, nil)])
		    | Ast.ExprExt ee => raise exNotRedundant
		    | Ast.ErrorExpr => raise exNotRedundant
	      end
             )
     in
	 ( let val (estr,affl) = expStrRepFn convert_assignop exp
	   in  ("{" ^ estr ^ "}", affl)  end
	 ) handle exNotRedundant => ("@",nil)
     end
  ) (* end fun expToStringAndAffLocs (expToAOs,expToCty,tidtab) convert_assignop(exp : Ast.expression) *)


  (************************** INTER-PROCEDURAL ANALYSES ***************************)
  fun outputDataflowOperators (bundle as {ast=edecls,tidtab,auxiliaryInfo={aidtab,...},...} : ParseToAst.astBundle,
				tc_inc_aid, expToCty, os) =
  (let
       val expToAOs = OptInterface.expToAbsObjects (expToCty,tidtab,{deref=false})
       val expToStrAndAffLocs = expToStringAndAffLocs (expToAOs,expToCty,tidtab) false
       val expToStrAndAffLocsCvtAsgOp = expToStringAndAffLocs (expToAOs,expToCty,tidtab) true

       (* file format markers *)
       val mPfx		= "! "	(*flow-sensitive prefix*)
       val  mStart	= "{ "
       val  mNext	= "; "
       val  mEnd	= "} "
       val   mParallel	= "p "
       val   mBranch	= "b "
       val   mWhile	= "w "
       val   mDoWhile	= "d "
       val   mFor	= "f "
       val   mIf	= "i "
       val   mSwitch	= "s "
       val   mNode	= "n "
       val   mFunction	= "x "
       val  mLabel	= ": "
       val  mJump	= "> "
       val   mGoto	= "g "
       val   mBreak	= "b "
       val   mContinue	= "c "
       val   mReturn	= "r "
       val   mVReturn	= "v "
       val   mCase	= "s "
       val   mDefault	= "d "
       val  mCall	= "c "
       val  mAssign	= "a "
       val  mPredicate	= "p "
       val   mPrAnd	= "a "
       val   mPrOr	= "o "
       val   mPrQC	= "q "
       val   mPrStmt	= "s "
       val   mPrSwitch	= "w "
       val  mFormal	= "f "
       val  mLocalDecl	= "l "
       val  mStaticDecl	= "s "
       val  mMallocDecl	= "m "
       val  mVerify	= "v "
       val   mVTag	= "t "
       val   mVTagPtr	= "u "
       val   mVTagPtrW	= "v "
       val   mVRhs	= "r "
       val   mVRhsPtr	= "s "
       val   mVPtr	= "p "
       val   mVPtrW	= "w "

       fun aidToExtString (aid:Aid.uid) = if aid = 0
					  then "0"
					  else (Int.toString (aid - tc_inc_aid))

       datatype structure_marker = SM_PARALLEL
				 | SM_BRANCH
				 | SM_WHILE
				 | SM_DOWHILE
				 | SM_FOR
				 | SM_IF
				 | SM_SWITCH
				 | SM_NODE of Aid.uid
				 | SM_FUNCTION of Pid.uid

       fun sm_to_str sm = case sm of SM_PARALLEL => mParallel
				   | SM_BRANCH => mBranch
				   | SM_WHILE => mWhile
				   | SM_DOWHILE => mDoWhile
				   | SM_FOR => mFor
				   | SM_IF => mIf
				   | SM_SWITCH => mSwitch
				   | SM_NODE aid => mNode ^ (aidToExtString aid) ^ " "
				   | SM_FUNCTION pid => mFunction ^ (Int.toString pid) ^ " "

       fun mark_start sm arg = TextIO.output (os, mPfx ^ mStart ^ (sm_to_str sm) ^ (Int.toString arg) ^ "\n")
       fun mark_next sm = TextIO.output (os, mPfx ^ mNext ^ (sm_to_str sm) ^ "\n")
       fun mark_end sm = TextIO.output (os,  mPfx ^ mEnd ^ (sm_to_str sm) ^ "\n")

       datatype vt_kind = VT_TAG | VT_RHS  | VT_NONE
       datatype vp_kind = VP_PTR | VP_PTRW | VP_NONE
       datatype pr_kind = PR_AND | PR_OR | PR_QC | PR_STMT | PR_SWITCH

       fun vkp_to_str (vtk,vpk) = case (vtk,vpk)
				    of (VT_TAG, VP_PTR)  => mVTagPtr
				     | (VT_TAG, VP_PTRW) => mVTagPtrW
				     | (VT_TAG, VP_NONE) => mVTag
				     | (VT_RHS, VP_PTR)  => mVRhsPtr
				     | (VT_RHS, VP_PTRW) => "x " (* should never occur *)
				     | (VT_RHS, VP_NONE) => mVRhs
				     | (VT_NONE,VP_PTR)  => mVPtr
				     | (VT_NONE,VP_PTRW) => mVPtrW
				     | (VT_NONE,VP_NONE) => "x " (* should never occur *)

       datatype malloc_kind = MK_MALLOC | MK_CALLOC | MK_ALLOCA

       datatype dfa_node = DF_CALL of Aid.uid * Ast.expression * Ast.expression list
			 | DF_ASSIGN of Aid.uid * (Rtc.absObject list * (string * Rtc.absObject list))
						* (Rtc.absObject list * (string * Rtc.absObject list))
			 | DF_PREDICATE of pr_kind * Aid.uid * (string * Rtc.absObject list)
			 | DF_FORMAL of int * Ast.id
			 | DF_LOCAL_DECL of Ast.id * {iszeroed:bool}
			 | DF_STATIC_DECL of Ast.id
			 | DF_MALLOC_DECL of Aid.uid * (Rtc.absObject list * (string * Rtc.absObject list)) * malloc_kind
			 | DF_VERIFY of (vt_kind * vp_kind) * Aid.uid * Rtc.absObject list * (string * Rtc.absObject list)

       fun aosToString nil = ". "
	 | aosToString (ao::tail) = ", " ^ (Rtc.aoToString ao) ^ (aosToString tail)

       fun write_df node =
	  (case node
	     of DF_CALL (aid,fexp,exps) =>
		TextIO.output (os, mPfx ^ mCall
					^ (aidToExtString aid) ^ " "
					^ (aosToString (expToAOs fexp))
					^ (Int.toString (List.length exps)) ^ " "
					^ (String.concat
						(List.map (fn (exp as Ast.EXPR (_,aid,_)) => 
							      let val exp_aos = expToAOs exp
								  val (exp_estr, exp_affaos) = expToStrAndAffLocs exp
							      in ((aidToExtString aid) ^ " "
								  ^ (aosToString (OptInterface.arrayToPtrAOs exp_aos))
								  ^ (aosToString exp_affaos)
								  ^ exp_estr
								  ^ " "
								 )
							      end
							   ) exps))
					^ "\n")
	      | DF_ASSIGN (aid,(e1aos,(e1str,e1affaos)),(e2aos,(e2str,e2affaos))) =>
		TextIO.output (os, mPfx ^ mAssign
					^ (aidToExtString aid) ^ " "
					^ (aosToString e1aos)
					^ (aosToString (OptInterface.arrayToPtrAOs e2aos))
					^ (aosToString e1affaos)
					^ (aosToString e2affaos)
					^ e1str
					^ e2str
					^ "\n")
	      | DF_PREDICATE (prk,aid,(estr,affaos)) =>
		TextIO.output (os, mPfx ^ mPredicate
					^ (case prk of PR_AND	=> mPrAnd
						     | PR_OR	=> mPrOr
						     | PR_QC	=> mPrQC
						     | PR_STMT	=> mPrStmt
						     | PR_SWITCH=> mPrSwitch
					  )
					^ (aidToExtString aid) ^ " "
					^ (aosToString affaos)
					^ estr
					^ "\n")
	      | DF_FORMAL (i,{uid,...}) =>
		TextIO.output (os, mPfx ^ mFormal
					^ (Int.toString i) ^ " "
					^ (Int.toString uid) ^ " "
					^ "\n")
	      | DF_LOCAL_DECL ({uid,...},{iszeroed}) =>
		TextIO.output (os, mPfx ^ mLocalDecl
					^ (if iszeroed then "z " else "")
					^ (Int.toString uid) ^ " "
					^ "\n")
	      | DF_STATIC_DECL {uid,...} =>
		TextIO.output (os, mPfx ^ mStaticDecl
					^ (Int.toString uid) ^ " "
					^ "\n")
	      | DF_MALLOC_DECL (aid,(eaos,(estr,affaos)),mkind) =>
		TextIO.output (os, mPfx ^ mMallocDecl
					^ (if mkind = MK_CALLOC
					   then "z "		(* "z" = "zeroed" *)
					   else if mkind = MK_ALLOCA
					   then "a "
					   else "")
					^ (Int.toString aid) ^ " " (* ao-lookup ahash-indexed aid, DON'T adjust by tc_inc_aid *)
					^ (aosToString eaos)
					^ (aosToString affaos)
					^ estr
					^ "\n")
	      | DF_VERIFY (vkp,aid,aos,(estr,affaos)) =>
		TextIO.output (os, mPfx ^ mVerify
					^ (vkp_to_str vkp)
					^ (aidToExtString aid) ^ " "
					^ (aosToString aos)
					^ (aosToString affaos)
					^ estr
					^ "\n")
	  )

       datatype dfa_substep = DS_PARALLEL of dfa_substep list list
			    | DS_BRANCH of dfa_node * (dfa_substep list * dfa_substep list)
			    | DS_NODE of dfa_node

       fun df_asgverify_eq (DS_NODE df1, DS_NODE df2) =
	   (case (df1,df2)
	      of (DF_ASSIGN args1, DF_ASSIGN args2) => args1 = args2
	       | (DF_VERIFY args1, DF_VERIFY args2) => args1 = args2
	       | _ => false
	   )
	 | df_asgverify_eq (_,_) = false

       val rmdup = Rtc.rmdup df_asgverify_eq

       fun write_dsl nil = ()
	 | write_dsl (ds::tail) =
	   let val _ = case ds
			 of DS_PARALLEL dsll => write_dsll SM_PARALLEL dsll
			  | DS_BRANCH (prednode,(dsl1,dsl2)) => write_dsll SM_BRANCH [[DS_NODE prednode],dsl1,dsl2]
			  | DS_NODE df => write_df df
	   in  write_dsl tail  end

       and write_dsll sm dsll =
	   let val _ = mark_start sm (List.length dsll)
	       val _ = List.app (fn dsl => ( mark_next sm
					   ; write_dsl dsl )
				 ) dsll
	       val _ = mark_end sm
	   in  ()  end

       fun write_node (aid,labellist) dsl =
	   let val _ = mark_start (SM_NODE aid) (List.length labellist)
	       val _ = List.app (fn str => TextIO.output (os, mPfx ^ mLabel ^ (aidToExtString aid) ^ " " ^ str ^ "\n")) labellist
	       val _ = mark_next (SM_NODE aid) (*redundant-verify*)
	       val _ = write_dsl dsl
	       val _ = mark_end (SM_NODE aid)
	   in  ()  end

       fun build_DS_PARALLEL dsll =
	   let
	       val dsll_no_nils = List.filter (not o null) dsll

	       fun collapseParallel nil = nil
		 | collapseParallel (dsl::tail) =
		   (case dsl of (DS_PARALLEL dsll)::nil => (collapseParallel dsll) @ (collapseParallel tail)
			      | dsl => dsl :: (collapseParallel tail)
		    )

	       val dsll' = collapseParallel dsll_no_nils
	   in
	       case dsll'
		 of nil => nil		(* if empty - no nodes *)
		  | (dsl::nil) => dsl		(* if singleton - return it *)
		  | dsll' => [DS_PARALLEL dsll']	(* if set - build parallel *)
	   end

       datatype jump_marker = JM_GOTO of string
			    | JM_BREAK
			    | JM_CONTINUE
			    | JM_RETURN of Rtc.absObject list * (string * Rtc.absObject list)
			    | JM_VOIDRETURN
			    | JM_CASE of LargeInt.int
			    | JM_DEFAULT

       fun write_jump (aid,labellist) jm =
	   let val aid = if null labellist then aid
					   else ( write_node (aid,labellist) nil ; 0 )
	   in  case jm
		 of JM_GOTO label => TextIO.output (os, mPfx ^ mJump ^ mGoto ^ (aidToExtString aid) ^ " " ^ label ^ "\n")
		  | JM_BREAK => TextIO.output (os, mPfx ^ mJump ^ mBreak ^ (aidToExtString aid) ^ "\n")
		  | JM_CONTINUE => TextIO.output (os, mPfx ^ mJump ^ mContinue ^ (aidToExtString aid) ^ "\n")
		  | JM_RETURN (aos, (estr, affaos)) =>
			TextIO.output (os, mPfx ^ mJump
						^ mReturn
						^ (aidToExtString aid) ^ " "
						^ (aosToString aos)
						^ (aosToString affaos)
						^ estr
						^ "\n")
		  | JM_VOIDRETURN => TextIO.output (os, mPfx ^ mJump ^ mVReturn ^ (aidToExtString aid) ^ "\n")
		  | JM_CASE li => TextIO.output (os, mPfx ^ mJump ^ mCase ^ (aidToExtString aid) ^ " " ^ (Rtc.largeIntToString li) ^ "\n")
		  | JM_DEFAULT => TextIO.output (os, mPfx ^ mJump ^ mDefault ^ (aidToExtString aid) ^ "\n")
	   end

       datatype asg_side = AS_RHS | AS_LHS | AS_NONE

       fun flagsToVKpair {enforce:bool,verify_ptr:bool,aside:asg_side} =
	 ( if enforce
	   then VT_TAG
	   else if aside = AS_RHS
		then VT_RHS
		else VT_NONE
	 , if verify_ptr
	   then if aside = AS_LHS
		then VP_PTRW
		else VP_PTR
	   else VP_NONE
	 )

       (* post-incr dsl (pdsl) will be folded at sequence points: fncall, &&, ||, ?:, comma, semicolon *)
       fun foldPostIncr (dsl, nil) = dsl
	 | foldPostIncr (dsl, pdsl) = dsl @ pdsl
	(****************************************************************)
	(* Current solution: clump pdsl *sequentially* after dsl	*)
	(* NOTE that this is unsafe w.r.t. the specs -- but surely	*)
	(* it's implemented by all?					*)
	(****************************************************************)
	(* The alternative, more conservative, solutions was:		*)
(*	   build_DS_PARALLEL (dsl::(List.map (fn dsl => [dsl]) pdsl))	*)
       (* The second return value of processExpr is post-increment assignment, which must be *)
       (* parallelized because the assignment may occur at any point before next sequence point. *)
       (* NOTE: will use it to build a "top-level" parallel, which may be imprecise, because *)
       (*	otherwise it's possible to build a non-trivial graph, e.g.:		*)
	(*	c = a[i++] * b[j++] should be	*)(*	but will be approximated as	*)
	(*	         -o-			*)(*	    ------o-------		*)
	(*	       /     \			*)(*	   /   /     \    \		*)
	(*	   vt(i)      vt(j)		*)(*	  /  vt(i)   vt(j) \		*)
	(*	  /   |       |    \		*)(*	 |    |       |     |		*)
	(*	 |   a[i]   b[j]    |		*)(*	 |   a[i]   b[j]    |		*)
	(*	i++     \   /      j++		*)(*	i++     \   /      j++		*)
	(*	 |    c=a[i]*b[j]   |		*)(*	 |    c=a[i]*b[j]   |		*)
	(*	  \       |        /		*)(*	  \       |        /		*)
	(*	   \      |       /		*)(*	   \      |       /		*)
	(*	    ------o-------		*)(*	    ------o-------		*)


       fun processExpr (outer_exp as Ast.EXPR (coreExpr,aid,loc), {enforce:bool,verify_ptr:bool,aside:asg_side}) =
	  (case coreExpr
	     of Ast.IntConst li => (nil,nil)
	      | Ast.RealConst r => (nil,nil)
	      | Ast.StringConst s => (nil,nil)
	      | Ast.Call (fexp,exps) =>
		let
		    (* used to fold post-incr assigns, since there is a seq point before the function call*)
		    fun pairToList (dsl, nil) = [dsl]
		      | pairToList (dsl, pdsl) = [dsl, pdsl]

		    fun processExprs (nil) = nil
		      | processExprs (head::tail) =
			( pairToList (processExpr (head, {enforce=false,verify_ptr=true,aside=AS_RHS}))
			) @ (processExprs (tail))
		    val exps_dsll = processExprs exps  (* enforce=false; but if skip call, enforce may be true? false is safe approx. *)

		in  if OptInterface.isMalloc fexp
		    orelse OptInterface.isAlloca fexp
		    then ( ( build_DS_PARALLEL exps_dsll )
			 @ [ DS_NODE (DF_MALLOC_DECL ( aid
						     , getOpt(OptInterface.evalMallocSize
									(fn e => SOME (expToAOs e, expToStrAndAffLocs e))
									(fn cty => let val aid = Aid.new ()
										       val _ = Aidtab.insert (aidtab, aid, cty)
										   in  aid  end)
									(fexp,exps)
							     , (nil,("@",nil)))
						     , if (OptInterface.isCalloc fexp) then MK_CALLOC
						       else if (OptInterface.isAlloca fexp) then MK_ALLOCA
						       else MK_MALLOC)
				      ) ], nil)
		    else
			 let val fexp_dsl = pairToList (processExpr (fexp, {enforce=true,verify_ptr=true,aside=AS_NONE}))
			     val fexp_exps_dsl = build_DS_PARALLEL (fexp_dsl @ exps_dsll)
			     val call_ds = DS_NODE (DF_CALL (aid,fexp,exps))

			     (* do verifyTag/verifyRhs with return object *)
			     val vkp = flagsToVKpair {enforce=enforce,verify_ptr=false,aside=aside}
			     val ver_dsl = if vkp = (VT_NONE, VP_NONE)
					   then nil
					   else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
			 in  (fexp_exps_dsl @ [call_ds] @ ver_dsl, nil)  end
		end

	      | Ast.QuestionColon (e1,e2,e3) =>
		let val e1_dsl = foldPostIncr (processExpr (e1,{enforce=true,verify_ptr=true,aside=AS_NONE}))
		    val (e2_dsl,e2_pdsl) = processExpr (e2,{enforce=enforce,verify_ptr=verify_ptr,aside=aside})
		    val (e3_dsl,e3_pdsl) = processExpr (e3,{enforce=enforce,verify_ptr=verify_ptr,aside=aside})
		    val e2_e3_ds = DS_BRANCH (DF_PREDICATE (PR_QC, aid, expToStrAndAffLocs e1),(e2_dsl,e3_dsl))
		in  (e1_dsl @ [e2_e3_ds], e2_pdsl @ e3_pdsl)  end

	      | Ast.Assign (e1,e2) =>
		let val (e1_dsl,e1_pdsl) = processExpr (e1,{enforce=false,verify_ptr=true,aside=AS_LHS})
		    val (e2_dsl,e2_pdsl) = processExpr (e2,{enforce=enforce,verify_ptr=true,aside=AS_RHS})
		    val e1_e2_dsl = build_DS_PARALLEL [e1_dsl,e2_dsl]
		    val write_ds = DS_NODE (DF_ASSIGN (aid,(expToAOs e1,expToStrAndAffLocs e1),(expToAOs e2,expToStrAndAffLocs e2)))
		in  (e1_e2_dsl @ [write_ds], e1_pdsl @ e2_pdsl)  end

	      | Ast.Comma (e1,e2) =>
		if OptInterface.isVargDummy e1
		then (nil,nil)
		else let val e1_dsl = foldPostIncr (processExpr (e1,{enforce=false,verify_ptr=false,aside=AS_NONE}))
			 val (e2_dsl, e2_pdsl) = processExpr (e2,{enforce=enforce,verify_ptr=verify_ptr,aside=aside})
		     in  (e1_dsl @ e2_dsl, e2_pdsl)  end

	      | Ast.Sub (e1,e2) =>
		let val (e1_dsl, e1_pdsl) = processExpr (e1,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val (e2_dsl, e2_pdsl) = processExpr (e2,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val e1_e2_dsl = build_DS_PARALLEL [e1_dsl,e2_dsl]
		    val vkp = flagsToVKpair { enforce = enforce
					    , verify_ptr = verify_ptr andalso not (TypeUtil.isArray tidtab (expToCty outer_exp))
					    , aside = aside}
		in  (e1_e2_dsl
		    @ ( if vkp = (VT_NONE, VP_NONE)
			then nil
			else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
		      ), e1_pdsl @ e2_pdsl)
		end

	      | Ast.Member (exp,mem) =>
		let val (dsl,pdsl) = processExpr (exp,{ enforce = not (OptInterface.isLvalExpr exp)
						      , verify_ptr = false , aside=AS_NONE })
		in (dsl @ ( let val vkp = flagsToVKpair {enforce = enforce
							,verify_ptr = verify_ptr andalso not (TypeUtil.isArray tidtab (expToCty outer_exp))
										 andalso (OptInterface.isDerefLvalExpr exp)
							,aside = aside}
			    in  if vkp = (VT_NONE, VP_NONE)
				then nil
				else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
			    end
			  ), pdsl)
		end
	      | Ast.Arrow (exp,mem) =>
		let val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		in (dsl @ ( let val vkp = flagsToVKpair {enforce    = enforce
							,verify_ptr = verify_ptr andalso not (TypeUtil.isArray tidtab (expToCty outer_exp))
							,aside      = aside}
			    in  if vkp = (VT_NONE, VP_NONE)
				then nil
				else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
			    end
			  ), pdsl)
		end
	      | Ast.Deref exp =>
		let val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		in (dsl @ ( let val vkp = flagsToVKpair {enforce    = enforce
							,verify_ptr = verify_ptr andalso not (TypeUtil.isArray tidtab (expToCty outer_exp))
							,aside      = aside}
			    in  if vkp = (VT_NONE, VP_NONE)
				then nil
				else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
			    end
			  ), pdsl)
		end
	      | Ast.AddrOf exp => processExpr (exp,{enforce=false,verify_ptr=false,aside=AS_NONE})
	      | Ast.Binop (binop,e1,e2) =>
		if isAndOr binop	(* logical and (&&), logical or (||) *)
		then let val e1_dsl = foldPostIncr (processExpr (e1,{enforce=true,verify_ptr=true,aside=AS_NONE}))
			 val (e2_dsl, e2_pdsl) = processExpr (e2,{enforce=true,verify_ptr=true,aside=AS_NONE})
			 val e2_br_ds = DS_BRANCH ( DF_PREDICATE ( if binop = Ast.And then PR_AND else PR_OR
								 , aid, expToStrAndAffLocs e1)
						  , if binop = Ast.And then (e2_dsl,nil) else (nil,e2_dsl))
		     in  (e1_dsl @ [e2_br_ds], e2_pdsl)  end
		else if OptInterface.isAssignBinop binop
		     then let (* note: even for assign-binops, e2 is not rhs *)
			      val (e1_dsl, e1_pdsl) = processExpr (e1,{enforce=true,verify_ptr=true,aside=AS_LHS})
			      val (e2_dsl, e2_pdsl) = processExpr (e2,{enforce=true,verify_ptr=true,aside=AS_NONE})
			  in ((build_DS_PARALLEL [e1_dsl,e2_dsl])
(* TODO?: add DF_VERIFY VK_RHS of outer_exp? More "systematic", but not useful to analyses? (unop below also) *)
			      @ [DS_NODE (DF_ASSIGN (aid,(expToAOs e1,expToStrAndAffLocs e1)
							,(expToAOs outer_exp,expToStrAndAffLocsCvtAsgOp outer_exp)))]
			     , e1_pdsl @ e2_pdsl)
			  end
		     else let val (e1_dsl, e1_pdsl) = processExpr (e1,{enforce=true,verify_ptr=true,aside=AS_NONE})
			      val (e2_dsl, e2_pdsl) = processExpr (e2,{enforce=true,verify_ptr=true,aside=AS_NONE})
			  in (build_DS_PARALLEL [e1_dsl,e2_dsl], e1_pdsl @ e2_pdsl)
			  end

	      | Ast.Unop (unop,exp) =>
		let val ((e_dsl,e_pdsl), unop_dsl) =
			if OptInterface.isAssignUnop unop
			then ( processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_LHS})	(* actually both AS_LHS and AS_RHS *)
			     , [ DS_NODE (DF_ASSIGN (aid,(expToAOs exp,expToStrAndAffLocs exp)
							,(expToAOs outer_exp,expToStrAndAffLocsCvtAsgOp outer_exp))) ])
			else ( processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
			     , nil)
		in  if isPostIncDec unop
		    then (e_dsl, e_pdsl @ unop_dsl)	(* only place where pdsl is generated *)
		    else (e_dsl @ unop_dsl, e_pdsl)
		end
	      | Ast.SizeOf ty => (nil,nil)
	      | Ast.Cast (ctype,exp) => (* Too lazy to detect conversion; enforce is safe approx? *)
		processExpr (exp,{enforce=enforce,verify_ptr=true,aside=aside})
	      | Ast.Id id => (( if (TypeUtil.isNonPointerFunction tidtab (#ctype id))
				orelse (TypeUtil.isArray tidtab (#ctype id))
				then nil (* don't verify if is function or array *)
				else let val vkp = flagsToVKpair {enforce=enforce,verify_ptr=false,aside=aside}
				     in if vkp = (VT_NONE, VP_NONE)
					then nil
					else [DS_NODE (DF_VERIFY (vkp,aid,expToAOs outer_exp,expToStrAndAffLocs outer_exp))]
				     end
				), nil)
	      | Ast.EnumId (pid,li) => (nil,nil)
	      | Ast.ExprExt ee => (nil,nil)
	      | Ast.ErrorExpr => (nil,nil)
	  )

       fun processInitExprs (nil) = nil
	 | processInitExprs (head::tail) = 
		let val head_dsl = processInitExpr  head
		    val tail_dsl = processInitExprs tail
		in  head_dsl @ tail_dsl  end
       and processInitExpr iexpr =
	   (case iexpr of Ast.Simple exp => foldPostIncr (processExpr (exp,{enforce=false,verify_ptr=true,aside=AS_RHS}))
					    (* technically, postincr should never occur within init-exprs; but if it's *)
					    (* allowed by GNU-C, there's probably a sequence point between init-exprs? *)
			| Ast.Aggregate iexps => processInitExprs iexps
	   )

       fun doAssign (dstao, srcaos, tcty, src_exp) =
	   [ DS_NODE (DF_ASSIGN (0, (* no aid for initializers!? *)
				 ([dstao], ("@",nil)),
				 (srcaos, expToStrAndAffLocs src_exp))) ]

       (*************************************************************************)
       (* NOTE: Must treat initializer assignments as parallel assignments.	*)
       (* This is necessary, because the normalized LHS may alias each other.	*)
       (* One example to demonstrate this is in RAN analysis. Consider:		*)
       (*   char *p[] = {"a","abc"};						*)
       (* If not parallelized, the initializer assignments			*)
       (*   *p <= "a"								*)
       (*   *p <= "abc"								*)
       (* would be serialized, resulting in an initial RAN fact that *p points	*)
       (* to a strlit of length 3+1.						*)
       (* When parallelized, the result of each assignment will be meet-ed.	*)
       (*************************************************************************)
       fun processInitializerAssigns pid (cty, initExprOp) =
	   build_DS_PARALLEL
		(List.map (fn dsl => [dsl])
			  (rmdup ( OptInterface.handleInitializer
					(tidtab, expToCty) 
					doAssign
					(fn () => Rtc.aoId pid)
					(cty, initExprOp)
				 )))

       fun processDecls nil = (nil,nil)
	 | processDecls (head::tail) =
		let val (head_dsl,head_edsl) = processDecl  head
		    val (tail_dsl,tail_edsl) = processDecls tail
		in  ( head_dsl @ tail_dsl, head_edsl @ tail_edsl )  end

       and processDecl decl =
	  (case decl
	     of Ast.TypeDecl _ => (nil,nil)
	      | Ast.VarDecl (id as {uid=pid,ctype,stClass,...}, initExprOp) =>
		if (TypeUtil.isNonPointerFunction tidtab ctype)
		orelse (stClass = Ast.EXTERN)
		then (nil,nil)
		else let val (iexp_dsl,asg_dsl) =
				case initExprOp
				  of SOME iexp => ( processInitExpr iexp
						  , processInitializerAssigns pid (ctype, initExprOp)
						  )
				   | NONE => ( nil
					     , if (stClass = Ast.STATIC)
					       then processInitializerAssigns pid (ctype, NONE)
					       else nil
					     )
			 val iexp_asg_dsl = iexp_dsl @ asg_dsl
		     in
			 if (stClass = Ast.STATIC)
			 then (nil, (DS_NODE (DF_STATIC_DECL id))::iexp_asg_dsl)
			 else ((DS_NODE (DF_LOCAL_DECL (id,{iszeroed=(isSome initExprOp)})))::iexp_asg_dsl, nil)
		     end
	  )

       fun processStmts nil = nil
	 | processStmts (head::tail) =
		let val head_edsl = processStmt  (head,nil)
		    val tail_edsl = processStmts tail
		in  head_edsl @ tail_edsl  end

       and processStmt (Ast.STMT (coreStmt,aid,_), labellist) =
	  (case coreStmt
	     of Ast.Expr expOp => (case expOp
				     of SOME exp =>
					let val exp_dsl = foldPostIncr (processExpr (exp,{enforce=false,verify_ptr=false,aside=AS_NONE}))
					    val _ = write_node (aid,labellist) exp_dsl
					in  nil  end
				      | NONE => if null labellist
						then nil
						else ( write_node (aid,labellist) nil
						     ; nil )
				  )
	      | Ast.Compound (decls, stmts) => 
		let val (decls_dsl, decls_edsl) = processDecls decls
		    val _ = write_node (aid,labellist) decls_dsl
		    val stmts_edsl = processStmts stmts
		in  decls_edsl @ stmts_edsl  end

	      | Ast.While (exp,stmt) =>
		let val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val exp_dsl = foldPostIncr (dsl @ [DS_NODE (DF_PREDICATE (PR_STMT, aid, expToStrAndAffLocs exp))],pdsl)
		    val _ = mark_start SM_WHILE aid
		    val _ = write_node (aid,labellist) exp_dsl
		    val _ = mark_next SM_WHILE (*redundant-verify*)
		    val stmt_edsl = processStmt (stmt,nil)
		    val _ = mark_end SM_WHILE
		in  stmt_edsl  end

	      | Ast.Do (exp,stmt) =>
		let val _ = mark_start SM_DOWHILE aid	(*NOTE: output in "reverse" (exp,stmt) order *)
		    val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val exp_dsl  = foldPostIncr (dsl @ [DS_NODE (DF_PREDICATE (PR_STMT, aid, expToStrAndAffLocs exp))],pdsl)
		    val _ = write_node (aid,nil) exp_dsl
		    val _ = mark_next SM_DOWHILE (*redundant-verify*)
		    val stmt_edsl = processStmt (stmt,labellist)
		    val _ = mark_end SM_DOWHILE
		in  stmt_edsl  end

	      | Ast.For (e1op,e2op,e3op,stmt) =>
		let val _ = mark_start SM_FOR aid
		    val e1_dsl =  case e1op of SOME e1 => foldPostIncr (processExpr (e1,{enforce=false,verify_ptr=false,aside=AS_NONE}))
					     | NONE => nil
		    val _ = write_node (aid,labellist) e1_dsl
		    val e2_dsl =  case e2op of SOME e2 =>
						let val (dsl,pdsl) = processExpr (e2,{enforce=true,verify_ptr=true,aside=AS_NONE})
						in  foldPostIncr ( dsl @ [DS_NODE (DF_PREDICATE (PR_STMT, aid, expToStrAndAffLocs e2))]
								 , pdsl )
						end
					     | NONE => [DS_NODE (DF_PREDICATE (PR_STMT, aid, ("@", nil)))]
							(* TODO: empty for-predicate, evaluate as "true" *)
		    val _ = write_node (0,nil) e2_dsl
		    val e3_dsl =  case e3op of SOME e3 => foldPostIncr (processExpr (e3,{enforce=false,verify_ptr=false,aside=AS_NONE}))
					     | NONE => nil
		    val _ = write_node (0,nil) e3_dsl
		    val _ = mark_next SM_FOR (*redundant-verify*)
		    val stmt_edsl = processStmt (stmt,nil)
		    val _ = mark_end SM_FOR

		in  stmt_edsl  end

	      | Ast.Labeled ({name,...},stmt) => processStmt (stmt, labellist @ [Symbol.name name])
	      | Ast.CaseLabel (li,exp,stmt) => ( write_jump (aid,labellist) (JM_CASE li)
						 ; processStmt (stmt, nil)
						)
	      | Ast.DefaultLabel stmt => ( write_jump (aid,labellist) (JM_DEFAULT)
					 ; processStmt (stmt, nil)
					 )
	      | Ast.Goto {name,...} => ( write_jump (aid,labellist) (JM_GOTO (Symbol.name name))
				 	 ; nil )
	      | Ast.Break => ( write_jump (aid,labellist) JM_BREAK
			     ; nil )
	      | Ast.Continue => ( write_jump (aid,labellist) JM_CONTINUE
				; nil )
	      | Ast.Return expOp =>
		( case expOp
		    of SOME exp =>
			   let val exp_dsl = foldPostIncr (processExpr (exp,{enforce=false,verify_ptr=true,aside=AS_RHS}))
			       val _ =	if null labellist andalso null exp_dsl
					then ()
					else write_node (0,labellist) exp_dsl
			       val _ = write_jump (aid,nil) (JM_RETURN (expToAOs exp, expToStrAndAffLocs exp))
			   in  nil  end
		     | NONE =>  ( write_jump (aid,labellist) JM_VOIDRETURN
				; nil )
		)

	      | Ast.IfThen (exp,stmt) =>
		let val _ = mark_start SM_IF aid
		    val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val exp_dsl  = foldPostIncr (dsl @ [DS_NODE (DF_PREDICATE (PR_STMT, aid, expToStrAndAffLocs exp))], pdsl)
		    val _ = write_node (aid,labellist) exp_dsl
		    val _ = mark_next SM_IF (*redundant-verify*)
		    val stmt_edsl = processStmt (stmt,nil)
		    val _ = mark_next SM_IF
		    val _ = mark_end SM_IF
		in  stmt_edsl  end

	      | Ast.IfThenElse (exp,stmt1,stmt2) =>
		let val _ = mark_start SM_IF aid
		    val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val exp_dsl = foldPostIncr (dsl @ [DS_NODE (DF_PREDICATE (PR_STMT, aid, expToStrAndAffLocs exp))], pdsl)
		    val _ = write_node (aid,labellist) exp_dsl
		    val _ = mark_next SM_IF (*redundant-verify*)
		    val stmt1_edsl = processStmt (stmt1,nil)
		    val _ = mark_next SM_IF
		    val stmt2_edsl = processStmt (stmt2,nil)
		    val _ = mark_end SM_IF
		in  stmt1_edsl @ stmt2_edsl  end

	      | Ast.Switch (exp,stmt) =>
		let val _ = mark_start SM_SWITCH aid
		    val (dsl,pdsl) = processExpr (exp,{enforce=true,verify_ptr=true,aside=AS_NONE})
		    val exp_dsl = foldPostIncr (dsl @ [DS_NODE (DF_PREDICATE (PR_SWITCH, aid, expToStrAndAffLocs exp))], pdsl)
		    val _ = write_node (aid,labellist) exp_dsl
		    val _ = mark_next SM_SWITCH (*redundant-verify*)
		    val stmt_edsl = processStmt (stmt,nil)
		    val _ = mark_end SM_SWITCH
		in  stmt_edsl  end

	      | Ast.StatExt se => nil
	      | Ast.ErrorStmt => nil
	  )

       fun processFormals (_,nil) = nil
	 | processFormals (i,id::tail) =
	   (DS_NODE (DF_FORMAL (i,id))) :: processFormals (i+1,tail)

      fun outputDataflowOperatorsForFun  (fn_pid, fn_aid, ids, stmt) =
	  let
	      val _ = mark_start (SM_FUNCTION fn_pid) fn_aid

	      val args_dsl = processFormals (1,ids)
	      val _ = write_node (fn_aid,nil) args_dsl

	      val _ = mark_next (SM_FUNCTION fn_pid)

	      val stmt_edsl = processStmt (stmt,nil)

	      val _ = mark_end (SM_FUNCTION fn_pid)

	  in  stmt_edsl  end

       fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => nil
	    | Ast.ExternalDecl (Ast.VarDecl (id as {uid,ctype,stClass,...},initExprOp)) =>
		if (TypeUtil.isNonPointerFunction tidtab ctype)
		orelse (stClass = Ast.EXTERN)
		then nil
		else ( DS_NODE (DF_STATIC_DECL id)
		     ) :: (processInitializerAssigns uid (ctype, initExprOp))
	    | Ast.FunctionDef (id as {uid=pid,...},ids,stmt) =>
		( OptInterface.assignCurVargAO (id,ids)
		; outputDataflowOperatorsForFun (pid, edecl_aid, ids, stmt)
		)
	    | Ast.ExternalDeclExt _ => nil
	)

       fun processExternalDecls nil = nil
	 | processExternalDecls (ed::edecls) =
		let val edlist = processExternalDecl ed
		    val edslist = processExternalDecls edecls
		in  edlist @ edslist  end

       val globals_dsl = processExternalDecls edecls
       val _ = write_dsl globals_dsl

   in  ()  end
  ) (* end fun outputDataflowOperators (bundle,expToCty,os) *)


  (************************** INTRA-PROCEDURAL ANALYSES ***************************)
  (*****************************************************
   Collect VerifyTag Aids
   -> collects the AIDs of all lvalue expressions that
      would be instrumented with verifyTag in the
      non-optimized RTC.
      This is used by the other analyses to determine
      whether a given lvalue expression would "correct"
      type errors.
   *****************************************************)
  fun collectVerifyTagAids (stmt, tidtab) =
  (let
       fun processExpr (Ast.EXPR (coreExpr,aid,loc), {enforce:bool}) =
	  (case coreExpr
	     of Ast.IntConst li => aidset.empty
	      | Ast.RealConst r => aidset.empty
	      | Ast.StringConst s => aidset.empty
	      | Ast.Call (fexp,exps) =>
		let fun processExprs (nil) = aidset.empty
		      | processExprs (head::tail) =
			let val (head_set) = processExpr  (head, {enforce=false})
			    val (tail_set) = processExprs (tail)
			in  aUnion (head_set,tail_set) end

		    val fexp_set = processExpr(fexp, {enforce=true})
		    val exps_set = processExprs(exps) (* enforce=false; but if skip call, enforce may be true? false is safe approx. *)
		in  aUnion (fexp_set, exps_set)  end

	      | Ast.QuestionColon (e1,e2,e3) =>
		let val e1_set = processExpr (e1,{enforce=true})
		    val e2_set = processExpr (e2,{enforce=enforce})
		    val e3_set = processExpr (e3,{enforce=enforce})
		in  aUnion' [ e1_set,e2_set,e3_set ]  end

	      | Ast.Assign (e1,e2) =>
		let val e1_set = processExpr (e1,{enforce=false})
		    val e2_set = processExpr (e2,{enforce=enforce})
		in  aUnion (e1_set, e2_set)  end

	      | Ast.Comma (e1,e2) =>
		if OptInterface.isVargDummy e1
		then aidset.empty
		else let val e1_set = processExpr (e1,{enforce=false})
			 val e2_set = processExpr (e2,{enforce=enforce})
		     in  aUnion (e1_set,e2_set)  end

	      | Ast.Sub (e1,e2) =>
		let val e1_set = processExpr (e1,{enforce=true})
		    val e2_set = processExpr (e2,{enforce=true})
		    val union_set = aUnion (e1_set, e2_set)
		in  if enforce
		    then aidset.add (union_set, aid)
		    else union_set
		end

	      | Ast.Member (exp,mem) =>
		(* NOTE: may want to adjust so that enforce = not (OPT.isLvalExpr exp) *)
		let val exp_set = processExpr (exp,{enforce=true})
		in  if enforce
		    then aidset.add (exp_set, aid)
		    else exp_set
		end

	      | Ast.Arrow (exp,mem) =>
		let val exp_set = processExpr (exp,{enforce=true})
		in  if enforce
		    then aidset.add (exp_set, aid)
		    else exp_set
		end

	      | Ast.Deref exp =>
		let val exp_set = processExpr (exp,{enforce=true})
		in  if enforce
		    then aidset.add (exp_set, aid)
		    else exp_set
		end

	      | Ast.AddrOf exp => processExpr (exp,{enforce=false})
	      | Ast.Binop (binop,e1,e2) =>
		let val e1_set = processExpr (e1,{enforce=true})
		    val e2_set = processExpr (e2,{enforce=true})
		in  aUnion (e1_set, e2_set)  end

	      | Ast.Unop (unop,exp) => processExpr (exp,{enforce=true})
	      | Ast.SizeOf ty => aidset.empty
	      | Ast.Cast (ctype,exp) => processExpr (exp,{enforce=enforce}) (* Too lazy to detect conversion; enforce is safe approx? *)
	      | Ast.Id id => if enforce
			     then aidset.singleton aid
			     else aidset.empty
	      | Ast.EnumId (pid,li) => aidset.empty
	      | Ast.ExprExt ee => aidset.empty
	      | Ast.ErrorExpr => aidset.empty
	  )

       fun processInitExprs nil = aidset.empty
	 | processInitExprs (head::tail) = 
		let val head_set = processInitExpr  head
		    val tail_set = processInitExprs tail
		in  aUnion (head_set,tail_set)  end

       and processInitExpr iexpr =
	   (case iexpr of Ast.Simple exp => processExpr (exp,{enforce=false})
			| Ast.Aggregate iexps => processInitExprs iexps
	   )

       fun processDecls nil = aidset.empty
	 | processDecls (head::tail) =
		let val head_set = processDecl  head
		    val tail_set = processDecls tail
		in  aUnion (head_set, tail_set)  end

       and processDecl decl =
	  (case decl
	     of Ast.TypeDecl _ => aidset.empty
	      | Ast.VarDecl (id as {uid=pid,ctype,name,stClass,...}, initExprOp) =>
		if (TypeUtil.isNonPointerFunction tidtab ctype)
		orelse (stClass = Ast.EXTERN)
		then aidset.empty
		else case initExprOp
		       of SOME iexp => processInitExpr iexp
			| NONE => aidset.empty 
	  )

       fun processStmts nil = aidset.empty
	 | processStmts (head::tail) =
		let val head_set = processStmt  head
		    val tail_set = processStmts tail
		in  aUnion (head_set, tail_set)  end

       and processStmt (Ast.STMT (coreStmt,_,_)) =
	  (case coreStmt
	     of Ast.Expr expOp => (case expOp
				     of SOME exp => processExpr (exp,{enforce=false})
				      | NONE => aidset.empty
				  )
	      | Ast.Compound (decls, stmts) => 
		let val decls_set = processDecls decls
		    val stmts_set = processStmts stmts
		in  aUnion (decls_set,stmts_set)  end

	      | Ast.While (exp,stmt) =>
		let val exp_set  = processExpr (exp,{enforce=true})
		    val stmt_set = processStmt stmt
		in  aUnion (exp_set,stmt_set)  end

	      | Ast.Do (exp,stmt) =>
		let val stmt_set = processStmt stmt
		    val exp_set  = processExpr (exp,{enforce=true})
		in  aUnion (stmt_set,exp_set)  end

	      | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1_set =  case e1op of SOME e1 => processExpr (e1,{enforce=false})
					     | NONE => aidset.empty
		    val e2_set =  case e2op of SOME e2 => processExpr (e2,{enforce=true})
					     | NONE => aidset.empty
		    val e3_set =  case e3op of SOME e3 => processExpr (e3,{enforce=false})
					     | NONE => aidset.empty
		    val stmt_set = processStmt stmt

		in  aUnion' [e1_set,e2_set,e3_set,stmt_set]  end

	      | Ast.Labeled (label,stmt) => processStmt stmt
	      | Ast.CaseLabel (li,exp,stmt) => processStmt stmt
	      | Ast.DefaultLabel stmt => processStmt stmt
	      | Ast.Goto label => aidset.empty
	      | Ast.Break => aidset.empty
	      | Ast.Continue => aidset.empty
	      | Ast.Return expOp => (case expOp of SOME exp => processExpr (exp,{enforce=false})
						 | NONE => aidset.empty
				    )
	      | Ast.IfThen (exp,stmt) =>
		let val exp_set  = processExpr (exp,{enforce=true})
		    val stmt_set = processStmt stmt
		in  aUnion (exp_set,stmt_set)  end

	      | Ast.IfThenElse (exp,stmt1,stmt2) =>
		let val exp_set = processExpr (exp,{enforce=true})
		    val s1_set  = processStmt stmt1
		    val s2_set  = processStmt stmt2
		in  aUnion' [exp_set,s1_set,s2_set]  end

	      | Ast.Switch (exp,stmt) =>
		let val exp_set  = processExpr (exp,{enforce=true})
		    val stmt_set = processStmt stmt
		in  aUnion (exp_set,stmt_set)  end

	      | Ast.StatExt se => aidset.empty
	      | Ast.ErrorStmt => aidset.empty
	  )

       val stmt_set = processStmt stmt

   in  stmt_set  end

  ) (* end fun collectVerifyTagAids (stmt, tidtab) *)

  (********************************************
   May-be-uninitialized analysis:
   facts: set of may-be-uninitialized UIDs
   returns (mbu_pids,mbu_aids)
  *********************************************)
  fun mayBeUninitAnalysis (stmt, tidtab, vt_set, expToAOs, aoAliases) =
  (let
       (* the stuff returned are (mbu_pids,mbu_aids,pamap)
	* where: mbu_pids = set of "may-be-uninit" pids -- instr decls w/ uninit
	*	 mbu_aids = set of "may-be-uninit" aids -- instr these occurrences
	*	 pamap = 'temporary' map (pid -> aid list) of "may-be-uninit" assignments
	*		 to pid; at the end, aid list will be unioned into mbu_aids iff
	*		 pid is in mbu_pids (meaning there was a "may-be-uninit" use
	*		 of pid, other than assignment)
	*)
       val paEmpty = (pidset.empty,aidset.empty,pamEmpty)
       fun paUnion ((pids1,aids1,pamap1),(pids2,aids2,pamap2)) =
	   (pUnion (pids1,pids2), aUnion (aids1,aids2), pamUnion (pamap1,pamap2))
       val paUnion' = List.foldl paUnion paEmpty

(*       val expToString = PPLib.ppToString (PPAst.ppExpression () tidtab) *)

       (* does aliases(exp) intersects with facts? *)
       fun aliasIntersectPids (exp,facts) =
	   let
	       val exp_aos = expToAOs exp
	       val aliases = aoAliases exp_aos
	   in
	       List.foldl ( fn (ao,pset) => case ao of Rtc.aoId pid =>
							if pidset.member (facts,pid)
							then pidset.add (pset, pid)
							else pset
						     | _ => pset
			   ) pidset.empty aliases
	   end

       (* False,NONE cases: skip this level, but instrument children, if any.
	* This version is called by addr-of, comma (arg1), and expr-stmts, and for loops (args1&3)
	*)
       fun processExprFalseNone (outer_exp as Ast.EXPR (coreExpr,aid,loc), facts) =
	  (case coreExpr
	     of Ast.Sub (e1,e2) =>
		let val (e1_sets,e1_facts) = processExpr (e1,facts)
		    val (e2_sets,e2_facts) = processExpr (e2,facts)
		in (paUnion (e1_sets, e2_sets),
		    pIntersect (e1_facts, e2_facts))
		end

	      | Ast.Member (exp,mem) => processExpr (exp,facts)
	      | Ast.Arrow (exp,mem) => processExpr (exp,facts)
	      | Ast.Deref exp => processExpr (exp,facts)
	      | Ast.Id id => (paEmpty, facts)
	      | _ => processExpr (outer_exp,facts)
	  )

       and processExpr (outer_exp as Ast.EXPR (coreExpr,aid,loc), facts) =
	  (case coreExpr
	     of Ast.IntConst li => (paEmpty, facts)
	      | Ast.RealConst r => (paEmpty, facts)
	      | Ast.StringConst s => (paEmpty, facts)
	      | Ast.Call (fexp,exps) =>
		let
		    val (fexp_sets,fexp_facts) = processExpr(fexp, facts)

		    fun processExprs (nil, facts) = (paEmpty, facts)
		      | processExprs (head::tail, facts) =
			let val (head_sets, head_facts) = processExpr  (head, facts)
			    val (tail_sets, tail_facts) = processExprs (tail, facts)
			in
			   (paUnion (head_sets,tail_sets),
			    pIntersect (head_facts,tail_facts))
			end
		    val (exps_sets,exps_facts) = processExprs(exps, facts)
		in
		    (paUnion (fexp_sets, exps_sets),
		     pIntersect (fexp_facts, exps_facts))
		end

	      | Ast.QuestionColon (e1,e2,e3) =>
		let val (e1_sets,e1_facts) = processExpr (e1,facts)
		    val (e2_sets,e2_facts) = processExpr (e2,e1_facts)
		    val (e3_sets,e3_facts) = processExpr (e3,e1_facts)
		in
		    (paUnion' [ e1_sets,e2_sets,e3_sets ],
		     pUnion (e2_facts, e3_facts))
		end

	      | Ast.Assign (e1,e2) =>
		let
		    fun getIdPidAid (Ast.EXPR (coreExpr,id_aid,_)) =
			case coreExpr
			  of Ast.Id (id as {uid=pid,...}) => SOME (pid,id_aid)
			   | _ => NONE

		    (* if e1 is a simple Id, kill e1; also, add (pid,e1_aid) to pamap *)
		    val (pamap',facts') = case getIdPidAid e1
					    of SOME (e1_pid,e1_aid) =>
						(pidaidmap.insert (pamEmpty,e1_pid,[aid(*e1_aid*)]),
										(* changed to match rtca-mbu implementation,
										   with assignments identified by the assignment
										   aid rather than e1_aid. I DID NOT VERIFY THE
										   COMPATIBILITY OF THIS CHANGE AT ALL! *)
						 pidset.delete (facts, e1_pid)
						 handle NotFound => facts)
					     | NONE => (pamEmpty, facts)

(*TODO: more sophisticated retset would allow tracking of individual members of structs*)
		    val (e1_sets,e1_facts) = processExpr (e1,facts')
		    val (e2_sets,e2_facts) = processExpr (e2,facts)

		    val (pset,aset,pamap) = paUnion (e1_sets, e2_sets)
		in
		    ((pset, aset, pamUnion (pamap,pamap')),
		     pIntersect (e1_facts, e2_facts))
		end

	      | Ast.Comma (e1,e2) =>
		if OptInterface.isVargDummy e1
		then (paEmpty,facts)	(* TODO: somehow check curvargAO? *)
		else let val (e1_sets,e1_facts) = processExprFalseNone (e1,facts)
			 val (e2_sets,e2_facts) = processExpr (e2,e1_facts)
		     in
			 (paUnion (e1_sets,e2_sets),
			 e2_facts)
		     end

	      | Ast.Sub (e1,e2) =>
		let val (e1_sets,e1_facts) = processExpr (e1,facts)
		    val (e2_sets,e2_facts) = processExpr (e2,facts)
		    val facts' = pIntersect (e1_facts, e2_facts)
		    val alias_pset = aliasIntersectPids (outer_exp,facts')
		    val this_sets = (alias_pset, if pidset.isEmpty alias_pset
						 then aidset.empty
						 else aidset.singleton aid, pamEmpty)
		in
		    (paUnion' [e1_sets, e2_sets, this_sets],
		     facts')
		end

	      | Ast.Member (exp,mem) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val alias_pset = aliasIntersectPids (outer_exp,exp_facts)
		    val this_sets = (alias_pset, if pidset.isEmpty alias_pset
						 then aidset.empty
						 else aidset.singleton aid, pamEmpty)
		in  (paUnion (exp_sets,this_sets), exp_facts)  end

	      | Ast.Arrow (exp,mem) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val alias_pset = aliasIntersectPids (outer_exp,exp_facts)
		    val this_sets = (alias_pset, if pidset.isEmpty alias_pset
						 then aidset.empty
						 else aidset.singleton aid, pamEmpty)
		in  (paUnion (exp_sets,this_sets), exp_facts)  end

	      | Ast.Deref exp =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val alias_pset = aliasIntersectPids (outer_exp,exp_facts)
		    val this_sets = (alias_pset, if pidset.isEmpty alias_pset
						 then aidset.empty
						 else aidset.singleton aid, pamEmpty)
		in  (paUnion (exp_sets,this_sets), exp_facts)  end

	      | Ast.AddrOf exp => processExprFalseNone (exp,facts)
	      | Ast.Binop (binop,e1,e2) =>
		if isAndOr binop (* Logical AND and OR: account for short-circuit *)
		then let
			 val (e1_sets,e1_facts) = processExpr (e1,facts)
			 val (e2_sets,e2_facts) = processExpr (e2,e1_facts)
		     in
			(paUnion (e1_sets, e2_sets),
			 pUnion (e1_facts, e2_facts))
		     end
		else let
			 val (e1_sets,e1_facts) = processExpr (e1,facts)
			 val (e2_sets,e2_facts) = processExpr (e2,facts)
		     in
			(paUnion (e1_sets, e2_sets),
			 pIntersect (e1_facts, e2_facts))
		     end

	      | Ast.Unop (unop,exp) => processExpr (exp,facts)
	      | Ast.SizeOf ty => (paEmpty,facts)
	      | Ast.Cast (ctype,exp) => processExpr (exp,facts)
	      | Ast.Id (id as {uid=pid,name,...}) =>
		if pidset.member (facts,pid)
		then ((pidset.singleton pid, aidset.singleton aid, pamEmpty),
				    if aidset.member (vt_set,aid)
				    then pidset.delete (facts,pid)
				    else facts
		     )
		else (paEmpty,facts)

	      | Ast.EnumId (pid,li) => (paEmpty,facts)
	      | Ast.ExprExt ee => (paEmpty,facts)
	      | Ast.ErrorExpr => (paEmpty,facts)
	  )

       (* Not sure what GNU C specs specify, but I think initializers
	  are evaluated in order. *)
       fun processInitExprs (nil,facts) = (paEmpty,facts)
	 | processInitExprs (head::tail,facts) = 
		let val (head_sets,facts) = processInitExpr  (head,facts)
		    val (tail_sets,facts) = processInitExprs (tail,facts)
		in  (paUnion (head_sets,tail_sets), facts) end

       and processInitExpr (iexpr,facts) =
	   (case iexpr of Ast.Simple exp => processExpr (exp,facts)
			| Ast.Aggregate iexps => processInitExprs (iexps,facts)
	   )

       (* returns (sets,facts,lvars), where lvars is the set of vars local to this scope *)
       fun processDecls (nil,facts) = (paEmpty,facts,pidset.empty)
	 | processDecls (head::tail, facts) =
		let val (head_sets,facts,head_lvars) = processDecl  (head,facts)
		    val (tail_sets,facts,tail_lvars) = processDecls (tail,facts)
		in (paUnion (head_sets, tail_sets),
		    facts,
		    pUnion (head_lvars, tail_lvars))
		end

       (* returns (sets,facts,lvars), where lvars is the set of auto vars local to this scope *)
       and processDecl (decl,facts) =
	  (case decl
	     of Ast.TypeDecl _ => (paEmpty,facts,pidset.empty)
	      | Ast.VarDecl (id as {uid=pid,ctype,name,stClass,...}, initExprOp) =>
		if (TypeUtil.isNonPointerFunction tidtab ctype)
		orelse (stClass = Ast.EXTERN)
		then (paEmpty,facts,pidset.empty) (* skip functions and externs *)
		else case initExprOp
		       of SOME iexp =>  let val (sets,facts) = processInitExpr (iexp,facts)
					in (sets,facts, if (stClass = Ast.STATIC)
							then pidset.empty
							else pidset.singleton pid)
					end
			| NONE => if (stClass = Ast.STATIC)
				  then (paEmpty, facts, pidset.empty)
				  else (paEmpty, pidset.add (facts, pid), pidset.singleton pid)
	  )

       (********************)
       (* synth shorthands *)
       fun synthUnion (synth1 as {breakfacts=b1,continuefacts=c1},
		       synth2 as {breakfacts=b2,continuefacts=c2}) =
	   {breakfacts=(pUnion (b1,b2)),
	    continuefacts=(pUnion (c1,c2))}
       val synthEmpty = {breakfacts=pidset.empty, continuefacts=pidset.empty}
       (********************)

       (* takes (set, facts, inherit as {allvars,switchfacts})
	    where allvars is all locals declared thusfar
	    and switchfacts are facts of the immediate enclosing switch stmt (if any)
	  returns (set, facts, synth as {breakfacts,continuefacts})
	    where breakfacts are (the union of) facts at all contained "break" stmts (if any)
	    and continuefacts are (the union of) facts at all contained "continue" stmts (if any)
	*)
       fun processStmts (nil,facts,inherit) = (paEmpty,facts,synthEmpty)
	 | processStmts (head::tail,facts,inherit) =
		let val (head_sets,facts,head_synth) = processStmt  (head,facts,inherit)
		    val (tail_sets,facts,tail_synth) = processStmts (tail,facts,inherit)
		in (paUnion (head_sets, tail_sets),
		    facts,
		    synthUnion (head_synth,tail_synth))
		end

       and processStmt (Ast.STMT(coreStmt,_,_), facts, inherit as {allvars,switchfacts}) =
	  (case coreStmt
	     of Ast.Expr expOp =>
		(case expOp
		   of SOME exp => let val (sets,facts) = processExprFalseNone (exp,facts)
				  in  (sets,facts,synthEmpty)  end
		    | NONE => (paEmpty,facts,synthEmpty)
		)

	      | Ast.Compound (decls, stmts) => 
		let val (decls_sets,facts,localvars) = processDecls (decls,facts)
		    val inherit' = {allvars=(pUnion (allvars,localvars)),
				    switchfacts=(pUnion (switchfacts,localvars))} (* All localvars immediately within switch are uninit *)
		    val (stmts_sets,facts,synth) = processStmts (stmts,facts,inherit')
		in (paUnion (decls_sets,stmts_sets),
		    pUnion (facts,localvars), (* at the end of a scope, include localvars as uninits *)
		    synth)
		end

	      | Ast.While (exp,stmt) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt,exp_facts,inherit)

		    (* Due to GOTOs, may need to process loop an extra time *)
		    val (extra_sets,extra_facts,extra_breakfacts) =
			if pidset.isEmpty (pidset.difference (pUnion (stmt_facts,continuefacts), facts))
			then (paEmpty,pidset.empty,pidset.empty)
			else let val (exp_sets,exp_facts) = processExpr (exp, pUnion (stmt_facts,continuefacts))
				 val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt,exp_facts,inherit)
			     in  (paUnion (exp_sets,stmt_sets),
				  pUnion (exp_facts,stmt_facts),
				  breakfacts)
			     end
		in (paUnion' [ exp_sets,stmt_sets, extra_sets ],
		    pUnion' [ exp_facts,stmt_facts, extra_facts, breakfacts,extra_breakfacts ],
		    synthEmpty)
		end

	      | Ast.Do (exp,stmt) =>
		(* SY note: continue jumps to exp, while break jumps past exp *)
		let val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt,facts,inherit)
		    val (exp_sets,exp_facts)  = processExpr (exp,pUnion (stmt_facts,continuefacts))

		    (* Due to GOTOs, may need to process loop an extra time *)
		    val (extra_sets,extra_facts,extra_breakfacts) =
			if pidset.isEmpty (pidset.difference (exp_facts, facts))
			then (paEmpty,pidset.empty,pidset.empty)
			else let val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt, exp_facts, inherit)
				 val (exp_sets,exp_facts)  = processExpr (exp, pUnion (stmt_facts,continuefacts))
			     in  (paUnion (stmt_sets,exp_sets),
				  exp_facts,
				  breakfacts)
			     end
		in (paUnion' [ stmt_sets,exp_sets, extra_sets ],
		    pUnion' [ exp_facts, extra_facts, breakfacts,extra_breakfacts ],
		    synthEmpty)
		end

	      | Ast.For (e1op,e2op,e3op,stmt) =>
		let val (e1_sets,e1_facts) =  case e1op of SOME e1 => processExprFalseNone (e1,facts)
							 | NONE => (paEmpty,facts)
		    val (e2_sets,e2_facts) =  case e2op of SOME e2 => processExpr (e2,e1_facts)
							 | NONE => (paEmpty,e1_facts)
		    val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt,e2_facts,inherit)
		    val stmt_cont_facts = pUnion (stmt_facts,continuefacts)
		    val (e3_sets,e3_facts) =  case e3op of SOME e3 => processExprFalseNone (e3,stmt_cont_facts)
							 | NONE => (paEmpty,stmt_cont_facts)

		    (* Due to GOTOs, may need to process loop an extra time *)
		    val (extra_sets,extra_facts,extra_breakfacts) =
			if pidset.isEmpty (pidset.difference (stmt_cont_facts, e1_facts))
			then (paEmpty,pidset.empty,pidset.empty)
			else let val (e2_sets,e2_facts) =  case e2op of SOME e2 => processExpr (e2,stmt_cont_facts)
								      | NONE => (paEmpty,stmt_cont_facts)
				 val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts}) = processStmt (stmt,e2_facts,inherit)
				 val stmt_cont_facts = pUnion (stmt_facts,continuefacts)
				 val (e3_sets,e3_facts) =  case e3op of SOME e3 => processExprFalseNone (e3,stmt_cont_facts)
								      | NONE => (paEmpty,stmt_cont_facts)
			     in (paUnion' [e2_sets,e3_sets,stmt_sets],
				 pUnion (e2_facts, e3_facts),
				 breakfacts)
			     end
		in (paUnion' [e1_sets,e2_sets,e3_sets,stmt_sets, extra_sets],
		    pUnion' [e2_facts,e3_facts, extra_facts, breakfacts,extra_breakfacts],
		    synthEmpty)
		end

	      | Ast.Labeled (label,stmt) => processStmt (stmt,allvars,inherit) (* assume all vars may be uninit *)
	      | Ast.CaseLabel (li,exp,stmt) => processStmt (stmt,pUnion(facts,switchfacts),inherit)
	      | Ast.DefaultLabel stmt => processStmt (stmt,pUnion(facts,switchfacts),inherit)
	      | Ast.Goto label => (paEmpty,pidset.empty,synthEmpty)
	      | Ast.Break => (paEmpty,pidset.empty,{breakfacts=facts,continuefacts=pidset.empty})
	      | Ast.Continue => (paEmpty,pidset.empty,{breakfacts=pidset.empty,continuefacts=facts})
	      | Ast.Return expOp => (case expOp of SOME exp => let val (sets,facts) = processExpr (exp,facts)
								in (sets,pidset.empty,synthEmpty) end
						 | NONE => (paEmpty,pidset.empty,synthEmpty)
				    )
	      | Ast.IfThen (exp,stmt) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val (stmt_sets,stmt_facts,synth) = processStmt (stmt,exp_facts,inherit)
		in  (paUnion (exp_sets,stmt_sets),
		     pUnion (exp_facts,stmt_facts),
		     synth)
		end

	      | Ast.IfThenElse (exp,stmt1,stmt2) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val (s1_sets,s1_facts,s1_synth) = processStmt (stmt1,exp_facts,inherit)
		    val (s2_sets,s2_facts,s2_synth) = processStmt (stmt2,exp_facts,inherit)
		in  (paUnion' [exp_sets,s1_sets,s2_sets],
		     pUnion (s1_facts,s2_facts),
		     synthUnion (s1_synth,s2_synth))
		end

	      | Ast.Switch (exp,stmt) =>
		let val (exp_sets,exp_facts) = processExpr (exp,facts)
		    val (stmt_sets,stmt_facts,synth as {breakfacts,continuefacts})
			= processStmt (stmt,exp_facts,{allvars=allvars,switchfacts=exp_facts})

		    fun hasDefaultLabel nil = false
		      | hasDefaultLabel ((Ast.STMT (coreStmt,_,_))::tail) =
			case coreStmt
			  of Ast.DefaultLabel stmt => true
			   | Ast.Labeled (label,stmt) => hasDefaultLabel (stmt::tail)
			   | Ast.CaseLabel (li,exp,stmt) => hasDefaultLabel (stmt::tail)
			   | _ => hasDefaultLabel tail

		    val stmt_facts = case stmt of Ast.STMT (Ast.Compound (_,stmts),_,_) =>
						  if (hasDefaultLabel stmts)
						  then stmt_facts
						  else pUnion (exp_facts,stmt_facts)
						| _ => stmt_facts
		in  (paUnion (exp_sets,stmt_sets),
		     pUnion (stmt_facts,breakfacts),
		     {breakfacts=pidset.empty,continuefacts=continuefacts})
		end

	      | Ast.StatExt se => (paEmpty,facts,synthEmpty)
	      | Ast.ErrorStmt => (paEmpty,facts,synthEmpty)
	  )

       val ((pid_set,aid_set,pamap), stmt_facts, stmt_synth)
		= processStmt (stmt, pidset.empty, {allvars=pidset.empty,switchfacts=pidset.empty})

       (* traverse pamap: for each pid->aidlist, if pid is in pid_set, then insert aidlist into aidset*)
       val aid_set' = pidaidmap.foldli ( fn (pid,aidlist,aset) =>
					    if pidset.member (pid_set,pid)
					    then aidset.addList (aset, aidlist)
					    else aset
					) aid_set pamap
   in  (pid_set,aid_set')  end

  ) (* end fun mayBeUninitAnalysis (stmt, tidtab, vt_set, expToAOs, aoAliases) *)

  (********************************************
   Redundant-Check Analysis
   facts: set of checked expressions
  *********************************************)
  fun redundantAnalysis (stmt, tidtab, vt_set, expToAOs, aoAliases) = aidset.empty

  (********************************************
   Escape Analysis:
   locals: set of local variables
   ptmap: points-to map
  *********************************************)
  fun escapeAnalysis (ids, stmt, tidtab) = aidset.empty

  (********************************************
   Top-level engine that calls the above
   analyses to process each function.
  *********************************************)
  fun flowSensitiveAnalyses (bundle as {ast=edecls,tidtab=ttab,...} : ParseToAst.astBundle,
			     expToAOs, aoAliases) =
  (let
       val empty_bunch = ((pidset.empty, aidset.empty), aidset.empty, aidset.empty)
       fun union_bunch (((pset1a,aset1a),aset2a,aset3a),((pset1b,aset1b),aset2b,aset3b)) =
	   let val pset1u = pUnion(pset1a,pset1b)
	       val aset1u = aUnion(aset1a,aset1b)
	       val aset2u = aUnion(aset2a,aset2b)
	       val aset3u = aUnion(aset3a,aset3b)
	   in  ((pset1u,aset1u),aset2u,aset3u)  end

       fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
	(case coreEdecl
	   of Ast.ExternalDecl _ => empty_bunch
	    | Ast.FunctionDef (id as {uid=pid,name=name,...},ids,stmt) =>
		let
		    val _ = OptInterface.assignCurVargAO (id,ids)
		    val vt_set = collectVerifyTagAids (stmt, ttab)
(*
val _ = print "VT: ["
val _ = aidset.app (fn aid => print (" " ^ (Int.toString aid))) vt_set
val _ = print " ]\n"
*)
		    val (mbu_pids, mbu_aids) =  if !Flags.mayBeUninit
						then mayBeUninitAnalysis (stmt,ttab,vt_set,expToAOs,aoAliases)
						else (pidset.empty, aidset.empty)
(*
val _ = print "MBU: ["
val _ = aidset.app (fn aid => print (" " ^ (Int.toString aid))) mbu_aids
val _ = print " ]\n"
*)
		    val red_set = redundantAnalysis (stmt,ttab,vt_set,expToAOs,aoAliases)
		    val esc_set = escapeAnalysis (ids,stmt,ttab)

		in  ((mbu_pids,mbu_aids), red_set, esc_set)  end
	    | Ast.ExternalDeclExt _ => empty_bunch
	)

       fun processExternalDecls nil = empty_bunch
	 | processExternalDecls (ed::edecls) =
		let val ed_bunch = processExternalDecl ed
		    val edecls_bunch = processExternalDecls edecls
		in  union_bunch (ed_bunch,edecls_bunch) end

  
       val ((mbu_pids,mbu_aids),red_set,esc_set) = processExternalDecls (#1 (OptInterface.stripTcInclude edecls))

       (* convert sets to lookup functions *)
       fun is_mbu_pid pid = pidset.member (mbu_pids,pid)
       fun is_mbu_aid aid = aidset.member (mbu_aids,aid)

       val is_red_aid = Rtc.False
       val is_mbi_pid = Rtc.True

   in  ((is_mbu_pid, is_mbu_aid), is_red_aid, is_mbi_pid)  end
  ) (* end fun flowSensitiveAnalyses (bundle,expToAOs,aoAliases) *)

  (************************** *************************)

end (* Structure FlowSensitive *)

