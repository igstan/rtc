(********************************************************
   Key to intermediate format:
	Number:	[0-9]+
	Types:	a - array	a N <T>
		b - bitfield	b N <T>
		c - char
		d - double
		e - long double
		f - float
		g - long long
		h - short
		i - int
		l - long
		p - pointer
		s - struct	s <T-list> ;
		u - union	u <T-list> ;
		v - void
		x - function	x <T> <T-list> ;
	AOs:	A - AddrOf	A <O>
		D - Star	D <O>
		E - Ext		E <O> <T> <T>
		F - Arg		F N <O>		(N is 1-based)
		I - Id		I N		(N is Pid)
		L - StringLit	L N		(N is Aid)
		M - Malloc	M N		(N is Aid)
		O - Op		O <O> <T>
		R - Return	R <O>
		S - SDot	S <O> <T-list> ;
		U - UDot	U <O> <T>
		V - Value	V <T>
		X - Function	X <O>		(O should be an Id object)
	(*	Y - Array	Y <O>		--deprecated--	*)
		$ - aliasref	$ N		(not yet implemented)
   File format - line-based:
     COMMON
	AID entry:
		@ [A|H|S] <N> <name>\n		(A=auto)
	PID entry:				(H=heap)
		% [A|H|S] <N> <name>\n		(S=static)
	AO alias definition:
		$ <N> <N> <O> \n		(not yet implemented: id ecr ao)
     PASGS file (ckit-assign ==> rtca)
	Assignment edge (declaration or copyTag):
		: <T> <O> <ignored> \n
		= <O> <ignored> \n		(true assignment, including arg/ret)
		- <O> <ignored> \n		(pseudo assignment: functions and arrays, and void returns)
	Static-Type/VerifyTag/VerifyPtr:
		+ <T> <O> <ignored> \n		(static-type)
		} <T> <O> <ignored> \n		(verifyTag)
		] <T> <O> <ignored> \n		(verifyPtr)
	Function def/Vararg identifier:
		f <N> <O> <ignored> \n		regular function defined (N is argno after last, O is ao of
		v <N> <O> <ignored> \n		vararg function defined    defined function, should be X)
     TSLS file (rtca ==> ckit-instr)
	(Old:) Type safety level:		(S=safe, T=tracked, U=unsafe)
		^ [S|T|U] \n			(default ts-level, usu first line)
		~ [S|T|U] <N> <O> \n		(N = ecr#)
	(New:) Type safety categories:
		^ [P|B|I|E|S] \n		(default ts-category)
		~ [P|B|I|S] <N> <O> \n		(P=poss-inval, B=badly-typed, I=influential, S=safe)
		~ E <N> <O> <T> \n		(E=exposed; type-parameterized?) (NYI)
		/ [A|S|N] [A|S|N] <O> \n	(free arg status: A=all,S=some, N=none)
						(1st: all AOs, 2nd: malloc AOs only)
		/V[A|S|N] [A|S|N] <O> \n	(free arg status, for vuln mode)
						(see also "! _ f" for interesting libfn tracked status)
		M <O> \n			(vp_may-be-uninit ao)
		T <O> \n			(vp_may-be-uninit tracked ao)
		U <O> \n			(vp_may-be-uninit unsafe ao)
	Vulnerable locations/derefs:
		V l <O> \n			(vulnerable location)
		V d <O> \n			(vulnerable dereference)
	Alias Analysis:
		> <N> <N-list>			(includes-to edges)
        Flow Sensitive:
		! ...				see flow-sensitive.sml for format
		_ [T|U] <O>			untouched ao
	Comment lines:
		# .* \n
		\n
 ********************************************************)
(********************************************************
   * SPECS FOR PORTABLE NAME: *
   ****************************
    variable ids:
	global/extern:	.x
	file-static:	"filename.c".x
	local:		<function-id>.x
			- where <function-id> is either .x or "filename.c".x
			  depending on whether it is file-static
	inner-scope:	<function-id>/n.x
			- where n is the nth scope in a given level (starting with 0)
			- nested scopes will look like <function-id>/m/n.x
			- the scope count counts all scopes, not just those with
			  declarations; the outermost function scope is not included
    string and malloc ids:
	global:		"filename.c".$strlit:n
	local:		<same signature as above>.$strlit:n
			<same signature as above>.$malloc:n
			- where n is the nth string literal or malloc call
			  at the given level/scope/function; string literals and
			  mallocs are counted separately
			- the order of numbering is as one would expect: top-down,
			  left-to-right
			- mallocs are identified as a call to any function which passes
			  the isMalloc function above
********************************************************)

structure OptInterface : OPT_INTERFACE =
struct 
  open Rtc

  structure pidset = SplaySetFn (struct
					type ord_key = Pid.uid
					val compare = Pid.compare
				 end)

  structure aidset = SplaySetFn (struct
					type ord_key = Aid.uid
					val compare = Aid.compare
				   end)

  structure aidmap = BinaryMapFn (struct
					type ord_key = Aid.uid
					val compare = Aid.compare
				   end)

  structure ecrset = ListSetFn (struct
                                  type ord_key = Int.int
                                  val compare = Int.compare
                                 end)

  datatype st_class = SC_AUTO | SC_HEAP | SC_STATIC

  fun scToString SC_AUTO = "A"
    | scToString SC_HEAP = "H"
    | scToString SC_STATIC = "S"

  datatype list_elt
	= leAid of Aid.uid * st_class * string * (string * SourceMap.location) option
						(* last item stores the stringlit and location, as a comment *)
	| lePid of Pid.uid * st_class * string

  type lookup_bundle =
        { ts_lookup_fn : Rtc.absObject list -> Rtc.ts_categ
        , ts_lookup_children_fn : Rtc.absObject list -> Rtc.ts_categ
        , freearg_lookup_fn : Rtc.absObject list -> Rtc.es_status
	, noins_libfn_lookup : bool -> Aid.uid -> bool
        , may_be_uninit_aid : Aid.uid * Rtc.mbuMarker -> bool
        , may_be_uninit_pid : Pid.uid -> bool
        , vp_may_be_uninit_ao : Rtc.absObject -> bool
	, vp_vuln_loc_ao : Rtc.absObject -> bool
	, vp_vuln_enclosing_loc_ao : Rtc.absObject -> bool
	, vp_vuln_deref_ao : Rtc.absObject -> bool
        , vp_redundant_aid : bool -> Aid.uid -> bool
        , vt_redundant_aid : Rtc.redMarker -> Aid.uid -> bool
	, array_inbounds_aid : Aid.uid -> bool
        }

  (* Vanilla output function *)
  fun warning msg = TextIO.output (TextIO.stdErr, msg)

  (* Informational output *)
  val inform = warning

  (* Debug output *)
  val debug = (* warning *)
	(**)  fn _ => ()

  (* output function with location info *)
  fun warningLoc loc msg =
      warning ("[" ^ (SourceMap.locToString loc) ^ "] " ^ msg)

  (* Convert ctype to tctype *)
  fun ctypeToTcType ttab ctype =
  (   let fun cvtCtype ctype =
	      case ctype
	        of Ast.Void            => tcVoid
		 | Ast.Qual (_,ty)     => cvtCtype ty
		 | Ast.Numeric (_,_,_, kind,_) =>
		      (case kind of
			   Ast.CHAR       => tcChar
			 | Ast.SHORT      => tcShort
			 | Ast.INT        => tcInt
			 | Ast.LONG       => tcLong
			 | Ast.LONGLONG   => tcLongLong
			 | Ast.FLOAT      => tcFloat
			 | Ast.DOUBLE     => tcDouble
			 | Ast.LONGDOUBLE => tcLongDouble)
		 | Ast.Array (SOME (li,_), ty)	=> tcArray (cvtCtype ty, li)
		 | Ast.Array (NONE, ty)		=> tcArray (cvtCtype ty, 0)  (* incomplete array *)
		 | Ast.Pointer (_)		=> tcPointer
		 | Ast.Function (rty, tlist)
			=> tcFunction (cvtCtype rty,
					List.mapPartial (fn ty => case ty of Ast.Ellipses => NONE
									   | _ => SOME (cvtCtype ty)
							 ) tlist)
		 | Ast.StructRef (tid) => cvtTid tid
		 | Ast.UnionRef (tid)  => cvtTid tid
		 | Ast.EnumRef _       => tcInt
		 | Ast.TypeRef (tid)   => cvtTid tid
		 | Ast.Ellipses	=> (warning "cvtCtype error: converting ellipsis type\n"; tcVoid)
		 | Ast.Error	=> (warning "cvtCtype error: converting error type\n"; tcVoid)

	  and cvtTid tid =
	      case Tidtab.find (ttab,tid) of
		  SOME {ntype=SOME(Bindings.Struct (uid, sflist)),name,global,location} =>
			let fun procsflist nil = nil
			      | procsflist ((fty,_,SOME li)::tail) = (tcBitField (cvtCtype fty, li))::(procsflist tail)
			      | procsflist ((fty,_,NONE)::tail) = (cvtCtype fty)::(procsflist tail)
			in  tcStruct (procsflist sflist) end
		| SOME {ntype=SOME(Bindings.Union (uid, uflist)),name,global,location} =>
			let fun procuflist nil = nil
			      | procuflist ((fty,_)::tail) = (cvtCtype fty)::(procuflist tail)
			in  tcUnion (procuflist uflist) end
		| SOME {ntype=SOME(Bindings.Enum _),name,global,location} => tcInt
		| SOME {ntype=SOME(Bindings.Typedef (_,ty)),name,global,location} => cvtCtype ty
		| SOME {ntype=NONE,name,global,location} =>
			(warningLoc location ("cvtTid error: tid " ^ (Tid.toString tid) ^ " undefined!\n");
			 tcVoid)
		| NONE => (warning ("cvtTid error: tid " ^ (Tid.toString tid) ^ " not found!\n");
			tcVoid)
      in cvtCtype ctype end
  ) (* end fun ctypeToTcType ttab ctype *)

  (****************** helper query functions *******************)

  (* Does this unary operator involve an assignment? *)
  fun isAssignUnop unop =
      case unop of
	  Ast.PreInc => true
	| Ast.PostInc => true
	| Ast.PreDec  => true
	| Ast.PostDec => true
	| _ => false

  (* Does this binary operator involve an assignment? *)
  fun isAssignBinop binop =
     (case binop of
	  Ast.PlusAssign => true
	| Ast.MinusAssign => true
	| Ast.TimesAssign => true
	| Ast.DivAssign => true
	| Ast.ModAssign => true
	| Ast.XorAssign => true
	| Ast.OrAssign => true
	| Ast.AndAssign => true
	| Ast.LshiftAssign => true
	| Ast.RshiftAssign => true
	| _ => false
     )

  (******** EVALCONST (helper used for computing Malloc size, could be used by others) **********)
  (* Evaluate expression to see if it is a constant or a (constant * sizeof type) expression	*)
  (*   if constant, returns SOME (tcUnit, size), where tcUnit is currently defined as tcChar	*)
  (*   if constant*sizeof, returns SOME (tcType, size)						*)
  (*   else returns NONE									*)
  (* NOTE: op-assign and pre/post-incr are assumed to have been evaluated	*)
  fun evalConst ctyToTcTy (Ast.EXPR(cexp,_,_)) =
      let val tcUnit = Rtc.tcChar
	  val evConst = evalConst ctyToTcTy
      in ( case cexp
	     of Ast.IntConst li => SOME (tcUnit, li)
(*	      | Ast.RealConst r		*)
(*	      | Ast.StringConst s		*)
	      | Ast.QuestionColon (e1,e2,e3) =>
		(case (evConst e1) of SOME (_,c1) => evConst (if (c1 <> 0) then e2 else e3)
				      | NONE => NONE
		)
	      | Ast.Assign (e1,e2) => evConst e2
	      | Ast.Comma (e1,e2) => evConst e2
(*	      | Ast.Sub (e1,e2) =>		*)
(*	      | Ast.Member (e,mem) =>	*)
(*	      | Ast.Arrow (e,mem) =>		*)
(*	      | Ast.Deref e =>		*)
(*	      | Ast.AddrOf e =>		*)
	      | Ast.Binop (bop,e1,e2) =>
		if (isAssignBinop bop)
		then evConst e1
		else(case (evConst e1, evConst e2)
		       of (SOME (t1,c1), SOME (t2,c2)) =>
			  (case bop
			     of Ast.Plus => if t1 = t2 then SOME (t1, c1 + c2) else NONE
			      | Ast.Minus => if t1 = t2 then SOME (t1, c1 - c2) else NONE
			      | Ast.Times => if t1 = tcUnit then SOME (t2, c1 * c2)
							    else if t2 = tcUnit then SOME (t1, c1 * c2)
										else NONE
			      | Ast.Divide => if t1 = t2 then SOME (tcUnit, c1 div c2)
							 else if t2 = tcUnit then SOME (t1, c1 div c2)
									     else NONE
			      | Ast.Mod => if t1 = t2 then SOME (tcUnit, c1 mod c2)
						      else if t2 = tcUnit then SOME (t1, c1 mod c2)
									  else NONE
			      | Ast.Gt => if t1 = t2 then SOME (tcUnit, if (c1 > c2) then 1 else 0) else NONE
			      | Ast.Lt => if t1 = t2 then SOME (tcUnit, if (c1 < c2) then 1 else 0) else NONE
			      | Ast.Gte => if t1 = t2 then SOME (tcUnit, if (c1 >= c2) then 1 else 0) else NONE
			      | Ast.Lte => if t1 = t2 then SOME (tcUnit, if (c1 <= c2) then 1 else 0) else NONE
			      | Ast.Eq => if t1 = t2 then SOME (tcUnit, if (c1 = c2) then 1 else 0) else NONE
			      | Ast.Neq => if t1 = t2 then SOME (tcUnit, if (c1 <> c2) then 1 else 0) else NONE
			      | Ast.And => SOME (tcUnit, if (c1 <> 0) andalso (c2 <> 0) then 1 else 0)
			      | Ast.Or => SOME (tcUnit, if (c1 <> 0) orelse (c2 <> 0) then 1 else 0)
			      | Ast.BitOr => if t1 = tcUnit andalso t2 = tcUnit
					     then SOME (t1, Word.toLargeInt (Word.orb (Word.fromLargeInt c1, Word.fromLargeInt c2)))
					     else NONE
			      | Ast.BitAnd => if t1 = tcUnit andalso t2 = tcUnit
					      then SOME (t1, Word.toLargeInt (Word.andb (Word.fromLargeInt c1, Word.fromLargeInt c2)))
					      else NONE
			      | Ast.BitXor => if t1 = tcUnit andalso t2 = tcUnit
					      then SOME (t1, Word.toLargeInt (Word.xorb (Word.fromLargeInt c1, Word.fromLargeInt c2)))
					      else NONE
			      | Ast.Lshift => if t2 = tcUnit
					      then SOME (t1, Word.toLargeInt (Word.<< (Word.fromLargeInt c1, Word.fromLargeInt c2)))
					      else NONE
			      | Ast.Rshift => if t2 = tcUnit
					      then SOME (t1, Word.toLargeInt (Word.>> (Word.fromLargeInt c1, Word.fromLargeInt c2)))
					      else NONE
			(*    | Ast.PlusAssign | Ast.MinusAssign | Ast.TimesAssign	*)
			(*    | Ast.DivAssign | Ast.ModAssign				*)
			(*    | Ast.XorAssign | Ast.OrAssign | Ast.AndAssign		*)
			(*    | Ast.LshiftAssign | Ast.RshiftAssign			*)
			(*    | Ast.BinopExt _						*)
			      | _ => NONE
			)
		   | _ => NONE
		)
	      | Ast.Unop (uop,e) =>
		if isAssignUnop uop
		then evConst e
		else(case uop
		       of Ast.Uplus => evConst e
			| Ast.Not => Option.map (fn (t,c) => (t, if (c <> 0) then 0 else 1)) (evConst e)
			| Ast.Negate => Option.map (fn (t,c) => (t,~c)) (evConst e)
			| Ast.BitNot => (case (evConst e)
					   of SOME (t,c) => if t = tcUnit
							    then SOME (t,Word.toLargeInt (Word.notb (Word.fromLargeInt c))) 
							    else NONE
					    | _ => NONE
					)
(*			| Ast.PreInc | Ast.PostInc | Ast.PreDec | Ast.PostDec	*)
(*			| Ast.UnopExt _						*)
			| _ => NONE
		    )
	      | Ast.SizeOf ty =>
		let fun flatten_array (Rtc.tcArray (elty, numelts)) =
			let val (elty',numelts') = flatten_array elty
			in  (elty',numelts * numelts')  end
		      | flatten_array ty = (ty,1)
		in  SOME (flatten_array (ctyToTcTy ty))  end
	      | Ast.Cast (ty,e) => evConst e
(*	      | Id id =>			*)
	      | Ast.EnumId (mem,li) => SOME (tcUnit, li)
(*	      | Ast.InitExpr exps =>		*)
(*	      | Ast.ExprExt ((ctype,exp,statement,binop) AstExt.expressionExt) =>	*)
	      | _ => NONE
       ) end
  (* end fun evalConst ctyToTcTy (Ast.EXPR(cexp,_,_)) *)

  (****************** helper query functions *******************)

  (* Is this a malloc id? *)
  fun isMalloc (Ast.EXPR (Ast.Id (id as {name=name,...}),_,_)) =
		(case (Symbol.name name)
		  of		"malloc" => true
		   | "_typecheck_malloc" => true
		   |		"calloc" => true
		   | "_typecheck_calloc" => true
		   |		"memalign" => true
		   | "_typecheck_memalign" => true
		   |		"realloc" => true
		   | "_typecheck_realloc" => true
		   |		"valloc" => true
		   | "_typecheck_valloc" => true
		   | _ => false
		)
    | isMalloc _ = false

  (* Is this a calloc id? *)
  fun isCalloc (Ast.EXPR (Ast.Id (id as {name=name,...}),_,_)) =
		(case (Symbol.name name)
		  of		"calloc" => true
		   | "_typecheck_calloc" => true
		   | _ => false
		)
    | isCalloc _ = false

  (* Is this a calloc id? *)
  fun isAlloca (Ast.EXPR (Ast.Id (id as {name=name,...}),_,_)) =
		(case (Symbol.name name)
		  of	      "alloca" => true
		   | "_builtin_alloca" => true
		   | _ => false
		)
    | isAlloca _ = false

  (* evaluate malloc arguments: apply evalFunc to an expression		*)
  (* representing the malloc size, or NONE if anything goes wrong	*)
  fun evalMallocSize evalFunc ctyToAid (Ast.EXPR (Ast.Id id,_,_), exps) =
      (case (Symbol.name (#name id))
	 of            "malloc"  => (case exps of e1::_ => evalFunc e1
						| _ => NONE)
	  | "_typecheck_malloc"  => (case exps of e1::_ => evalFunc e1
						| _ => NONE)
(*TODO: use "size_t" instead of stdInt?*)
	  |            "calloc"  => (case exps of e1::e2::_ => evalFunc (Ast.EXPR (Ast.Binop (Ast.Times, e1,e2)
										   , ctyToAid TypeUtil.stdInt, SourceMap.UNKNOWN))
						| _ => NONE)
	  | "_typecheck_calloc"  => (case exps of e1::e2::_ => evalFunc (Ast.EXPR (Ast.Binop (Ast.Times, e1,e2)
										   , ctyToAid TypeUtil.stdInt, SourceMap.UNKNOWN))
						| _ => NONE)
	  |            "memalign" => (case exps of e1::e2::_ => evalFunc e2
						 | _ => NONE)
	  | "_typecheck_memalign" => (case exps of e1::e2::_ => evalFunc e2
						 | _ => NONE)
	  |            "realloc" => (case exps of e1::e2::_ => evalFunc e2
						| _ => NONE)
	  | "_typecheck_realloc" => (case exps of e1::e2::_ => evalFunc e2
						| _ => NONE)
	  |            "valloc"  => (case exps of e1::_ => evalFunc e1
						| _ => NONE)
	  | "_typecheck_valloc"  => (case exps of e1::_ => evalFunc e1
						| _ => NONE)
	  |         "alloca"  => (case exps of e1::_ => evalFunc e1
					     | _ => NONE)
	  | "builtin_alloca"  => (case exps of e1::_ => evalFunc e1
					     | _ => NONE)
	  | _ => NONE
      )
    | evalMallocSize _ _ _ = NONE

  (* intercept va_arg calls, which the tc preprocessor tags
     with a "tc_varg_dummy" id on the LHS of a comma expression *)
  fun isVargDummy (exp as Ast.EXPR (cexp,_,_)) =
	(case cexp
	   of Ast.Id (id as {name=name,...}) => (Symbol.name name) = "_tc_varg_dummy"
	    | _ => false
	)

  fun isDerefLvalExpr (exp as Ast.EXPR(cexp,_,_)) =
    ( case cexp
	of Ast.Sub (e1,e2) => true
	 | Ast.Member (e,mem) => isDerefLvalExpr e
	 | Ast.Arrow (e,mem) => true
	 | Ast.Deref e => true
	 | Ast.Id id => false
	 | _ => false
    )

  fun isLvalExpr (exp as Ast.EXPR(cexp,_,_)) =
    ( case cexp
	of Ast.Sub (e1,e2) => true
	 | Ast.Member (e,mem) => isLvalExpr e
	 | Ast.Arrow (e,mem) => true
	 | Ast.Deref e => true
	 | Ast.Id id => true
	 | _ => false
    )

  (* check for constant 0; can conservative return false even if 0 possible *)
  fun isZero (exp as Ast.EXPR(cexp,_,exp_loc)) =
  (   case cexp
	of Ast.IntConst li => (li = 0)
(*	 | Ast.RealConst r		*)
(*	 | Ast.StringConst s		*)
	 | Ast.QuestionColon (e1,e2,e3) => (isZero e2) andalso (isZero e3)
	 | Ast.Assign (e1,e2) => isZero e2
	 | Ast.Comma (e1,e2) => isZero e2
	 | Ast.Sub (e1,e2) => false
(*	 | Ast.Member (e,mem) =>	*)
(*	 | Ast.Arrow (e,mem) =>		*)
(*	 | Ast.Deref e =>		*)
(*	 | Ast.AddrOf e =>		*)
	 | Ast.Binop (bop,e1,e2) =>
	   ( case bop
	       of Ast.Plus => (isZero e1) andalso (isZero e2)
		| Ast.Minus => (isZero e1) andalso (isZero e2)
		| Ast.Times => (isZero e1) orelse (isZero e2)
		| Ast.Divide => isZero e1
		| Ast.Mod => isZero e1
(*		| Ast.Gt | Ast.Lt | Ast.Gte | Ast.Lte | Ast.Eq | Ast.Neq	*)
		| Ast.And => (isZero e1) orelse (isZero e2)
		| Ast.Or => (isZero e1) andalso (isZero e2)
		| Ast.BitOr => (isZero e1) andalso (isZero e2)
		| Ast.BitAnd => (isZero e1) orelse (isZero e2)
(*		| Ast.BitXor						*)
		| Ast.Lshift => isZero e1
		| Ast.Rshift => isZero e1
(*		| Ast.PlusAssign | Ast.MinusAssign | Ast.TimesAssign	*)
(*		| Ast.DivAssign | Ast.ModAssign				*)
(*		| Ast.XorAssign | Ast.OrAssign | Ast.AndAssign		*)
(*		| Ast.LshiftAssign | Ast.RshiftAssign			*)
(*		| Ast.BinopExt of AstExt.binopExt			*)
		| _ => false
	   )
	 | Ast.Unop (uop,e) =>
	   ( case uop
	       of Ast.Uplus => isZero e
(*		| Ast.Not			*)
		| Ast.Negate => isZero e
(*		| Ast.BitNot			*)
(*		| Ast.PreInc | Ast.PostInc | Ast.PreDec | Ast.PostDec	*)
(*		| Ast.UnopExt of AstExt.unopExt				*)
		| _ => false
	   )
(*	 | Ast.SizeOf ty =>		*)
	 | Ast.Cast (ty,e) => isZero e
(*	 | Id id =>			*)
	 | Ast.EnumId (mem,li) => (li = 0)
(*	 | Ast.InitExpr exps =>		*)
(*	 | Ast.ExprExt ((ctype,exp,statement,binop) AstExt.expressionExt) =>	*)
	 | _ => false
      )


  fun isExtern (id as {stClass=stClass,...} : Ast.id) = (stClass = Ast.EXTERN)

  fun isStatic (id as {stClass=stClass,...} : Ast.id) = (stClass = Ast.STATIC)

  fun simple_lookup _ = TSL_UNSAFE

  fun stripTcInclude edecls =
      let fun procEdecls nil = (nil,0)
	    | procEdecls (edecl::edecls) =
		case edecl
		  of Ast.DECL(Ast.ExternalDecl(Ast.VarDecl(id as {name=name,...},_)),decl_aid,_) =>
			if (Symbol.name name) = "_tc_h_end" then (edecls,decl_aid)
			else procEdecls edecls
		   | _ => procEdecls edecls

	  val (edecls',tc_inc_aid) = procEdecls edecls
	  val _ = if tc_inc_aid = 0
		  then warning ("stripTcInclude: typecheck.h was not included in this file\n" ^
				"\tor the file was not preprocessed!\n")
		  else ()
      in  (edecls',tc_inc_aid)  end

  (** helper functions to "normalize" abstract objects **)
  fun applyOp nty aoTgt =
     (case aoTgt
	of aoOp (ty,ao) => aoOp (nty,ao)
	 | aoValue ty => aoValue nty
	 | _ => aoOp (nty, aoTgt)
      )
  fun applyExt (nty,oty) aoTgt =
     (case aoTgt
	of aoExt (tty,fty,ao) => aoExt (nty,fty,ao)
	 | aoValue ty => aoValue nty
	 | _ => aoExt (nty,oty,aoTgt)
      )
  fun applyStar aoTgt =
     (case aoTgt
	of aoAddrOf ao => ao
	 | _ => aoStar aoTgt
      )
  fun applyAddrOf aoTgt =
     (case aoTgt
	of aoStar ao => ao
	 | _ => aoAddrOf aoTgt
      )

  (* gets root object *)
  fun aoGetRoot aobj =
     (case aobj of aoId pid => aobj
		 | aoMalloc aid => aobj
		 | aoSDot (ao,tylist) => aoGetRoot ao
		 | aoUDot (ao,ty) => aoGetRoot ao
		 | aoReturn ao => aoGetRoot ao
		 | aoArg (ao,li) => aoGetRoot ao
		 | aoStar ao => aoGetRoot ao
		 | aoValue ty => aobj
		 | aoStringLit aid => aobj
		 | aoOp (ty,ao) => aoGetRoot ao
		 | aoExt (tty,fty,ao) => aoGetRoot ao
		 | aoAddrOf ao => aoGetRoot ao
		 | aoFunction ao => aoGetRoot ao
      )

  fun getEnclosingStruct aobj =
     (case aobj of aoSDot (ao,tylist) => aoGetRoot ao
		 | aoUDot (ao,ty) => aoGetRoot ao
		 | _ => aobj
      )

  fun aoEq (o1:Rtc.absObject, o2) = (o1 = o2)

  (* filters out non-Id/malloc objects, which can't legitimately be called *)
  (* then applyStar *)
  fun prepareCallExps nil = nil
    | prepareCallExps (head::tail) =
      (case aoGetRoot head
	 of aoId _ => (applyStar head)::(prepareCallExps tail)
	  | aoMalloc _ => (applyStar head)::(prepareCallExps tail)
	  | _ => (prepareCallExps tail)
      )
	    
  (************************** GENERAL HELPER FUNCTION *************************)
  (* C allows scalar initializers to be optionally surrounded by braces:
   *   int i = {0};
   * This function will strip out {} if ctype is not an array, struct, or union.
   *)
  fun simplifyIfScalar _ (_,NONE) = NONE
    | simplifyIfScalar ttab (ctype, SOME initExpr) =
	if	not (TypeUtil.isArray ttab ctype)
	andalso	not (isSome (TypeUtil.isStructOrUnion ttab ctype))
	then case initExpr
	       of Ast.Aggregate ((Ast.Simple exp)::nil) =>
			SOME (Ast.Simple exp)
		| Ast.Aggregate _ =>
			( warning ("simplifyIfScalar: initializing scalar with aggregate initializer:"
				   ^ (PPLib.ppToString (PPAst.ppInitExpression () ttab) initExpr)
				   ^ "\n")
			; SOME initExpr
			)
		| Ast.Simple _ => SOME initExpr
	else SOME initExpr
	
  (************************** HELPER FUNCTION FOR BUILDSTRINGMAP/BUILDUIDMAPS *************************)
  fun buildNameMap (bundle as {ast=edecls,tidtab=ttab,...} : ParseToAst.astBundle,
		    fname) =
  (let
    type pstate = {
	  filename : string,
	  function : string,
	  scope : int list,
	  string_count: int,
	  malloc_count: int
	}

    val fname = "\"" ^ fname ^ "\""

    (*-- Not only must we output the pid-info for declared ids,
	 we must do so for undeclared functions that are called;
	 use this imperative tree to store and lookup declared functions *)
    val declaredFuns = ref pidset.empty

    fun addDeclaredFun pid = 
	(declaredFuns := (pidset.add (!declaredFuns, pid)))

    fun isDeclaredFun pid = (pidset.member (!declaredFuns, pid))

    (*-- helper function to form the name prefix *)
    fun formPrefix (pstate as {filename,function,scope,string_count,malloc_count}) =
	let fun slashify nil = ""
	      | slashify (head::tail) = (slashify tail) ^ "/" ^ (Int.toString head)
	in (filename
		^ (if (String.size function = 0) then "" else ("." ^ function))
		^ (case scope  of nil => ""
				| head::tail => slashify tail)

(***********************************)
(* ALTERNATE SUIF VERSION, IF NEEDED: just ignore filename
   ALSO NEED TO CHANGE TWO CASES BELOW: search for "isStatic"
function ^ (case scope  of nil => "" | head::tail => slashify tail)
*)
(***********************************)

	) end

    (*-- helper function to normalize TC library-function names, including "_prog_main" *)
    fun processName sym =
	let val name = Symbol.name sym
	in  if String.isPrefix "_typecheck_" name
	    then String.extract (name,11,NONE)
	    else if name = "_prog_main"
		 then "main"
		 else name
	end

    fun processExprs (nil, pstate:pstate) = (nil, pstate)
      | processExprs (head::tail, pstate:pstate) =
		let val (head_map,pstate) = processExpr  (head, pstate)
		    val (tail_map,pstate) = processExprs (tail, pstate)
		in  (head_map @ tail_map, pstate) end

    and processExpr (Ast.EXPR (coreExpr,aid,loc), pstate as {filename,function,scope,string_count,malloc_count}) =
	(case coreExpr
	    of Ast.IntConst li => (nil,pstate)
	     | Ast.RealConst r => (nil,pstate)
	     | Ast.StringConst s => ([leAid (aid,SC_STATIC, (formPrefix pstate) ^ ".$strlit:" ^ (Int.toString string_count),SOME (s,loc))],
					{filename=filename,function=function,scope=scope,malloc_count=malloc_count,
					 string_count=string_count+1})
	     | Ast.Call (fexp,exps) =>
			if (isMalloc fexp) orelse (isAlloca fexp)
			then let val malloc_map = leAid (aid, if (isAlloca fexp) then SC_AUTO else SC_HEAP
							, (formPrefix pstate) ^ ".$malloc:" ^ (Int.toString malloc_count),NONE)
				 val (exps_map,pstate) = processExprs (exps,
					{filename=filename,function=function,scope=scope,string_count=string_count,
					 malloc_count=malloc_count+1})
			     in  (malloc_map :: exps_map, pstate) end
			else let fun getUndeclaredPidEntry (Ast.EXPR (Ast.Id (id as {uid=pid,name=name,...}),_,_)) =
					if not (isDeclaredFun pid)
					then [lePid (pid,SC_STATIC, "." ^ (processName name))] (* undeclared function; assume extern *)
					else nil
				   | getUndeclaredPidEntry _ = nil
				 val fn_map = getUndeclaredPidEntry fexp
				 val (fexp_map,pstate) = processExpr  (fexp,pstate)
				 val (exps_map,pstate) = processExprs (exps,pstate)
			     in  (fn_map @ fexp_map @ exps_map, pstate) end
	     | Ast.QuestionColon (e1,e2,e3) =>
			let val (e1_map,pstate) = processExpr (e1,pstate)
			    val (e2_map,pstate) = processExpr (e2,pstate)
			    val (e3_map,pstate) = processExpr (e3,pstate)
			in  (e1_map @ e2_map @ e3_map, pstate) end
	     | Ast.Assign (e1,e2) =>
			let val (e1_map,pstate) = processExpr (e1,pstate)
			    val (e2_map,pstate) = processExpr (e2,pstate)
			in  (e1_map @ e2_map, pstate) end
	     | Ast.Comma (e1,e2) =>
			let val (e1_map,pstate) = processExpr (e1,pstate)
			    val (e2_map,pstate) = processExpr (e2,pstate)
			in  (e1_map @ e2_map, pstate) end
	     | Ast.Sub (e1,e2) =>
			let val (e1_map,pstate) = processExpr (e1,pstate)
			    val (e2_map,pstate) = processExpr (e2,pstate)
			in  (e1_map @ e2_map, pstate) end
	     | Ast.Member (exp,mem) => processExpr (exp,pstate)
	     | Ast.Arrow (exp,mem) => processExpr (exp,pstate)
	     | Ast.Deref exp => processExpr (exp,pstate)
	     | Ast.AddrOf exp => processExpr (exp,pstate)
	     | Ast.Binop (binop,e1,e2) =>
			let val (e1_map,pstate) = processExpr (e1,pstate)
			    val (e2_map,pstate) = processExpr (e2,pstate)
			in  (e1_map @ e2_map, pstate) end
	     | Ast.Unop (unop,exp) => processExpr (exp, pstate)
	     | Ast.SizeOf ty => (nil,pstate)
	     | Ast.Cast (ctype,exp) => processExpr (exp,pstate)
	     | Ast.Id id => (nil,pstate)
	     | Ast.EnumId (pid,li) => (nil,pstate)
	     | Ast.ExprExt ee => (nil,pstate)
	     | Ast.ErrorExpr => (nil,pstate)
	)

    fun processInitExprs (nil,pstate) = (nil,pstate)
      | processInitExprs (head::tail,pstate) = 
		let val (head_map,pstate) = processInitExpr  (head,pstate)
		    val (tail_map,pstate) = processInitExprs (tail,pstate)
		in  (head_map @ tail_map, pstate) end

    and processInitExpr (iexpr,pstate) =
	(case iexpr of Ast.Simple exp => processExpr (exp,pstate)
		     | Ast.Aggregate iexps => processInitExprs (iexps,pstate)
	)

    fun processDecls (nil,pstate) = (nil,pstate)
      | processDecls (head::tail, pstate) =
		let val (head_map,pstate) = processDecl  (head,pstate)
		    val (tail_map,pstate) = processDecls (tail,pstate)
		in  (head_map @ tail_map, pstate) end

    and processDecl (decl,pstate) =
	( case decl
	   of Ast.TypeDecl _ => (nil,pstate)
	    | Ast.VarDecl (id as {uid=pid,ctype=cty,name=name,...}, initExprOp) =>
		let val isfun = TypeUtil.isNonPointerFunction ttab cty
		    val _ = if isfun then addDeclaredFun pid else ()
		    val id_map = if (isStatic id)
				 then if isfun
				      then lePid (pid, SC_STATIC, fname ^ "." ^ (processName name))
				      else lePid (pid, SC_STATIC, (formPrefix pstate) ^ "." ^ (processName name))
				 else if (isExtern id) orelse isfun
				      then lePid (pid, SC_STATIC, "." ^ (processName name))
				      else lePid (pid, SC_AUTO, (formPrefix pstate) ^ "." ^ (processName name))
		    val (iexp_map,pstate) = ( case initExprOp
						of SOME iexp => processInitExpr (iexp,pstate)
						 | NONE => (nil,pstate)
					    )
		in  (id_map :: iexp_map, pstate) end
	)

    fun processStmts (nil,pstate) = (nil,pstate)
      | processStmts (head::tail,pstate) =
		let val (head_map,pstate) = processStmt  (head,pstate)
		    val (tail_map,pstate) = processStmts (tail,pstate)
		in  (head_map @ tail_map, pstate) end

    and processStmt (Ast.STMT(coreStmt,_,_), pstate as {filename,function,scope,string_count,malloc_count}) =
     (case coreStmt
	of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => processExpr (exp,pstate)
		 | NONE => (nil,pstate)
	    )

	 | Ast.Compound (decls, stmts) => 
		let val lstate = {filename=filename,function=function,
				  string_count=0, (* if reset at function only: string_count=string_count, *)
				  malloc_count=0, (* if reset at function only: malloc_count=malloc_count, *)
				  scope=0::scope}
		    val (decls_map,lstate) = processDecls (decls,lstate)
		    val (stmts_map,lstate) = processStmts (stmts,lstate)
		    val pstate = {filename=filename,function=function,string_count=string_count,malloc_count=malloc_count,
				  scope=(case scope of nil => 3746::nil (* error *)
						     | head::tail => (head+1)::tail)
				  }
		in  (decls_map @ stmts_map,pstate) end

	 | Ast.While (exp,stmt) =>
		let val (exp_map,pstate)  = processExpr (exp,pstate)
		    val (stmt_map,pstate) = processStmt (stmt,pstate)
		in  (exp_map @ stmt_map, pstate) end

	 | Ast.Do (exp,stmt) =>
		let val (stmt_map,pstate) = processStmt (stmt,pstate)
		    val (exp_map,pstate)  = processExpr (exp,pstate)
		in  (stmt_map @ exp_map, pstate) end

	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val (e1_map,pstate) = case e1op of SOME e1 => processExpr (e1,pstate)
						     | NONE => (nil,pstate)
		    val (e2_map,pstate) = case e2op of SOME e2 => processExpr (e2,pstate)
						     | NONE => (nil,pstate)
		    val (e3_map,pstate) = case e3op of SOME e3 => processExpr (e3,pstate)
						     | NONE => (nil,pstate)
		    val (stmt_map,pstate) = processStmt (stmt,pstate)
		in  (e1_map @ e2_map @ e3_map @ stmt_map, pstate) end

	 | Ast.Labeled (label,stmt) => processStmt (stmt,pstate)
	 | Ast.CaseLabel (li,exp,stmt) => processStmt (stmt,pstate)
	 | Ast.DefaultLabel stmt => processStmt (stmt,pstate)
	 | Ast.Goto label => (nil,pstate)
	 | Ast.Break => (nil,pstate)
	 | Ast.Continue => (nil,pstate)
	 | Ast.Return expOp => (case expOp of SOME exp => processExpr (exp,pstate)
					    | NONE => (nil,pstate)
				)
	 | Ast.IfThen (exp,stmt) =>
		let val (exp_map,pstate)  = processExpr (exp,pstate)
		    val (stmt_map,pstate) = processStmt (stmt,pstate)
		in  (exp_map @ stmt_map, pstate) end

	 | Ast.IfThenElse (exp,stmt1,stmt2) =>
		let val (exp_map,pstate)   = processExpr (exp,pstate)
		    val (stmt1_map,pstate) = processStmt (stmt1,pstate)
		    val (stmt2_map,pstate) = processStmt (stmt2,pstate)
		in  (exp_map @ stmt1_map @ stmt2_map, pstate) end

	 | Ast.Switch (exp,stmt) =>
		let val (exp_map,pstate)  = processExpr (exp,pstate)
		    val (stmt_map,pstate) = processStmt (stmt,pstate)
		in  (exp_map @ stmt_map, pstate) end

	 | Ast.StatExt se => (nil,pstate)
	 | Ast.ErrorStmt => (nil,pstate)
     )

    fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc),
			     pstate as {filename,function,scope,string_count,malloc_count}) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => (nil,pstate)
	    | Ast.ExternalDecl (Ast.VarDecl (id as {uid=pid,ctype=cty,name=name,...},initExprOp)) =>
		let val _ = if TypeUtil.isNonPointerFunction ttab cty
			    then addDeclaredFun pid else ()
		    val id_map = lePid (pid, SC_STATIC, (if (isStatic id) then filename else "")
							^ "." ^ (processName name))
		    val (iexp_map,pstate) = ( case initExprOp
						of SOME iexp => processInitExpr (iexp, pstate)
						 | NONE => (nil,pstate)
					    )
		in  (id_map :: iexp_map, pstate) end

	    | Ast.FunctionDef (id as {uid=pid,name=name,...},ids,stmt) =>
		let val _ = addDeclaredFun pid
		    val fnname = processName name
		    val flname = if (isStatic id) then filename else ""
		    val id_map = lePid (pid, SC_STATIC, flname ^ "." ^ fnname)
		    val fstate = {filename=flname,function=fnname,scope=nil,string_count=0,malloc_count=0}
		    val formal_map = List.map ( fn id as {uid=pid,name=name,...} =>
						   lePid (pid, SC_AUTO, (formPrefix fstate) ^ "." ^ (processName name))
						) ids
		    val (stmt_map,_) =  processStmt (stmt, fstate)
		in  (id_map :: (formal_map @ stmt_map), pstate) end

	    | Ast.ExternalDeclExt _ => (nil,pstate)
	)

    fun processExternalDecls (nil,pstate) = nil
      | processExternalDecls (ed::edecls,pstate) =
		let val (ed_map,pstate) = processExternalDecl (ed,pstate) 
		    val edecls_map = processExternalDecls (edecls,pstate)
		in  (ed_map @ edecls_map) end

    val the_map = processExternalDecls (edecls, {filename=fname,function="",scope=nil,string_count=0,malloc_count=0})
  in  the_map  end
  ) (* end fun buildNameMap (bundle, fname) *)

  (************************** MAP AID/PID TO PORTABLE NAME *************************)
  fun buildUidMaps (bundle, filename) =
  (let
      val nmap = buildNameMap (bundle,filename)

      fun countElts nil = (0,0)
	| countElts (le::tail) =
	  let val (acnt,pcnt) = countElts tail
	  in  case le of leAid _ => (acnt+1, pcnt)
		       | lePid _ => (acnt, pcnt+1)
	  end

      val (acnt, pcnt) = countElts nmap

      val aidhash = HashTable.mkTable (Aid.toWord,Aid.equal) (acnt,exTC "buildUidMaps:aidhash")
      val pidhash = HashTable.mkTable (Pid.toWord,Pid.equal) (pcnt,exTC "buildUidMaps:pidhash")

      fun insertle le =
	case le of leAid (aid, sc, string, commOp) => HashTable.insert aidhash (aid,(sc,string,commOp))
		 | lePid (pid, sc, string) => HashTable.insert pidhash (pid,(sc,string))

      val _ = List.app insertle nmap
  in  (aidhash, pidhash)  end
  ) (* end fun buildUidMaps (bundle, filename) *)

  (************************** MAP PORTABLE NAME TO AID/PID *************************)
  fun buildStringMap (bundle, filename) =
  (let
      val nmap = buildNameMap (bundle,filename)

      val strhash = HashTable.mkTable
			(HashString.hashString, (fn (s1,s2) => (String.compare (s1,s2)) = EQUAL))
			(List.length nmap, exTC "buildStringMap:strhash")

      fun insertle le =
	case le of leAid (aid, sc, string, _) => HashTable.insert strhash (string,aid)
		 | lePid (pid, sc, string) => HashTable.insert strhash (string,pid)

      val _ = List.app insertle nmap

  in  strhash  end
  ) (* end fun buildStringMap (bundle, filename) *)

  (************************** ABSTRACT OBJECT TREE *************************)

  fun readTSlevels (shash,filename,filestem,tc_inc_aid) =
  (   let
	  (******** HELPER FUCTIONS ********)
	  (****** hash tables for looking up ts_categ for abstract object  ******)
	  datatype aoConstr = acId	(* pid *)
			    | acMalloc		(* aid *)
			    | acStringLit	(* aid *)
			    | acSDot of tcType list	(* ao *)
			    | acUDot of tcType		(* ao *)
			    | acReturn			(* ao *)
			    | acArg of LargeInt.int	(* ao *)
			    | acStar			(* ao *)
			    | acOp of tcType		(* ao *)
			    | acExt of tcType * tcType	(* ao *)
			    | acAddrOf			(* ao *)
			    | acFunction		(* ao *)
			    | acValue		(* value indicator *)
			    | acNotFound	(* not-found indicator *)

	  type ao_facts = { tsc: ts_categ option,
			    ecr: Int.int option,
			    esf: es_status option	(*freearg exposed status*)
			  }
	  fun af_apply ({tsc=tsc1,ecr=ecr1,esf=esf1}:ao_facts,
			{tsc=tsc2,ecr=ecr2,esf=esf2}:ao_facts) =
		{ tsc = if isSome tsc1 then tsc1 else tsc2,
		  ecr = if isSome ecr1 then ecr1 else ecr2,
		  esf = if isSome esf1 then esf1 else esf2
		}

	  val af_empty = { tsc=NONE, ecr=NONE, esf=NONE }

	  datatype ac_entry = acE of aoConstr * ao_facts * ac_entry list

	  (*TODO: init size to more meaningful number?*)
	  val athash = HashTable.mkTable (Aid.toWord,Aid.equal) (100,exTC "readTSlevels:athash") (* (Aid.uid , ac_entry list) hash *)
	  val pthash = HashTable.mkTable (Pid.toWord,Pid.equal) (100,exTC "readTSlevels:athash") (* (Pid.uid , ac_entry list) hash *)

	  val noins_libfn_p_aset = ref aidset.empty
	  val noins_libfn_v_aset = ref aidset.empty

	  val mbu_amap = ref aidmap.empty
	  val mbu_pset = ref pidset.empty
	  fun mbu_aInsert (aid,obj) = (mbu_amap := (aidmap.insert (!mbu_amap, aid, obj)))
	  fun mbu_pInsert pid = (mbu_pset := (pidset.add (!mbu_pset, pid)))

	  val redpw_aset = ref aidset.empty
	  val redpa_aset = ref aidset.empty
	  val redtg_aset = ref aidset.empty
	  val redtv_aset = ref aidset.empty

	  val ran_aset = ref aidset.empty

	  (* (Int.int , ao list) hash *)
	  val ecrToAO_hash = HashTable.mkTable (Word.fromInt,fn (x:int,y) => x=y) (100,exTC "readTSlevels:ecrToAO_hash")
	  (* (Int.int , Int.int list) hash *)
	  val ecrIncl_hash = HashTable.mkTable (Word.fromInt,fn (x:int,y) => x=y) (100,exTC "readTSlevels:ecrIncl_hash")

	  (****** insert entry into hash table ******)
	  fun insertTSentry (ao,af_new) =
	  (   let fun processAO (ao,aclist) =
		  (   let
			  fun updateEntryList (nil, ac, aclist, af) = [updateEntry(NONE, ac, aclist, af)]
			    | updateEntryList (((entry' as acE (ac',_,_))::tail), ac, aclist, af) =
				      if ac' = ac
				      then updateEntry(SOME entry', ac, aclist, af) :: tail
				      else entry' :: (updateEntryList (tail, ac, aclist, af))

			  and updateEntry (entOp, ac, aclist, af) =
			      case entOp
				of SOME (acE (ac', af', enlist')) =>	(* should assert ac' = ac *)
				  (case aclist
				     of nil => acE (ac', af_apply(af,af'), enlist')
				      | head::tail => acE (ac', af', updateEntryList (enlist', head, tail, af))
				   )
				 | NONE =>
				  (case aclist
				     of nil => acE (ac, af, nil)
				      | head::tail => acE (ac, af_empty, updateEntryList (nil, head, tail, af))
				   )
			      
		      in case ao
			   of aoId pid =>
				let val new_entry = updateEntry (HashTable.find pthash pid, acId, aclist, af_new)
				in  HashTable.insert pthash (pid,new_entry)  end
			    | aoMalloc aid =>
				let val new_entry = updateEntry (HashTable.find athash aid, acMalloc, aclist, af_new)
				in  HashTable.insert athash (aid,new_entry)  end
			    | aoSDot (ao,tylist) => processAO (ao, (acSDot tylist)::aclist)
			    | aoUDot (ao,ty) => processAO (ao, (acUDot ty)::aclist)
			    | aoReturn ao => processAO (ao, acReturn::aclist)
			    | aoArg (ao,li) => processAO (ao, (acArg li)::aclist)
			    | aoStar ao => processAO (ao, acStar::aclist)
			    | aoValue ty => () (* value always safe - do nothing *)
			    | aoStringLit aid =>
				let val new_entry = updateEntry (HashTable.find athash aid, acStringLit, aclist, af_new)
				in  HashTable.insert athash (aid,new_entry)  end
			    | aoOp (ty,ao) => processAO (ao, (acOp ty)::aclist)
			    | aoExt (tty,fty,ao) => processAO (ao, (acExt (tty,fty))::aclist)
			    | aoAddrOf ao => processAO (ao, acAddrOf::aclist)
			    | aoFunction ao => processAO (ao, acFunction::aclist)
		      end
		  ) (* end fun processAO (ao,aclist) *)

	      in processAO (ao,nil) end
	  ) (* end fun insertTSentry (ao,af_new) *)

	  (****** lookup ts_categ for one abstract object ******)
	  fun lookupAOfacts (athash,pthash) ao =
	  (   let fun deref tsc = case tsc of TSC_POSS_INVALID => TSC_POSS_INVALID
					    | TSC_BADLY_TYPED => TSC_POSS_INVALID
					    | TSC_INFLUENTIAL => TSC_BADLY_TYPED
					    | TSC_EXPOSED => TSC_SAFE
					    | TSC_SAFE => TSC_SAFE
		  fun get_ao_entry ao =
		      case ao
			of aoId pid =>
				( case (HashTable.find pthash pid)
				    of SOME entry => entry	(* should assert entry = (acId,_,_)? *)
				     | NONE => acE (acNotFound, af_empty, nil)
				)
			 | aoMalloc aid => 
				( case (HashTable.find athash aid)
				    of SOME entry => entry	(* should assert entry = (acMalloc,_,_)? *)
				     | NONE => acE (acNotFound, af_empty, nil)
				)
			 | aoSDot (ao,tlist) =>
				let val acE (aoc,{tsc,ecr,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, {tsc=tsc, (*inherit tsc*)
								     ecr=ecr, (*inherit ecr? for now*)
								     esf=NONE
								    }, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acSDot tlist' => if (tlist = tlist') then entry else find tail
						  | _ => find tail
				in  find elist  end
			 | aoUDot (ao,ty) =>
				let val acE (aoc,{tsc,ecr,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, {tsc=tsc, (*inherit tsc*)
								     ecr=ecr, (*inherit ecr? for now*)
								     esf=NONE
								    }, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acUDot ty' => if (ty = ty') then entry else find tail
						  | _ => find tail
				in  find elist  end
			 | aoReturn ao =>
				let val acE (aoc,_,elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, af_empty, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acReturn => entry
						  | _ => find tail
				in  find elist  end
			 | aoArg (ao,li) =>
				let val acE (aoc,_,elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, af_empty, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acArg li' => if (li = li') then entry else find tail
						  | _ => find tail
				in  find elist  end
			 | aoStar ao =>
				let val acE (aoc,{tsc,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, {tsc = (*inherit tsc,no tracked*)
									   SOME (deref (getOpt (tsc, !Flags.ts_default))),
								     ecr=NONE, esf=NONE
								    }, nil)

				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acStar => entry
						  | _ => find tail
				in  find elist  end
			 | aoValue ty =>
				acE (acValue, {tsc = SOME TSC_SAFE, ecr=NONE,esf=NONE}, nil)	(* value always safe *)
			 | aoStringLit aid => 
				( case (HashTable.find athash aid)
				    of SOME entry => entry	(* should assert entry = (acStringLit,_,_)? *)
				     | NONE => acE (acNotFound, af_empty, nil)
				)
			 | aoOp (ty,ao) => 
				let val acE (aoc,{ecr,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound,
							{ tsc = SOME (	if ty = tcPointer
									then TSC_POSS_INVALID (* op to pointer always unsafe *)
									else !Flags.ts_default ), (*op is safe by default*)
							  ecr = ecr, (*inherit ecr*)
							  esf=NONE
							}, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acOp ty' => if (ty = ty') then entry else find tail
						  | _ => find tail
				in  find elist  end
			 | aoExt (tty,fty,ao) => 
				let val acE (aoc,{tsc,ecr,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, {tsc=tsc, (*inherit tsc*)
								     ecr=ecr, (*inherit ecr*)
								     esf=NONE
								    }, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acExt (tty',fty') => if (tty = tty') andalso (fty = fty')
									 then entry else find tail
						  | _ => find tail
				in  find elist  end
			 | aoAddrOf ao => 
				let val acE (aoc,_,elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, af_empty, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acAddrOf => entry
						  | _ => find tail
				in  find elist  end
			 | aoFunction ao => 
				let val acE (aoc,{tsc,...},elist) = get_ao_entry ao
				    fun find nil = acE (acNotFound, { tsc = (*inherit tsc,no tracked*)
									    SOME (deref (getOpt (tsc, !Flags.ts_default))),
								      ecr = NONE, esf=NONE
								    }, nil)
				      | find ((entry as acE (aoc,_,_))::tail) =
					case aoc of acFunction => entry
						  | _ => find tail
				in  find elist  end
		  val acE (_, aofacts, elist) = get_ao_entry ao
(*	      in  (getOpt (tsc, !Flags.ts_default), getOpt (ecr, 0), elist)  end
*)
	      in  (aofacts, elist)  end
	  ) (* end fun lookupAOfacts (athash,pthash) ao *)

	  (******** flow-sensitive results reader ********)

	  val fs_thisfile_index = ref 0

	  fun flowSensProcessLine strToAO str =
	  (
	    if (String.sub (str,0)) = #"@"
	    then let val ss = Substring.extract (str,2,NONE) (* skip leading "@ " *)
		     val (num_ss,name_ss) = Substring.splitl Char.isDigit ss
		 in  case Int.fromString (Substring.string num_ss)
		       of SOME num =>
			  let val name_ss = Substring.dropl Char.isSpace name_ss
			      val name_ss = Substring.dropr Char.isSpace name_ss
			  in  if (Substring.string name_ss) = filestem
			      then ( print ("Thisfile id is " ^ (Int.toString num) ^ "\n")
				   ; fs_thisfile_index := num )
			      else (
		(*		     print ("Bypassing @ directive: " ^ str)		*)
				   )
			  end
			| NONE => warning ("flowSensProcessLine(@): invalid number in line: " ^ str)
		 end
	    else let fun strToInt str =
			 case Int.fromString str
			   of SOME num => num
			    | NONE => raise (exTC ("strToInt: invalid int in [" ^ str ^ "]"))

		     val ss = Substring.all str
		     val (num_ss,str_ss) = Substring.splitl Char.isDigit ss
		     val num = strToInt (Substring.string num_ss)
		 in  if num = (!fs_thisfile_index)
		     then (case (Substring.sub (str_ss,1))
			     of #"f" => (* libfn tracked status: ! N f p [ASN] A  #-ptr  *)
					(*                       ! N f v [ASN] A  #-vuln *)
				(let val aid = tc_inc_aid + (strToInt (Substring.string (Substring.triml 7 str_ss)))
				 in (case (Substring.sub (str_ss,3)) 
				       of #"p" => (* ptr/ptrw mode: only instrument "A" calls *)
						(case (Substring.sub (str_ss,5)) 
						   of #"A" => ()
						    | #"S" => (noins_libfn_p_aset := (aidset.add (!noins_libfn_p_aset, aid)))
						    | #"N" => (noins_libfn_p_aset := (aidset.add (!noins_libfn_p_aset, aid)))
						    | c => raise (exTC ("expecting [A|S|N] found [" ^ (Char.toString c) ^ "]"))
						)
					| #"v" => (* vuln mode: instrument only if "N" *)
						(case (Substring.sub (str_ss,5)) 
						   of #"A" => (noins_libfn_v_aset := (aidset.add (!noins_libfn_v_aset, aid)))
						    | #"S" => (noins_libfn_v_aset := (aidset.add (!noins_libfn_v_aset, aid)))
						    | #"N" => ()
						    | c => raise (exTC ("expecting [A|S|N] found [" ^ (Char.toString c) ^ "]"))
						)
					| c => raise (exTC ("expecting [p|v] found [" ^ (Char.toString c) ^ "]"))
				    )
				 end
				 handle exTC str => (print ("Error Reading libfn-tracked result: " ^ str ^ "\n"))
				)

			      | #"u" => (* MBU result : ! N u g A           assign, A=aid		*)
					(*              ! N u c A           call, A=aid ->OBSOLETE?<-	*)
					(*              ! N u v A # comm    verifyTag|verifyRHS, A=aid	*)
					(*              ! N u d <AO>        decl			*)
					(*              ! N u a A           fn argument, A=aid		*)
					(*              ! N u r A           fn return, A=aid		*)
				     let fun addAidResult mbum i = 
						mbu_aInsert (i, mbum)

					 fun addDeclResult ao =
					     case ao
					       of aoId pid =>
						  mbu_pInsert pid
						| aoMalloc aid =>
						  mbu_aInsert (aid, MBU_Decl)
						| _ =>
						  print ("flowSensProcessLine: invalid DECL ao: " ^ (aoToString ao) ^ "\n")

					 val str_ss5 = (Substring.string (Substring.triml 5 str_ss))

				     in (case (Substring.sub (str_ss,3)) 
					   of #"g" => addAidResult MBU_Assign (tc_inc_aid + (strToInt str_ss5))
(* "call": obsolete?
					    | #"c" => addAidResult MBU_Call (tc_inc_aid + (strToInt str_ss5))
*)
					    | #"v" => addAidResult MBU_Verify (tc_inc_aid + (strToInt str_ss5))
					    | #"d" => addDeclResult (strToAO str_ss5)
					    | #"a" =>
						(* FOR NOW: tag for arg is always passed, so we'll ignore MBU_Arg results *)
						(* TODO: include this, which would mean some of the components of an argAddr *)
						(*       structure may be UNALLOC -- must be a global approach, incorporating *)
						(*       the type-safety levels of each aoArg object, etc... *)
				(*			addAidResult MBU_Arg (tc_inc_aid + (strToInt str_ss5))			*)
							()
					    | #"r" => addAidResult MBU_Return (tc_inc_aid + (strToInt str_ss5))
					    | _ => (print ("Unrecognized MBU result: " ^ str))
					) handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n"))
				     end
			      | #"r" => (* RED results : ! N r p [aw] A	*)
					(*		 ! N r t [gv] A	*)
				    (let val aid = tc_inc_aid + (strToInt (Substring.string (Substring.triml 7 str_ss)))
				     in (case (Substring.sub (str_ss,3)) 
					   of #"p" => (* verifyPtr *)
						(case (Substring.sub (str_ss,5)) 
						   of #"a" => (redpa_aset := (aidset.add (!redpa_aset, aid)))
						    | #"w" => (redpw_aset := (aidset.add (!redpw_aset, aid)))
						    | c => raise (exTC ("expecting [a|w] found [" ^ (Char.toString c) ^ "]"))
						)
					    | #"t" => (* verifyTag *)
						(case (Substring.sub (str_ss,5)) 
						   of #"g" => (redtg_aset := (aidset.add (!redtg_aset, aid)))
						    | #"v" => (redtv_aset := (aidset.add (!redtv_aset, aid)))
						    | c => raise (exTC ("expecting [g|v] found [" ^ (Char.toString c) ^ "]"))
						)
					    | c => raise (exTC ("expecting [p|t] found [" ^ (Char.toString c) ^ "]"))
					 )
				     end
				     handle exTC str => (print ("Error Reading RED result: " ^ str ^ "\n"))
				    )
			      | #"b" => (* RAN results : ! N b A	*)
				    (let val aid = tc_inc_aid + (strToInt (Substring.string (Substring.triml 3 str_ss)))
				     in	 (ran_aset := (aidset.add (!ran_aset, aid)))
				     end
				     handle exTC str => (print ("Error Reading RAN result: " ^ str ^ "\n"))
				    )
			      | _ => (print ("Unrecognized flow-sensitive result: " ^ str))
			  )
		     else () (* skip: not relevant to this file *)
		 end
	  )

	  (******** local mapping from external aid/pid to internal aid/pid ********)
	  (*TODO: init size to more meaningful number?*)
	  val amaphash = HashTable.mkTable (Word.fromInt,fn (x:int,y) => x=y) (100,exTC "readTSlevels:amaphash")
	  val pmaphash = HashTable.mkTable (Word.fromInt,fn (x:int,y) => x=y) (100,exTC "readTSlevels:pmaphash")

	  (* TODO: use more efficient data structures for these? *)
	  val pmbu_aolist = ref nil
	  val vuln_loc_list = ref nil
	  val vuln_deref_list = ref nil
	  val vuln_enclosing_loc_list = ref nil	(* HACKERY: this only collects enclosing structs
						   (inefficiently in a list) for use when calling
						   ts_lookup_children -- works OK if ts_lookup_children
						   is only ever called on outermost structs (as it does
						   currently, I think); I doubt this behavior will change,
						   but if it does, beware! *)

          (******** input function *******)
	  fun processLine is =
	  (   let val str = TextIO.inputLine is
		  fun processNumName (insertfn,str) =
		      let val ss = Substring.extract (str,2,NONE) (* skip leading "@ " or "% " *)
			  val (num_ss,name_ss) = Substring.splitl Char.isDigit ss
		      in  case Int.fromString (Substring.string num_ss)
			    of SOME num =>
			       let val name_ss = Substring.dropl Char.isSpace name_ss
				   val name_ss = Substring.dropr Char.isSpace name_ss
				   val name = Substring.string name_ss
			       in  case (HashTable.find shash name)
				     of SOME uid =>
(**)					( debug ("READ-A[" ^ Int.toString num ^ "=>" ^ Int.toString uid ^ "]:[" ^ name ^ "]\n") ;
					  insertfn (num,uid)
(**)					)

				      | NONE =>
					( debug ("readTSlevels/processLine: uid name not found [" ^ name ^ "]\n") )
			       end
			     | NONE => warning ("readTSlevels/processLine/processNumName: invalid number in line: " ^ str)
		      end

		  fun letterToTSL ltr = case ltr
					  of #"P" => TSC_POSS_INVALID
					   | #"B" => TSC_BADLY_TYPED
					   | #"I" => TSC_INFLUENTIAL
					   | #"E" => TSC_EXPOSED
					   | #"S" => TSC_SAFE
					   | #"T" => TSL_TRACKED
					   | #"U" => TSL_UNSAFE
					   | _ =>
						(warning ("readTSlevels/processLine: invalid TS level [" ^ (String.str ltr) ^ "]\n");
						 raise (exTC ("readTSlevels:letterToTSL(" ^ (String.str ltr) ^ ")")))
		  exception exTCnotFound
		  fun intToUid maphash exid =
			case (HashTable.find maphash exid)
			  of SOME uid => uid
			   | NONE =>
				(* not an error, because exid may not map to something in current file *)
				(debug ("readTSlevels/intToUid(stringToAO): external id not found ["
						^ (Int.toString exid) ^ "]\n");
				 raise exTCnotFound
				)

	      in
		  if (str = "") then ()
		  else (case String.sub (str,0)
			  of #"@" => (* AID entry: @ N <name>\n *)
				processNumName (HashTable.insert amaphash, str)

			   | #"%" => (* PID entry: % N <name>\n *)
				processNumName (HashTable.insert pmaphash, str)

			   | #"^" => (* Default TS level : ^ [S|T|U | P|B|I|E|S]\n *)
				((Flags.ts_default := letterToTSL (String.sub (str,2)))
				  handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n"))
				; inform ("Default type-safety level set to " ^ (tsToDescr (!Flags.ts_default)) ^ "\n")
				)

			   | #"~" => (* TS level : ~ [S|T|U | P|B|I|E|S] <N> <O>\n *)
				(let val tsc = letterToTSL (String.sub (str,2))
				     val ss = Substring.extract (str,4,NONE) (* skip leading "~ L " *)
				     val (ecrno_ss,ao_ss) = Substring.splitl Char.isDigit ss
				     val ecrOp = Int.fromString (Substring.string ecrno_ss)
				     val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO")
						(Substring.string ao_ss)
(**)				     val _ = debug ("READ[" ^ (tsToDescr tsc) ^ "]:["
(**)							    ^ (Int.toString (getOpt (ecrOp,0))) ^ "]:["
(**)							    ^ (aoToString ao) ^ "]\n")

				     (* Update ECR/AOlist hash table *)
				     val _ = case ecrOp
					       of SOME ecrno =>
						  let val ecr_aolist = getOpt (HashTable.find ecrToAO_hash ecrno, nil)
						  in  HashTable.insert ecrToAO_hash (ecrno,ao::ecr_aolist)  end
					        | NONE => ()

				     (* Update athash/pthash entry *)
				 in  insertTSentry (ao,{tsc=SOME tsc,ecr=ecrOp,esf=NONE})  end

				 handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* letterToTSL or stringToAO failure *)
				      |	exTCnotFound => ()
				)

			   | #"M" => (* May be uninit (ptr/ptrw mode): M <O>\n *)
				(let val ss = Substring.extract (str,2,NONE) (* skip leading "M " *)
				     val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO")
						(Substring.string ss)
				 in  if (!Flags.nullifyMBUptrs)
				     then (pmbu_aolist := (ao::(!pmbu_aolist)))
				     else ()
				 end
				 handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* stringToAO failure *)
				      |	exTCnotFound => ()
				)

			   | #"T" => (* May be uninit tracked (ptr/ptrw mode): T <O>\n *)
				(let val ss = Substring.extract (str,2,NONE) (* skip leading "T " *)
				     val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO")
						(Substring.string ss)
				 in  if not (!Flags.nullifyMBUptrs)
				     andalso not (!Flags.nullifyAllAutos)
				     then insertTSentry (ao,{tsc=SOME TSC_EXPOSED,ecr=NONE,esf=NONE})
				     else ()
				 end
				 handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* stringToAO failure *)
				      |	exTCnotFound => ()
				)

			   | #"U" => (* May be uninit unsafe (ptr/ptrw mode): U <O>\n *)
				(let val ss = Substring.extract (str,2,NONE) (* skip leading "U " *)
				     val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO")
						(Substring.string ss)
				 in  if not (!Flags.nullifyMBUptrs)
				     andalso not (!Flags.nullifyAllAutos)
				     then insertTSentry (ao,{tsc=SOME TSC_POSS_INVALID,ecr=NONE,esf=NONE})
				     else ()
				 end
				 handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* stringToAO failure *)
				      |	exTCnotFound => ()
				)

			   | #">" => (* ECR inclusion : > N <N-list>\n *)
				let val numstrlist = String.tokens (not o Char.isDigit) str
				    val numlist = List.mapPartial Int.fromString numstrlist (*SY: no warning*)
				in  case numlist
				      of nil => warning ("readTSlevels/ecr-incl-case: no numbers on line: " ^ str)
				       | (head::nil) => warning ("readTSlevels/ecr-incl-case: empty incl list: " ^ str)
				       | (head::tail) => HashTable.insert ecrIncl_hash (head,tail)
				end

			   | #"/" => (* free trackedness attribute: / [A|S|N] [A|S|N] <O> \n	*)
				     (*        for vulnerable mode: /V[A|S|N] [A|S|N] <O> \n	*)
				if (if ((String.sub (str,1)) = #"V") then !Flags.vuln
								     else not (!Flags.vuln))
				then (let val position = if !Flags.freeMallocOnly then 4 else 2
					  val esf = case (String.sub (str,position))
						      of #"A" => ES_ALL
						       | #"S" => ES_SOME
						       | #"N" => ES_NONE
						       | ltr => raise (exTC ("readTSlevels:invalid ES(" ^ (String.str ltr) ^ ")"))

					  val ss = Substring.extract (str,6,NONE) (* skip leading "/ E E " *)
					  val ao = stringToAO
						   (intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO(/)")
						   (Substring.string ss)

					  (* Update athash/pthash entry *)
				      in  insertTSentry (ao,{tsc=NONE,ecr=NONE,esf=SOME esf})  end

				      handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* invalid ES or stringToAO failure *)
					   | exTCnotFound => ()
				     )
				else ()
			   | #"!" => (* Flow-sensitive results *)
				flowSensProcessLine (stringToAO (intToUid amaphash,
								 intToUid pmaphash,
								 exTC "flowSensProcessLine:stringToAO"))
						    (String.extract (str,2,NONE))
			   | #"_" => (* Flow-sensitive (range analysis) untouched exposed ao: _ [T|U] <O> \n *)
				(let val untouched = ((String.sub (str,2)) = #"U")
				     val ss = Substring.extract (str,4,NONE) (* skip leading "_ [T|U] " *)
				     val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO(_)")
						(Substring.string ss)

				     (* Update athash/pthash entry *)
				 in  if untouched
				     then insertTSentry (ao,{tsc=SOME (TSC_SAFE),ecr=NONE,esf=NONE})
				     else ()
				 end

				 handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* stringToAO failure *)
				      |	exTCnotFound => ()
				)
			   | #"V" => (* Vulnerable mode: V [l|d] <O> \n *)
				if !Flags.vuln
				then (let val isloc = ((String.sub (str,2)) = #"l")
					  val ss = Substring.extract (str,4,NONE) (* skip leading "V [l|d] " *)
					  val ao = stringToAO
						(intToUid amaphash,intToUid pmaphash,exTC "readTSlevels:stringToAO(V)")
						(Substring.string ss)

					  (* Update vuln loc/deref list *)
				      in  if isloc
					  then ( (vuln_loc_list := (ao::(!vuln_loc_list)))
					       ; (vuln_enclosing_loc_list := ((getEnclosingStruct ao)::(!vuln_enclosing_loc_list)))
					       )
					  else (vuln_deref_list := (ao::(!vuln_deref_list)))
				      end
				      handle exTC str => (print ("EXCEPTION : " ^ str ^ "\n")) (* stringToAO failure *)
				  	   | exTCnotFound => ()
				     )
				else ()
			   | #"#" => () (* comment line ignored *)
			   | _ => warning ("readTSlevels: unrecognized input line " ^ str) (* unrecognized line, ignore *)
			) before processLine is
	      end
	  ) (* end fun processLine is *)
          (******** end input function *******)

	  (******** "MAIN" PART ********)
	  val is = TextIO.openIn filename

	  val _ = inform ("Reading type-safety levels from file " ^ filename ^ "\n")

	  val _ = processLine is
	
	  val _ = TextIO.closeIn is

	  fun lookup_fun nil = !Flags.ts_default
	    | lookup_fun (ao::tail) = ts_min (getOpt (#tsc (#1 (lookupAOfacts (athash,pthash) ao)), !Flags.ts_default),
					      lookup_fun tail)

	  fun lookup_freearg_status nil = ES_NONE
	    | lookup_freearg_status (ao::tail) =
		let val aofacts = (#1 (lookupAOfacts (athash,pthash) ao))
		in  case (#esf aofacts)
		      of NONE => lookup_freearg_status tail
		       | SOME ES_NONE =>
			 let val estail = lookup_freearg_status tail
			 in  if (estail = ES_ALL) then ES_SOME
						  else estail
			 end
		       | SOME ES_SOME => ES_SOME
		       | SOME ES_ALL => if (null tail)
					orelse ((lookup_freearg_status tail) = ES_ALL)
					then ES_ALL
					else ES_SOME
		end

	  fun lookup_children_fun nil = !Flags.ts_default
	    | lookup_children_fun (ao::tail) =
		let fun findTrackedChildren nil = TSC_SAFE
		      | findTrackedChildren ((acE (aoc,{tsc,...},elist))::tail) =
			case aoc of acSDot _ => ts_min (ts_min (getOpt (tsc,!Flags.ts_default),
								findTrackedChildren elist),
								findTrackedChildren tail)
				  | acUDot _ => ts_min (ts_min (getOpt (tsc,!Flags.ts_default),
								findTrackedChildren elist),
								findTrackedChildren tail)
				  | _ => findTrackedChildren tail

		    val (aofacts,elist) = lookupAOfacts (athash,pthash) ao

		in  ts_min (ts_min (getOpt (#tsc aofacts, !Flags.ts_default),
				    findTrackedChildren elist),
				    lookup_children_fun tail)
		end

	  (* Cache for speeding up lookup_aliases_fun -- not sure if speedup actually helps! *)
	  (* (Int.int , ao list * ecrset) hash *)
	  val alias_cache = HashTable.mkTable (Word.fromInt,fn (x:int,y) => x=y) (100,exTC "lookup_alias_fun:alias_cache")
	  fun lookup_aliases_fun nil = nil
	    | lookup_aliases_fun (ao::tail) =
	      let
		  fun getECRaliasList (ecr,eset) =
		     (let fun doTraverse () =
			  let val inclToECRs = getOpt (HashTable.find ecrIncl_hash ecr, nil)
			      val (alist',eset') = followInclToECRs (inclToECRs, eset)
			      val thislist = getOpt (HashTable.find ecrToAO_hash ecr, nil)
			  in  (alist' @ thislist, eset')  end
		      in case (HashTable.find alias_cache ecr)
			   of SOME (alist,eset') =>
				if ecrset.isEmpty (ecrset.difference (ecrset.intersection (eset,eset'), ecrset.singleton ecr))
				then (alist,ecrset.union (eset',eset))
				else doTraverse ()
			    | NONE =>
				let val (list',eset') = doTraverse ()
				    val _ = if ecrset.isEmpty (ecrset.difference (ecrset.intersection (eset,eset'), ecrset.singleton ecr))
					    then HashTable.insert alias_cache (ecr,(list',eset'))
					    else ()
				in  (list',eset')  end
		      end
		     )

		  and followInclToECRs (nil,eset) = (nil,eset)
		    | followInclToECRs (ecr::tail, eset) =
		      let val (aolist',eset') = if ecrset.member (eset,ecr)
						then (nil,eset)
						else getECRaliasList (ecr, ecrset.add (eset,ecr))
			  val (aolist'',eset'') = followInclToECRs (tail,eset')
		      in  (aolist' @ aolist'', eset'')  end

		  val ecr = #ecr (#1 (lookupAOfacts (athash,pthash) ao))
		  val alist = case ecr
				of SOME ecrno => #1 (getECRaliasList (ecrno, ecrset.singleton ecrno))
				 | NONE => nil
	      in  alist  end

	  (* Old uncached version of lookup; may want to revert to this if cached version causes problems *)
	  fun lookup_aliases_fun_uncached nil = nil
	    | lookup_aliases_fun_uncached (ao::tail) =
	      let fun followECRinclTo (ecr,eset) =
		      let fun followECRlistInclTo (nil,eset) = eset
			    | followECRlistInclTo (ecr::tail, eset) =
			      let val eset = if ecrset.member (eset, ecr)
					 then eset
					 else followECRinclTo (ecr, ecrset.add (eset,ecr))
			      in
				  followECRlistInclTo (tail, eset)
			      end

			  val inclToList = getOpt (HashTable.find ecrIncl_hash ecr, nil)
			  val eset' = followECRlistInclTo (inclToList, eset)
		      in  eset'  end

		  val ecr = #ecr (#1 (lookupAOfacts (athash,pthash) ao))
		  val eset = case ecr
			       of SOME ecrno => followECRinclTo (ecrno, ecrset.singleton ecrno)
				| NONE => ecrset.empty

	      in  ecrset.foldl (fn (ecr,aolist) =>
				   (getOpt (HashTable.find ecrToAO_hash ecr, nil)
				    ) @ aolist
				) nil eset
	      end

(***
(* diagnostic function to validate result of uncached lookup_aliases_fun *)
fun lacf aolist =
let
    val _ = print "---Look OLD\n"
    val oldalist = lookup_aliases_fun_uncached aolist
    val _ = print " --Look NEW\n"
    val newalist = lookup_aliases_fun aolist
    val _ = print "------Look DONE-----\n"

    fun listEqual (nil,nil) = true
      | listEqual (nil,_) = false
      | listEqual (_,nil) = false
      | listEqual (h1::t1,list2) =
	let fun remove (e1,nil) = [Rtc.aoValue Rtc.tcVoid]
	      | remove (e1,h::t) = if e1 = h
				   then t
				   else h::remove(e1,t)
	in  listEqual (t1,remove (h1,list2))  end

    val _ = if not (listEqual (oldalist, newalist))
	    then (print ("================LISTS DIFFER! "
			^ (Int.toString (List.length oldalist)) ^ "/"
			^ (Int.toString (List.length newalist)) ^ "===============\n")
		 ; List.app (fn ao => print ((Rtc.aoToString ao) ^ "\n")) oldalist
		 ; print "----^ old/new v---\n"
		 ; List.app (fn ao => print ((Rtc.aoToString ao) ^ "\n")) newalist
		 )
	    else ()
in  newalist  end
***)
	  fun mbu_lookup_aid (aid,mbum) =
	      let fun mm_to_str mm =
		    ( case mm  of MBU_Assign => "Assign"
	(*			| MBU_Call => "Call"	*)
				| MBU_Verify => "Verify"
				| MBU_Decl => "Decl"
       				| MBU_Arg => "Arg"
				| MBU_Return => "Return"
		    )
	      in  case (aidmap.find (!mbu_amap, aid))
		    of SOME mm =>
			( if (mbum = mm)
			  then ()
			  else print ("mbu_lookup_aid (" ^ (Int.toString aid) ^ "): mismatched MBU markers: "
					^ (mm_to_str mbum) ^ "/" ^ (mm_to_str mm) ^ "\n")
			; true )
		     | NONE => false
	      end

	  fun mbu_lookup_pid pid = pidset.member (!mbu_pset, pid)

	  fun pmbu_lookup_ao ao =
		(List.exists (fn ao' => ao = ao') (!pmbu_aolist))
	  fun vuln_loc_lookup_ao ao =
		(List.exists (fn ao' => ao = ao') (!vuln_loc_list))
	  fun vuln_enclosing_loc_lookup_ao ao =
		(List.exists (fn ao' => ao = ao') (!vuln_enclosing_loc_list))
	  fun vuln_deref_lookup_ao ao =
		(List.exists (fn ao' => ao = ao') (!vuln_deref_list))

	  fun vp_red_lookup_aid isptrw aid =
	      if isptrw then aidset.member (!redpw_aset, aid)
			else aidset.member (!redpa_aset, aid)

	  fun noins_libfn_aid vuln aid =
	      if vuln then aidset.member (!noins_libfn_v_aset, aid)
		      else false	(*FOR NOW: don't use in -ptr/ptrw mode*)
(*			   aidset.member (!noins_libfn_p_aset, aid)	*)

	  fun vt_red_lookup_aid RED_Assign aid = aidset.member (!redtg_aset, aid)
	    | vt_red_lookup_aid RED_Verify aid = aidset.member (!redtv_aset, aid)

	  fun ran_lookup_aid aid = aidset.member (!ran_aset, aid)

      in ({ ts_lookup_fn = lookup_fun
	  , ts_lookup_children_fn = lookup_children_fun
	  , freearg_lookup_fn = lookup_freearg_status
	  , noins_libfn_lookup = noins_libfn_aid
	  , may_be_uninit_aid = mbu_lookup_aid
	  , may_be_uninit_pid = mbu_lookup_pid
	  , vp_may_be_uninit_ao = pmbu_lookup_ao
	  , vp_vuln_loc_ao = vuln_loc_lookup_ao
	  , vp_vuln_enclosing_loc_ao = vuln_enclosing_loc_lookup_ao
	  , vp_vuln_deref_ao = vuln_deref_lookup_ao
	  , vp_redundant_aid = vp_red_lookup_aid
	  , vt_redundant_aid = vt_red_lookup_aid
	  , array_inbounds_aid = ran_lookup_aid
	  }, if !Flags.cacheAlias
	     then lookup_aliases_fun
	     else lookup_aliases_fun_uncached
	 )
      end

  ) (* end fun readTSlevels shash filename *)

  (************************** ADDR TAKEN ANALYSIS *************************)
  fun addrTakenAnalysis (bundle as {ast as edecls,
			 tidtab,
			 errorCount,
			 warningCount,
			 auxiliaryInfo as {aidtab, implicits, env as symtab}}) =
  (let

    datatype process_mode = PM_TOP | PM_ASSIGN | PM_ADDR

    fun processExprs (nil, mode:process_mode) = nil
      | processExprs ((head::tail), mode:process_mode) = (processExpr (head,mode)) @ (processExprs (tail,mode))

    and processExpr ((Ast.EXPR (coreExpr,_,_)), mode:process_mode) =
	(case coreExpr
	    of Ast.IntConst li => nil
	     | Ast.RealConst r => nil
	     | Ast.StringConst s => nil
	     | Ast.Call (fexp,exps) => (processExpr (fexp,mode)) @ (processExprs (exps, PM_ASSIGN))
	     | Ast.QuestionColon (e1,e2,e3) => (processExpr (e1,PM_TOP)) @ (processExpr (e2,mode)) @ (processExpr (e3,mode))
	     | Ast.Assign (e1,e2) => (processExpr (e1,mode)) @ (processExpr (e2,PM_ASSIGN))
	     | Ast.Comma (e1,e2) => (processExpr (e1,PM_TOP)) @ (processExpr (e2,mode))
	     | Ast.Sub (e1,e2) => (processExpr (e1,PM_TOP)) @ (processExpr (e2,PM_TOP))
	     | Ast.Member (exp,mem) =>
		   let	val elist = processExpr (exp,mode)
			fun appendmem nil = nil
			  | appendmem ((id, mlist)::tail) = (id, mlist @ [mem])::(appendmem tail)
		   in appendmem elist end
	     | Ast.Arrow (exp,mem) => processExpr (exp,PM_TOP)
	     | Ast.Deref exp => (processExpr (exp,PM_TOP))
	     | Ast.AddrOf exp => (processExpr (exp,PM_ADDR))
	     | Ast.Binop (binop,e1,e2) => (processExpr (e1,mode))
				@ (processExpr (e2, if (isAssignBinop binop) then PM_ASSIGN else mode))
	     | Ast.Unop (unop,exp) => processExpr (exp, if (isAssignUnop unop) then PM_ASSIGN else mode)
	     | Ast.SizeOf ty => nil
	     | Ast.Cast (ctype,exp) => processExpr (exp,mode)
	     | Ast.Id (id as {uid=pid,ctype=cty,...}) =>
			if (mode = PM_ADDR)
			orelse ((mode = PM_ASSIGN) andalso (TypeUtil.isArray tidtab cty))
			then [(pid,nil)] else nil
	     | Ast.EnumId (pid,li) => nil
	     | Ast.ExprExt ee => nil
	     | Ast.ErrorExpr => nil
	)

    fun processInitExprs nil = nil
      | processInitExprs (head::tail) = (processInitExpr head) @ (processInitExprs tail)

    and processInitExpr iexpr =
	(case iexpr of Ast.Simple exp => processExpr (exp, PM_ASSIGN)
		     | Ast.Aggregate iexps => processInitExprs iexps
	)

    fun processDecls nil = nil
      | processDecls (head::tail) = (processDecl head) @ (processDecls tail)

    and processDecl decl =
	( case decl
	   of Ast.TypeDecl _ => nil
	    | Ast.VarDecl (id as {uid=pid,ctype=cty,...}, initExprOp) =>
	      ( if (TypeUtil.isArray tidtab cty)
		orelse (TypeUtil.isUnion tidtab cty)
		then [(pid,nil)] else nil )
	      @ ( case initExprOp
		    of SOME iexp => processInitExpr iexp
		     | NONE => nil
		)
	)

    fun processStmts nil = nil
      | processStmts (head::tail) = (processStmt head) @ (processStmts tail)

    and processStmt ((Ast.STMT(coreStmt,_,_))) =
     (case coreStmt
	of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => processExpr (exp,PM_TOP)
		 | NONE => nil
	    )

	 | Ast.Compound (decls, stmts) => (processDecls decls) @ (processStmts stmts)

	 | Ast.While (exp,stmt) => (processExpr (exp,PM_TOP)) @ (processStmt stmt)

	 | Ast.Do (exp,stmt) => (processExpr (exp,PM_TOP)) @ (processStmt stmt)

	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1pids = case e1op of SOME exp1 => processExpr (exp1, PM_TOP)
					   | NONE => nil
		    val e2pids = case e2op of SOME exp2 => processExpr (exp2, PM_TOP)
					   | NONE => nil
		    val e3pids = case e3op of SOME exp3 => processExpr (exp3, PM_TOP)
					   | NONE => nil
		in
		    e1pids @ e2pids @ e3pids @ (processStmt stmt)
		end

	 | Ast.Labeled (label,stmt) => processStmt stmt
	 | Ast.CaseLabel (li,exp,stmt) => processStmt stmt
	 | Ast.DefaultLabel stmt => processStmt stmt
	 | Ast.Goto label => nil
	 | Ast.Break => nil
	 | Ast.Continue => nil
	 | Ast.Return expOp => (case expOp
				  of SOME exp => processExpr (exp, PM_ASSIGN)
				   | NONE => nil
				)
	 | Ast.IfThen (exp,stmt) => (processExpr (exp,PM_TOP)) @ (processStmt stmt)
	 | Ast.IfThenElse (exp,stmt1,stmt2) =>
			(processExpr (exp,PM_TOP)) @ (processStmt stmt1) @ (processStmt stmt2)
	 | Ast.Switch (exp,stmt) => (processExpr (exp,PM_TOP)) @ (processStmt stmt)
	 | Ast.StatExt se => nil
	 | Ast.ErrorStmt => nil
     )

    fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => nil
	    | Ast.ExternalDecl (Ast.VarDecl (id as {uid=pid,ctype=cty,...},initExprOp)) =>
	      ( if (TypeUtil.isArray tidtab cty)
		orelse (TypeUtil.isUnion tidtab cty)
	        then [(pid,nil)] else nil)
	      @ (case initExprOp
		   of SOME iexp => processInitExpr iexp
		    | NONE => nil
		)
	    | Ast.FunctionDef (id,ids,stmt) => processStmt stmt
	    | Ast.ExternalDeclExt _ => nil
	)

    fun processExternalDecls nil = nil
      | processExternalDecls (ed::edecls) =
		(processExternalDecl ed) @ (processExternalDecls edecls)

    val addrpids = processExternalDecls (edecls)

    fun buildPidTree(nil) = pidset.empty
      | buildPidTree((pid,_)::tail) =
		pidset.add (buildPidTree(tail), pid)

    val pidtree = buildPidTree addrpids

    fun lookup_one_ts_level absObj =
	(case absObj
	   of aoId pid => if (pidset.member (pidtree, pid))
			  then TSL_UNSAFE else TSL_SAFE
	    | aoMalloc aid => TSL_UNSAFE
	    | aoSDot (ao,tlist) => lookup_one_ts_level ao
	    | aoUDot (ao,ty) => TSL_UNSAFE
	    | aoReturn ao => TSL_UNSAFE (* should resolve? *)
	    | aoArg (ao,li) => TSL_UNSAFE (* should resolve w/formal *)
	    | aoStar ao => TSL_UNSAFE
	    | aoValue ty => TSL_SAFE
	    | aoStringLit aid => TSL_TRACKED
	    | aoOp (ty,ao) => lookup_one_ts_level ao (* TODO: change to TSL_SAFE *)
	    | aoExt (tty,fty,ao) => TSL_UNSAFE
	    | aoAddrOf ao => TSL_SAFE
	    | aoFunction ao => TSL_SAFE (* should never be looked up? *)
	)

    fun lookup_ts_level nil = TSL_SAFE
      | lookup_ts_level (ao::tail) = ts_min(lookup_one_ts_level ao, lookup_ts_level tail)

  in (lookup_ts_level,lookup_ts_level) end
  ) (* end fun addrTakenAnalysis bundle *)

  (**********************************************************************)
  (* HACK: if expToAbsObjects is called on a va_arg (tc_varg_dummy)	*)
  (*	comma expression, then it will return (!curvargAO).		*)
  (*	This breaks the "pure function"-ness of expToAbsObjects, but	*)
  (*	since va_args are rare, this cleans up the interface		*)
  (*	significantly.  So basically, the unclean part is that anyone	*)
  (*	who wants to call expToAbsObjects should call assignCurVargAO	*)
  (*    when processing the enclosing function's formal.		*)
  val curvargAO = ref (aoId 0)	(* the argument AO for vargs *)

  fun assignCurVargAO (fun_id : Ast.id, formal_ids) =
      let val cfAO = aoFunction (aoId (#uid fun_id))
	  fun processFormals (cnt,nil) = (curvargAO := aoArg (cfAO,cnt))
	    | processFormals (cnt, (id : Ast.id)::tail) =
		(case (#ctype id)
		   of Ast.Ellipses => (curvargAO := aoArg (cfAO,cnt))
		    | _ => processFormals (cnt+1, tail)
		)
      in  processFormals (1,formal_ids)  end

  (*********************************************************)
  (** helper functions to convert struct/union signatures **)
  datatype fieldsig = fsStruct of tcType list | fsUnion of tcType
  fun getFieldSig tidtab (ctype, mem, {deref=deref}) =
    ( let fun getFieldSigTid (tid, mem as {uid=mid,name,location,ctype,kind}) =
	      case Tidtab.find (tidtab,tid)
		of SOME{ntype=SOME(Bindings.Struct (uid, sflist)),name,global,location} =>
			let fun procsflist nil = nil
			      | procsflist ((fty,memOp,liOp)::tail) =
				let val thisTy = case liOp
						   of SOME li => tcBitField (ctypeToTcType tidtab fty, li)
						    | NONE => ctypeToTcType tidtab fty
				    val match = case memOp
						  of SOME (mem' as {uid=mid',name,location,ctype,kind}) => Pid.equal (mid, mid')
						   | NONE => false
				in  if match then [thisTy]
					     else thisTy::(procsflist tail)
				end
			in  fsStruct (procsflist sflist) end
		| SOME{ntype=SOME(Bindings.Union (uid, uflist)),name,global,location} =>
			let fun procuflist nil = tcVoid (* error - not found*)
			      | procuflist ((fty,mem' as {uid=mid',name,location,ctype,kind})::tail) =
				if Pid.equal (mid,mid') then ctypeToTcType tidtab fty
							else procuflist tail
			in  fsUnion (procuflist uflist) end
		| _ => fsStruct []

      in case (TypeUtil.getCoreType tidtab ctype)
	   of Ast.Qual (_,ty) => getFieldSig tidtab (ty, mem, {deref=deref})
	    | Ast.Pointer (ty)  => if deref then getFieldSig tidtab (ty, mem, {deref=false})
				   else fsStruct [] (* error *)
	    | Ast.StructRef (tid) => getFieldSigTid (tid,mem)
	    | Ast.UnionRef (tid) => getFieldSigTid (tid,mem)
	    | _ => fsStruct [] (* error *)
      end
    )

  (************************** EXPRESSION TO ABSTRACT OBJECT *************************)
  (* deref=true : malloc() maps to aoMalloc, and
   *		  "strlit" maps to aoStringLit
   * deref=false : malloc() maps to aoAddrof aoMalloc, and
   *		  "strlit" maps to aoAddrof aoStringLit
   *)
  fun expToAbsObjects (expCty, ttab, {deref:bool}) expr =
  (
      let
	  fun isFunctionPointer ty =
	      case TypeUtil.deref ttab ty
		of SOME dty => TypeUtil.isNonPointerFunction ttab dty
		 | NONE => false

	  (********************************************************
	   * NOTE ON ARRAY HANDLING: (13jun03) for array int a[10]
	   *  - (aoId <a>) represents the array abstract location
	   *  - all occurrences of <a> in the program is converted
	   *	to (aoAddrOf aoId <a>). These are now handled by
	   *	converting all array-typed lvalues (id,sub,mem,arrow,deref)
	   *  - Problems:
	   *	- compatible with old handling of stuff? not sure
	   *	  of impact yet.
	   *	- for multi-dim arrays, we can now have aoAddrOf aoAddrOf.
	   *	  This *may* be ok, assuming we handle the fact that
	   *	  &&a --ptsTo--> a (not currently done).
	   *TODO: This needs further assessment!!
	   ********************************************************)

	  (** the main conversion function **)
	  fun eToAOs (exp as Ast.EXPR (coreExpr,aid,errloc)) =
	      if isZero exp
	      then [aoValue tcZero]
	      else (
	       case coreExpr
		 of Ast.IntConst li => [aoValue (ctypeToTcType ttab (expCty exp))]
		  | Ast.RealConst r => [aoValue (ctypeToTcType ttab (expCty exp))]
		  | Ast.StringConst s => [ if deref then aoStringLit aid
						    else aoAddrOf (aoStringLit aid)
					 ]
		  | Ast.Call (fexp,exps) =>
			if (isMalloc fexp) orelse (isAlloca fexp)
			then [ if deref then aoMalloc aid
			       else aoAddrOf (aoMalloc aid)
			     ]
			else (map (fn ao => aoReturn ao) (prepareCallExps (eToAOs fexp)))
		  | Ast.QuestionColon (e1,e2,e3) => (eToAOs e2) @ (eToAOs e3)
		  | Ast.Assign (e1,e2) => eToAOs e1
		  | Ast.Comma (e1,e2) =>
			if isVargDummy e1
			then [(!curvargAO)]
			else eToAOs e2
		  | Ast.Sub (e1,e2) =>
		    ( if TypeUtil.isArray ttab (expCty exp)
		      then map applyAddrOf
		      else (fn x => x)
		    ) ( if isZero e2
			then (* optimize: if e2 is zero, then treat as *e1 (or e1 if fnptr or array) *)
			     if isFunctionPointer (expCty e1)
			     then eToAOs e1
			     else (map applyStar (eToAOs e1))
			else ( if (TypeUtil.isArray ttab (expCty e2)) then nil (* if e2 is array, skip e1 *)
			       else map ( if isFunctionPointer (expCty e1)
					  then applyOp tcPointer
					  else applyStar o (applyOp tcPointer)
					) (eToAOs e1)
			     ) @ (
			       if (TypeUtil.isArray ttab (expCty e1)) then nil (* if e1 is array, skip e2 *)
			       else map ( if isFunctionPointer (expCty e2)
					  then applyOp tcPointer
					  else applyStar o (applyOp tcPointer)
					) (eToAOs e2)
			     )
		    )
		  | Ast.Member (e,mem) =>
		    ( if TypeUtil.isArray ttab (expCty exp)
		      then map applyAddrOf
		      else (fn x => x)
		    ) ( let val memsig = getFieldSig ttab (expCty e, mem, {deref=false})
			in (case memsig
			      of fsStruct tctylist =>
				 (map (fn ao => aoSDot (ao,tctylist)) (eToAOs e))
			       | fsUnion tcty =>
				 (map (fn ao => aoUDot (ao,tcty)) (eToAOs e))
			) end
		    )
		  | Ast.Arrow (e,mem) => 
		    ( if TypeUtil.isArray ttab (expCty exp)
		      then map applyAddrOf
		      else (fn x => x)
		    ) ( let val memsig = getFieldSig ttab (expCty e, mem, {deref=true})
			in (case memsig
			      of fsStruct tctylist =>
				 (map (fn ao => aoSDot (applyStar ao,tctylist)) (eToAOs e))
			       | fsUnion tcty =>
				 (map (fn ao => aoUDot (applyStar ao,tcty)) (eToAOs e))
			) end
		    )

		  (*
		     (FPTR) To handle function pointers:
		     1.	for *e, if e is a pointer-to-function, return AO(e)
		     2.	for &e, if &e is a pointer-to-function, return AO(e)
		     FMI, see comment below in genAssignEdges (search for FPTR).
		  *)
		  | Ast.Deref e =>
		    ( if TypeUtil.isArray ttab (expCty exp)
		      then map applyAddrOf
		      else (fn x => x)
		    ) ( if isFunctionPointer (expCty e)
			then eToAOs e
			else (map applyStar (eToAOs e))
		    )
		  | Ast.AddrOf e => 
			if isFunctionPointer (expCty exp)
			then eToAOs e
			else if TypeUtil.isArray ttab (expCty e)	(* HACK: to handle &a, which apparently equals a or &a[0]. *)
			then (map (fn aoAddrOf ao => aoAddrOf ao	(* NOTE: this will incorrectly(?) treat *&a as equal to *a. *)
				    | ao => applyAddrOf ao
				  ) (eToAOs e))
			else (map applyAddrOf (eToAOs e))

		  | Ast.Binop (binop,e1,e2) =>
			(map (applyOp (ctypeToTcType ttab (expCty exp))) (eToAOs e1))
			@ (map (applyOp (ctypeToTcType ttab (expCty exp))) (eToAOs e2))
		  | Ast.Unop (unop,e) => 
			(map (applyOp (ctypeToTcType ttab (expCty exp))) (eToAOs e))
		  | Ast.SizeOf ty => [aoValue tcInt]
		  | Ast.Cast (ctype,e) =>
			let datatype cast_kind = cast_cpy | cast_ext | cast_cvt | cast_array | cast_yarra
			    fun classifyCast (tgtty, srcty) =
				if srcty = tgtty
				then cast_cpy
				else case (tgtty,srcty)
				       of (tcShort,tcChar)		=> cast_ext
					| (tcInt,tcChar)		=> cast_ext
					| (tcLong,tcChar)		=> cast_ext
					| (tcLongLong,tcChar)		=> cast_ext
					| (tcPointer,tcChar)		=> cast_ext
					| (tcInt,tcShort)		=> cast_ext
					| (tcLong,tcShort)		=> cast_ext
					| (tcLongLong,tcShort)		=> cast_ext
					| (tcPointer,tcShort)		=> cast_ext
					| (tcLong,tcInt)	=> cast_cpy
					| (tcLongLong,tcInt)		=> cast_ext
					| (tcPointer,tcInt)	=> cast_cpy
					| (tcInt,tcLong)	=> cast_cpy
					| (tcLongLong,tcLong)	=> cast_cpy
					| (tcPointer,tcLong)		=> cast_ext
					| (tcInt,tcPointer)	=> cast_cpy
					| (tcLong,tcPointer)	=> cast_cpy
					| (tcLongLong,tcPointer)	=> cast_ext
					  (* treat array as pointer *)
					| (tcPointer,tcArray _)	=> cast_array
					| (tcArray _,tcPointer)	=> cast_yarra
					| _	=> cast_cvt
			    val srcty = ctypeToTcType ttab (expCty e)
			    val tgtty = ctypeToTcType ttab ctype
			in
			   case classifyCast (tgtty, srcty)
			     of cast_cpy   => eToAOs e
			      | cast_ext   => (map (applyExt (tgtty, srcty)) (eToAOs e))
			      | cast_cvt   => (map (applyOp tgtty) (eToAOs e))
		(*	      | cast_array => (map applyAddrOf (eToAOs e))	*) (* old hack: no longer needed!?  -14jun03 *)
			      | cast_array => (eToAOs e)
			      | cast_yarra => ( warningLoc errloc "Yarra case encountered!\n"
						; eToAOs e)
			end
		  | Ast.Id (id as {uid=pid,ctype=cty,...}) =>
		    ( if TypeUtil.isArray ttab (expCty exp)
		      then [aoAddrOf (aoId pid)]
		      else [aoId pid]
		    )
		  | Ast.EnumId (pid,li) => [aoValue tcInt]
		  | Ast.ExprExt ee => []
		  | Ast.ErrorExpr => []
	  )

      in Rtc.rmdup aoEq (eToAOs expr) end
  ) (* end fun expToAbsObjects (expCty, ttab, {deref:bool}) expr *)

  (************************** APPLY func TO EACH EXPRESSION *************************)
  fun applyToExp func ({ast=edecls,...} : ParseToAst.astBundle) =
  (let
    fun processExprs nil = ()
      | processExprs (head::tail) = (processExpr head ;  processExprs tail)

    and processExpr (exp as Ast.EXPR (coreExpr,_,_)) =
	(case coreExpr
	    of Ast.IntConst li => ()
	     | Ast.RealConst r => ()
	     | Ast.StringConst s => ()
	     | Ast.Call (fexp,exps) => ( processExpr fexp ; processExprs exps )
	     | Ast.QuestionColon (e1,e2,e3) => (processExpr e1; processExpr e2; processExpr e3)
	     | Ast.Assign (e1,e2) => ( processExpr e1 ; processExpr e2 )
	     | Ast.Comma (e1,e2) => (processExpr e1 ; processExpr e2)
	     | Ast.Sub (e1,e2) => (processExpr e1 ; processExpr e2)
	     | Ast.Member (exp,mem) => processExpr exp
	     | Ast.Arrow (exp,mem) => processExpr exp
	     | Ast.Deref exp => processExpr exp
	     | Ast.AddrOf exp => processExpr exp
	     | Ast.Binop (binop,e1,e2) => (processExpr e1 ; processExpr e2 )
	     | Ast.Unop (unop,exp) => processExpr exp
	     | Ast.SizeOf ty => ()
	     | Ast.Cast (ctype,exp) => processExpr exp
	     | Ast.Id id => ()
	     | Ast.EnumId (pid,li) => ()
	     | Ast.ExprExt ee => ()
	     | Ast.ErrorExpr => ()
	) before (func exp)

    fun processInitExprs nil = ()
      | processInitExprs (head::tail) =  ( processInitExpr head ; processInitExprs tail )

    and processInitExpr iexpr =
	(case iexpr of Ast.Simple exp => processExpr exp
		     | Ast.Aggregate iexps => processInitExprs iexps
	)

    fun processDecls nil = ()
      | processDecls (head::tail) = ( processDecl head ; processDecls tail )

    and processDecl decl =
	( case decl
	   of Ast.TypeDecl _ => ()
	    | Ast.VarDecl (id, initExprOp) => ( case initExprOp
						  of SOME iexp => processInitExpr iexp
						   | NONE => ()
						)
	)

    fun processStmts nil = ()
      | processStmts (head::tail) = (processStmt head ; processStmts tail)

    and processStmt (Ast.STMT(coreStmt,_,_)) =
     (case coreStmt
	of Ast.Expr expOp => ( case expOp
				 of SOME exp => processExpr exp
				  | NONE => ()
				)
	 | Ast.Compound (decls, stmts) => (processDecls decls ; processStmts stmts)
	 | Ast.While (exp,stmt) => (processExpr exp ; processStmt stmt)
	 | Ast.Do (exp,stmt) => (processExpr exp ; processStmt stmt)
	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val _ = case e1op  of SOME exp1 => processExpr exp1 | NONE => ()
		    val _ = case e2op  of SOME exp2 => processExpr exp2 | NONE => ()
		    val _ = case e3op  of SOME exp3 => processExpr exp3 | NONE => ()
		in  processStmt stmt end 
	 | Ast.Labeled (label,stmt) => processStmt stmt
	 | Ast.CaseLabel (li,exp,stmt) => processStmt stmt
	 | Ast.DefaultLabel stmt => processStmt stmt
	 | Ast.Goto label => ()
	 | Ast.Break => ()
	 | Ast.Continue => ()
	 | Ast.Return expOp => (case expOp
				  of SOME exp => processExpr exp
				   | NONE => ()
				)
	 | Ast.IfThen (exp,stmt) => (processExpr exp ; processStmt stmt)
	 | Ast.IfThenElse (exp,stmt1,stmt2) =>
			(processExpr exp ; processStmt stmt1 ;  processStmt stmt2)
	 | Ast.Switch (exp,stmt) => (processExpr exp ; processStmt stmt)
	 | Ast.StatExt se => ()
	 | Ast.ErrorStmt => ()
     )

    fun processExternalDecl (edecl as Ast.DECL(coreEdecl,_,_)) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => ()
	    | Ast.ExternalDecl (Ast.VarDecl (id,initExprOp)) =>
		(case initExprOp
		   of SOME iexp => processInitExpr iexp
		    | NONE => ()
		)
	    | Ast.FunctionDef (id,ids,stmt) => processStmt stmt
	    | Ast.ExternalDeclExt _ => ()
	)

    fun processExternalDecls nil = ()
      | processExternalDecls (ed::edecls) =
		(processExternalDecl ed ; processExternalDecls edecls)

  in  processExternalDecls edecls  end
  ) (* end fun applyToExp func bundle *)

  (************************** OUTPUT AID/PID ALIASES *************************)
  fun outputAidPidAliases (ahash, phash, os) =
      let val _ = HashTable.appi (fn (aid,(sc,desc,commOp)) =>
				TextIO.output (os, "@ " ^ (scToString sc) ^ " " ^ (Int.toString aid) ^ " " ^ desc ^ "\n"
						   ^ (case commOp
							of SOME (str,loc) =>
								"# " ^ desc ^ " = ["
								^ (SourceMap.locToString loc) ^ "]:\""
								^ (String.toCString str) ^ "\"\n"
							 | NONE => "")
						)
			    ) ahash
	  val _ = HashTable.appi (fn (pid,(sc,desc)) =>
				TextIO.output (os, "% " ^ (scToString sc) ^ " " ^ (Int.toString pid) ^ " " ^ desc ^ "\n")
			    ) phash
      in () end

  (************************** ARRAY TO PTR AOs (HELPER) *************************)
  fun arrayToPtrAO ao =
      (case ao of aoValue (tcArray _) => aoValue tcPointer
		| aoOp (tcArray _, ao) => aoOp (tcPointer, ao)
		| aoExt (tcArray _, fty, ao) => aoExt (tcPointer, fty, ao)
		| aoAddrOf ao => (* HACK: this is truly a hack, to handle array special case *)
		  aoAddrOf (case ao of aoValue (tcArray (elty,_)) => aoValue elty
				     | aoOp (tcArray (elty,_), ao) => aoOp (elty, ao)
				     | aoExt (tcArray (elty,_), fty, ao) => aoExt (elty, fty, ao)
				     | _ => ao
			    )
		| _ => ao
	)
  val arrayToPtrAOs = List.map arrayToPtrAO

  (************************** HANDLE INITIALIZER (HELPER) *************************)
  (* function initializes expressions in initExprOp:
	- if initExpr is NONE, then default initialize scalars to VALUE_zero
	- otherwise, initialize to expression, including
	  - string literals
	  - address-of expressions
       Anyhow, this in fact results in our being able to handle the GNU extension
       that allows non-constant expressions in initializer lists.
  *)
  fun handleInitializer (tidtab,expToCty) doAssign (aobuilder) (cty, initExprOp) =
     (let
	  val expToAOs = arrayToPtrAOs o (expToAbsObjects (expToCty,tidtab,{deref=false}))

	  (***********************************************)
	  (* process a single initialization expression  *)
	  (***********************************************)
	  fun handleItem (aobuilder, ty, initexprOp) =
	      (case initexprOp
		 of SOME (Ast.Aggregate initexprs) =>
			(case TypeUtil.getCoreType tidtab ty
			   of Ast.Array (sizeop, ety) =>
				  handleArray (aobuilder, ety, 0,
					case sizeop of SOME (li,_) => li | NONE => 0,
					initexprs)
			    | Ast.StructRef tid =>
				(case Tidtab.find (tidtab,tid)
				   of SOME {ntype=SOME(Bindings.Struct (_, fields)),name,global,location} =>
					handleStructFields (aobuilder,nil,fields,initexprs)
				    | _ => nil (* error! *)
				)
			    | Ast.UnionRef tid =>
				(case Tidtab.find (tidtab,tid)
				   of SOME {ntype=SOME(Bindings.Union (_, fields)),name,global,location} =>
					handleUnionFields (aobuilder,fields,initexprs)
				     | _ => nil (* error! *)
				)
			    | _ => nil
			)
		  | SOME (Ast.Simple (exp as Ast.EXPR (cexp,aid,_))) =>
			let val expaos = Rtc.rmdup aoEq (expToAOs exp)	(* add makeCastExplicit? *)
			in  doAssign (aobuilder(), expaos, ctypeToTcType tidtab ty, exp)
			end
		  | NONE =>
			(case TypeUtil.getCoreType tidtab ty
			   of Ast.Array (sizeop, ety) =>
				handleArray (aobuilder, ety, 0,
						case sizeop of SOME (li,_) => li | NONE => 0,
						nil)

			    | Ast.StructRef tid =>
				(case Tidtab.find (tidtab,tid)
				   of SOME {ntype=SOME(Bindings.Struct (_, fields)),name,global,location} =>
					handleStructFields (aobuilder,nil,fields,nil)
				    | _ => nil (* error! *)
				)
			    | Ast.UnionRef tid =>
				(case Tidtab.find (tidtab,tid)
				   of SOME {ntype=SOME(Bindings.Union (_, fields)),name,global,location} =>
					handleUnionFields (aobuilder,fields,nil)
				     | _ => nil (* error! *)
				)
			    | _ => doAssign (aobuilder (), [aoValue tcZero], ctypeToTcType tidtab ty,
							   Ast.EXPR (Ast.IntConst 0, 0, SourceMap.UNKNOWN))
			)

	      )

	  (********************)
	  (* process an array *)
	  (********************)
	  and handleArray (aobuilder, ety, index, size, ielist) =
	     (case ielist
		of (iehead::ietail) =>
			let val hlist = handleItem (aobuilder, ety, SOME iehead)
			    val tlist = handleArray (aobuilder, ety, index + 1, size, ietail)
			in  hlist @ tlist  end
		 | nil => (* for current scheme, sufficient to summarize in one assignment;
			     in general, must repeat following for subsequent elements, up to size *)
			if index < size
			then handleItem (aobuilder, ety, NONE)
			else nil
	     )

	  (***********************)
	  (* process a structure *)
	  (***********************)
	  and handleStructFields (aobuilder, prev_sig, (fty,memop,liOp)::ftail,ielist) =
		let val this_sig = prev_sig @ [ let val tcty = ctypeToTcType tidtab fty
						in  case liOp  of SOME sz => tcBitField (tcty, sz)
								| NONE => tcty
						end ]
		    fun mem_builder () = aoSDot (aobuilder (), this_sig)
		    val (thislist, ielist') =
			if not (isSome memop) (*skip unnamed fields*)
			then (nil, ielist)
			else case ielist
			       of (iehead::ietail) => (handleItem (mem_builder,fty,SOME iehead), ietail)
				| nil => (handleItem (mem_builder,fty,NONE), nil)
		in
		    thislist @ (handleStructFields (aobuilder,this_sig,ftail,ielist'))
		end
	    | handleStructFields (_,_,_,_) = nil

	  (*******************)
	  (* process a union *)
	  (*******************)
	  and handleUnionFields (aobuilder,(fty,_)::_,ielist) =
		let fun mem_builder () = aoUDot (aobuilder (), ctypeToTcType tidtab fty)
		    val ieOp = case ielist of iehead::_ => SOME iehead
					    | nil => NONE
		in  handleItem (mem_builder,fty,ieOp)
		end
	    | handleUnionFields (_,_,_) = nil
      in
	  handleItem (aobuilder, cty, initExprOp)
      end
    ) (* end fun handleInitializer (tidtab,expToCty) doAssign (aobuilder) (cty, initExprOp) *)

  (************************** OUTPUT ASSIGNMENT GRAPH ***************************)
  (* Writes several things:							*)
  (* 1. assignment edges - two kinds: true assign and peusdo assign,		*)
  (*	the latter for function and array object assignment and void return.	*)
  (* 2. static type - for each "important" ao (?)				*)
  (* 3. verifyTag - translates to "setRequiredType" in rtca engine, used for	*)
  (*	type-safety-level analysis (rtc mode only).				*)
  (* 4. verifyPtr - translates to "setVerifyPtrType" in rtca engine,		*)
  (*	CURRENTLY NOT USED FOR ANYTHING.					*)
  (* TODO: processExpr needs both "enforce" and "verify_ptr" flags, to mirror	*)
  (*	   instr[LR]valueExpr. (Current version does not output "verifyPtr"	*)
  (*	   entirely correctly, mainly for Ast.Member case, I think).		*)
  fun genAssignEdges (bundle as {ast as edecls,
				 tidtab,
				 errorCount,
				 warningCount,
				 auxiliaryInfo as {aidtab, implicits, env as symtab}},
			os) =
  (let
    (* ---- next, prepare to write assignment edges ---- *)
    val curfuncAO = ref (aoId 0) (* lazy global approach, instead of passing argument through tree*)
    val curfuncRetTy = ref (Ast.Void) (* retty: needed because return type implicit cast not recorded by ckit *)

    val pseudoAssign = "- "	(* for function and array object assignments, as well as void returns *)
    val trueAssign = "= "	(* for "true" assignments, including function argument, return mimic *)

    fun writeAssignEdges assignType (tgt, srclist, tcty) =
	let val _ = TextIO.output (os, ": " ^ (tcTypeToString tcty) ^ (aoToString tgt) ^ "\n")
	    fun writeSrcLines nil = ()
	      | writeSrcLines (head::tail) =
		let val _ = TextIO.output (os, assignType ^ (aoToString head) ^ "\n")
		in  writeSrcLines tail end
	in  writeSrcLines srclist  end

    val staticType = "+ "
    val verifyTag  = "} "
    val verifyPtr  = "] "

    fun writeTypeObj prefix (aolist, tcty) =
	let fun writeAOLines nil = ()
	      | writeAOLines (head::tail) =
		let val _ = TextIO.output (os, prefix ^ (tcTypeToString tcty) ^ (aoToString head) ^ "\n")
		in  writeAOLines tail end
	in  writeAOLines aolist  end

    fun arrayToPtr tcty =
	case tcty of tcArray _ => tcPointer
		   | _ => tcty

    fun writeFnArgRetStaticTypes (fao,ftcty) =
	case ftcty
	  of tcFunction (rty, atys) =>
	     let (* static type for return node: even if void! *)
		 val _ = writeTypeObj staticType ([aoReturn fao], rty)

		 (* static type for arg nodes *)
		 fun processArgs (_,nil) = ()
		   | processArgs (cnt, aty::tail) = (* array arguments, convert to pointer *)
			let val _ = writeTypeObj staticType ([aoArg (fao,cnt)], arrayToPtr aty)
			in  processArgs (cnt+1, tail)  end

		 val _ = processArgs (1,atys)
	     in  ()  end
	   | _ => ()

    fun expToCty (Ast.EXPR(_, aid, errloc)) =
	case Aidtab.find (aidtab,aid)
	  of SOME ct => ct
	   | NONE => (warningLoc errloc ("genAssignEdges:expToCty: no type for expression");
			Ast.Void)

    (* note: this version reuses the AID, if want to export to global, must change to use Aid.new() *)
    fun makeCastExplicitCty (cast_type, exp as Ast.EXPR(_,aid,errloc)) =
	if TypeUtil.equalType tidtab (expToCty exp, cast_type)
	then exp
	else Ast.EXPR(Ast.Cast (cast_type, exp),aid,errloc) (* warning: recycling aid *)

    fun makeCastExplicit (exp as Ast.EXPR(_,aid,_)) =
	case Aidtab.find (implicits,aid)
	  of SOME cast_type => makeCastExplicitCty (cast_type, exp)
         | NONE => exp

    (* deref=false means convert each strlit and malloc object into &strlit, &malloc *)
    val expToAOs = expToAbsObjects (expToCty,tidtab,{deref=false})

    (* for ao of type tcty, find and return arrays nested within structs or unions *)
    fun findStructNestedArrays (ao,tcty) =
	case tcty of tcArray (elty,siz) => [ao]
		   | tcStruct (tylist) =>
		     let fun procStruct (head,nil) = nil
			   | procStruct (head,curty::tail) =
			     (findStructNestedArrays (aoSDot (ao,head@[curty]), curty))
			     @ (procStruct (head@[curty], tail))
		     in  procStruct (nil, tylist)  end
	           | tcUnion (tylist) =>
		     List.foldr (fn (ty,aol) =>
				    (findStructNestedArrays (aoUDot (ao,ty), ty)) @ aol
				 ) nil tylist
		   | _ => nil

    val implicit_pids = ref nil

    fun processExprs nil = ()
      | processExprs (head::tail) = (processExpr (head,{enforce=false}) ;  processExprs tail)

    and processExpr (oex as Ast.EXPR (coreExpr,aid,_), {enforce:bool}) =
	(case coreExpr
	    of Ast.IntConst li => ()
	     | Ast.RealConst r => ()
	     | Ast.StringConst s => writeTypeObj staticType ([aoStringLit aid],
						tcArray(tcChar, LargeInt.fromInt ((String.size s)+1)))

	     | Ast.Call (fexp as Ast.EXPR (fcexp,_,_),exps) =>
			let val _ = processExpr (fexp,{enforce=true})
			    val _ = processExprs exps

			in  if (isMalloc fexp) orelse (isAlloca fexp)
			    then (* "instantiate" nodes in exps *)
(* Note: RTC currently doesn't verifyTag on malloc's args! Need to change RTC *)
				 let val malloc_ty = case evalMallocSize (evalConst (ctypeToTcType tidtab)) (fn _ => 0) (fexp,exps)
						       of SOME (tcty, size) => if size = 1 then tcty
											   else tcArray(tcty,size)
							| NONE => tcVoid
				     val _ = writeTypeObj staticType ([aoMalloc aid], malloc_ty)

				     fun instantiateAOs exp =
					 let val aos = expToAOs exp
					     val tcty = (ctypeToTcType tidtab (expToCty exp))
					 in  List.app (fn ao => writeTypeObj verifyTag ([ao],tcty)) aos  end
				 in  List.app instantiateAOs exps
				 end
			    else let
				     (* Must "declare" undeclared functions: *)
				     (* HACK: include printf and fprintf, which are complicated by pctn pre-processing *)
				     val _ = case fcexp
					       of Ast.Id {uid=pid,name,status,...} =>
						  let val fnname = Symbol.name name
						  in  if status = Ast.IMPLICIT
						      orelse fnname = "printf" orelse fnname = "fprintf"
						      then (* insert f = &Function(f) *)
							   if not (List.exists (fn uid => uid = pid) (!implicit_pids))
							   then ( writeAssignEdges pseudoAssign
										   (aoId pid, [aoAddrOf (aoFunction (aoId pid))], tcPointer)
								; implicit_pids := (pid::(!implicit_pids)))
							   else ()
						      else ()
						  end
						| _ => ()


				     val fe_aos = prepareCallExps (expToAOs fexp)
				     fun processActuals (_,nil) = ()
				       | processActuals (cnt,(exp::tail)) =
					 let val exp' = makeCastExplicit exp

					     (* HACK: ckit does not currently implicitly cast arrays into
					        pointers, so do it ourselves
						NOTE: ckit will convert formal arrays into pointers, but
						here, in the argument type, arrays remain as arrays *)
					     val (exp', exp'_type) = case (TypeUtil.getCoreType tidtab (expToCty exp'))
								       of Ast.Array (_,elty) =>
									  (makeCastExplicitCty (Ast.Pointer elty, exp), Ast.Pointer elty)
									| cty => (exp', cty)

					     val exp'_aos = arrayToPtrAOs (expToAOs exp')
					     val exp'_tcty = ctypeToTcType tidtab exp'_type

					     val _ = writeTypeObj staticType (List.map (fn fao => aoArg (fao,cnt)) fe_aos, exp'_tcty)
					     val _ = List.app (fn fao => writeAssignEdges trueAssign
									 (aoArg (fao,cnt),exp'_aos,exp'_tcty)) fe_aos
					 in  processActuals (cnt+1,tail) end
				     val _ = processActuals (1,exps)

				     val rtcty = ctypeToTcType tidtab (expToCty oex)
				     val _ = writeTypeObj staticType (List.map (fn fao => aoReturn fao) fe_aos, rtcty)
				     val _ = if enforce
					     then writeTypeObj verifyTag (List.map (fn fao => aoReturn fao) fe_aos, rtcty)
					     else ()
				 in  () end
				 end

	     | Ast.QuestionColon (e1,e2,e3) =>
			let val _ = processExpr (e1,{enforce=true})
			    val _ = processExpr (e2,{enforce=enforce})
			    val _ = processExpr (e3,{enforce=enforce})
			in () end

	     | Ast.Assign (e1,e2) =>
			let val _ = processExpr (e1,{enforce=false})
			    val _ = processExpr (e2,{enforce=enforce})

			    val e1aos = expToAOs e1
			    val e1tcty = (ctypeToTcType tidtab (expToCty e1))
			    val e2aos = arrayToPtrAOs (expToAOs (makeCastExplicit e2))
			in  List.app (fn ao1 => writeAssignEdges trueAssign (ao1,e2aos,e1tcty)) e1aos
			end

	     | Ast.Comma (e1,e2) => if isVargDummy e1 then ()
				    else (processExpr (e1,{enforce=false}); processExpr (e2,{enforce=enforce}))

	     | Ast.Sub (e1,e2) =>
		let val _ = processExpr (e1,{enforce=true})
		    val _ = processExpr (e2,{enforce=true})
		    val oextcty = ctypeToTcType tidtab (expToCty oex)
		in case oextcty of tcArray _ => ()	(* no verifyTag/Ptr if array *)
				 | _ => if enforce then writeTypeObj verifyTag (expToAOs oex, oextcty)
						   else writeTypeObj verifyPtr (expToAOs oex, oextcty)
		end

	     | Ast.Member (exp,mem) =>
		let val _ = processExpr (exp,{ enforce = not (isLvalExpr exp) })	(* only enforce exp if not lval (e.g. call, ?:) *)
		    val oextcty = ctypeToTcType tidtab (expToCty oex)
		in  case oextcty of tcArray _ => ()	(* no verifyTag if Array *)
				  | _ => if enforce then writeTypeObj verifyTag (expToAOs oex, oextcty)
						    else if (isDerefLvalExpr exp)
							 then writeTypeObj verifyPtr (expToAOs oex, oextcty)
							 else ()
		end

	     | Ast.Arrow (exp,mem) =>
		let val _ = processExpr (exp,{enforce=true})
		    val oextcty = ctypeToTcType tidtab (expToCty oex)
		in case oextcty of tcArray _ => ()	(* no verifyTag/Ptr if array *)
				 | _ =>  if enforce then writeTypeObj verifyTag (expToAOs oex, oextcty)
						    else writeTypeObj verifyPtr (expToAOs oex, oextcty)
		end

	     | Ast.Deref exp =>
		let val _ = processExpr (exp,{enforce=true})
		    val oextcty = ctypeToTcType tidtab (expToCty oex)
		in case oextcty of tcArray _ => ()	(* no verifyTag/Ptr if array *)
				 | _ => if enforce then writeTypeObj verifyTag (expToAOs oex, oextcty)
						   else writeTypeObj verifyPtr (expToAOs oex, oextcty)
		end

	     | Ast.AddrOf exp => processExpr (exp,{enforce=false})

	     | Ast.Binop (binop,e1,e2) =>
			(processExpr (e1,{enforce=true}) ;
			 processExpr (e2,{enforce=true}) ;
			 if (isAssignBinop binop)
			 then let val e1aos = expToAOs e1
				  val e1tcty = (ctypeToTcType tidtab (expToCty e1))
				  val oex_aos = expToAOs oex
			      in  List.app (fn ao1 => writeAssignEdges trueAssign (ao1,oex_aos,e1tcty)) e1aos
			      end
			 else ()
			)
	     | Ast.Unop (unop,exp) =>
			(processExpr (exp,{enforce=true}) ;
			 if (isAssignUnop unop)
			 then let val exp_aos = expToAOs exp
				  val exp_tcty = (ctypeToTcType tidtab (expToCty exp))
				  val oex_aos = expToAOs oex
			      in  List.app (fn ao1 => writeAssignEdges trueAssign (ao1,oex_aos,exp_tcty)) exp_aos
			      end
			 else ()
			)
	     | Ast.SizeOf ty => ()
	     | Ast.Cast (ctype,exp) => processExpr (exp,{enforce=true}) (* SY: enforce=true is safe approx *)
	     | Ast.Id (id as {ctype=idty,...}) =>
			if enforce
			andalso not (TypeUtil.isNonPointerFunction tidtab idty)
			andalso not (TypeUtil.isArray tidtab idty)
			then let val oextcty = ctypeToTcType tidtab (expToCty oex)
			     in  writeTypeObj verifyTag (expToAOs oex, oextcty)
			     end
			else ()  (* no verifyTag if non-ptr function or array *)

	     | Ast.EnumId (pid,li) => ()
	     | Ast.ExprExt ee => ()
	     | Ast.ErrorExpr => ()
	)

    fun processInitializerAssigns pid (cty, initExprOp) =
	let fun compareTriple ((dao1,saos1,tcty1),(dao2,saos2,tcty2)) =
		(dao1 = dao2) andalso (saos1 = saos2)
	    fun writeStringTyAndCollectTriple (dest_ao,src_aos,tcty, Ast.EXPR (coreExpr,aid,_)) =
		(let val _ = (case coreExpr
				of Ast.StringConst s =>
				   writeTypeObj staticType
						( [aoStringLit aid]
						, tcArray(tcChar, LargeInt.fromInt ((String.size s)+1)))
				 | _ => () )
		 in [(dest_ao,src_aos,tcty)] end
		)
	in  List.app (writeAssignEdges trueAssign)
		     (Rtc.rmdup compareTriple
				(handleInitializer (tidtab, expToCty)
						   writeStringTyAndCollectTriple
						   (fn () => aoId pid)
						   (cty, initExprOp)
				)
		     )
	end

    fun processDecls nil = ()
      | processDecls (head::tail) = (processDecl head ; processDecls tail)

    and processDecl decl =
	( case decl
	   of Ast.TypeDecl _ => ()
	    | Ast.VarDecl (id as {uid=pid,ctype=cty,...}, initExprOp) =>
		let (* write array and function pseudo-object assignments *)
		    val _ = if (TypeUtil.isNonPointerFunction tidtab cty)
			    then (* insert f = &Function(f) *)
				 writeAssignEdges pseudoAssign (aoId pid, [aoAddrOf (aoFunction (aoId pid))], tcPointer)
			    else ()
		    (* write static-types *)
		    val _ = if (isExtern id)
			    andalso not (TypeUtil.hasKnownStorageSize tidtab cty)
			    then ()	(* incomplete extern type: skip *)
			    else if (TypeUtil.isNonPointerFunction tidtab cty)
			    then (* write static type for "f", "Function(f)", and arg/ret *)
				 let val ftcty = ctypeToTcType tidtab cty (* note: may trigger TID error if
									     args includes incomplete type *)
				     val _ = writeTypeObj staticType ([aoFunction (aoId pid)], ftcty)
				     val _ = writeTypeObj staticType ([aoId pid], tcPointer)
				     val _ = writeFnArgRetStaticTypes(aoFunction (aoId pid), ftcty)
				 in  ()  end
			    else writeTypeObj staticType ([aoId pid], ctypeToTcType tidtab cty)
		    val initExprOp = simplifyIfScalar tidtab (cty, initExprOp)
		in(case initExprOp
		     of SOME (Ast.Simple exp) =>
			(processExpr (exp,{enforce=false}) ;
			 let               (* HACK, because cast not included in implicit *)
			     val exp_aos = arrayToPtrAOs (expToAOs (makeCastExplicitCty (cty,exp)))
			     val id_ao = aoId pid
			 in  writeAssignEdges trueAssign (id_ao, exp_aos, ctypeToTcType tidtab cty)
			 end
			)
		      | SOME (Ast.Aggregate iexps) =>
			processInitializerAssigns pid (cty, initExprOp)
		      | NONE =>
			 if isStatic id
			 then processInitializerAssigns pid (cty, NONE)
			 else ()
		)end
	)

    fun processStmts nil = ()
      | processStmts (head::tail) = (processStmt head ; processStmts tail)

    and processStmt (Ast.STMT(coreStmt,_,_)) =
     (case coreStmt
	of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => processExpr (exp,{enforce=false})
		 | NONE => ()
	    )

	 | Ast.Compound (decls, stmts) => (processDecls decls ; processStmts stmts)

	 | Ast.While (exp,stmt) =>
		let val _ = processExpr (exp,{enforce=true})
		    val _ = processStmt stmt
		in () end

	 | Ast.Do (exp,stmt) =>
		let val _ = processStmt stmt
		    val _ = processExpr (exp,{enforce=true})
		in () end

	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val _ =	case e1op  of SOME exp1 => processExpr (exp1,{enforce=false})
					    | NONE => ()
		    val e2aos =	case e2op  of SOME exp2 => let val _ = processExpr (exp2,{enforce=true})
							   in  expToAOs exp2  end
					    | NONE => nil
		    val _ =	case e3op  of SOME exp3 => processExpr (exp3,{enforce=false})
					    | NONE => ()
		    val _ = processStmt stmt
		in () end

	 | Ast.Labeled (label,stmt) => processStmt stmt
	 | Ast.CaseLabel (li,exp,stmt) => processStmt stmt
	 | Ast.DefaultLabel stmt => processStmt stmt
	 | Ast.Goto label => ()
	 | Ast.Break => ()
	 | Ast.Continue => ()
	 | Ast.Return expOp =>
		(case expOp
		   of SOME exp =>
			( processExpr (exp,{enforce=false}) ;
			  let (* make cast explicit: ckit doesn't have entry
				 in implicits table for return statement. *)
			      val exp_aos = expToAOs (makeCastExplicitCty (!curfuncRetTy,exp))

			  in  writeAssignEdges trueAssign (aoReturn (!curfuncAO), exp_aos,
							   arrayToPtr (ctypeToTcType tidtab (expToCty exp)))
			  end
			 )
		    | NONE => writeAssignEdges pseudoAssign (aoReturn (!curfuncAO), [aoValue tcVoid], tcVoid)
		)
	 | Ast.IfThen (exp,stmt) =>
		let val _ = processExpr (exp,{enforce=true})
		    val _ = processStmt stmt
		in () end		

	 | Ast.IfThenElse (exp,stmt1,stmt2) =>
		let val _ = processExpr (exp,{enforce=true})
		    val _ = processStmt stmt1
		    val _ = processStmt stmt2
		in () end		

	 | Ast.Switch (exp,stmt) =>
		let val _ = processExpr (exp,{enforce=true})
		    val _ = processStmt stmt
		in () end		

	 | Ast.StatExt se => ()
	 | Ast.ErrorStmt => ()
     )

    fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => ()
	    | Ast.ExternalDecl (Ast.VarDecl (id as {uid=pid,ctype=cty,...},initExprOp)) =>
		let (* write array and function pseudo-object assignments *)
		    val _ = if (TypeUtil.isNonPointerFunction tidtab cty)
			    then (* insert f = &Function(f) *)
				 writeAssignEdges pseudoAssign (aoId pid, [aoAddrOf (aoFunction (aoId pid))], tcPointer)
			    else ()
		    (* write static-types *)
		    val _ = if (isExtern id)
			    andalso not (TypeUtil.hasKnownStorageSize tidtab cty)
			    then ()	(* incomplete extern type: skip *)
			    else if (TypeUtil.isNonPointerFunction tidtab cty)
			    then (* write static type for "f", "Function(f)", and arg/ret *)
				 let val ftcty = ctypeToTcType tidtab cty (* note: may trigger TID error if
									     args includes incomplete type *)
				     val _ = writeTypeObj staticType ([aoFunction (aoId pid)], ftcty)
				     val _ = writeTypeObj staticType ([aoId pid], tcPointer)
				     val _ = writeFnArgRetStaticTypes(aoFunction (aoId pid), ftcty)
				 in  ()  end
			    else writeTypeObj staticType ([aoId pid], ctypeToTcType tidtab cty)
		    val initExprOp = simplifyIfScalar tidtab (cty, initExprOp)
		in
		    if (TypeUtil.isNonPointerFunction tidtab cty)	(* skip function decls  *)
		    then ()
		    else (* - assume zero-initialization for globals is always well-typed;
			      including externs! *)
			  processInitializerAssigns pid (cty, initExprOp)
		end

	    | Ast.FunctionDef (id as {uid=pid,ctype=fty,...},ids,stmt) =>
		let

		    (*
			(FPTR) To handle the whole function/function-pointer mess:
			We treat the function ID as a pointer to a function object
			(like how we treat arrays).
			This involves the following:
			1. at the function def (here), add an assignment f = & aoFunction(f)
			2. function formals and returns are attached to aoFunction(f)
			3. at callsites, attach actuals and returns to *f (see prepareCallExps)

			Next, to fix the syntactic headache, we must normalize functions
			and fn-ptrs in the call context.
			Ckit types each function id expression as a function
			only in the call context; in all other contexts the
			expression is typed pointer-to-function.
			
			We make these changes in expToAbsObjects:
			1. for *e, if e is a pointer-to-function, return AO(e)
			   1a. for e1[e2], if e1 is a pointer-to-function, return AO(e1)
			   1b.             if e2 is a pointer-to-function, return AO(e2)
			2. for e+f, if e is a pointer-to-function, return AO(e)
			3. for &e, if &e is a pointer-to-function, return AO(e)
		    *)

		    val cfAO = aoFunction (aoId pid)	(* to be used by return statement *)
		    val _ = (curfuncAO := cfAO)
		    val _ = (curfuncRetTy := (case fty
						of Ast.Function (rty, _) => rty
						 | _ => ( warning "genAssignEdges:FunctionDef: non-function ctype encountered\n"
							; Ast.Void )
			    		     ))
		    val _ = writeAssignEdges pseudoAssign (aoId pid, [aoAddrOf cfAO], tcPointer)

		    (* write static type for function/function id *)
		    val ftcty = ctypeToTcType tidtab fty
		    val _ = writeTypeObj staticType ([aoFunction (aoId pid)], ftcty)
		    val _ = writeTypeObj staticType ([aoId pid], tcPointer)

		    (*NOTE: we could've used writeFnArgRetStaticTypes to output static types
			    for args/ret, but for old-style function parameter declarations,
			    CKIT does not include the argument types in the function type,
			    so we would miss some cases. *)
		 (* val _ = writeFnArgRetStaticTypes(cfAO, ftcty) *)

		    (* write static type for return node *)
		    val _ = case ftcty
			      of tcFunction (rty, _) =>
				 writeTypeObj staticType ([aoReturn cfAO], rty)
			       | _ => () (* ERROR! *)

		    (* write static type for argument nodes *)
		    fun processFormals (cnt,nil) = (cnt,false)
		      | processFormals (cnt, (id as {uid=pid,ctype=cty,...} : Ast.id)::tail) =
			(case cty
			   of Ast.Ellipses => (cnt,true)
			    | _ => let (* Convert array type to pointer type
					  -- even though it seems this has already been normalized out! *)
				       val tcty = arrayToPtr (ctypeToTcType tidtab cty)
				       val _ = writeAssignEdges trueAssign (aoId pid, [aoArg (cfAO,cnt)], tcty)
				       (* write static type for both formal and arg *)
				       val _ = writeTypeObj staticType ([aoArg (cfAO,cnt),aoId pid], tcty)

				   in  processFormals (cnt+1, tail)  end
			)

		    val (fcnt,is_varg) = processFormals (1,ids)
		    val _ = TextIO.output (os, (if is_varg then "v " else "f ")
						   ^ (LargeInt.toString fcnt) ^ " " ^ (aoToString cfAO) ^ "\n")
		    val _ = assignCurVargAO (id, ids)

		    val _ = processStmt stmt

		    (* If there is a path to the function exit node with no "return" statement,
		       effect an empty "return" statement *)

		    (* NOTE: currently approximates syntactically.
			     It assumes switch statements can fall
			     through without returning, and bypasses
			     GOTOs; for now we use an additional
			     function to check if there are GOTOs.
			     If so, we balk. *)
		    fun findReturnInStmt (Ast.STMT(coreStmt,_,_)) =
		     (case coreStmt
			of Ast.Expr expOp => false
			 | Ast.Compound (decls, stmts) =>
				let fun findReturnInStmts nil = false
				      | findReturnInStmts (head::tail) =
					(findReturnInStmt head) orelse (findReturnInStmts tail)
				in  findReturnInStmts stmts  end
			 | Ast.While (exp,stmt) => false
			 | Ast.Do (exp,stmt) => findReturnInStmt stmt
			 | Ast.For (e1op,e2op,e3op,stmt) => false
			 | Ast.Labeled (label,stmt) => findReturnInStmt stmt
			 | Ast.CaseLabel (li,exp,stmt) => findReturnInStmt stmt
			 | Ast.DefaultLabel stmt => findReturnInStmt stmt
			 | Ast.Goto label => false
			 | Ast.Break => false
			 | Ast.Continue => false
			 | Ast.Return expOp => true
			 | Ast.IfThen (exp,stmt) => false
			 | Ast.IfThenElse (exp,stmt1,stmt2) => (findReturnInStmt stmt1) andalso (findReturnInStmt stmt2)
			 | Ast.Switch (exp,stmt) => false
			 | Ast.StatExt se => false
			 | Ast.ErrorStmt => false
		     )

		    fun hasGotos nil = false
		      | hasGotos (Ast.STMT(coreStmt,_,_)::tail) =
			case coreStmt
			  of Ast.Compound (decls, stmts) => hasGotos (stmts @ tail)
			   | Ast.While (exp,stmt) => hasGotos (stmt::tail)
			   | Ast.Do (exp,stmt) => hasGotos (stmt::tail)
			   | Ast.For (e1op,e2op,e3op,stmt) => hasGotos (stmt::tail)
			   | Ast.Labeled (label,stmt) => hasGotos (stmt::tail)
			   | Ast.CaseLabel (li,exp,stmt) => hasGotos (stmt::tail)
			   | Ast.DefaultLabel stmt => hasGotos (stmt::tail)
			   | Ast.Goto label => true
			   | Ast.IfThen (exp,stmt) => hasGotos (stmt::tail)
			   | Ast.IfThenElse (exp,stmt1,stmt2) => hasGotos (stmt1::(stmt2::tail))
			   | Ast.Switch (exp,stmt) => hasGotos (stmt::tail)
			   | _ => hasGotos tail

(******************* TODO: handle gotos and switches?
		    fun getGotoLabels nil = nil
		      | getGotoLabels (Ast.STMT(coreStmt,_,_)::tail) =
			case coreStmt
			  of Ast.Compound (decls, stmts) => getGotoLabels (stmts @ tail)
			   | Ast.While (exp,stmt) => getGotoLabels (stmt::tail)
			   | Ast.Do (exp,stmt) => getGotoLabels (stmt::tail)
			   | Ast.For (e1op,e2op,e3op,stmt) => getGotoLabels (stmt::tail)
			   | Ast.Labeled (label,stmt) => getGotoLabels (stmt::tail)
			   | Ast.CaseLabel (li,exp,stmt) => getGotoLabels (stmt::tail)
			   | Ast.DefaultLabel stmt => getGotoLabels (stmt::tail)
			   | Ast.Goto label => label::(getGotoLabels tail)
			   | Ast.IfThen (exp,stmt) => getGotoLabels (stmt::tail)
			   | Ast.IfThenElse (exp,stmt1,stmt2) => getGotoLabels (stmt1::(stmt2::tail))
			   | Ast.Switch (exp,stmt) => getGotoLabels (stmt::tail)
			   | _ => getGotoLabels tail

		    val gotoLabels = getGotoLabels [stmt]

		    datatype starting_point = SP_STARTED | SP_LABEL of Ast.label | SP_CASE of li | SP_DEFAULT
*********************)

		     val _ = if (findReturnInStmt stmt) andalso not (hasGotos [stmt])
			     then ()
			     else writeAssignEdges pseudoAssign (aoReturn cfAO, [aoValue tcVoid], tcVoid)
								(*should technically be static type *)

		in () end
	    | Ast.ExternalDeclExt _ => ()
	)

    fun processExternalDecls nil = ()
      | processExternalDecls (ed::edecls) =
		(processExternalDecl ed ; processExternalDecls edecls)

  in ( processExternalDecls edecls ) end
  ) (* end fun genAssignEdges (bundle, filestem) *)

end (* Structure OptInterface *)


