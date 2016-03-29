signature OPT_INTERFACE =
sig
  structure pidset : ORD_SET where type Key.ord_key = Pid.uid
  structure aidmap : ORD_MAP where type Key.ord_key = Aid.uid

  val ctypeToTcType : (Bindings.tidBinding Tidtab.uidtab) -> Ast.ctype -> Rtc.tcType
 
  val isAssignUnop : Ast.unop -> bool
  val isAssignBinop : Ast.binop -> bool
  val isMalloc : Ast.expression -> bool
  val isCalloc : Ast.expression -> bool
  val isAlloca : Ast.expression -> bool
  val isVargDummy : Ast.expression -> bool
  val isDerefLvalExpr : Ast.expression -> bool
  val isLvalExpr : Ast.expression -> bool
  val isZero : Ast.expression -> bool
  val isExtern : Ast.id -> bool
  val isStatic : Ast.id -> bool

  val evalMallocSize : (Ast.expression -> 'a option)
			-> (Ast.ctype -> Aid.uid)
			-> Ast.expression * Ast.expression list
			-> 'a option

  val stripTcInclude : Ast.externalDecl list -> Ast.externalDecl list * Aid.uid

  val simple_lookup : Rtc.absObject list -> Rtc.ts_categ

  val simplifyIfScalar : (Bindings.tidBinding Tidtab.uidtab) -> Ast.ctype * Ast.initExpression option -> Ast.initExpression option

  val buildStringMap : ParseToAst.astBundle * string -> (string, int) HashTable.hash_table

  datatype st_class = SC_AUTO | SC_HEAP | SC_STATIC

  val buildUidMaps : ParseToAst.astBundle * string ->
				( (Aid.uid, st_class * string * (string * SourceMap.location) option) HashTable.hash_table
				* (Pid.uid, st_class * string) HashTable.hash_table)

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

  val readTSlevels : (string, int) HashTable.hash_table * string * string * Aid.uid
			-> lookup_bundle
			 * (Rtc.absObject list -> Rtc.absObject list)	(* lookup alias *)

  val addrTakenAnalysis : ParseToAst.astBundle -> (Rtc.absObject list -> Rtc.ts_categ) * (Rtc.absObject list -> Rtc.ts_categ)

  datatype fieldsig = fsStruct of Rtc.tcType list | fsUnion of Rtc.tcType
  val getFieldSig : (Bindings.tidBinding Tidtab.uidtab) -> Ast.ctype * Ast.member * {deref:bool} -> fieldsig

  val assignCurVargAO : Ast.id * Ast.id list -> unit
  val expToAbsObjects : (Ast.expression -> Ast.ctype) * (Bindings.tidBinding Tidtab.uidtab) * { deref : bool } -> Ast.expression
			-> Rtc.absObject list

  val outputAidPidAliases : ((Aid.uid, st_class * string * (string * SourceMap.location) option) HashTable.hash_table
				* (Pid.uid, st_class * string) HashTable.hash_table
				* TextIO.outstream) -> unit
  val genAssignEdges : ParseToAst.astBundle * TextIO.outstream -> unit

  val applyToExp : (Ast.expression -> unit) -> ParseToAst.astBundle -> unit

  val arrayToPtrAOs : Rtc.absObject list -> Rtc.absObject list

  val handleInitializer : ((Bindings.tidBinding Tidtab.uidtab) * (Ast.expression -> Ast.ctype))
			  -> ((Rtc.absObject * Rtc.absObject list * Rtc.tcType * Ast.expression) -> 'a list)
			  -> (unit -> Rtc.absObject)
			  -> (Ast.ctype * Ast.initExpression option)
			  -> 'a list
end
