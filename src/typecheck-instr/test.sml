(* usage: use "test.sml" *)
(****************************************************************************)
(* testing recursive data structure *)
(****************************************************************************)

datatype repr
  = Str of string
  | Int of int

and pgmnode
  = Pnode of repr * cfgnode list ref

and cfgnode
  = CFGplain of pgmnode * cfgnode list * cfgnode
  | CFGif of pgmnode * cfgnode list * cfgnode * cfgnode
  | CFGloop of pgmnode * cfgnode list * cfgnode * cfgnode

fun rToString (r : repr) =
  case r of Str s => s
          | Int i => Int.toString i

fun cfgtest () =
  let
      val r1 = Str "abcd"
      val r2 = Int 5
      val p1 = Pnode (r1, ref nil)
      val p2 = Pnode (r2, ref nil)
  in
      print ("r1 = " ^ (rToString r1) ^ " r2 = " ^ (rToString r2) ^ "\n")
  end

structure stringSets = SplaySetFn (struct
					type ord_key = string
					val compare = String.compare
				   end)
fun setTest() =
  let

      val myset = stringSets.empty
      val newset = stringSets.add (myset, "foo")
      val newset = stringSets.add (newset, "bar")
      val _ = print ("Member foo = " ^ (Bool.toString (stringSets.member (myset, "foo"))) ^ "\n")
      val _ = print ("Member foo = " ^ (Bool.toString (stringSets.member (newset, "foo"))) ^ "\n")
  in () end


(****************************************************************************)
(* old test - for function call order??? *)
(****************************************************************************)

fun foo (f,x,y) = (f x; f y)

fun bar () =
    let val _ = print "before1\n"
(*
	val ffn = (fn y => (print "before2\n"; print ("processing "^y^"\n"); print "after2\n"))
*)
	fun ffn y = (print "before3\n"; print ("processing "^y^"\n"); print "after3\n")
	val _ = foo (ffn, "hello", "goodbye")
	val _ = print "after1\n"
    in () end

fun mysort nil = nil
  | mysort (v::tail) =
    let val tail' = mysort tail
        fun insert (v,nil) = [v]
          | insert (v,h::tail) =
            if v < h then v::(h::tail)
                     else h::(insert (v,tail))
    in  insert (v,tail')  end
