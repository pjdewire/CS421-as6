signature CANON = 
sig
    val linearize : Tree.stm -> Tree.stm list
        (* From an arbitrary Tree statement, produce a list of cleaned trees
	   satisfying the following properties:
	      1.  No SEQ's or ESEQ's
	      2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
        *)

    val basicBlocks : Tree.stm list -> (Tree.stm list list * Temp.label)
        (* From a list of cleaned trees, produce a list of
	 basic blocks satisfying the following properties:
	      1. and 2. as above;
	      3.  Every block begins with a LABEL;
              4.  A LABEL appears only at the beginning of a block;
              5.  Any JUMP or CJUMP is the last stm in a block;
              6.  Every block ends with a JUMP or CJUMP;
           Also produce the "label" to which control will be passed
           upon exit.
        *)

    val traceSchedule : Tree.stm list list * Temp.label -> Tree.stm list
         (* From a list of basic blocks satisfying properties 1-6,
            along with an "exit" label,
	    produce a list of stms such that:
	      1. and 2. as above;
              7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
            The blocks are reordered to satisfy property 7; also
	    in this reordering as many JUMP(T.NAME(lab)) statements
            as possible are eliminated by falling through into T.LABEL(lab).
         *)
end

structure Canon : CANON = 
struct

 structure T = Tree

 fun linearize(stm0: T.stm) : T.stm list =
 let
  infix %
  fun (T.EXP(T.CONST _)) % x = x
    | x % (T.EXP(T.CONST _)) = x
    | x % y = T.SEQ(x,y)

  fun commute(T.EXP(T.CONST _), _) = true
    | commute(_, T.NAME _) = true
    | commute(_, T.CONST _) = true
    | commute _ = false

  val nop = T.EXP(T.CONST 0)

  fun reorder do_exp (exps, build) =
       let fun f ((e as T.CALL _ )::rest) =
                        let val t = Temp.newtemp()
                         in f(T.ESEQ(T.MOVE(T.TEMP t, e), T.TEMP t)
			      :: rest)
		        end
	     | f(a::rest) =
	         let val (stms,e) = do_exp a
                     val (stms',el) = f rest
                  in if commute(stms',e)
                     then (stms % stms',e::el)
		     else let val t = Temp.newtemp()
			  in (stms % T.MOVE(T.TEMP t, e) % stms',
			      T.TEMP t :: el)
			  end
		 end
             | f nil = (nop,nil)
           val (stms, el) = f exps
        in (stms, build el)
       end

  fun expl(el,f) = reorder do_exp (el,f)

  and expl'(el,f) = let val (stms,s) = reorder do_exp (el,f)
                     in stms%s
		    end

  and do_stm(T.SEQ(a,b)) = do_stm a % do_stm b
    | do_stm(T.JUMP(e,labs)) = expl'([e],fn [e] => T.JUMP(e,labs))
    | do_stm(T.CJUMP(T.TEST(p,a,b),t,f)) = 
               expl'([a,b], fn[a,b]=> T.CJUMP(T.TEST(p,a,b),t,f))
    | do_stm(T.MOVE(T.TEMP t,T.CALL(e,el))) = 
             expl'(e::el,fn e::el => T.MOVE(T.TEMP t,T.CALL(e,el)))
    | do_stm(T.MOVE(T.TEMP t,b)) = expl'([b],fn[b]=>T.MOVE(T.TEMP t,b))
    | do_stm(T.MOVE(T.MEM(e,sz),b)) = 
             expl'([e,b],fn[e,b]=>T.MOVE(T.MEM(e,sz),b))
    | do_stm(T.MOVE(T.ESEQ(s,e),b)) = do_stm s % do_stm(T.MOVE(e,b))
    | do_stm(T.MOVE(a,b)) = expl'([a,b], fn[a,b] => T.MOVE(a,b))
    | do_stm(T.EXP(T.CALL(e,el))) = 
             expl'(e::el,fn e::el => T.EXP(T.CALL(e,el)))
    | do_stm(T.EXP e) = expl'([e],fn[e]=>T.EXP e)
    | do_stm s = expl'([],fn[]=>s)

  and do_exp(T.BINOP(p,a,b)) = expl([a,b], fn[a,b]=>T.BINOP(p,a,b))
    | do_exp(T.CVTOP(p,a,s1,s2)) = expl([a], fn[a]=>T.CVTOP(p,a,s1,s2))
    | do_exp(T.MEM(a,sz)) = expl([a], fn[a]=>T.MEM(a,sz))
    | do_exp(T.ESEQ(s,e)) = let val stms = do_stm s
                             val (stms',e) = expl([e],fn[e]=>e)
                          in (stms%stms',e)
                         end
    | do_exp(T.CALL(e,el)) = expl(e::el, fn e::el => T.CALL(e,el))
    | do_exp e = expl([],fn[]=>e)

  (* linear gets rid of the top-level SEQ's, producing a list *)
  fun linear(T.SEQ(a,b),l) = linear(a,linear(b,l))
    | linear(s,l) = s::l

 in (* body of linearize *)
    linear(do_stm stm0, nil)
 end

  type block = T.stm list

  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)

  fun basicBlocks stms = 
     let val done = Temp.newlabel()
         fun blocks((head as T.LABEL _) :: tail, blist) =
	       let fun next((s as (T.JUMP _))::rest, thisblock) =
		                endblock(rest, s::thisblock)
		     | next((s as (T.CJUMP _))::rest, thisblock) =
                                endblock(rest,s::thisblock)
		     | next(stms as (T.LABEL lab :: _), thisblock) =
                         next(T.JUMP(T.NAME lab,[lab]) :: stms, thisblock)
		     | next(s::rest, thisblock) = next(rest, s::thisblock)
		     | next(nil, thisblock) = 
                         next([T.JUMP(T.NAME done, [done])], thisblock)
		 
		   and endblock(stms, thisblock) = 
		         blocks(stms, rev thisblock :: blist)
		     
	        in next(tail, [head])
	       end
	   | blocks(nil, blist) = rev blist
	   | blocks(stms, blist) = 
               blocks(T.LABEL(Temp.newlabel())::stms, blist)
      in (blocks(stms,nil), done)
     end

  fun enterblock(b as (T.LABEL s :: _), table) = Symbol.enter(table,s,b)
    | enterblock(_, table) = table

  fun splitlast([x]) = (nil,x)
    | splitlast(h::t) = let val (t',last) = splitlast t in (h::t', last) end

  fun trace(table,b as (T.LABEL lab :: _),rest) = 
   let val table = Symbol.enter(table, lab, nil)
    in case splitlast b
     of (most,T.JUMP(T.NAME lab, _)) =>
	  (case Symbol.look(table, lab)
            of SOME(b' as _::_) => most @ trace(table, b', rest)
	     | _ => b @ getnext(table,rest))
      | (most,T.CJUMP(T.TEST(opr,x,y),t,f)) =>
          (case (Symbol.look(table,t), Symbol.look(table,f))
            of (_, SOME(b' as _::_)) => b @ trace(table, b', rest)
             | (SOME(b' as _::_), _) => 
		           most @ [T.CJUMP(T.TEST(T.notRel opr,x,y),f,t)]
		                @ trace(table, b', rest)
             | _ => let val f' = Temp.newlabel()
		     in most @ [T.CJUMP(T.TEST(opr,x,y),t,f'), 
				T.LABEL f', T.JUMP(T.NAME f,[f])]
			     @ getnext(table,rest)
                        end)
      | (most, T.JUMP _) => b @ getnext(table,rest)
     end

  and getnext(table,(b as (T.LABEL lab::_))::rest) = 
           (case Symbol.look(table, lab)
             of SOME(_::_) => trace(table,b,rest)
              | _ => getnext(table,rest))
    | getnext(table,nil) = nil

  fun traceSchedule(blocks,done) = 
       getnext(foldr enterblock Symbol.empty blocks, blocks)
         @ [T.LABEL done]

end
