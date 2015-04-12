structure DotGraph :
  sig val dot : TextIO.outstream * Graph.graph * (Graph.node->string) -> unit
  end =

struct
  structure T = Graph.Table
  fun bug s = ErrorMsg.impossible ("DotGraph:" ^ s)

  fun getsome (SOME x) = x
    | getsome (NONE) = bug "unexpected case in getsome"

  fun dot(out,graph,label) =
   let fun say s = TextIO.output(out, s)
       val nodes = Graph.nodes graph
       val (_,numtab) = 
             foldl (fn(n,(i,tab))=>(i+1,T.enter(tab,n,i))) (0,T.empty) nodes
       fun dotname node = "n" ^ Int.toString(getsome(T.look(numtab,node)))

       fun mkedge src dst =
           (say (dotname src); say " -> "; say (dotname dst); say "\n")

       val marks = ref(T.empty : unit T.table)
       fun mark node = marks := T.enter(!marks,node,())
       fun marked node = case T.look(!marks,node) of SOME() => true 
                                                   | NONE =>false

       fun basicblock(first,last,labs') = 
            (say (dotname first); say "[shape=box,height=0.1,label=\"";
	     say (concat labs'); say "\"];\n";
             app (mkedge first) (Graph.succ last))

       fun next(first,node,labs) =
         let val labs' = case label node
                          of "" => labs | lab => "\\n" :: lab :: labs
          in mark node;
             case Graph.succ node
              of [s] => (case Graph.pred s 
			  of [_] => next(first,s,labs')
                           | _ => basicblock(first,node, rev labs'))
               | _ => basicblock(first,node,rev labs')
        end
            

       fun mknode node = if marked node then () else
         case Graph.pred node
          of [p] => (case Graph.succ p of [_] => mknode p
                                       | _ => next(node,node,nil))
           | _ => next(node,node,nil)
         

    in say "digraph g { \nsize=\"7,10\"\n";
       app mknode nodes;
       say "}\n\n"
   end
end



