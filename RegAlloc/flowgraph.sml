(* flowgraph.sml *)

signature FLOW = 
sig
  structure Graph : GRAPH 
  datatype flowgraph 
    = FGRAPH of {control: Graph.graph,
                 def: Temp.temp list Graph.Table.table,
                 use: Temp.temp list Graph.Table.table,
                 ismove: bool Graph.Table.table}

  val dot : TextIO.outstream * flowgraph -> unit   
end (* signature FLOW *)

structure Flow : FLOW =
struct
  fun bug s = ErrorMsg.impossible ("Flow:" ^ s)

  structure Graph : GRAPH = Graph
  datatype flowgraph 
    = FGRAPH of {control: Graph.graph,
                 def: Temp.temp list Graph.Table.table,
                 use: Temp.temp list Graph.Table.table,
                 ismove: bool Graph.Table.table}

  (*
   * Note:  any "use" within the block is assumed to be BEFORE a "def" 
   * of the same variable.  If there is a def(x) followed by use(x)
   * in the same block, do not mention the use in this data structure,
   * mention only the def.
   *
   * More generally:
   *   If there are any nonzero number of defs, mention def(x).
   *   If there are any nonzero number of uses BEFORE THE FIRST DEF,
   *        mention use(x).
   *
   * For any node in the graph,  
   *       Graph.Table.look(def,node) = SOME(def-list)
   *       Graph.Table.look(use,node) = SOME(use-list)
   *)

  local structure D = DotGraph
        fun getsome (SOME x) = x
          | getsome (NONE) = bug "unexpected case in getsome"

        fun look(t,x) = getsome(Graph.Table.look(t,x))
        fun mklab (FGRAPH{control,def,use,ismove}) node =
           case (look(use,node), look(def,node))
            of (nil,nil) => ""
             | (uses,defs) =>
                  let fun names(t,rest) = Temp.makestring t :: " " :: rest
                      val uses' = foldr names nil uses
                      val defs' = foldr names nil defs
                      val mov = if look(ismove,node)
                                    then  ["<= "]  else ["<- "]
                   in concat(defs' @ mov @ uses')
                  end
     in fun dot(out,fg as FGRAPH{control,def,use,ismove}) = 
            D.dot(out,control,mklab fg)
    end

end (* structure Flow *)
