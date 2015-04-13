(* 
* Peter Dewire
*
* liveness.sml 
*
*)

signature LIVENESS =
sig

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}


  val interferenceGraph : 
        Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)

  val test : unit -> unit

  (*
   *  val show : outstream * igraph -> unit
   *)

end (* signature LIVENESS *)

structure Liveness : LIVENESS = 
struct

  structure F = Flow
  structure G = Graph
  structure GT = Graph.Table
  structure M = MakeGraph

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  (* To construct the interference graph, it is convenient to
     construct a liveness map at each node in the FlowGraph first.
     For each node in the flowgraph, i.e., for each assembly 
     instruction, we want to easily look up the set S of live 
     temporaries. 
   *)

  type liveSet = unit Temp.Table.table * Temp.temp list
  type livenessMap = liveSet Flow.Graph.Table.table

  (* don't think this is actually necessary, can just compare lists using = *)
  fun listCompare ((x1 : int, x2 : int list)::xs, (y1 : int, y2 : int list)::ys)
        = if (x1 = y1 andalso x2 = y2) then listCompare(xs, ys) else false
    | listCompare ([], []) = true
    | listCompare (_, _) = false

  (* initialize blank liveness map, which looks like:
           [(0, []), (1, []), ... , (x - 1, [])]             *)
  fun initLM (n, i) = 
    if n = 0 then (print "initLM error"; [])
        else if n = i then []
          else (i, [])::initLM(n, i + 1)

  (* For simplicity, I just return the livenessMap as a list, since we are not
  * worried about efficiency *)
  fun constructLM (fgraph, nodeList, lastLO, lastLI) =
        let
          val (newLO, newLI) = cLMhelp(fgraph, nodeList, lastLO, lastLI, 0);
        in
          if (listCompare(newLO, lastLO) andalso listCompare(newLI, lastLI))
            then (newLO, newLI)
            else constructLM(fgraph, nodeList, newLO, newLI)
        end

  and cLMhelp (fgraph as F.FGRAPH{control, use, def, ismove}, nodeList,
        lastLO, lastLI, node_index) =
        if node_index < List.length(nodeList) then (* length is 8 *)
            let
              val x = print ("in cLMhelp " ^ Int.toString(node_index) ^ "\n");
              val curNodeLO = List.nth (lastLO, node_index + 1);
              val y = print "down here\n";
              val node = List.nth (nodeList, node_index);
              val (nodeNum, loSubDef) = subtractDef(curNodeLO, def, node);
              val useList = getOpt(GT.look(use, node), []);
              val newLiveIn = union(useList, loSubDef);
              val newLiveOut = unionMany(G.succ(node), lastLI, nodeList);
              (* val outSubDef = newLO - def *)
              (* G.succ(node) *)
            in
              cLMhelp (fgraph, nodeList, lastLO, lastLI, node_index + 1)
            end
        (* otherwise we have iterated through all nodes *)
        else (lastLO, lastLI)

  (* performs the (out[n] - def[n]) in the second line of the for loop *)
  and subtractDef((nodeNum, outList), tbl, node) = 
        let
          val defs = getOpt(GT.look(tbl, node), [])
        in
          (nodeNum, subtractList(outList, defs))
        end

  and subtractList (x::xs, toDel) = if inList(x, toDel) 
          then subtractList(xs, toDel)
          else x::subtractList(xs, toDel)
    | subtractList([], _) = []

  and inList (x, y::ys) = if x = y then true else inList(x, ys)
    | inList (x, []) = false

  and union (x::xs, y::ys) = if x = y 
          then x::union(xs, ys)
          else x::y::union(xs, ys)
    | union ([], l) = l
    | union (l, []) = l

  and unionMany (x::xs, liveInList, nodeList) = 
        let 
          val (nodeNum, liveIn) = List.nth(liveInList, listIndex(x, nodeList,
               0));
        in
          union(liveIn, unionMany(xs, liveInList, nodeList))
        end
    | unionMany ([], liveInList, nodeList) = []

  and listIndex (elt, x::xs, i) = if G.nodename(x) = G.nodename(elt) then i
       else listIndex(elt, xs, i + 1)
    | listIndex (elt, [], i) = (print "listIndex error\n"; ~1)


  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)

  fun interferenceGraph (fgraph as F.FGRAPH{control, use, def, ismove}) =
    let
      val intGraph = G.newGraph();
      val tn = Temp.Table.empty;
      val gt = GT.empty;
      val m = [];

      val igraph = IGRAPH{graph=intGraph, tnode=tn, gtemp=gt, moves=m};
      fun node2temp (fgNode) = []
    in
      (igraph, node2temp)
    end


  fun test () = 
    ( 
     print "testing...\n";
     let
       val (fgraph, nodeList) = M.instrs2graph(M.instrs);
       val len = List.length(nodeList);
       val blankLI = initLM(len, 0);
       val blankLO = initLM(len, 0);
       val (liveIn, liveOut) = constructLM(fgraph, nodeList, blankLO, blankLI);
       val x = print "hello!\n";
     in 
       print "poop!\n"
     end
    )



end (* structure Liveness *)
