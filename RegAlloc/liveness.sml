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
  (* val subtractDef : (int * int list) * Temp.temp list Graph.Table.table * *)
        (* Flow.Graph.node -> int list *)

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


  (* printing functions *)

  fun printLM ((x, l)::xs) =
        (
         print ("n" ^ Int.toString(x) ^ ": " ^ pList(l));
         printLM (xs)
        )
    | printLM ([]) = ()

  and pTable(tbl, node::n_tail) = (print (G.nodename(node) ^ ": " ^
        pList(getOpt(GT.look(tbl, node), [~1])));
        pTable(tbl, n_tail))
    | pTable(tbl, []) = ()

  and pList(x::xs) = (Int.toString(x) ^ " " ^ pList(xs))
    | pList([]) = "\n"

  (* end printing functions *)

  (* functions to construct liveness map *)

  (* For simplicity, I just return the livenessMap as a list, since we are not
  * worried about efficiency *)
  fun constructLM (fgraph, nodeList, lastLO, lastLI) =
        let
          val l = List.length(nodeList);
          val (newLO, newLI) = cLMhelp(fgraph, nodeList, lastLO, lastLI, l - 1);
        in
          if (listCompare(newLO, lastLO) andalso listCompare(newLI, lastLI))
            then (newLO, newLI)
            else constructLM(fgraph, nodeList, newLO, newLI)
        end

  and cLMhelp (fgraph as F.FGRAPH{control, use, def, ismove}, nodeList,
        lastLO, lastLI, node_index) =
        if node_index > ~1 (* List.length(nodeList) *)then (* length is 8 *)
            let
              val curNodeLO = List.nth (lastLO, node_index);
              val node = List.nth (nodeList, node_index);

              (* line 3 *)
              val newLiveOut = unionMany(G.succ(node), lastLI, nodeList);

              (* line 2 *)
              val loSubDef = subtractDef(newLiveOut, def, node);
              val useList = getOpt(GT.look(use, node), []);

              val newLiveIn = union(useList, loSubDef);

              val newLO = replaceElt(lastLO, (node_index, newLiveOut), 
                    node_index, 0);
              val newLI = replaceElt(lastLI, (node_index, newLiveIn), 
                    node_index, 0);
            in
              cLMhelp (fgraph, nodeList, newLO, newLI, node_index - 1)
            end
        (* otherwise we have iterated through all nodes *)
        else (lastLO, lastLI)

  (* performs the (out[n] - def[n]) in the second line of the for loop *)
  and subtractDef(outList, tbl, node) = 
        let
          val defs = getOpt(GT.look(tbl, node), [])
        in
          subtractList(outList, defs)
        end

  and subtractList (x::xs, toDel) = if inList(x, toDel) 
          then subtractList(xs, toDel)
          else x::subtractList(xs, toDel)
    | subtractList([], _) = []

  and inList (x, y::ys) = if x = y then true else inList(x, ys)
    | inList (x, []) = false

  and union (x::xs, l) = if inList(x, l)
          then union(xs, l)
          else x::union(xs, l)
    | union ([], l) = l

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

  and replaceElt(x::xs, elt, n, i) = 
        if i = n then elt::xs
          else x::replaceElt(xs, elt, n, i + 1)
    | replaceElt([], elt, n, i) = []

  (* end liveness construction *)

  (* getTemps returns a list of temporaries defined and used in the control flow
  * graph *)
  (* the number defined should be the total number, right? *)
  fun getTemps (node::n_tail, use, def) =
      let
        val uTemps = getOpt(GT.look(use, node), []);
        val dTemps = getOpt(GT.look(def, node), []);
        val tot = union(uTemps, dTemps);
      in
        union(tot, getTemps(n_tail, use, def))
      end
    | getTemps ([], use, def) = []


  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)

  fun sortList(l) = 
    if isSorted(l) then l
      else sortList(sortOnce(l))

  and sortOnce([]) = []
    | sortOnce(x::[]) = x::[]
    | sortOnce(x::xs) = if x > hd(xs) then hd(xs)::sortOnce(x::tl(xs))
        else x::sortOnce(xs)

  and isSorted ([]) = true
    | isSorted (x::[]) = true
    | isSorted (x::xs) = if x > hd(xs) then false else isSorted(xs)

  (* getNodeList creates new blank nodes for every instr in Assem.instr list *)
  fun getNodeList (instr::xs, c) = Graph.newNode(c)::getNodeList(xs, c)
    | getNodeList ([], c) = []


  fun mapTemps (temp::xs, intGraph, tn, gt) = 
        let
          val newNode = Graph.newNode(intGraph);
          val tn = Temp.Table.enter(tn, temp, newNode);
          val gt = Graph.Table.enter(gt, newNode, temp);
        in
          mapTemps(xs, intGraph, tn, gt)
        end

    | mapTemps ([], _, tn, gt) = (tn, gt)


  (* add edges to interference graph *)
  fun addEdges (node::xs, def, (x, thisliveOut)::lo_tail, tn) =
        let
          val thisNodeDefs = getOpt(GT.look(def, node), []);
        in
          (
           aeHelp(thisNodeDefs, thisliveOut, tn);
           addEdges (xs, def, lo_tail, tn)
          )
        end

    | addEdges ([], def, _, tn) = ()   (* this should happen *)
    | addEdges (_, def, [], tn) = ()   (*  at the same time  *)

  and aeHelp (d::ds, liveList, tn) = aeHelp2 (d, liveList, tn)
    | aeHelp ([], liveList, tn) = ()

  and aeHelp2 (d, live::ls, tn) = 
      let 
        val f = getOpt(Temp.Table.look(tn, d), G.newNode(G.newGraph()));
        val t = getOpt(Temp.Table.look(tn, live), G.newNode(G.newGraph()));
      in
        (
         G.mk_edge({from=f, to=t});
         aeHelp2(d, ls, tn)
        )
      end
    | aeHelp2 (d, [], tn) = ()
   (* end adding edges *)


  fun interferenceGraph (fgraph as F.FGRAPH{control, use, def, ismove}) =
    let
      (* get liveness map *)
      val nodeList = G.nodes(control);
      val len = List.length(nodeList);
      val blankLO = initLM(len, 0);
      val blankLI = initLM(len, 0);
      val (liveOut, liveIn) = constructLM(fgraph, nodeList, blankLO, blankLI);

      (* get temporaries *)
      val tempList = sortList(getTemps(nodeList, use, def));

      (* create nodes, map them to temporaries, and do reverse mapping *)
      val intGraph = G.newGraph();
      val tn = Temp.Table.empty;
      val gt = GT.empty;
      val (tn, gt) = mapTemps(tempList, intGraph, tn, gt)

      val m = [];

      val igraph = IGRAPH{graph=intGraph, tnode=tn, gtemp=gt, moves=m};
      fun node2temp (fgNode) = []
    in
      (
      addEdges(nodeList, def, liveOut, tn);
      (igraph, node2temp)
      )
    end


  fun test () = 
    ( 
     print "testing...\n";
     let
       val (fgraph, nodeList) = M.instrs2graph(M.instrs);
       val x = interferenceGraph(fgraph);
       val len = List.length(nodeList);
       val blankLI = initLM(len, 0);
       val blankLO = initLM(len, 0);
       val (liveOut, liveIn) = constructLM(fgraph, nodeList, blankLO, blankLI);
     in 
       (
        print "live out:\n";
        printLM (liveOut);
        print "live in:\n";
        printLM (liveIn)
       )
     end
    )



end (* structure Liveness *)
