(* 
* Peter Dewire
* 
* makegraph.sml 
*
*)

signature MAKEGRAPH = 
sig

  val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
  val instrs : Assem.instr list
  val test : unit -> unit (* Flow.flowgraph * Flow.Graph.node list *)
  val getNodeN : Flow.Graph.node list * int * int -> Flow.Graph.node
  val gInfo : Flow.flowgraph -> 
        Temp.temp list Graph.Table.table * Temp.temp list Graph.Table.table
  val printTable : Flow.flowgraph * Flow.Graph.node list -> unit
  val printNodeInfo : Flow.Graph.node list -> unit
  val removeDups : Graph.node list -> Graph.node list
  val removeDupsString : string list -> string list

end

structure MakeGraph : MAKEGRAPH =
struct


(* The "instrs2graph" function takes a list of assembly instructions,
   and constructs its flowgraph and also returns the list of nodes in 
   the flowgraph. The instructions exactly correspond to the nodes in 
   the graph. If instruction m can be followed by instruction n (either
   by a jump or by falling through), there should be an edge from m to n
   in the graph.

   The flowgraph also maintains several attributes for each node in the 
   graph, i.e., the "def" set, the "use" set, and the "ismove" flag

 *)

  structure FG = Flow.Graph
  structure GT = Graph.Table
  structure F = Flow
  structure G = Graph
  structure S = Symbol

  (* getNodeN returns the Nth node in nodeList *)
  fun getNodeN (node::n_tail : FG.node list, n, i) : FG.node = 
        if n = i then node
          else getNodeN (n_tail, n, i + 1)

    | getNodeN ([], n, i) = (print "error: getNodeN"; FG.newNode(FG.newGraph()))

  (* getNodeList creates new blank nodes for every instr in Assem.instr list *)
  fun getNodeList (instr::xs, c) = Graph.newNode(c)::getNodeList(xs, c)
    | getNodeList ([], c) = []

  (* makes a list of (label, instr_num) pairs for use in OPER jumps *)
  fun getLabPairs (Assem.LABEL{assem=a, lab=label}::xs, instr_num) = 
        (label, instr_num)::getLabPairs(xs, instr_num + 1)
    | getLabPairs (instr::xs, instr_num) = getLabPairs(xs, instr_num + 1)
    | getLabPairs ([], instr_num) = []

  fun prLabList ((lab, num)::p_tail) = (print (S.name(lab) ^ " " ^ (Int.toString
        num) ^ "\n"); prLabList(p_tail))
    | prLabList ([]) = ()

  (* makes edges for OPER instrs with jumps *)
  fun makeEdges (curNode, nodeList, pairsList, label::lab_tail) =
        (
          makeEdgesHelp(curNode, nodeList, pairsList, label);
          makeEdges(curNode, nodeList, pairsList, lab_tail)
        )
    | makeEdges (_, _, _, []) = ()

  and makeEdgesHelp (curNode, nodeList, (l1, n1)::pl_tail, label) = (
          if l1 = label then G.mk_edge({from=curNode, to=getNodeN(nodeList, n1,
          0)})
            else ();
          makeEdgesHelp(curNode, nodeList, pl_tail, label)
        )
    | makeEdgesHelp (_, _, [], _) = ()

  (* i2g performs the work of instrs2graph *)

  fun i2g (Assem.OPER{assem=a, dst=defTemps, src=srcTemps, jump=NONE}::i_tail, fg
        as F.FGRAPH{control, def, use, ismove}, nodeList, n_instr, pairList) = 
        let
          val curNode = getNodeN (nodeList, n_instr, 0)
          val upDefs = GT.enter(def, curNode, defTemps)
          val upUses = GT.enter(use, curNode, srcTemps)
          val upIM = GT.enter(ismove, curNode, false)
        in
          (
            if i_tail = [] then () else (Graph.mk_edge({from=curNode,
              to=getNodeN (nodeList, n_instr + 1, 0)}); ());
            i2g(i_tail, F.FGRAPH{control=control, def=upDefs, use=upUses,
              ismove=upIM}, nodeList, n_instr + 1, pairList)
          )
        end

    | i2g (Assem.OPER{assem=a, dst=defTemps, src=srcTemps, 
        jump=SOME(labList)}::i_tail, fg as F.FGRAPH{control, def, use, ismove}, 
        nodeList, n_instr, pairList) = 
        let 
          val curNode = getNodeN (nodeList, n_instr, 0);
          val upDefs = GT.enter(def, curNode, defTemps);
          val upUses = GT.enter(use, curNode, srcTemps);
          val upIM = GT.enter(ismove, curNode, false);
        in
          (
            makeEdges(curNode, nodeList, pairList, labList);
            i2g(i_tail, F.FGRAPH{control=control, def=upDefs, use=upUses,
            ismove=upIM}, nodeList, n_instr + 1, pairList)
          )
        end

    (* for now, I am assuming that labels are associated with the instruction
    * that directly follows them. So we can always add "fall-through" edges
    * (except when jump is specified and does not include the succeeding instr,
    * as they will fall through to the label and then the next OPER *)
    | i2g (Assem.LABEL{assem=a, lab=label}::i_tail, fg, nodeList, n_instr,
             pairList) =
                       (* this condition should never be satisfied *)
        if i_tail = [] then i2g(i_tail, fg, nodeList, n_instr + 1, pairList)
          else (G.mk_edge({from=getNodeN (nodeList, n_instr, 0),
                           to=getNodeN(nodeList, n_instr + 1, 0)}); 
                           i2g(i_tail, fg, nodeList, n_instr + 1, pairList))

    | i2g (Assem.MOVE{assem=a, dst=dstTemps, src=srcTemps}::i_tail, fg,
        nodeList, n_instr, pairList) = 
        i2g(i_tail, fg, nodeList, n_instr + 1, pairList)

    | i2g ([], fg, nodeList, n_instr, pairList) = (fg, nodeList)

  fun instrs2graph (instrs : Assem.instr list) = 
    let 
      val cGraph = Graph.newGraph();
      (*
      val d = Graph.Table.enter(d, [], []);
      val u = Graph.Table.enter(u, [], []);
      val i = Graph.Table.enter(i, [], false);
      *)
      val defTemps = Graph.Table.empty;
      val usedTemps = Graph.Table.empty;
      val i = Graph.Table.empty;
      val fg = Flow.FGRAPH{control=cGraph, def=defTemps, use=usedTemps, ismove=i};
      (* correct? *)
      val nodeList = getNodeList(instrs, cGraph);
      val pairList = getLabPairs(instrs, 0);
    in
      i2g(instrs, fg, nodeList, 0, pairList)
    end

    (*
    * example (from Appel pg. 212)
     
          1. a := 0
                |
                v
          2. b := a + 1   <--            ( L1 )
                |            \
                v            |
          3. c := c + b      |
                |            |
                v            |
          4. a := b * 2      |
                |            |
                v            /
          5. a < N      -----            (if a < N, goto L1)
                |
                v
          6. return c
   
    *)


  (* what is the assem field supposed to be? any string?  *)
  (* the temporaries aren't already divided like this, are they? *)
  val n0 = Assem.OPER{assem="pet", dst=[1], src=[], jump=NONE};
  val n1 = Assem.LABEL{assem="l2", lab=Symbol.symbol("l1")};
  val n2 = Assem.OPER{assem="i2", dst=[2], src=[1], jump=NONE};
  val n3 = Assem.OPER{assem="i3", dst=[3], src=[3, 2], jump=NONE};
  val n4 = Assem.OPER{assem="i4", dst=[1], src=[2], jump=NONE};
  val n5 = Assem.OPER{assem="i5", dst=[], src=[1],
    jump=SOME([Symbol.symbol("l1"), Symbol.symbol("l6")])};
  val n6 = Assem.LABEL{assem="l6", lab=Symbol.symbol("l6")};
  val n7 = Assem.OPER{assem="i6", dst=[], src=[3], jump=NONE}
  val instrs = [n0, n1, n2, n3, n4, n5, n6, n7];

  fun printNList (node::n_tail) = (print (G.nodename(node) ^ ", "); 
        printNList(n_tail))
    | printNList ([]) = ()

  fun removeDups (x::xs : G.node list) = x::removeDups(rdHelp(x, xs))
      | removeDups ([]) = []

    and rdHelp (y, x::[]) = if G.nodename(x) = G.nodename(y) then [] else x::[]
      | rdHelp (y, x::xs) = if G.nodename(x) = G.nodename(y) then rdHelp(y, xs) 
                              else x::rdHelp(y, xs)
      | rdHelp (y, []) = []

  fun removeDupsString (x::xs : string list) = 
        x::removeDupsString(rdsHelp(x, xs))
    | removeDupsString ([]) = []

  and rdsHelp (y, x::[]) = if x = y then [] else x::[]
    | rdsHelp (y, x::xs) = if x = y then rdsHelp(y, xs) else x::rdsHelp(y, xs)
    | rdsHelp (y, []) = []


  fun printNodeInfo (node::n_tail) = 
        (
          print ("\n" ^ G.nodename(node) ^ ": ");
          print "adj: ";
          printNList(removeDups(G.adj(node)));
          (* print " --- pred: "; *)
          (* printNList(G.pred(node)); *)
          printNodeInfo(n_tail)
        )
    | printNodeInfo ([]) = ()

  fun printTable (fgraph as F.FGRAPH{control, use, def, ismove}, nodeList) =
        (
          print "\n\nuse table\n";
          pTable(use, nodeList);
          print "\ndef table\n";
          pTable(def, nodeList)
        )
        
  and pTable(tbl, node::n_tail) = (print (G.nodename(node) ^ ": " ^
        pList(getOpt(GT.look(tbl, node), [~1])));
        pTable(tbl, n_tail))
    | pTable(tbl, []) = ()

  and pList(x::xs) = (Int.toString(x) ^ " " ^ pList(xs))
    | pList([]) = "\n"


  fun test () = 
    (
      print "testing...\n"; 
      let 
        val (fgraph, nodeList) = instrs2graph(instrs);
        val pairList = getLabPairs(instrs, 0);
      in 
        (
         (* prLabList (pairList); *)
         printNodeInfo (nodeList);
         printTable(fgraph, nodeList)
        )
      end
    )
   
  fun gInfo (fg as F.FGRAPH{control, use, def, ismove}) = (use, def)

end
