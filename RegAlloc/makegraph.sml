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

  (* getNodeN returns the Nth node in nodeList *)
  fun getNodeN (node::n_tail : FG.node list, n, i) : FG.node = 
        if n = i then node
          else getNodeN (n_tail, n, i + 1)

    | getNodeN ([], n, i) = (print "error: getNodeN"; FG.newNode(FG.newGraph()))

  (* getNodeList creates new blank nodes for every instr in Assem.instr list *)
  fun getNodeList (instr::xs, c) = Graph.newNode(c)::getNodeList(xs, c)
    | getNodeList ([], c) = []


  (* i2g performs the work of instrs2graph *)

  fun i2g (Assem.OPER{assem=a, dst=defTemps, src=srcTemps, jump=NONE}::i_tail, fg
        as F.FGRAPH{control, def, use, ismove}, nodeList, n_instr) = 
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
              ismove=upIM}, nodeList, n_instr + 1)
          )
        end

    | i2g (Assem.OPER{assem=a, dst=dstTemps, src=srcTemps, 
        jump=SOME(labList)}::i_tail, fg, nodeList, n_instr) = 
        i2g(i_tail, fg, nodeList, n_instr + 1)

    (* for now, I am assuming that labels are associated with the instruction
    * that directly follows them. So we can always add "fall-through" edges
    * (except when jump is specified and does not include the succeeding instr,
    * as they will fall through to the label and then the next OPER *)
    | i2g (Assem.LABEL{assem=a, lab=label}::i_tail, fg, nodeList, n_instr) =
                       (* this condition should never be satisfied *)
        if i_tail = [] then i2g(i_tail, fg, nodeList, n_instr + 1)
          else (G.mk_edge({from=getNodeN (nodeList, n_instr, 0),
                           to=getNodeN(nodeList, n_instr + 1, 0)}); 
                           i2g(i_tail, fg, nodeList, n_instr + 1))

    | i2g (Assem.MOVE{assem=a, dst=dstTemps, src=srcTemps}::i_tail, fg,
        nodeList, n_instr) = 
        i2g(i_tail, fg, nodeList, n_instr + 1)

    | i2g ([], fg, nodeList, n_instr) = (fg, nodeList)

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
    in
      i2g(instrs, fg, nodeList, 0)
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
  val l1 = Assem.LABEL{assem="l2", lab=Symbol.symbol("l1")};
  val l2 = Assem.LABEL{assem="l6", lab=Symbol.symbol("l6")};
  val i1 = Assem.OPER{assem="pet", dst=[1], src=[], jump=NONE};
  val i2 = Assem.OPER{assem="i2", dst=[2], src=[1], jump=NONE};
  val i3 = Assem.OPER{assem="i3", dst=[3], src=[1, 2], jump=NONE};
  val i4 = Assem.OPER{assem="i4", dst=[1], src=[2], jump=NONE};
  val i5 = Assem.OPER{assem="i5", dst=[], src=[1],
    jump=SOME([Symbol.symbol("l2"), Symbol.symbol("l6")])};
  val i6 = Assem.OPER{assem="i6", dst=[], src=[3], jump=NONE}
  val instrs = [l1, l2, i1, i2, i3, i4, i5, i6];

  fun printNList (node::n_tail) = (print (G.nodename(node) ^ " poop\n"); 
        printNList(n_tail))
    | printNList ([]) = ()

  fun printNodeInfo (node::n_tail) = 
        (
          print ("\n===============\ninfo for " ^ G.nodename(node) ^ ":\n");
          print "------- succ --------\n";
          printNList(G.succ(node));
          print "-------- pred --------\n";
          printNList(G.pred(node));
          printNodeInfo(n_tail)
        )
    | printNodeInfo ([]) = ()

  fun test () = 
    (
      print "testing...\n"; 
      let 
        val (fgraph, nodeList) = instrs2graph(instrs);
      in 
        (
         printNodeInfo (nodeList)
        )
      end
    )
   

end
