(* regalloc.sml *)

signature REG_ALLOC =
sig
   structure R : REGISTER_STD
   
   type allocation = Register.register Temp.Table.table

   val color : {interference : Liveness.igraph,
                initial : allocation,
                registers : R.register list} -> allocation

   val test : unit -> unit

end (* signature REG_ALLOC *)

functor RegAllocGen(Register : REGISTER_STD) : REG_ALLOC =
struct
   structure R : REGISTER_STD = Register
   structure G = Graph
   structure L = Liveness
   structure M = MakeGraph

   type allocation = R.register Temp.Table.table

   (* The color function talkes an initial allocation table (which assigns
      temporary variables such as FP or SP into certain fixed registers)
      plus an interference graph and a list of registers, and returns
      a new allocation table (mapping from temporaries to registers).

      Notice, you don't need to implement spilling and coalescing. 
      Just do the "simplify" and then do the "select".
    *)

   fun printNodes (x::xs) = (print (G.nodename(x) ^ "\n"); printNodes(xs))
     | printNodes ([]) = ()

   (* simplify builds the stack of nodes that can be colored one by one and
    * returns the stack   *)
   fun simplify(nodeList, stack, k, gt) =
         if List.length(stack) = List.length(nodeList) then stack
           else 
             let
               val toRemove = chooseNode (nodeList, nodeList, stack, k);
             in
               simplify(nodeList, toRemove::stack, k, gt)
             end

   (* choose node chooses a node with deg(node) < k that is not already in the
    * stack   *)
   and chooseNode(node::ns, nodeList, stack, k) = 
        let
          val node_degree = findDegree(node, stack);
        in
          (* if degree <= k - 1 and node has not already been removed *)
          if (node_degree < k andalso not(inList(node, stack)))
            then node
            else chooseNode(ns, nodeList, stack, k)
        end

     | chooseNode([], nodeList, stack, k) = cnHelp(nodeList, stack) 

  and cnHelp (node::ns, stack) = 
        if inList(node, stack) 
          then cnHelp(ns, stack)
          else node
     | cnHelp ([], stack) = hd(stack)    (* this should never happen *)

  and findDegree(node, stack) = 
       let
         val adj_nodes = M.removeDups(G.adj(node));
         val intrsct_stack = intersection(adj_nodes, stack);
         val t = if inList(node, adj_nodes) then 1 else 0;
         val deg = List.length(adj_nodes) - List.length(intrsct_stack) - t;
       in
         deg
       end

   and intersection(x::xs, l) = if inList(x, l) 
          then x::intersection(xs, l) else intersection(xs, l)
     | intersection([], l) = []

  and inList (x, y::ys) = if G.nodename(x) = G.nodename(y) then true else 
          inList(x, ys)
    | inList (x, []) = false

  (* end simplify *)


   (* begin rebuilding graph while coloring *)

   exception no_colors

   fun select(pop::stack_tail, registers, alloc, gtemp) =
        let
          val adj_nodes = M.removeDups(G.adj(pop));
          val adj_popped = removeStack(adj_nodes, stack_tail);
          val used_colors = M.removeDupsString(getColors(adj_popped, alloc,
                gtemp));
          val poss_colors = subColors(registers, used_colors);
          val color = if List.length(poss_colors) = 0 then raise no_colors
                 else hd(poss_colors)
          val temp = getOpt(Graph.Table.look(gtemp, pop), ~1);
          val new_alloc = Temp.Table.enter(alloc, temp, color);
        in
          select(stack_tail, registers, new_alloc, gtemp)
        end
        
     | select([], _, alloc, _) = alloc

   and removeStack(x::xs, cur_stack) = 
        if inList(x, cur_stack) then removeStack(xs, cur_stack) 
           else x::removeStack(xs, cur_stack)
     | removeStack([], cur_stack) = []
     
   and getColors (node::ns, alloc, gt) =
        let
          (* does this need to be accessed by node or by temporary *)
          val temp = getOpt(Graph.Table.look(gt, node), ~1);
          (* this only works when the fail option is a string, so I'm assuming
           * R.register is a string, and the colors are strings *)
          val color = getOpt(Temp.Table.look(alloc, temp), "");
        in
          if color = "" then getColors(ns, alloc, gt) else 
            color::getColors(ns, alloc, gt)
        end

     | getColors ([], alloc, gt) = []

  and subColors (x::xs, used_colors) =
       if inListString(x, used_colors) then subColors(xs, used_colors)
         else x::subColors(xs, used_colors)
     | subColors ([], used_colors) = []

   and inListString(x, y::ys) = if x = y then true else inListString(x, ys)
     | inListString(x, []) = false   

   (* end select *)


   fun color {interference as L.IGRAPH{graph, tnode, gtemp, moves}, initial, 
        registers} =
        let 
          val k = List.length(registers);
          val nodeList = G.nodes(graph);
          val stack = simplify(nodeList, [], k, gtemp);
          val rebuilt = select(stack, registers, initial, gtemp);
        in
          rebuilt
        end

    (* testing/printing functions *)

    fun printAlloc(alloc, nodeList, gt) = 
       (print("allocation:\n");
        paHelp(alloc, nodeList, gt)
       )

    and paHelp(alloc, node::ns, gt) = 
         let
           val t = valOf(Graph.Table.look(gt, node))
          in
             (
              print ("t" ^ Int.toString(t) ^ " assigned to " ^ 
                valOf(Temp.Table.look(alloc, t)) ^ "\n");
              paHelp(alloc, ns, gt)
             )
          end
      | paHelp (alloc, [], gt) = ()

   fun test () =
     (
      print "testing\n";
      let
        val (fgraph, nodeList) = M.instrs2graph(M.instrs);
        val (igraph as Liveness.IGRAPH{graph, tnode, gtemp, moves}, node2temp) = 
              L.interferenceGraph(fgraph);
        val initAlloc = Temp.Table.empty
        val alloc = color{interference=igraph, initial=initAlloc, 
              registers=["1", "2"]};
        val nodeList = G.nodes(graph);
      in
        printAlloc(alloc, nodeList, gtemp)
      end
     )

    (* end test/print *)

end (* functor RegAllocGen *)
