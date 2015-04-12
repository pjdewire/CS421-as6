(* frame.sml *)

signature FRAME =
sig 
  type offset = int
  type frame
  
  val newFrame : int -> frame * offset list
  val allocInFrame : frame -> offset

  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame} 
                | DATA of {lab : Temp.label, s: string}

end (* signature FRAME *)


structure Frame : FRAME = 
struct
  type offset = int
  type frame = {formals : int,         (* number of formal parameters *)
                offlst : offset list,  (* offset list for formals *)
                locals : int ref,      (* # of local variables so far *)
                maxargs : int ref}     (* max outgoing args for the function *)

  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame} 
                | DATA of {lab : Temp.label, s: string}

  local exception NotImplemented
  in
  fun newFrame _ = raise NotImplemented
  fun allocInFrame _ = raise NotImplemented
  end

end (* structure Frame *)

