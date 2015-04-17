signature FRAME =
sig
  type offset = int
  type frame

  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame}
                | DATA of {lab : Temp.label, s: string}

  val wordSize: int

  (* Input integer is number of formals. *)
  val newFrame : int -> frame
  val offsetOfNthLocal : int -> offset
    (* Offset in the frame, of the n-th local variable. *)
  val offsetOfNthParam : int -> offset
  val allocInFrame : frame -> offset
  val contentsOf : Tree.exp -> Tree.exp
    (* Returns the contents of wordSize bytes starting at the Tree.exp, which
    corresponds to an address in memory. *)
  val externalCall : string * Tree.exp list -> Tree.exp
    (* Calls a runtime-system function. Args are the name of the function, and a
       list of arguments. *)
end (* signature FRAME *)


structure Frame : FRAME = 
struct

  structure T = Tree
  structure Te = Temp
  structure Er = ErrorMsg
  structure S = Symbol
  structure R : REGISTER_STD = Register

  type offset = int
  type frame = {numFormals : int,      (* number of formal parameters *)
                offlst : offset list,  (* offset list for formals *)
                locals : int ref,      (* # of local variables so far *)
                maxargs : int ref}     (* max outgoing args for the function *)

  datatype frag = PROC of {name : Te.label, body: T.stm, frame: frame} 
                | DATA of {lab : Te.label, s: string}

  val wordSize = 4

  (* The offsets of the local variables are
     LBO, LBO - 4, LBO - 8, ..., LBO - 4 * (n-1), where LBO is localsBaseOffset
     defined in register.sml. *)
  fun offsetOfNthLocal(n) = R.localsBaseOffset - wordSize * (n - 1)
  fun offsetOfNthParam(n) = R.paramBaseOffset + wordSize * n

  (* Generate the offsets of the incoming arguments.
     The offsets for FUNCTION PARAMETERS are at
     PBO + 4, PBO + 8, ... , PBO + 4 * n, where PBO is the paramBaseOffset
     defined in register.sml.
     Static link occupies bytes 8-11. *)
  fun generateOffsets n =
    if n = 0 then [] else generateOffsets (n-1) @ [offsetOfNthParam(n)]

  local exception NotImplemented
  in

  fun newFrame numFormals =
    {numFormals=numFormals, offlst=generateOffsets(numFormals), locals=ref 0,
     maxargs=ref 2 (* Easy fix! *)}

  fun allocInFrame (frame : frame) = (
    let
      val numLocals = !(#locals(frame)) + 1
    in
      (* Number of local variables increases by 1. *)
      #locals(frame) := numLocals;
      offsetOfNthLocal(numLocals)
    end
  )
  end

  fun contentsOf(exp) = T.MEM(exp, wordSize)

  fun externalCall(str, args) = T.CALL(T.NAME(Te.namedlabel str), args)
end (* structure Frame *)
