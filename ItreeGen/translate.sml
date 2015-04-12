(* translate.sml *)

signature TRANSLATE = 
sig 
  type level
  type access
  type frag

  (*
   * val outermost : level
   * val newLevel : {parent: level, formals: 'a list} -> 
   *                                   (level * (('a * access) list))
   * val allocInFrame : level -> access
   * val allocInRegister : unit -> access
   *)

  val getResult : unit -> frag list

  type gexp

  (* ... *)

end (* signature TRANSLATE *)


functor TranslateGen(Register : REGISTER_STD) : TRANSLATE = 
struct

  structure F = Frame
  structure Tr = Tree
  structure T = Temp
  structure Er = ErrorMsg

  datatype level = LEVEL of {frame : F.frame,              
                             sl_offset : int,
                             parent : level} * unit ref
                 | TOP

  type access = level * int  (* might needs to be changed later *)
  type frag = F.frag

  val fragmentlist = ref ([] : frag list)
  fun getResult () = !fragmentlist

  datatype gexp = Ex of Tr.exp
                | Nx of Tr.stm
                | Cx of T.label * T.label -> Tr.stm

  (* ...... details ....... *)

end (* functor TranslateGen *)





     
