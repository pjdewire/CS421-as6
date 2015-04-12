(* reg-std.sig *)

signature REGISTER_STD = 
sig
  type register = string

  val RV : Temp.temp  (* return value *)
  val FP : Temp.temp  (* frame pointer *)

  val SP   : Temp.temp (* stack pointer *)

(****
  val TOC  : Temp.temp
  val RA   : Temp.temp
  val ZERO : Temp.temp
  val ARG1 : Temp.temp
  val ARG2 : Temp.temp
  val ARG3 : Temp.temp
  val ARG4 : Temp.temp
  val ARG5 : Temp.temp
  val ARG6 : Temp.temp
  val ARG7 : Temp.temp
  val ARG8 : Temp.temp
  val argregs : (Temp.temp * register) list
 ****)

  val paramBaseOffset : int
  val localsBaseOffset : int
  val specialregs : (Temp.temp * register) list
  val calleesaves : register list
  val callersaves : register list

end  (* signature REGISTER_STD *)



