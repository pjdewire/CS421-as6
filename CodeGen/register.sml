(* register.sml *)

signature REGISTER =
sig 
  include REGISTER_STD

  val ECX : Temp.temp
  val EBX : Temp.temp
  val ESI : Temp.temp
  val EDI : Temp.temp
  val EDX : Temp.temp
  val ERROR : Temp.temp

 (* we maintain a separate list here of true callersaves, so that
  * CodeGen will not emit code to "save" the pseudo-registers, since
  * they already live on the stack.
  *)
  val truecallersaves : Temp.temp list (* CodeGen use only! *)

  (* number of pseudo-registers: *)
  val NPSEUDOREGS : int  (* CodeGen use only! *)
  val availregs : register list (* this is exported for RegAlloc! *)
  

end (* signature REGISTER *)


structure Register : REGISTER = 
struct

  type register = string

  val RV = Temp.newtemp() (* EAX *)
  val FP = Temp.newtemp() (* EBP *)
  val SP = Temp.newtemp() (* ESP *)

  val EBX = Temp.newtemp()
  val ECX = Temp.newtemp() (* saved/not for regalloc *)
  val EDX = Temp.newtemp() (* saved/not for regalloc *)
  val ESI = Temp.newtemp()
  val EDI = Temp.newtemp()
  val ERROR = Temp.newtemp()

  val NPSEUDOREGS = 50 (* change this to the proper value *)
  val localsBaseOffset : int = ~4 + (NPSEUDOREGS * ~4)  (* change this to the proper value *)
  val paramBaseOffset : int = 8  (* change this to the proper value *)

  val specialregs : (Temp.temp * register) list = [(RV,"eax"),(FP,"ebp"),(SP,"esp"),(EBX,"ebx"),(ECX,"ecx"),(EDX,"edx"),(ESI,"esi"),(EDI,"edi")]

  val argregs : (Temp.temp * register) list = [] (* not implementing *)
  val calleesaves : register list = [] (* not implementing *)
  val truecallersaves : Temp.temp list = [RV,ECX,EDX]
  val callersaves : register list = [] (* Not needed *)

  (* below should have EAX, EBX, ESI, EDI *) 
  (* no ECX or EDX either, these are used for loading pseudo *)

  (* generating the availalable registers, in particular pseudos *)
  val availregs_base = ["ebx","esi","edi"]
  fun genavailregs 0 = availregs_base
   | genavailregs n = ("f"^Int.toString(n))::genavailregs(n-1)

  val availregs = rev(genavailregs(NPSEUDOREGS))



end (* structure Register *)


