
signature CODEGEN =
sig
  structure F : FRAME
  structure R : REGISTER

  (* translate each canonical tree into a list of assembly instructions *)
  val codegen : Tree.stm -> Assem.instr list 

  (* converting a string fragment into the actual assembly code *)
  val string : Temp.label * string -> string

  (* procEntryExit sequence + function calling sequence tune-up 
   * + mapping pseudo-registers to memory load/store instructions 
   * and actual registers.
   * This is a post-pass, to be done after register allocation.
   *)

  val munchStm : Tree.stm -> unit
  val munchExp : Tree.exp -> Assem.temp

  val procEntryExit : {name : Temp.label, 
                          body : (Assem.instr * Temp.temp list) list,
                          allocation : R.register Temp.Table.table,
                          frame : Frame.frame} -> Assem.instr list

end (* signature CODEGEN *)

structure Codegen : CODEGEN =
struct

 structure T = Tree
 structure A = Assem
 structure Er = ErrorMsg
 structure F = Frame
 structure R = Register
 structure S = Symbol
 structure TE = Temp

 fun x86_toString(n) = if (n >= 0) then Int.toString(n) else ("-"^Int.toString(0-n))
 fun iassem(n) = "$"^x86_toString(n)


 fun operassem T.PLUS = "addl"
 | operassem T.MINUS = "subl"
 | operassem T.MUL = "imull"
 | operassem T.DIV = "idivl"
 | operassem T.AND = "andl"
 | operassem T.OR = "orl"
 | operassem T.XOR = "xorl"

 fun rassem T.EQ =  "je"
 	| rassem T.NE = "jne"
 	| rassem T.LT = "jl"
 	| rassem T.GT = "jg"
 	| rassem T.LE = "jle"
 	| rassem T.GE = "jge"
 	| rassem T.ULT = "jb"
 	| rassem T.ULE = "jbe"
 	| rassem T.UGT = "ja"
 	| rassem T.UGE = "jae"

 val ilist = ref (nil:A.instr list)
 fun emit x = ilist := x :: !ilist



 fun munchStm(T.MOVE(T.TEMP t, T.CALL(T.NAME lab, args))) = 
 		( (*pushCallerSaves(R.truecallersaves); *)
 		emit(A.OPER{assem = "pushl %`s0\n",src=[R.RV],dst=[R.SP],jump=NONE}) ;
 		emit(A.OPER{assem="IN",src=[],dst=[],jump=NONE});
 		munchArgs(args);
 		emit(A.OPER({assem = "call " ^ S.name(lab)^"\n",src=[],dst=[R.RV],jump=NONE}));
 		(if length(args) > 0 then popArgs(args) else ());
 		emit(A.MOVE{assem="movl %eax,%`d0\n",src=R.RV,dst=t}) ;
 		emit(A.OPER{assem="OUT",src=[],dst=[],jump=NONE}) ;
 		emit(A.OPER({assem = "popl %`d0\n",src=[R.SP],dst=[R.RV,R.SP],jump=NONE}))
 		(* popCallerSaves(R.truecallersaves) *)
 		)

 	| munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i),_),e2)) =
 		emit(A.OPER{assem="movl %`s0,"^x86_toString(i)^"(%`s1)\n",
 		src=[munchExp e2, munchExp e1],dst=[],jump=NONE})

 	| munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST(i), e1),_),e2)) = 
 		emit(A.OPER{assem="movl %`s0,"^x86_toString(i)^"(%`s1)\n",
 		src=[munchExp e2, munchExp e1],dst=[],jump=NONE})

 	| munchStm(T.MOVE(T.MEM(e1,_),T.MEM(e2,_))) = 
 		let val t = TE.newtemp() 
 		val (me1,me2) = (munchExp e1, munchExp e2)
 		in 
 		(emit(A.MOVE{assem="movl (%`s0),%`d0\n",
 			src=me2,dst=t});
 		emit(A.OPER{assem="movl %`s0,(%`s1)\n",
 			src=[t,me1],dst=[],jump=NONE}))
 		end

 	| munchStm(T.MOVE(T.MEM(e1,_),e2)) = 
 		emit(A.OPER{assem="movl %`s0,(%`s1)\n",
 			src=[munchExp e2, munchExp e1],dst=[],jump=NONE})

 	| munchStm(T.MOVE(T.TEMP i,e2)) = 
 		emit(A.OPER{assem="movl %`s0,%`d0\n",
 			src=[munchExp e2],dst=[i],jump=NONE})


 	| munchStm(T.JUMP(T.NAME(lab),list)) = 
 		emit(A.OPER{assem = "jmp "^S.name(lab)^"\n",
 			src=[],dst=[],jump=SOME(list)})

 	| munchStm(T.CJUMP(T.TEST(relop,e1,e2),ltrue,lfalse)) = 
 		(emit(A.OPER{assem="cmpl %`s0,%`s1\n",
 			src=[munchExp e2,munchExp e1],dst=[],jump=NONE});
 		emit(A.OPER({assem=rassem(relop)^" "^S.name(ltrue)^"\n",
 			src=[],dst=[],jump=SOME[ltrue,lfalse]})))
 			
 	| munchStm(T.LABEL(lab))=
 		emit(A.LABEL{assem=S.name(lab)^":\n", lab=lab})

 	(* not sure this works!! 
 	| munchStm(T.JUMP(e,list)) = 
 		emit(A.OPER{assem = "jmp %`s0\n",
 			src=[munchExp e],dst=[],jump=SOME(list)}) *)

 	| munchStm(T.EXP(e)) =  
 		(munchExp e ; ())

 	| munchStm _ = (Er.impossible "Munch stm did not match";())

 and result(gen) = let val t = TE.newtemp() in gen t ; t end

 and pushCallerSaves([]) = ()
 	| pushCallerSaves(a::r) = 
 	(emit (A.OPER({assem = "pushl %`s0\n",src=[a],dst=[R.SP],jump=NONE}));
 	pushCallerSaves(r))

and popArgs(args) = emit(A.OPER{assem = "addl "^ iassem(4 * (length args)) ^",%esp\n",
	src=[R.SP],dst=[R.SP],jump=NONE})

and popCallerSaves([]) = () 
	| popCallerSaves(a::r) = 
	(popCallerSaves(r);
	emit (A.OPER({assem = "popl %`d0\n",src=[R.SP],dst=[a,R.SP],jump=NONE})))

and munchArgs([]) = ()
	| munchArgs(a::r) = 
	(munchArgs(r) ;
	emit(A.OPER({assem = "pushl %`s0\n",src=[munchExp a],dst=[R.SP],jump=NONE})))

 and munchExp(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i),_))=
 		result(fn r => 
 		emit(A.OPER({assem="movl "^ x86_toString(i)^"(%`s0),%`d0\n",
 			src=[munchExp e1],dst=[r],jump=NONE})))

 	| munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1),_)) = 
 		result(fn r => 
 		emit(A.OPER({assem="movl "^ x86_toString(i) ^"(%`s0),%`d0\n",
 			src=[munchExp e1],dst=[r],jump=NONE})))

 	| munchExp(T.MEM(T.CONST i,_)) = 
 	 	result(fn r => 
 		emit(A.OPER({assem="movl ("^iassem(i)^"),%`d0\n",
 			src=[],dst=[r],jump=NONE})))

 	| munchExp(T.MEM(e1,_)) = 
 		result(fn r =>
 		emit(A.OPER({assem = "movl (%`s0),%`d0\n",
 			src=[munchExp e1],dst=[r],jump=NONE})))

 	| munchExp(T.CONST i)=
 		result(fn r =>
 		emit(A.OPER({assem = "movl "^iassem(i)^",%`d0\n",
 			src=[],dst=[r],jump=NONE})))

 	(* e1/e2 *)
	| munchExp(T.BINOP(T.DIV,e1,e2)) = 
		(emit(A.OPER({assem = "movl $0,%edx\n",src=[],dst=[R.EDX],jump=NONE}));
		emit(A.OPER({assem = "movl %`s0,%eax\n",src=[munchExp e1],dst=[R.RV],jump=NONE}));
		emit(A.OPER({assem = "idivl %`s0\n",src=[munchExp e2,R.RV],dst = [R.RV],jump=NONE}));
		R.RV)

	(* e1-e2 -> temp *)
 	| munchExp(T.BINOP(oper,e1,e2)) = 
 		result(fn r =>
 		let val (me1,me2) = (munchExp e1,munchExp e2) in
 		(emit(A.MOVE({assem = "movl %`s0,%`d0\n",src=me1,dst=r}));
 		emit(A.OPER({assem = operassem(oper)^" %`s0,%`d0\n",src=[me2],dst=[r],jump=NONE})))
 		end)

 	(* [RV=EAX,EBX,ECX,EDX,ESI,EDI] = caller saves *)
 	(* write calldefs! *)

 	| munchExp(T.CALL(T.NAME lab,args)) = 
 		let val t = TE.newtemp() in
 		( emit(A.OPER{assem="IN",src=[],dst=[],jump=NONE});
 		(*pushCallerSaves(R.truecallersaves); *)
 		munchArgs(args);
 		emit(A.OPER({assem = "call " ^ S.name(lab)^"\n",src=[],dst=[R.RV],jump=NONE}));
 		(if length(args) > 0 then popArgs(args) else ());
 		emit(A.OPER{assem="movl %eax,%`d0\n",src=[R.RV],dst=[t],jump=NONE}) ;
 		(* popCallerSaves(R.truecallersaves); *)
 		emit(A.OPER{assem="OUT",src=[],dst=[],jump=NONE}) ;
 		t
 		)
 		end

 	| munchExp(T.NAME(lab)) = 
 		result(fn r =>
 		emit(A.OPER({assem="leal " ^ S.name(lab) ^ ",%`d0\n",
 			src=[],dst=[r],jump=NONE})))

 	| munchExp(T.TEMP t) = t

 	| munchExp _ = (Er.impossible "Munch exp did not match" ; R.ERROR)

 
 fun codegen itree = ((ilist := (nil:A.instr list)) ; munchStm itree ; 
 	let val ret = rev(!ilist) in ((ilist := (nil:A.instr list));ret) end)

  (* converting a string fragment into the actual assembly code *)
  fun string(lab,s) = 
  	(S.name(lab) ^ ":\n.4byte " ^ Int.toString(String.size(s)) ^ "\n.ascii \"" ^ String.toCString(s) ^ "\"\n")


  (* regname -- produce an assembly language name for the given machine
   * register or psuedo-register.
   * psuedo-registers are mapped to an expression for psuedo-register's
   * location in stack frame.
   *)
  (* regname : R.register -> string *)
  fun regname reg =
      if (String.isPrefix "f" reg) then
	  (* it's a psuedo-register *)
	  let
	      val (SOME prNum) = Int.fromString (String.extract(reg,1,NONE));
	      val offset = (prNum + 1) * 4
	  in
	      "-" ^ Int.toString(offset) ^ "(%ebp)"
	  end
      else
	  reg

 (* genSpills -- do our "poor man's spilling".  Maps all pseudo-register
  * references to actual registers, by inserting instructions to load/store
  * the pseudo-register to/from a real register
  *)

 	fun genSpills (insns,saytemp) =
     let
	  (* loadit() -- given name of a source register src, and a true
	   * machine register mreg, will return a load instruction (if needed)
	   * and a machine register.
		loadit: Temp.temp * Temp.temp -> string * Temp.temp *)
	  fun loadit (src,mreg) =
	      let 
		  val srcnm = (saytemp src)
	      in
		  if (String.isPrefix "f" srcnm) then
		      (* it's a fake register: *)
		      let
			  (* val _ = print ("loadit(): mapping pseudo-register `" ^ srcnm ^
              * "' to machine reg. `" ^ (saytemp mreg) ^"'\n"); *)
			  val loadInsn = "\tmovl\t" ^ (regname srcnm) ^ ", %" ^ (saytemp mreg) ^ " # load pseudo-register\n"
		      in
			  (loadInsn, mreg)
		      end
		  else
		      (* no mapping needed *)
		      ("", src)
	      end

	  (* mapsrcs : produce a sequence of instructions to load
	   * pseudo-registers into real registers, and produce a list
	   * of sources which reflects the real registers.
	   *)
	  (* mapsrcs : Temp.temp list * Temp.temp list -> string * Temp.temp list *)
	  fun mapsrcs ([],_) = ("",[])
	    | mapsrcs (src::srcs,mreg::mregs) =
              let
                  val (loadInsn, src') = loadit(src,mreg)
                  val (loadRest, srcs') = mapsrcs(srcs,mregs)
              in
                  (loadInsn ^ loadRest, src'::srcs')
              end
	  (* findit -- like List.find, but returns SOME i, where i is index
	   * of element, if found
	   *)
          fun findit f l =
	      let
		  fun dosrch([],f,_) = NONE
		    | dosrch(el::els,f,idx) =
		      if f(el) then
			  SOME idx
		      else
			  dosrch(els,f,idx+1)
	      in
		  dosrch(l,f,0)
	      end

	  (* mapdsts -- after we have mapped sources to real machine
	   * registers, iterate through dsts.
	   * If dst is a pseudo-register then
	   *    if dst was also a src,
	   *         replace dst with machine register to which src is already
	   *         mapped
	   *    else
	   *         map dst to its own machine register (just use %ecx)
	   *    generate a store insn for dst to save result
	   *)
          (* mapdsts : Temp.temp list * Temp.temp list * Temp.temp list ->
	   *           string * Temp.temp list
	   *)
          (* N.B.!  This only works for dst of length 0 or 1 !! *)
          (* pre: length(dsts) <= 1 *)
	  fun mapdsts([],_,_) = ("",[])
	    | mapdsts(dst::dsts,srcs,newsrcs) =
	      let
		  val found = findit (fn e => e=dst) srcs
		  val dstnm = (saytemp dst)
	      in
		  if (isSome(found)) then
		      (* this dst is also a source *)
		      let
			  val idx=valOf(found)
			  val src=List.nth (srcs,idx)
			  val mreg=List.nth (newsrcs,idx)
		      in
			  if (src <> mreg) then
			      ("\tmovl\t%`d0, " ^ (regname dstnm) ^ " # save pseudo-register\n", mreg::dsts)
			  else
			      (* no mapping *)
			      ("", dst::dsts)
		      end
		  else
		      (* this dst isn't a source, but it might be a pseudo-register *)
                      if (String.isPrefix "f" dstnm) then
                          (* it's a fake register: *)
                          (* we can safely just replace this destination with
                           * %ecx, and then write out %ecx to the pseudo-register
                           * location.  Even if %ecx was used to hold a different
                           * source pseudo-register, we won't end up clobbering
                           * it until after the source has been used...
                           *)
                          ("\tmovl\t%`d0, " ^ (regname dstnm) ^ " # save pseudo-register\n", R.ECX::dsts)
                      else
                          (* no mapping *)
                          ("", dst::dsts)
	      end

	  fun mapInstr(A.OPER{assem=insn, dst=dsts, src=srcs, jump=jmp}) = 
	      let
		  val (loadinsns, newsrcs) = mapsrcs(srcs, [R.ECX, R.EDX]);
          val (storeinsns, newdsts) = mapdsts(dsts, srcs, newsrcs); 
	      in
		  A.OPER{assem=loadinsns ^ insn ^ storeinsns,
			 dst=newdsts, src=newsrcs, jump=jmp}
	      end
	    | mapInstr(instr as A.LABEL _) = instr
	    | mapInstr(A.MOVE{assem=insn,dst=dsts,src=srcs}) = 
	    let
          val (loadinsns, newsrcs) = mapsrcs([srcs], [R.ECX, R.EDX]);
          val (storeinsns, newdsts) = mapdsts([dsts], [srcs], newsrcs);
        in
          A.OPER{assem=loadinsns ^ insn ^ storeinsns,
             dst=newdsts, src=newsrcs, jump=NONE}
        end
     in
         map mapInstr insns
     end

   fun procEntryExit({name=name,body=body,allocation=allocation,frame=frame:(F.frame)})=
 	(* push ebp onto stack *)
 	(* then the mov ebp,esp setting the new frame pointer to be caller's stack pointer
	decrement the stack pointer by frame size *) 
	(* glue in the tuned up code *)
	(* move the ebp to esp  *)
	(* call "ret" (no arguments) *)

	(let 
	fun instrs_only([]) = []
		| instrs_only ((instr,temps)::r) = instr :: (instrs_only(r))

	val displ = (!(#locals(frame)))*4 + R.NPSEUDOREGS*4 
	fun saytemp temp = valOf(TE.Table.look(allocation,temp))
	val lab = S.name(name)
	val entryseq = (".globl " ^ lab ^ "\n" ^ 
	".type " ^ lab ^ ", @function\n" ^ 
	lab ^ ":\n" ^ 
	"pushl %ebp\n" ^ 
	"movl %esp,%ebp\n" ^ 
	"subl $"^Int.toString(displ)^ ",%esp\n" ^
	"pushl %ebx\n" ^
    "pushl %edi\n" ^
    "pushl %esi\n"
	)
	val exitseq = (
	"popl %esi\n" ^
    "popl %edi\n" ^
    "popl %ebx\n" ^
    "movl %ebp,%esp\n" ^ 
	"popl %ebp\n" ^ 
	"ret\n")

		fun caller_save([]) = []
		| caller_save( (A.OPER{assem="IN",...}) ::r ) =
			(A.OPER({assem = "pushl %ecx\n",src=[],dst=[],jump=NONE}) ::
			A.OPER({assem = "pushl %edx\n",src=[],dst=[],jump=NONE}) :: 
			caller_save(r))
		| caller_save( (A.OPER{assem="OUT",...}) :: r) = 
			(A.OPER({assem = "popl %edx\n",src=[],dst=[],jump=NONE}) ::
			A.OPER({assem = "popl %ecx\n",src=[],dst=[],jump=NONE}) :: 
			caller_save(r))
		| caller_save( a::r ) = a::caller_save(r)

	in
	(A.OPER({assem=entryseq, src=[],dst=[],jump=NONE}) ::
	(genSpills(caller_save(instrs_only body),saytemp) @
	[A.OPER({assem=exitseq ,src=[],dst=[],jump=NONE})]))
	end)

end (* structure Codegen *)
