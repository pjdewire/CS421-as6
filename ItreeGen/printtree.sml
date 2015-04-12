signature PRINTTREE = sig
    val printtree : TextIO.outstream * Tree.stm -> unit 
end

structure Printtree : PRINTTREE = 
struct

structure T = Tree

fun printtree (outstream, s0) =
  let fun say s =  TextIO.output(outstream,s)
      fun sayln s= (say s; say "\n") 
      val makestring = Int.toString

      fun indent 0 = ()
        | indent i = (say " "; indent(i-1))

      fun stm(T.SEQ(a,b),d) =
                 (indent d; sayln "SEQ("; stm(a,d+1); 
                  sayln ","; stm(b,d+1); say ")")
        | stm(T.LABEL lab, d) = 
                 (indent d; say "LABEL "; say (Symbol.name lab))
        | stm(T.JUMP (e,_), d) =  
                 (indent d; sayln "JUMP("; exp(e,d+1); say ")")
        | stm(T.CJUMP(T.TEST(r,a,b),t,f),d) = 
                 (indent d; sayln "CJUMP(";
                  relop r; sayln ",";
	          exp(a,d+1); sayln ","; 
                  exp(b,d+1); sayln ","; 
		  indent(d+1); say(Symbol.name t); 
	          say ","; say (Symbol.name f); say ")")
        | stm(T.MOVE(a,b),d) = 
                 (indent d; sayln "MOVE("; 
                  exp(a,d+1); sayln ",";
  		  exp(b,d+1); say ")")
        | stm(T.EXP e, d) = (indent d; sayln "EXP("; exp(e,d+1); say ")")

      and exp(T.BINOP(p,a,b),d) = 
                 (indent d; say "BINOP("; binop p; sayln ",";
		  exp(a,d+1); sayln ","; exp(b,d+1); say ")")
        | exp(T.CVTOP(p,e,s1,s2),d) =
                 (indent d; say "CVTOP["; say(makestring s1); 
  	          say ","; say (makestring s2); say "]("; 
 	          cvtop p; sayln ","; exp(e,d+1); say ")")
        | exp(T.MEM(e,s),d) = 
                 (indent d; say "MEM["; say(makestring s);
		  sayln "]("; exp(e,d+1); say ")")
        | exp(T.TEMP t, d) = (indent d; say "TEMP T"; say(makestring t))
        | exp(T.ESEQ(s,e),d) = 
                 (indent d; sayln "ESEQ("; stm(s,d+1); 
                  sayln ",";  exp(e,d+1); say ")")
        | exp(T.NAME lab, d) = 
                 (indent d; say "NAME "; say (Symbol.name lab))
        | exp(T.CONST i, d) = 
                 (indent d; say "CONST "; say(makestring i))
        | exp(T.CONSTF r, d) = 
                 (indent d; say "CONSTF "; say(Real.toString r))
        | exp(T.CALL(e,el),d) = 
                 (indent d; sayln "CALL("; exp(e,d+1);
		  app (fn a => (sayln ","; exp(a,d+2))) el;
		  say ")")

      and binop T.FPLUS = say "FPLUS"
        | binop T.FMINUS = say "FMINUS"
        | binop T.FDIV = say "FDIV"
        | binop T.FMUL = say "FMUL"
        | binop T.PLUS = say "PLUS"
        | binop T.MINUS = say "MINUS"
        | binop T.MUL = say "MUL"
        | binop T.DIV = say "DIV"
        | binop T.AND = say "AND"
        | binop T.OR = say "OR"
        | binop T.LSHIFT = say "LSHIFT"
        | binop T.RSHIFT = say "RSHIFT"
        | binop T.ARSHIFT = say "ARSHIFT"
        | binop T.XOR = say "XOR"

      and relop T.EQ = say "EQ"
        | relop T.NE = say "NE"
        | relop T.LT = say "LT"
        | relop T.GT = say "GT"
        | relop T.LE = say "LE"
        | relop T.GE = say "GE"
        | relop T.ULT = say "ULT"
        | relop T.ULE = say "ULE"
        | relop T.UGT = say "UGT"
        | relop T.UGE = say "UGE"
        | relop T.FEQ = say "FEQ"
        | relop T.FNE = say "FNE"
        | relop T.FLT = say "FLT"
        | relop T.FLE = say "FLE"
        | relop T.FGT = say "FGT"
        | relop T.FGE = say "FGE"

      and cvtop T.CVTSU = say "CVTSU"
        | cvtop T.CVTSS = say "CVTSS"
        | cvtop T.CVTSF = say "CVTSF"
        | cvtop T.CVTUU = say "CVTUU"
        | cvtop T.CVTUS = say "CVTUS"
        | cvtop T.CVTFS = say "CVTFS"
        | cvtop T.CVTFF = say "CVTFF"

   in stm(s0,0); sayln ""; 
      TextIO.flushOut outstream
  end  (* function printtree *)

end (* structure Printtree *)

