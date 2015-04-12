(* tree.sml *)

signature TREE = sig 

type size

datatype stm = SEQ of stm * stm
             | LABEL of Temp.label
             | JUMP of exp * Temp.label list
             | CJUMP of test * Temp.label * Temp.label
	     | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | CVTOP of cvtop * exp * size * size
             | MEM of exp * size
             | TEMP of Temp.temp
             | ESEQ of stm * exp
             | NAME of Temp.label
             | CONST of int
             | CONSTF of real
	     | CALL of exp * exp list

     and test = TEST of relop * exp * exp

     and binop = FPLUS | FMINUS | FDIV | FMUL
               | PLUS | MINUS | MUL | DIV 
               | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

     and relop = EQ | NE | LT | GT | LE | GE 
               | ULT | ULE | UGT | UGE
	       | FEQ | FNE | FLT | FLE | FGT | FGE

     and cvtop = CVTSU | CVTSS | CVTSF | CVTUU 
               | CVTUS | CVTFS | CVTFF

val notRel : relop -> relop
val commute: relop -> relop

end (* signature TREE *)



structure Tree : TREE = struct

type label = Temp.label
type temp = Temp.temp
type size = int

datatype stm = SEQ of stm * stm
             | LABEL of label
             | JUMP of exp * label list
             | CJUMP of test * label * label
	     | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | CVTOP of cvtop * exp * size * size
             | MEM of exp * size
             | TEMP of temp
             | ESEQ of stm * exp
             | NAME of label
             | CONST of int
             | CONSTF of real
	     | CALL of exp * exp list

     and test = TEST of relop * exp * exp

     and binop = FPLUS | FMINUS | FDIV | FMUL
               | PLUS | MINUS | MUL | DIV 
               | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

     and relop = EQ | NE | LT | GT | LE | GE 
               | ULT | ULE | UGT | UGE
               | FEQ | FNE | FLT | FLE | FGT | FGE

     and cvtop = CVTSU | CVTSS | CVTSF | CVTUU 
               | CVTUS | CVTFS | CVTFF


fun notRel EQ = NE 
  | notRel NE = EQ
  | notRel LT = GE
  | notRel GE = LT
  | notRel GT = LE 
  | notRel LE =  GT
  | notRel ULT = UGE
  | notRel UGE = ULT
  | notRel ULE = UGT 
  | notRel UGT =  ULE
  | notRel FEQ = FNE 
  | notRel FNE = FEQ
  | notRel FLT = FGE 
  | notRel FGE = FLT
  | notRel FGT = FLE
  | notRel FLE = FGT

fun commute EQ = EQ
  | commute NE = NE
  | commute LT = GT
  | commute GE = LE
  | commute GT = LT 
  | commute LE =  GE
  | commute ULT = UGT
  | commute UGE = ULE
  | commute ULE = UGE 
  | commute UGT =  ULT
  | commute FEQ = FEQ 
  | commute FNE = FNE
  | commute FLT = FGT 
  | commute FGE = FLE
  | commute FGT = FLT
  | commute FLE = FGE

end (* structure Tree *)

