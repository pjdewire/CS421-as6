structure Absyn = struct

type pos = int
type symbol = Symbol.symbol

datatype var 
   = SimpleVar of symbol * pos
   | FieldVar of var * symbol * pos
   | SubscriptVar of var * exp * pos

and exp 
   = VarExp of var
   | NilExp
   | IntExp of int
   | StringExp of string * pos
   | AppExp of {func: symbol, args: exp list, pos: pos}
   | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
   | RecordExp of {fields: efield list, typ: symbol, pos: pos}
   | SeqExp of (exp * pos) list
   | AssignExp of {var: var, exp: exp, pos: pos}
   | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
   | WhileExp of {test: exp, body: exp, pos: pos}
   | ForExp of {var: vardec, lo: exp, hi: exp, body: exp, pos: pos}
   | BreakExp of pos
   | LetExp of {decs: dec list, body: exp, pos: pos}
   | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}

and dec 
   = FunctionDec of fundec list
   | VarDec of {var: vardec, typ: (symbol * pos) option, init: exp, pos: pos}
   | TypeDec of {name: symbol, ty: ty, pos: pos} list

and ty 
   = NameTy of symbol * pos
   | RecordTy of tfield list
   | ArrayTy of symbol * pos

and oper 
   = PlusOp | MinusOp | TimesOp | DivideOp
   | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype efield = symbol * exp * pos
     and tfield = {name: symbol, typ: symbol, pos: pos}
     and vardec = {name: symbol, escape: bool ref}
     and formals = {var: vardec, typ: symbol, pos: pos}
     and fundec = {name: symbol, params: formals list,
		   result: (symbol * pos) option,
		   body: exp, pos: pos}
end
        
