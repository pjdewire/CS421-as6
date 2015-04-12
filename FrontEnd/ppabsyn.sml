(*  PP - Pretty Printer for abstract syntax
    Use as follows:
      PP.pp e         where e : Absyn.exp
    Or use my test functions TS & TN to print the abstract syntax of the
    test cases.
 *)

signature PPSIG =
sig 
  val pp : Absyn.exp -> string
  val TS : string -> unit
  val TN : int -> unit
end

structure PP : PPSIG = struct

local open Absyn
      fun decode s = "\"" ^ s ^ "\""   (* later decode \n et al *)
      fun N s = Symbol.name s
      fun LF r = ListFormat.formatList r
in
   fun pp e =
     case e 
      of VarExp v                    => ppv v
       | NilExp                      => "Nil"
       | IntExp i                    => makestring i
       | StringExp (s,_)             => decode s
       | AppExp{func,args,pos}       => LF{init= N func ^ "(",
                                           sep= ", ",final= ")",fmt= pp} args
       | OpExp{left,oper,right,pos}  => ("(" ^ pp left ^ ppo oper ^ 
                                         pp  right ^ ")")
       | RecordExp {typ,fields,pos}  => LF{init= N typ ^ "{",sep= ", ",
                                           final= "}",
                                           fmt= (fn (s,e,_)=> N s^"  = "^pp e)}
                                         fields
       | SeqExp (eps)                => LF{init="(",sep="; ",final=")",
                                          fmt= pp o #1} eps
       | AssignExp{var,exp,pos}      => ppv var ^ " := "  ^ pp exp
       | IfExp {test,then',else'=NONE,pos} => "if " ^ pp test
                                              ^ " then " ^ pp then'
       | IfExp {test,then',else'=SOME(e),pos} => "if " ^ pp test ^ " then "
                                             ^ pp then' ^ " else " ^ pp e
       | WhileExp{test,body,pos}     => "while " ^ pp test ^ " do "  ^ pp body
       | ForExp{var,lo,hi,body,pos}  => "for " ^ N (#name var)
                                        ^ " := " ^ pp lo ^ " to "
                                        ^ pp hi ^ " do " ^ pp body
       | BreakExp _                  => "break"
       | LetExp {decs,body,pos}      => "let\n" ^ ppds decs
                                        ^ "\nin " ^ pp body ^ " end"
       | ArrayExp{typ,size,init,pos} => N typ ^ "[" ^ pp size ^ "] of " 
                                        ^ pp init
   and ppv v =
     case v 
      of SimpleVar (s,p)     => N s
       | FieldVar (v,s,p)    => ppv v ^ "." ^ N s
       | SubscriptVar(v,e,p) => ppv v ^ "[" ^ pp e ^ "]"

   and ppfd {name,params,result,body,pos} = 
           "function " ^ N name ^ LF{init="(",sep=", ",
                                     final=")",fmt= ppfms} params
           ^ (case result of NONE        => ""
                           | SOME (ty,_) => "  : " ^ N ty)
           ^ " = " ^ pp body ^ "\n"

   and pptd {name,ty,pos} = "type " ^ N name ^ " = " ^ ppt ty ^ "\n"

   and ppd (FunctionDec (fd::fds)) = ppfd fd ^ ppd (FunctionDec fds)
     | ppd (FunctionDec [])        = ""
     | ppd (TypeDec (td::tds))     = pptd td ^ ppd (TypeDec tds)
     | ppd (TypeDec [])            = ""
     | ppd (VarDec{var,typ,init,pos}) = 
           "var " ^ N (#name var) ^ (case typ 
                                      of NONE      => ""
                                       | SOME(n,_) => " : " ^ N n)
           ^ " := " ^ pp init

   and ppds (d::ds) = ppd d ^ ppds ds
     | ppds []      = ""

   and ppt (NameTy(s,_))  = N s
     | ppt (RecordTy fields) = LF{init="{",sep=", ",final="}",fmt=ppf} fields
     | ppt (ArrayTy(s,_)) = "array of " ^ N s

   and ppo oper =
     case oper 
      of PlusOp  => "+"
       | MinusOp => "-"
       | TimesOp => "*"
       | DivideOp=> "/"
       | EqOp    => "="
       | NeqOp   => "<>"
       | LtOp    => "<"
       | LeOp    => "<="
       | GtOp    => ">"
       | GeOp    => ">="

   and ppf {name,typ,pos} = N name ^ " : " ^ N typ

   and ppfms {var,typ,pos} = N (#name var) ^ " : " ^ N typ

 end

fun TS(s) =
  ((print o pp o Parse.parse)
     ("/homes/classes/cs421/as/testcases/" ^ s ^ ".toy");
   print "\n")

fun TN(n :int) = TS("test" ^ makestring n)

end


