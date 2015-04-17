signature SEMANT =
sig
  type ir_code
  val transprog : Absyn.exp -> Frame.frag list
end

functor SemantGen (Register: REGISTER_STD) : SEMANT =

struct

  structure A = Absyn
  structure Translate = TranslateGen(Register)
  structure E = EnvGen(Translate)
  structure S = Symbol
  structure T = Types
  structure Tr = Translate
  structure F = Format
  val error = ErrorMsg.error
  val breakCnt = ref [0]
  val HACKsym = S.symbol("")
  val HACK = ref ()   (* unique for keeping track of FOR loop counter types *)
  type ir_code = Tr.exp

  (********************************************
   *              Error Messages              *
   ********************************************)

  (* Expression errors *)
  val msgAppExp01  = "Type mismatch"
  val msgAppExp02  = "Function called with wrong number of parameters."
  val msgAppExp03  = "Identifier is not bound to a function.  Assuming function returns INT."
  val msgAppExp04  = "Undefined identifier in function call.  Assuming function returns INT."
  val msgArithExp01= "Left operand of arithmetic expression does not evaluate to an INT."
  val msgArithExp02= "Right operand of arithmetic expression does not evaluate to an INT."
  val msgArrayExp01= "Type of array size does not evaluate to an INT."
  val msgArrayExp02= "Type of array initializer does not agree with array declaration."
  val msgArrayExp03= "Attempt to assign a variable to a non-array type."
  val msgArrayExp04= "Attempt to declare an array of an unknown type."
  val msgAssignExp01= "Attempt to assign to a FOR loop induction variable."
  val msgAssignExp02= "Type of rvalue in assignment expression does not match lvalue."
  val msgBreakExp  = "BREAK expression found outside of WHILE or FOR loop."
  val msgCmpExp01  = "Illegal type for order comparison "
  val msgCmpExp02  = "Type mismatch in order comparison "
  val msgForExp01  = "LO condition in FOR statement does not evaluate to an INT."
  val msgForExp02  = "HI condition in FOR statement does not evaluate to an INT."
  val msgForExp03  = "Body expression in FOR statement does not evaluate to UNIT."
  val msgIfExp01   = "Test expression in IF statement does not evaluate to an INT."
  val msgIfExp02   = "THEN expression in IF..THEN statement does not evaluate to UNIT."
  val msgIfExp03   = "Type mismatch between THEN and ELSE clauses.  Assuming expression is an INT."
  val msgOpEqExp01 = "Cannot compare NIL with NIL (\"=\")."
  val msgOpEqExp02 = "Type mismatch in equality comparison \"=\"."
  val msgOpNeqExp01= "Cannot compare NIL with NIL (\"<>\")"
  val msgOpNeqExp02= "Type mismatch in inequality comparison \"<>\"."
  val msgRecordExp01= "Attempt to create an undefined record type."
  val msgRecordExp02= "Record type specifier is not defined as a record type."
  val msgRecordExp03= "Record binding incorrect."
  val msgRecordExp04 = "Type of field `%s' initializer does not agree with record declaration."
  val msgRecordExpMissingField = "No binding found for field `%s'."
  val msgRecordExpExtraField = "Extraneous binding of field"

  val msgWhileExp01= "Test expression in WHILE statement does not evaluate to an INT."
  val msgWhileExp02= "Body expression in WHILE statement does not evaluate to UNIT."

  (* Var errors *)
  val msgH0x       = "Undefined type in variable expression."
  val msgH00       = "Identifier is not of valid lvalue type, it is a function name ("
  val msgH01       = "Variable ("
  val msgH01b      = ") is of an unknown type, or the variable has not been defined."
  val msgH02       = "Attempt to access an undefined record field ("
  val msgH03       = "Attempt to access a field of a non-record type."
  val msgH04       = "Expression for array index does not resolve to an INT."
  val msgH05       = "Attempt to index into a non-array variable."

  (* Utility function errors *)
  val breakFatalMsg= "FATAL ERROR. While/For scoping counter stack exhausted "
  val checkFuncs01 = "Formal parameter in function declaration is of an undeclared type."
  val checkFuncs02 = "Result type in function declaration is of an undeclared type."
  val checkFuncs03 = "Result type of function body does not match declared return type."
  val checkFuncs04 = "Couldn't find function when trying to add function fragment."
  val checkFuncs05 = "Function entry stored not as FUNentry."
  val getFormalList01 = "Undeclared type in formal parameter list of function declaration ("
  val getFormalList02 = ")."
  val msgAddFunc01 = "Undeclared result type in function declaration.  Assuming function returns an INT."
  val msgAddFunc02 = "Function declaration makes use of an identifier already defined for this function list ("
  val msgAddType01 = "Type declaration makes use of an identifier already defined for this list of types ("
  val msgCheckTypes01="can't find type."
  val msgCheckTypes02="funky error."
  val msgCheckInt  = "Type mismatch, expected an INT. Replacing with INT."
  val msgCheckUnit = "Expected type of UNIT.  Replacing with UNIT."
  val msgFieldNum = "Couldn't find field in the fields list."
  val msgNameFollow= "Primitive (non-array/record) cycle detected in recursive type definition; Type forced to INT. ("
  val msgtransty01 = "Attempt to declare an array of an unknown type."
  val msgtransty02 = "Attempt to declare a variable of an unknown type."
  val msgtransty03 = "Record field "
  val msgtransty04 = " is of an unknown type."
  val transdec01   = "Type mismatch between variable declaration and initialization expression."
  val transdec02   = "Type declaration of an unknown type."
  val transdec03   = "Illegal assignment of NIL to declaration of a variable of an unknown type."

  (*************************************************************************
   *                       UTILITY FUNCTIONS                               *
   *************************************************************************)

  fun checkInt ({exp=_, ty=T.INT},_,_) = ()
  | checkInt ({exp=_, ty=T.RECORD(s,u)},pos,"") = if u=HACK then () else error pos msgCheckInt
  | checkInt ({exp=_, ty=T.RECORD(s,u)},pos,errMsg) = if u=HACK then () else error pos errMsg
  | checkInt (_,pos,"") = error pos msgCheckInt
  | checkInt (_,pos,errMsg) = error pos errMsg

  fun checkUnit({exp=_, ty=T.UNIT},_,_) = ()
  | checkUnit(_,pos,"") = error pos (msgCheckUnit)
  | checkUnit(_,pos,errMsg) = error pos (errMsg)

(**
  * The memory cell breakCnt keeps a list of loop depth counters.
  * Each time we enter a loop, the head counter is incremented, and
  * decremented when we leave.  If a break expression is encountered
  * and the counter is zero, then we have an illegally placed break.
  * Whenever we enter a function declaration, we have the start the
  * count over again, so a new counter is added to the list and initialized
  * to zero.  It is removed after the function declaration.
  *)
  fun   breakCntHelper(x::xs,d) = ((x+d)::xs)
  |   breakCntHelper(nil,_) = raise Fail(breakFatalMsg^" (inc/dec).\n")
  fun   popBreakCntHelper(x::xs) = breakCnt:=xs
  |   popBreakCntHelper(nil) = raise Fail(breakFatalMsg^" (pop).\n")
  fun pushBreakCntHelper(xs) = breakCnt:=(0::xs)
  fun incBreakCnt() = (breakCnt:=breakCntHelper(!breakCnt,1); ())
  fun decBreakCnt() = (breakCnt:=breakCntHelper(!breakCnt,~1); ())
  fun popBreakCnt() = popBreakCntHelper(!breakCnt)
  fun pushBreakCnt() = pushBreakCntHelper(!breakCnt)

(**
  * isField(st,id) checks to see if id matches one of the fields in
  * st (symbol*ty list).  Returns NONE if there is no match
  *)
  fun isField((id,t)::remainder,matchId) =
    (
      if S.name(id) = S.name(matchId) then
        SOME(t)
      else
        isField(remainder,matchId)
    )
  |   isField(nil,_) = NONE

(**
  * Search a string list for an instance of name
  *)
  fun nameExists(name,x::xs) =
    (
      if (x=S.name(name)) then
        true
      else
        nameExists(name,xs)
    )
  |   nameExists(_,nil) = false

(**
  * Given a T.ty, if it is a name, follow it until you find out what
  * the actual type cound to it is.  If not a name, or if the chain of
  * names has been exhausted, just return the original type.  This
  * function also keeps track of identifiers it has seen in a chain,
  * so it can detect cycles if they exist.
  *)
  fun nameFollowHelper(tenv,ty,addedList,pos) =
    (
      case ty of
        T.NAME(s,to1) =>
          (
            let
              val workingList =
              (
                if nameExists(s,addedList) then
                  (error pos (msgNameFollow^S.name(s)^").");to1:=SOME(T.INT); addedList)
                else (S.name(s)::addedList)
              )
            in
              case (!to1) of
                SOME(T.NAME(n,u)) =>
                (
                  case !u of
                    SOME(tY) => nameFollowHelper(tenv,tY,workingList,pos)
                  | NONE => SOME(ty)
                )
              | SOME(otherType) => SOME(otherType)
              | NONE => SOME(ty)
            end
          )
      | t => SOME(t)
    )

(**
  * Wrapper for above function.
  *)
  fun nameFollow(tenv,ty,pos) = nameFollowHelper(tenv,ty,nil,pos)

(**
  * Compare two types for type equality, and return the type (as an option).
  * Return NONE if types are not equal.  Name types are first handled by
  * resolving to whatever type it is that they are supposed to resolve to.
  *)
  fun tyCmp(tenv,ty1,ty2,pos) =
    (
      case (nameFollow(tenv,ty1,pos),nameFollow(tenv,ty2,pos)) of
        (NONE,_) => NONE
      | (_,NONE) => NONE
      | (SOME(T.INT),SOME(T.RECORD(s,u))) => if u=HACK then SOME(T.INT) else NONE
      | (SOME(T.RECORD(s,u)),SOME(T.INT)) => if u=HACK then SOME(T.INT) else NONE
      | (SOME(T.INT),SOME(T.INT)) => SOME(T.INT)
      | (SOME(T.NIL),SOME(T.NIL)) => SOME(T.NIL)
      | (SOME(T.STRING),SOME(T.STRING)) => SOME(T.STRING)
      | (SOME(T.UNIT),SOME(T.UNIT)) => SOME(T.UNIT)
              (* arrays can only be compared if same declaration *)
      | (SOME(T.ARRAY(t1,u1)),SOME(T.ARRAY(t2,u2))) => if u1<> u2 then NONE else SOME(T.ARRAY(t1,u1))
              (* records can only be compared if same declaration *)
      | (SOME(T.RECORD(s1,u1)),SOME(T.RECORD(s2,u2))) => if(u1<> u2) then NONE else SOME(T.RECORD(s1,u1))
      | (SOME(T.RECORD(s,u)),SOME(T.NIL)) => SOME(T.RECORD(s,u))
      | (SOME(T.NIL),SOME(T.RECORD(s,u))) => SOME(T.RECORD(s,u))
      | (SOME(_),SOME(_)) => NONE
    )

(**
  *  Create a (T.ty list) from a (A.formals list)
  *)
  fun getFormalList(tenv,{var,typ,pos}::params) =
    (
      case S.look(tenv,typ) of
        SOME(t) => t::getFormalList(tenv,params)
      | NONE => ((error pos (getFormalList01^S.name(typ)^getFormalList02); T.INT::getFormalList(tenv,params)))
    )
  |   getFormalList(_,nil) = nil

(**
  * Go through a list of possibly mutually recursive function declarations
  * and add E.FUNentry's for each one.  The list addedList keeps a list of a
  * identifiers that have been used in this function declaration list, and
  * checks for repeated usage.
  *)
  fun addFuncNames(env,tenv,{name,params,result,body,pos}::decs,addedList,level) =
    (
      let
        val resultTy =
        (
          case result of
            SOME(id,pos) =>
            (
              case S.look(tenv,id) of
                SOME(ty) => ty
              | NONE => (error pos msgAddFunc01; T.INT)
            )
          | NONE => T.UNIT
        )
        val formalsList = getFormalList(tenv, params)
        val numFormals = length(formalsList)
        val newLevel = Tr.newLevel({parent=level, numFormals=numFormals})
        val env' = S.enter(env,name,E.FUNentry{level=newLevel,
                                               label=(Temp.newlabel()),
                                               formals=formalsList,
                                               result=resultTy})
        val workingList =
        (
          if nameExists(name,addedList) then
            (error pos (msgAddFunc02^S.name(name)^")"); addedList)
          else
            (S.name(name)::addedList)
        )
      in
        addFuncNames(env',tenv,decs,workingList,level)
      end
    )
  |   addFuncNames(env,_,nil,_,_) = env

(**
  * Go through a list of possibly mutually recursive type declarations
  * and add T.ty's for each one.  The list addedList keeps a list of a
  * identifiers that have been used in this type declaration list, and
  * checks for repeated usage.
  *)
  fun addTypeNames(env,tenv,{name,ty,pos}::decs,addedList) =
    (
      let
        val tenv' = S.enter(tenv,name,T.NAME(name,ref NONE))
        val workingList =
        (
          if nameExists(name,addedList) then
            (error pos (msgAddType01^S.name(name)^")"); addedList)
          else
            (S.name(name)::addedList)
        )
      in
        addTypeNames(env,tenv',decs,workingList)
      end
    )
  |   addTypeNames(_,tenv,nil,_) = tenv

  (*
   * If id is the i-th field of a record, return the integer i.
   * st is a (S.symbol, T.ty) list containing the record fields.
   * Num is how many fields of the record we have already processed.
   *)
  fun fieldNum(id, [], pos) = (
    error pos msgFieldNum;
    0
  ) | fieldNum(id, (sym, ty)::l, pos) = (
    if (id = sym) then
      0
    else
      1 + fieldNum(id, l, pos)
  )
 (*****************************************************************
  *                   TRANSLATING TYPE EXPRESSIONS                *
  *                                                               *
  *              transty : (E.tenv * A.ty) -> T.ty)               *
  *****************************************************************)
  fun transty (tenv, A.ArrayTy(id, pos)) =
    (
      case S.look(tenv,id) of
        SOME(t) => T.ARRAY(t,ref ())
      | NONE => (error pos msgtransty01; T.INT )
    )
  |   transty (tenv, A.NameTy(id,pos)) =
    (
      case S.look(tenv,id) of
        SOME(t) => t
      | NONE => (error pos msgtransty02; T.INT )
    )
  |   transty (tenv, A.RecordTy(tlist)) =
    (
      let
          fun recConv({name,typ,pos}::fields) =
          (case S.look(tenv,typ) of
            SOME(t) =>(name,t)::recConv(fields)
          | NONE => ((error pos (msgtransty03^S.name(name)^msgtransty04); (name,T.INT)::recConv(fields)))
          )
        | recConv(nil) = nil
      in
        T.RECORD(recConv(tlist), ref())
      end
    )

(**
  * The name checkTypes is just for symmetry with checkFuncsAddFragments.  What it does is
  * to walk through the type definitions and bind the unique memory cell to
  * an actual type.  The actual type may end up being a T.NAME, but the rest
  * of the code can follow through the pointers after they are resolved.
  *)
  fun checkTypes(tenv,{name,ty,pos}::decs) =
      let
        val t = S.look(tenv,name)
      in
        case t of
          NONE => (error pos msgCheckTypes01)
        | SOME(tmp as T.NAME(s,u)) =>
            let
              val typ = transty(tenv,ty)
            in
              case typ of
                T.NAME(s0,u0) => u:=SOME(typ)
              | actType => u:=SOME(actType)
            end
        | _ => error pos msgCheckTypes02;
        checkTypes(tenv,decs)
      end
  |   checkTypes(tenv,nil) = tenv

(**
  * After all of the type definitions are entered, we run through all
  * of the entries one more time to check for cycles (using the nameFollow
  * function).  If we ever encounter a cycle, we break it by changing the
  * type of the type declaration that closes the loop into a T.INT.  This
  * could cause other problems later on of course.
  *)
  fun checkCycles(tenv,{name,ty,pos}::decs) =
    (
      nameFollow(tenv,transty(tenv,ty),pos);
      checkCycles(tenv,decs)
    )
  | checkCycles(_,nil) = ()


 (**************************************************************************
  *                   TRANSLATING EXPRESSIONS                              *
  *                                                                        *
  *  transexp : (E.env * E.tenv * Tr.level * Temp.label) ->                *
  *             (A.exp -> {exp : ir_code, ty : T.ty})                      *
  **************************************************************************)
  (* The doneLabel argument is the done label of the nearest enclosing while
     loop. *)
  fun transexp (env:E.env, tenv:E.tenv, level:Tr.level, doneLabel) expr =
    let fun g (A.NilExp) = {exp=Tr.nilTree(),ty=T.NIL}
    | g (A.IntExp i) = {exp=Tr.integer(i),ty=T.INT}
    | g (A.StringExp (str,pos)) = {exp=Tr.strItree(str),ty=T.STRING}
    | g (A.AppExp {func,args,pos}) = (
        case S.look(env,func) of
          SOME(E.FUNentry{level=level2,label,formals,result}) => (
            let
              fun expsTys([]) = []
              |   expsTys(h::t) = (g(h))::expsTys(t)
              fun exps([]) = []
              |   exps({exp, ty}::t) = exp::exps(t)
              fun tys([]) = []
              |   tys({exp, ty}::t) = ty::tys(t)
              val expsTysList = expsTys(args)
              val expsList = exps(expsTysList)
              val tysList = tys(expsTysList)

              fun checkArgs(ty::Tys,f::fs) =
              (
                case tyCmp(tenv,ty,f,pos) of
                  NONE => (checkArgs(Tys,fs); error pos msgAppExp01)
                | SOME(_) => checkArgs(Tys,fs)
              )
              | checkArgs(nil,nil) = ()
              | checkArgs(_,_) = error pos msgAppExp02
            in
              (* Update the maximum number of outgoing args of the F.frame
                 within level. *)
              Tr.updateMaxArgs(level, length formals);
              checkArgs(tysList,formals);
              {exp=Tr.funcCall(label, expsList, level2, level), ty=result}
            end
          )
        | SOME(E.VARentry{access,ty}) => (
            error pos msgAppExp03;
            {exp=Tr.nullTree(), ty=T.INT}
          )
        | NONE => (
            error pos msgAppExp04;
            {exp=Tr.nullTree(), ty=T.INT}
          )
      )
    | g (A.RecordExp{fields,typ,pos}) =
        let
          fun expsTys([]) = []
          |   expsTys((sym, exp, pos)::t) = (g exp)::expsTys(t)
          fun exps([]) = []
          |   exps({exp, ty}::t) = exp::exps(t)
          fun tys([]) = []
          |   tys({exp, ty}::t) = ty::tys(t)
          val expsTysList = expsTys(fields)
          val expsList = exps(expsTysList)
          val tysList = tys(expsTysList)

          fun cmpField ([], [], _) = ()
            | cmpField ((fieldSym,_)::fs, [], _) =
                (error pos (F.format msgRecordExpMissingField
                                     [F.STR (S.name fieldSym)]);
                 cmpField (fs, [], []))
            | cmpField ([], (sym,exp,pos)::es, _) =
                (error pos msgRecordExpExtraField;
                 cmpField ([], es, []))
            | cmpField ((fieldSym,fieldTy)::fs, (sym,exp,pos)::es, tysList) = (
                case tysList of
                  [] => (error pos "record typechecker error") |
                  ty::t => (
                    if fieldSym <> sym
                    then error pos msgRecordExp03
                    else if isSome(tyCmp(tenv,fieldTy,ty,pos))
                      then ()
                      else error pos (F.format msgRecordExp04
                                      [F.STR (S.name fieldSym)]);
                    cmpField (fs,es, t)
                  )
              )

          val ty =
              (case S.look (tenv, typ) of
                   SOME t => nameFollow(tenv, t, pos)
                 | NONE => NONE)
        in
          case ty of
            SOME(t as T.RECORD(symTyList,uniq)) =>
              (cmpField (symTyList, fields, tysList);
               {exp=Tr.recordItree(expsList), ty=t})
          | x =>
              (error pos (if isSome x
                          then msgRecordExp02
                          else msgRecordExp01);
               {exp=Tr.nullTree(), ty=T.INT}
               )
        end
    | g (A.SeqExp exprs) =
        let
          fun getExps([]) = []
          |   getExps((exp, pos)::t) = exp::getExps(t)
          val expsOnly = getExps(exprs)
          fun expsTys([]) = []
          |   expsTys(h::t) = (g(h))::expsTys(t)
          fun exps([]) = []
          |   exps({exp, ty}::t) = exp::exps(t)
          fun tys([]) = []
          |   tys({exp, ty}::t) = ty::tys(t)
          val expsTysList = expsTys(expsOnly)
          val expsList = exps(expsTysList)
          val tysList = tys(expsTysList)
        in
          {exp=Tr.seqItree(expsList),ty= (
            case tysList of [] => T.UNIT |
            h::t => List.last(tysList)
          )}
        end
    | g (A.IfExp {test,then',else'=NONE,pos}) =     (* IF..THEN *)
    (
      let
        val {exp=testExp, ty=testTy} = g(test)
        val {exp=thenExp, ty=thenTy} = g(then')
      in
        checkInt({exp=testExp, ty=testTy}, pos, msgIfExp01);
        checkUnit({exp=thenExp, ty=thenTy}, pos, msgIfExp02);
        {exp=Tr.ifThen(testExp, thenExp),ty=T.UNIT}
      end
    )
    | g (A.IfExp {test,then',else'=SOME(elseexp),pos}) =    (* IF..THEN..ELSE *)
    (
      let
        val {exp=testExp, ty=testTy} = g(test)
        val {exp=thenExp, ty=thenTy} = g(then')
        val {exp=elseExp, ty=elseTy} = g(elseexp)
      in
        checkInt({exp=testExp, ty=testTy},pos,msgIfExp01);
        case tyCmp(tenv,thenTy,elseTy,pos) of
          SOME(t) => {exp=Tr.ifThenElse(testExp, thenExp, elseExp),ty=t}
        | NONE => (error pos msgIfExp03; {exp=Tr.nullTree(),ty=T.INT})
      end
    )

    | g (A.WhileExp {test,body,pos}) =
    (
      let
        val newDoneLabel = Temp.newlabel()
        val {exp=testExp, ty=testTy} = transexp (env, tenv, level, doneLabel) test
      in
        checkInt({exp=testExp, ty=testTy}, pos, msgWhileExp01);
        incBreakCnt();
          let
            val {exp=bodyExp, ty=bodyTy} = transexp (env, tenv, level, newDoneLabel) body
          in
            checkUnit({exp=bodyExp, ty=bodyTy}, pos, msgWhileExp02);
            decBreakCnt();
            {exp=Tr.whileLoop(testExp, bodyExp, newDoneLabel), ty=T.UNIT}
          end
      end
    )

(* MEGA HACK *)

    | g (A.ForExp {var,lo,hi,body,pos}) =
    (
      let
        val (lev, offset) = Tr.allocInFrame(level)
        val env' = S.enter(env,#name(var),E.VARentry{access=(lev, offset),
                                                     ty=T.RECORD([(HACKsym,T.INT)],HACK)})
        val newDoneLabel = Temp.newlabel()

        val {exp=loExp, ty=loTy} = g lo
        val {exp=hiExp, ty=hiTy} = g hi
      in
        checkInt({exp=loExp, ty=loTy},pos,msgForExp01);
        checkInt({exp=hiExp, ty=hiTy},pos,msgForExp02);
        incBreakCnt();
        let
          val {exp=bodyExp, ty=bodyTy} = transexp (env', tenv, level, newDoneLabel) body
        in
          checkUnit({exp=bodyExp, ty=bodyTy},pos,msgForExp03);
          decBreakCnt();
          {exp=Tr.forLoop(loExp, hiExp, bodyExp, newDoneLabel, offset),ty=T.UNIT}
        end
      end
    )
    | g (A.BreakExp(pos)) =
      if (hd(!breakCnt)=0)
      then (error pos msgBreakExp; {exp=Tr.nullTree(),ty=T.UNIT})
      else {exp=Tr.break(doneLabel),ty=T.UNIT}

    | g (A.LetExp{decs,body,pos}) =
      let
        val (env',tenv', transExpList) = transdecs(env,tenv,decs,level, doneLabel)
        val {exp, ty} = transexp(env', tenv', level, doneLabel) body
      in
        {exp=Tr.seqItree(transExpList @ [exp]), ty=ty}
      end

    | g (A.ArrayExp{typ,size,init,pos}) =   (* need to check size is int, and typeof(init) = typ *)
    (
      checkInt(g size,pos,msgArrayExp01);
      let
        val tenvEnt =
        (
          case S.look(tenv,typ) of
            SOME(t) => nameFollow(tenv,t,pos)
          | NONE => NONE
        )
      in
        case tenvEnt of
          SOME(T.ARRAY(ty,u)) =>
            let
              val {exp=initExp, ty=initTy} = g(init)
              val {exp=sizeExp, ty=sizeTy} = g(size)
            in
              case tyCmp(tenv,ty,initTy,pos) of
                SOME(_) => {exp=Tr.arrayItree(sizeExp, initExp),ty=T.ARRAY(initTy,u)}
              | NONE => (error pos msgArrayExp02; {exp=Tr.nullTree(),ty=T.ARRAY(T.INT,ref ())})
            end
        | SOME(t) => (error pos msgArrayExp03; {exp=Tr.nullTree(),ty=T.ARRAY(T.INT,ref())})
        | NONE => (error pos msgArrayExp04; {exp=Tr.nullTree(),ty=T.ARRAY(T.INT,ref())})
      end
    )

(* [In]equality comparison operators apply to all types *)
    | g (A.OpExp {left,oper=A.NeqOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case (leftTy, rightTy) of
          (T.NIL, T.NIL) => error pos msgOpNeqExp01
        | (_,_) => ();
        case tyCmp(tenv,leftTy, rightTy,pos) of
          SOME(T.STRING) => {exp=Tr.strRelop(A.NeqOp, leftExp, rightExp),ty=T.INT}
        | SOME(_) => {exp=Tr.iarRelop(A.NeqOp, leftExp, rightExp), ty=T.INT}
        | NONE => (error pos msgOpNeqExp02; {exp=Tr.nullTree(),ty=T.INT})
      end
    )
      | g (A.OpExp {left,oper=A.EqOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case (leftTy, rightTy) of
          (T.NIL,T.NIL) => error pos msgOpEqExp01
        | (_,_) => ();
        case tyCmp(tenv,leftTy, rightTy,pos) of
          SOME(T.STRING) => {exp=Tr.strRelop(A.EqOp, leftExp, rightExp), ty=T.INT}
        | SOME(t) => {exp=Tr.iarRelop(A.EqOp, leftExp, rightExp),ty=T.INT}
        | NONE => (error pos msgOpEqExp02; {exp=Tr.nullTree(),ty=T.INT})
      end
    )
(* Order comparison operators <, >, <=, <= apply to INTs and STRINGs only *)
    | g (A.OpExp {left,oper=A.GeOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case tyCmp(tenv,leftTy,rightTy,pos) of
          SOME(t) => (
            case t of
              T.INT => {exp=Tr.iarRelop(A.GeOp, leftExp, rightExp),ty=T.INT}
            | T.STRING => {exp=Tr.strRelop(A.GeOp, leftExp, rightExp),ty=T.INT}
            | _ => (error pos (msgCmpExp01^"\">=\"."); {exp=Tr.nullTree(),ty=T.INT})
          )

        | NONE => (error pos (msgCmpExp02^"\">=\"."); {exp=Tr.nullTree(),ty=T.INT})
      end
    )
    | g (A.OpExp {left,oper=A.GtOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case tyCmp(tenv, leftTy,rightTy,pos) of
          SOME(t) =>
          (
            case t of
              T.INT => {exp=Tr.iarRelop(A.GtOp, leftExp, rightExp),ty=T.INT}
            | T.STRING => {exp=Tr.strRelop(A.GtOp, leftExp, rightExp),ty=T.INT}
            | _ => (error pos (msgCmpExp01^"\">\"."); {exp=Tr.nullTree(),ty=T.INT})
          )
        | NONE => (error pos (msgCmpExp02^"\">\"."); {exp=Tr.nullTree(),ty=T.INT})
      end
    )
    | g (A.OpExp {left,oper=A.LeOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case tyCmp(tenv,leftTy,rightTy,pos) of
          SOME(t) =>
          (
            case t of
              T.INT => {exp=Tr.iarRelop(A.LeOp, leftExp, rightExp),ty=T.INT}
            | T.STRING => {exp=Tr.strRelop(A.LeOp, leftExp, rightExp),ty=T.INT}
            | _ => (error pos (msgCmpExp01^"\"<=\"."); {exp=Tr.nullTree(),ty=T.INT})
          )
        | NONE => (error pos (msgCmpExp02^"\"<=\"."); {exp=Tr.nullTree(),ty=T.INT})
      end
    )
    | g (A.OpExp {left,oper=A.LtOp,right,pos}) =
    (
      let
        val {exp=leftExp, ty=leftTy} = g(left)
        val {exp=rightExp, ty=rightTy} = g(right)
      in
        case tyCmp(tenv,leftTy,rightTy,pos) of
          SOME(t) =>
          (
            case t of
              T.INT => {exp=Tr.iarRelop(A.LtOp, leftExp, rightExp),ty=T.INT}
            | T.STRING => {exp=Tr.strRelop(A.LtOp, leftExp, rightExp),ty=T.INT}
            | _ => (error pos (msgCmpExp01^"\"<\"."); {exp=Tr.nullTree(),ty=T.INT})
          )
          | NONE => (error pos (msgCmpExp02^"\"<\"."); {exp=Tr.nullTree(),ty=T.INT})
      end
    )
(* ADD,SUB,TIMES,DIVIDE apply only to INTs *)
    | g (A.OpExp {left,oper,right,pos}) = (
        let
          val {exp=leftExp, ty=leftTy} = g(left)
          val {exp=rightExp, ty=rightTy} = g(right)
        in
          checkInt ({exp=leftExp, ty=leftTy}, pos, msgArithExp01);
          checkInt ({exp=rightExp, ty=rightTy}, pos, msgArithExp02);
          {exp=Tr.binop(oper, leftExp, rightExp), ty=T.INT}
        end
      )
    | g (A.VarExp var) = h(var)
    | g (A.AssignExp {var,exp,pos}) =
    (
      let
        val {exp=assignTo, ty=varTy} = h var
        val {exp=assignFrom, ty=expTy} = g exp
      in
        case varTy of
          T.RECORD(s,u) => if u=HACK then (error pos msgAssignExp01) else ()
        | _ => ();
        case tyCmp(tenv, varTy, expTy,pos) of
          SOME(_) => (
            {exp=Tr.assign(assignTo, assignFrom),ty=T.UNIT}
          )
        | NONE => (error pos msgAssignExp02; {exp=Tr.nullTree(),ty=T.UNIT})
      end
    )

(**
  * Function dealing with "var", mutually recursive
  *)
    and h (A.SimpleVar (id,pos)) =
    (
      let
        val envEnt = S.look(env,id)
      in
        case envEnt of
          SOME(E.VARentry{access,ty}) =>
          (
            case nameFollow(tenv,ty,pos) of
              SOME(t) => {exp=Tr.simpleVar(access, level),ty=t}
            | NONE => (error pos msgH0x; {exp=Tr.nullTree(),ty=T.INT})
          )
        | SOME(E.FUNentry{level,label,formals,result}) => (error pos (msgH00^S.name(id)^")."); {exp=Tr.nullTree(),ty=T.INT})
        | NONE => (error pos (msgH01^S.name(id)^msgH01b); {exp=Tr.nullTree(),ty=T.INT})
      end
    )
    | h (A.FieldVar (v,id,pos)) =
    (
      let
        val {exp=vexp, ty=vty} = h(v)
        val leftPiece = nameFollow(tenv,vty,pos)
      in
        case leftPiece of
          (* st is a (Symbol.Symbol * Types.ty) list. *)
          SOME(T.RECORD(st,_)) =>
          (
            case isField(st,id) of
              SOME(t) => {exp=Tr.recordField(vexp, fieldNum(id, st, pos)),ty=t}
            | NONE => (error pos (msgH02^S.name(id)^")."); {exp=Tr.nullTree(),ty=T.INT})
          )
        | _ => (error pos msgH03; {exp=Tr.nullTree(),ty=T.INT})
      end
    )
    | h (A.SubscriptVar (v,exp,pos)) =
    ( 
      let
        val {exp=vexp, ty=vty} = h(v)
        val {exp=ssexp, ty=ssty} = g(exp)
        val leftPiece = nameFollow(tenv,vty,pos)
      in
      (
        checkInt ({exp=ssexp, ty=ssty}, pos, msgH04);
        case leftPiece of
          SOME(T.ARRAY(t,_)) => {exp=Tr.arrayElt(vexp, ssexp),ty=t}
        | _ => (error pos msgH05; {exp=Tr.nullTree(),ty=T.INT})
      )
      end
    )
  in
    g expr
  end
 (** END of transexp **)

(**
  * Given a list of function declarations and environments that have been
  * appropriately `prepped' (see addFuncNames), adds the params to the
  * environment and type checks the function bodies.
  *)
  and checkFuncsAddFragments(env,tenv,({name,params,result,body,pos}:A.fundec)::decs,level, doneLabel) =
    (* I-tree code comment: This function will also signal what the bodies are
       to Translate, so that it can generate function fragments. *)
    let
      val env' =
      (
        let
          fun addParams(penv,{var,typ,pos}::params, lengthParams) =
            let
              val t =
              (
                case S.look(tenv,typ) of
                  NONE => (error pos checkFuncs01; T.UNIT)
                | SOME(ty) => ty
              )
              val penv' = (
                case S.look(env, name) of
                  NONE => (error pos checkFuncs04; env) |
                  SOME (E.FUNentry{level=newLevel, label, formals, result}) => (
                    S.enter(penv,#name(var:A.vardec),
                      E.VARentry{access=(newLevel, Tr.offsetOfNthParam(lengthParams - length(params))),ty=t})
                  ) |
                  SOME(_) => (error pos checkFuncs05; env)
              )
            in
              addParams(penv', params, lengthParams)
            end
          | addParams(penv,nil, _) = penv
        in
          addParams(env,params, length params)
        end
      )
      val resultTy =
      (
        case result of
          NONE => T.UNIT
        | SOME(s,pos) =>
          (
            case S.look(tenv,s) of
              NONE => (error pos checkFuncs02; T.INT)
            | SOME(t) => t
          )
      )
    in      
      (* Add the function fragment. *)
      case S.look(env', name) of
        NONE => (error pos checkFuncs04)
      | SOME (E.FUNentry{level, label, formals, result}) => (
          let
            val bodyEval = transexp (env', tenv, level, doneLabel) body
          in
            case tyCmp(tenv,#ty bodyEval,resultTy,pos) of
              NONE => error pos checkFuncs03
            | SOME(t) => ();
            Tr.addFunctionFrag(label, level, #exp bodyEval)
          end
        )
      | SOME (_) => (error pos checkFuncs05);

      checkFuncsAddFragments(env,tenv,decs,level, doneLabel)
    end
  |   checkFuncsAddFragments(_,_,nil,_, _) = ()


 (**************************************************************************
  *                   TRANSLATING DECLARATIONS                             *
  *                                                                        *
  *  transdec : (E.env * E.tenv * A.dec * level * Temp.label) ->           *
  *             (E.env * E.tenv * ir_code list)                            *
  **************************************************************************)
  and transdec (env, tenv, A.VarDec{var,typ,init,pos}, level, doneLabel) =
    (* transdec returns an TransExp list of one element, corresponding to
       Itree code. *)
    let
      val initTy = transexp(env,tenv,level, doneLabel) init
      val initTy' =
      (
        case typ of
          SOME(ty,pos) =>
          (
            case S.look(tenv,ty) of
              SOME(t) => t
            | NONE => #ty(initTy)
          )
        | NONE => #ty(initTy)
      )
    in
    (
      case typ of
        SOME(ty,pos) =>
        (
          case S.look(tenv,ty) of
            SOME(t) =>
            (
              case tyCmp(tenv,#ty(initTy),t,pos) of
                SOME(t') => ()
              | NONE => error pos transdec01
            )
          | NONE => error pos transdec02
        )
      | NONE => case #ty(initTy) of T.NIL => (error pos transdec03) | _ => ();
      (S.enter(env,#name(var),E.VARentry{access=Tr.allocInFrame(level),ty=initTy'}),
       tenv, [Tr.vardec(#exp(initTy), level)])
    )
    end
    | transdec (env, tenv, A.FunctionDec(declist), level, doneLabel) =
    let
                (* gotta re-init loop depth counter *)
      val env' = (pushBreakCnt(); addFuncNames(env,tenv,declist,nil,level))
    in
      checkFuncsAddFragments(env',tenv,declist,level,doneLabel);
      popBreakCnt();    (* restore the loop depth counter *)
      (env', tenv, [])
    end

    | transdec (env, tenv, A.TypeDec(declist), level, doneLabel) =
    let
      val tenv' = addTypeNames(env,tenv,declist,nil)
      val tenv'' = checkTypes(tenv',declist)
    in
      checkCycles(tenv'',declist);
      (env, tenv'', [])
    end

  (*** transdecs : (E.env * E.tenv * A.dec list) ->
                   (E.env * E.tenv * ir_code list)  ***)
  and transdecs (env,tenv,nil,_, _) = (env, tenv, [])
    | transdecs (env,tenv,dec::decs,level, doneLabel) =
        let
          val (env', tenv', transExp) = transdec (env,tenv,dec,level, doneLabel)
          val (env'', tenv'', transExpList) = transdecs (env', tenv', decs, level, doneLabel)
        in
          (env'', tenv'', transExp@transExpList)
        end

  (*** transprog : A.exp -> Frame.frag list ***)
  fun transprog prog = (
    let
      val lev = Tr.newLevel({parent=Tr.outermost, numFormals=0})
      val {exp=progexp, ty=progty} =
        transexp (E.base_env, E.base_tenv, lev, Temp.newlabel()) prog;
    in
          Tr.addTigermainFrag (progexp, lev);
          Tr.getResult ()
    end
  )
end  (* structure Semant *)

