signature TRANSLATE = 
sig
  (* Type and variable definitions *)
  type level
  type exp
  type access
    (* The access information for a variable v is a pair (l, k) where l is the
       level in which v is defined and k is the frame offset. *)
  type frag

  val outermost : level

  (* Frame management *)
  val newLevel : {parent: level, numFormals: int} -> level
  val allocInFrame : level -> access
    (* For the time being we will always alloc in frame, and never alloc in
    register. Alloc-ing in frame always works; alloc-ing in register is just
    an optimization. *)
  val updateMaxArgs : level * int -> unit
  val getResult : unit -> frag list
  val offsetOfNthParam : int -> Frame.offset

  (* Absyn.exp -> I-tree conversions *)
  val nilTree : unit -> exp
  val integer : int -> exp
  val strItree : string -> exp

  (* Array / Record Creation *)
  val arrayItree : exp * exp -> exp
  val recordItree : exp list -> exp

  (* Sequence I-trees *)
  val seqItree : exp list -> exp

  (* Assignment *)
  val assign : exp * exp -> exp
    (* Assign from second exp to first exp. *)

  (* I-tree accessors *)
  val nullTree : unit -> exp
  val simpleVar : access * level -> exp
    (* Returns the I-tree corresponding to a local variable x. Args are x's
    access and the level of the function in which x is used. *)
  val arrayElt : exp * exp -> exp
    (* Returns the I-tree corresponding to an array element a[i]. Args are exp's
    corresponding to the base location of the array, and the subscript. *)
  val recordField : exp * int -> exp
    (* Returns the I-tree corresponding to a field of a record. *)

  (* Binop / Relop I-trees *)
  (* The binary operators are +, -, *, /.
     The relational operators are =, <>, >, >=, <, <=. *)
  val binop : Absyn.oper * exp * exp -> exp
  val iarRelop : Absyn.oper * exp * exp -> exp
    (* Relops for ints, arrays, and records. If the relop is not = or <>, the
    exp's must be of type int. *)
  val strRelop : Absyn.oper * exp * exp -> exp
    (* Relops for strings. *)

  (* Conditionals *)
  val ifThenElse : exp * exp * exp -> exp
    (* if exp1 then exp2 else exp3 *)
  val ifThen : exp * exp -> exp
    (* if exp1 then exp2 *)

  (* While / For Loops *)
  val whileLoop : exp * exp * Temp.label -> exp
    (* First exp arg is the test exp; Second exp arg is the body exp. The done
    label is also passed in. *)

  val forLoop : exp * exp * exp * Temp.label * Frame.offset -> exp
    (* First two exp args are the lo and hi values of the for loop variable. The
    third exp arg is the body exp.  The done label is also passed in. *)

  val break : Temp.label -> exp

  (* Simple Variable Declarations *)
  val vardec : exp * level -> exp
    (* Returns an I-tree that moves the exp arg into the current level's frame.
    *)

  (* Add Function Fragments *)
  val addFunctionFrag : Temp.label * level * exp -> unit
    (* Adds the function frag to the global fragments list. *)

  val addTigermainFrag : exp * level -> unit

  (* Function Calls *)
  val funcCall: Temp.label * exp list * level * level -> exp
    (* Call a function. Label of function, exp list of arguments, level of
    function, level of caller. *)

end (* signature TRANSLATE *)


functor TranslateGen(Register : REGISTER_STD) : TRANSLATE = 
struct

  structure A = Absyn
  structure Er = ErrorMsg
  structure F = Frame
  structure R : REGISTER_STD = Register
  structure S = Symbol
  structure T = Tree
  structure Te = Temp

(* -------------------- TYPE AND VARIABLE DEFINITIONS ----------------------- *)
  datatype level = LEVEL of {frame : F.frame,
                             sl_offset : F.offset,
                             parent : level} * unit ref
                 | TOP
  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Te.label * Te.label -> T.stm
  type access = level * F.offset
  type frag = F.frag
  val outermost = TOP
  val tnil = T.CONST 0
    (* Tiger nil. *)

(* --------------------------- UTILITY FUNCTIONS ---------------------------- *)
  fun isTop(level) = (level = TOP)

  (* Create a Tree.SEQ of from a list of Tree.stms. *)
  fun seq([a, b]) = T.SEQ(a, b)
    | seq(a::(b::(c::l))) = T.SEQ(seq([a, b]), seq(c::l))
    | seq([a]) = a
    | seq([]) = T.EXP (T.CONST 530)
      (* Last expression is a filler so that this function typechecks. *)

  (* Sum of two addresses. *)
  fun sum(a, b) = T.BINOP(T.PLUS, a, b)

  (* The Tree.exp corresponding to the contents of address (a + b), where a and
     b are Tree.exp's. *)
  fun atSum(a, b) = F.contentsOf(sum(a, b))

  (* The Tree.exp corresponding to (a * b), where a and b are Tree.exp's. *)
  fun mult(exp1, exp2) = T.BINOP(T.MUL, exp1, exp2)

  (* Returns the size of n words, where n is a Tree.exp. *)
  fun sizeOfNWords(n) = mult(n, T.CONST F.wordSize)

  (* The Tree.exp corresponding to !a, where a is an integer.
     It evaluates to 1 if a == 0, and 0 if a != 0. *)
  fun not(exp) = T.CALL(T.NAME(Te.namedlabel "not"), [exp])

  val fragmentlist = ref ([] : F.frag list)

  (* addFrag : F.frag -> unit
     Adds a fragment to the global fragments list. *)
  fun addFrag(frag : F.frag) = (
    fragmentlist := frag::(!fragmentlist)
  )

(*  --------------------------- FRAME MANAGEMENT ---------------------------- *)
  fun newLevel ({parent=p, numFormals}) =
    let
      val frame = F.newFrame(numFormals)
    in
      (LEVEL ({frame=frame, sl_offset=R.paramBaseOffset, parent=p}, ref ()))
    end

  fun allocInFrame (level) = (
    case level of
      TOP => (
        Er.error 0 "TOP level cannot be passed to Translate.allocInFrame";
        (level, 0)
      ) |
      LEVEL ({frame, sl_offset, parent}, _) => (level, F.allocInFrame(frame))
  )

  fun offsetOfNthParam(x) = F.offsetOfNthParam(x)

  fun updateMaxArgs (level, numArgs) =
    case level of
      TOP => (
        Er.error 0 "TOP level cannot be passed to Translate.allocInFrame"
      ) |
      LEVEL ({frame, sl_offset, parent}, _) => (
        #maxargs(frame) := Int.max(!(#maxargs(frame)), numArgs)
      )

  fun getResult () = !fragmentlist

(* -------------------------- TREE.EXP CONVERSIONS -------------------------- *)
  (* Exp conversions:
     val unEx : exp -> Tree.exp
     val unNx : exp -> Tree.stm
     val unCx : exp -> (Temp.label * Temp.label) -> Tree.stm *)

  fun unEx (Ex e) = e
    | unEx (Nx s) = T.ESEQ(s, T.CONST 0)
    | unEx (Cx test) =
        (* We return a temp containing 1 if test evalutes to true, and a temp
           containing 0 otherwise. *)
        let
          val r = Te.newtemp()
          val t = Te.newlabel() and f = Te.newlabel()
        in
          T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                     test (t, f),
                     T.LABEL f,
                     T.MOVE(T.TEMP r, T.CONST 0),
                     T.LABEL t],
                 T.TEMP r)
        end

  (* Apply unEx to all elements of a list. *)
  fun unExList([]) = []
  |   unExList(h::t) = (unEx h)::unExList(t)

  fun unNx (Ex e) = T.EXP(e)
    | unNx (Nx s) = s
    | unNx (Cx test) = T.EXP(unEx(Cx test))

  fun unNxList([]) = []
  |   unNxList(h::t) = (unNx h)::unNxList(t)

  fun unCx (Ex e) =
      (* We return a function f: (t, f) -> Tree.stm.  The Tree.stm returned
         evalutes whether the expression e is equal to 0: If so, then the false
         label is returned. If not, the true label is returned. *)
      let fun f(t, f) = T.CJUMP(T.TEST(T.EQ, e, T.CONST 0), f, t)
      in
        f
      end
    | unCx (Nx _) =
      (* This case should never occur when compiling a well-typed Tiger
         program. *)
      let
        fun f(a, b) = T.EXP (T.CONST 0)
      in
        Er.error 0 "UnCx(Nx) should never occur.";
        f
      end
    | unCx (Cx test) = test

(* --------------------  ABSYN.EXP -> I-TREE CONVERSIONS -------------------- *)
  fun nilTree () = Ex tnil

  fun integer (i) = Ex(T.CONST i)

  fun strItree (str) =
    let
      val lab = Te.newlabel()
    in (
      addFrag(F.DATA {lab=lab, s=str});
      Ex (T.NAME lab)
    ) end

(* ------------------------- ARRAY / RECORD CREATION ------------------------ *)
  fun arrayItree(sizeTransExp, initTransExp) =
    let
      val size = unEx sizeTransExp
      val init = unEx initTransExp
      val r = Te.newtemp()
    in
      (* Key for array bounds check: Create an extra slot for the array size.
         The array size goes in the first slot. *)
      Ex(T.ESEQ(seq[T.MOVE(T.TEMP r, F.externalCall("initArray",
                        [T.BINOP(T.PLUS, size, T.CONST 1), init])),
                    T.MOVE(F.contentsOf(T.TEMP r), size)],
                T.TEMP r))
    end

  (* Generate a list of I-tree branches that move the i-th exp into the i-th
     field. The pointer to the record is r. The number of fields that have
     already been processed is numDone. *)
  fun recordInitFields(exps, numDone, 0, r) = ([])
  |   recordInitFields(exps, numDone, numLeft, r) = (
        case exps of [] => (
          Er.error 0 "recordInitFields passed less exps than size.";
          []
        ) |        h::t => (
          T.MOVE(atSum(T.TEMP r, T.CONST(numDone*F.wordSize)), h)
                 :: recordInitFields(t, numDone + 1, numLeft - 1, r)
        )
      )

  fun recordItree(transExps) =
    let
      val size = length transExps
      val exps = unExList transExps
      val r = Te.newtemp()
      val branch = T.MOVE(T.TEMP r, F.externalCall("allocRecord",
          [T.CONST (size * F.wordSize)]))
    in
      Ex(T.ESEQ(seq([branch] @ recordInitFields(exps, 0, size, r)), T.TEMP r))
    end

(* ---------------------------- SEQUENCE I-TREES ---------------------------- *)
  fun seqItree (trExps) = (
    let
      fun allButLast([]) = []
      |   allButLast([h]) = []
      |   allButLast(h::t) = h::allButLast(t)
      val ablTrExps = allButLast(trExps)
    in
      case trExps of [] => (Nx (T.EXP (T.CONST 0))) |
                     h::t =>
        Ex (T.ESEQ(seq (unNxList ablTrExps), unEx (List.last trExps)))
    end
  )

(* ------------------------------- ASSIGNMENT ------------------------------- *)
  fun assign(toTrans, fromTrans) =
    let
      val toExp = unEx toTrans
      val fromExp = unEx fromTrans
    in
      Nx (T.MOVE (toExp, fromExp))
    end

(* ---------------------------- I-TREE ACCESSORS ---------------------------- *)
  fun nullTree () = Ex (T.CONST 0)

  (* Locate a simple var using static links. Returns Tree.exp. *)
  fun generateSum(tree, c_parent, c_ref, d_ref, xOffset) =
    case c_parent of
      TOP => (Er.error 0 "Chased functions to TOP for simple var.";
              T.CONST 3581) |
      LEVEL({frame=b_frame,sl_offset=b_sl_offset,parent=b_parent}, b_ref) => (
        if (b_ref = d_ref) then
          atSum(tree, T.CONST xOffset)
        else 
          generateSum(atSum(tree, T.CONST b_sl_offset), b_parent, b_ref, d_ref,
                      xOffset)
      )

  fun simpleVar ((levelDefined, xOffset), levelCalled) =
    (* There are two valid cases:
       1. x is inside static scope.
          This only happens if levelDefined = levelCalled.
       2. x is not inside static scope.
          This only happens if levelDefined < levelCalled.
       If levelDefined > levelCalled, this is a type-checking error. *)
    case levelDefined of
      TOP => (Er.error 0 "Simple var defined at TOP level."; Ex (T.CONST 0)) |
      LEVEL ({frame=d_frame,sl_offset=d_sl_offset,parent=d_parent}, d_ref) => (
        case levelCalled of
          TOP =>
            (Er.error 0 "Simple var called at TOP level."; Ex (T.CONST 0)) |
          LEVEL ({frame=c_frame,sl_offset=c_sl_offset,parent=c_parent},
                 c_ref) => (
            if (c_ref = d_ref) then (
              (* Case 1: x is inside static scope *)
              Ex (atSum(T.CONST xOffset, T.TEMP R.FP))
            )
            else (
              (* Case 2: x is not inside static scope *)
              Ex (generateSum(atSum(T.TEMP R.FP, T.CONST c_sl_offset), c_parent,
                              c_ref, d_ref, xOffset))
            )
          )
      )

  (* Boolean that tells if the array out-of-bounds string has been created. *)
  val aobStringCreated = ref false
  (* Array out of bounds string. Create it only if we encounter a program that
     has an array access.  Note that it might be created even for perfectly
     valid programs that never actually throw an array out of bounds error. *)
  val aobString = ref (Ex (T.CONST 0))

  fun arrayElt (vTransExp, ssTransExp) =
    let
      (* Convert from Translate.exp's to Tree.exp's. *)
      val vexp = unEx(vTransExp)
      val ssexp = unEx(ssTransExp)
      val exit = Te.newlabel() and norm1 = Te.newlabel() and
          norm2 = Te.newlabel() and err = Te.newlabel()
      val ret = Te.newtemp()
    in
      (* Create the array out of bounds string if it hasn't been created yet. *)
      if !aobStringCreated = false then (
        aobString := strItree("Array subscript out of bounds\n");
        aobStringCreated := true
      ) else ();
      (* Add wordSize * (subscript+1) to the memory address containing a[0]. *)
      Ex(T.ESEQ(seq[T.MOVE(T.TEMP ret, sum(vexp, T.CONST 4)),
                    T.CJUMP(T.TEST(T.LT, ssexp, F.contentsOf(vexp)), norm1, err),
                    T.LABEL(norm1),
                    T.CJUMP(T.TEST(T.GE, ssexp, T.CONST 0), norm2, err),
                    T.LABEL(norm2),
                    T.MOVE(T.TEMP ret, sum(vexp, sizeOfNWords(sum(ssexp, T.CONST 1)))),
                    T.JUMP(T.NAME exit, [exit]),
                    T.LABEL(err),
                    T.EXP(T.CALL(T.NAME(Te.namedlabel "print"), [unEx (!aobString)])),
                    T.EXP(T.CALL(T.NAME(Te.namedlabel "exit"), [])),
                    T.LABEL(exit)],
                F.contentsOf(T.TEMP ret)))
    end

  fun recordField (vTransExp, fieldNum) =
    let
      val vexp = unEx(vTransExp)
    in
      (* Add wordSize * fieldNum to memory address containing record's first
         field. *)
      case vexp of (T.CONST x) => (
        Er.error 0 "Cannot access field of nil"; Ex (T.CONST 0)
      ) |
      _ => 
        Ex (atSum(vexp, sizeOfNWords(T.CONST fieldNum)))
    end

(* -------------------------- BINOP / RELOP I-TREES ------------------------- *)
  fun binop (oper, transA, transB) =
    let
      val a = unEx transA
      val b = unEx transB
    in
      case oper of A.PlusOp =>   Ex (T.BINOP(T.PLUS, a, b))   |
                   A.MinusOp =>  Ex (T.BINOP(T.MINUS, a, b))  |
                   A.TimesOp =>  Ex (mult(a, b))              |
                   A.DivideOp => Ex (T.BINOP(T.DIV, a, b))    |
                   _ => (Er.error 0 "Invalid binary operator"; Ex (T.CONST 0))
    end

  (* Produces the relop i-tree given a Tree.relop and two Tree.exp's. *)
  fun iarRelopHelper(relop, a, b) =
    let
      fun test(t, f) = T.CJUMP(T.TEST(relop, a, b), t, f)
    in
      Cx test
    end

  fun iarRelop (oper, transA, transB) =
    let
      val a = unEx transA
      val b = unEx transB
    in
      case oper of
        A.EqOp =>  iarRelopHelper(T.EQ, a, b) |
        A.NeqOp => iarRelopHelper(T.NE, a, b) |
        A.GtOp =>  iarRelopHelper(T.GT, a, b) |
        A.GeOp =>  iarRelopHelper(T.GE, a, b) |
        A.LtOp =>  iarRelopHelper(T.LT, a, b) |
        A.LeOp =>  iarRelopHelper(T.LE, a, b) |
        _ => (Er.error 0 "iarRelop: Invalid relational op"; Ex (T.CONST 0))
    end

  fun strRelop(oper, transA, transB) =
    let
      val a = unEx transA
      val b = unEx transB
    in
      Cx(unCx(Ex(
        case oper of
          A.EqOp => F.externalCall("stringEqual", [a, b]) |
          A.NeqOp => not(F.externalCall("stringEqual", [a, b])) |
          A.GtOp => F.externalCall("stringLessThan", [b, a]) |
          A.GeOp => not(F.externalCall("stringLessThan", [a, b])) |
          A.LtOp => F.externalCall("stringLessThan", [a, b]) |
          A.LeOp => not(F.externalCall("stringLessThan", [b, a]))  |
          _ => (Er.error 0 "strRelop: Invalid relational op"; T.CONST 0)
      )))
    end

(* ------------------------------ CONDITIONALS ------------------------------ *)
  fun ifThenElse(testTransExp, thenTransExp, elseTransExp) =
    let
      val testExp = unCx(testTransExp)
      val thenExp = unEx(thenTransExp)
      val elseExp = unEx(elseTransExp)
      val r = Te.newtemp()
      val t = Te.newlabel() and f = Te.newlabel() and join = Te.newlabel()
    in
      (* We first see if testExp evaluates to true or false.
         If true, we evaluate/return thenExp and jump to join.
         If false, we evaluate/return elseExp and jump to join. *)
      Ex(T.ESEQ(seq[testExp(t, f),
                    T.LABEL t, T.MOVE(T.TEMP r, thenExp),
                               T.JUMP(T.NAME join, [join]),
                    T.LABEL f, T.MOVE(T.TEMP r, elseExp),
                    T.LABEL join],
                T.TEMP r))
    end

  fun ifThen(testTransExp, thenTransExp) =
    let
      val testExp = unCx(testTransExp)
      val thenExp = unEx(thenTransExp)
      val t = Te.newlabel() and f = Te.newlabel()
    in
      (* If testExp evaluates to true, then evaluate thenExp. Else skip it. *)
      Nx(seq[testExp(t, f), T.LABEL t, T.EXP(thenExp), T.LABEL f])
    end

(* ---------------------------- WHILE / FOR LOOPS --------------------------- *)
  (* Return the wrapped I-tree, as well as the done label so that Semant knows
     where to jump to if it encounters a break. *)
  fun whileLoop(testTransExp, bodyTransExp, done) =
    let
      val testExp = unCx(testTransExp)
      val bodyExp = unEx(bodyTransExp)
      val test = Te.newlabel()
      val t = Te.newlabel()
    in
      (* [While loop I-tree format]
         test:
           if not (condition) goto done
           body
           goto test
         done: *)
      Nx (seq[T.LABEL test, testExp(t, done),
              T.LABEL t, T.EXP(bodyExp), T.JUMP(T.NAME test, [test]),
              T.LABEL done])
    end

  fun forLoop(loTrExp, hiTrExp, bodyTrExp, done, offset) = (
    let
      val loExp = unEx(loTrExp)
      val hiExp = unEx(hiTrExp)
      val bodyExp = unEx(bodyTrExp)
    in
      let
        (* The for loop variable is located at offset. *)
        val hi = Te.newtemp()
        (* Labels for the test, body, and increment stages. *)
        val test = Te.newlabel() and body = Te.newlabel()
                                 and inc = Te.newlabel()
      in
        Nx (seq[T.MOVE(atSum(T.TEMP R.FP, T.CONST offset), loExp),
                T.MOVE(T.TEMP hi, hiExp),
                T.CJUMP(T.TEST(T.LE, atSum(T.TEMP R.FP, T.CONST offset), T.TEMP hi), body, done),
                T.LABEL(body),
                T.EXP(bodyExp),
                T.CJUMP(T.TEST(T.LT, atSum(T.TEMP R.FP, T.CONST offset), T.TEMP hi), inc, done),
                T.LABEL(inc),
                T.MOVE(atSum(T.TEMP R.FP, T.CONST offset), T.BINOP(T.PLUS, atSum(T.TEMP R.FP, T.CONST offset), T.CONST 1)),
                T.JUMP(T.NAME body, [body]),
                T.LABEL(done)])
      end
    end
  )

  fun break(done) = Nx (T.JUMP (T.NAME done, [done]))

    
(* ------------------------------ DECLARATIONS ------------------------------ *)
  fun vardec(transExp, level) =
    (* Sequence of instructions to initialize a local variable. *)
    let
      val exp = unEx transExp
    in
      case level of
        TOP => (Er.error 0 "TOP level passed to Tr.vardec"; Ex (T.CONST 0)) |
        LEVEL ({frame={numFormals, offlst, locals, maxargs},  (* F.frame *)
                sl_offset, parent}, uref) => (
          Nx(T.MOVE(atSum(T.TEMP R.FP, T.CONST (F.offsetOfNthLocal(!locals))),
                    exp))
        )
    end

(* ------------------------- ADD FUNCTION FRAGMENTS ------------------------- *)
  fun addFunctionFrag(label, level, transExp) =
    let
      val exp = unEx transExp
    in
      case level of
        TOP => (Er.error 0 "TOP level shouldn't have a function fragment") |
        LEVEL ({frame, sl_offset, parent}, uref) => (
          (* Printtree.printtree(TextIO.stdOut, T.EXP exp); *)
          addFrag(F.PROC {name=label, body=T.MOVE(T.TEMP R.RV, exp), frame=frame})
        )
    end

  fun addTigermainFrag(transExp, level) =
    let
      val exp = unEx transExp
    in
      case level of TOP => (Er.error 0 "wrong level TOP") |
        LEVEL({frame, sl_offset, parent}, uref) => (
      (* Printtree.printtree(TextIO.stdOut, T.EXP exp); *)
      addFrag(F.PROC {name=Te.namedlabel("tigermain"), body=T.MOVE(T.TEMP R.RV, exp), frame=frame})
      )
    end

(* ---------------------------- FUNCTION CALLS ------------------------------ *)
  (* Find the static link given the levels of the fn and calling fn. *)
  fun findFnSlink(lf, lc) = (
    case lf of
      TOP => (
        (* Library function. No static link needed. *)
        T.CONST 0
      ) |
      LEVEL ({frame=f_frame,sl_offset=f_sl_offset,parent=f_parent}, f_ref) => (
        case lc of
          TOP => (
            Er.error 0 "Cannot call a fn from TOP level";
            T.CONST 0
          ) |
          LEVEL ({frame=c_frame, sl_offset=c_slo, parent=c_parent}, c_ref) => (
            if (c_ref = f_ref orelse c_parent = f_parent) then (
              atSum(T.TEMP R.FP, T.CONST c_slo)
            )
            else (
              T.TEMP R.FP
            )
          )
      )
  )

  fun funcCall(label, trexpList (* ARGS *), levelF, levelCaller) = (
    let
      val expList = unExList trexpList
    in (
      case levelF of
        TOP => (
          (* Library function. *)
          Ex (T.CALL (T.NAME label, expList))
        ) |
        LEVEL ({frame, sl_offset, parent}, uref) => (
          let
            val slink = findFnSlink(levelF, levelCaller)
          in
            Ex (T.CALL(T.NAME label, slink::expList))
          end
        )
      )
    end
  )

end (* functor TranslateGen *)
