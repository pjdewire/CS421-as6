signature ENV =
sig
  type access
  type level
  type label
  type ty

  datatype enventry
    = VARentry of {access: access, ty: ty}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table
  type env = enventry Symbol.table

  val base_tenv : tenv
  val base_env : env
end

functor EnvGen(Translate: TRANSLATE) : ENV =

struct

  structure S = Symbol
  structure T = Types

  type access = Translate.access
  type level = Translate.level
  type label = Temp.label
  type ty = T.ty

  datatype enventry
    = VARentry of {access: access, ty: ty}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table

  type env = enventry Symbol.table

  val base_tenv =
    let
      val myTable = ref Symbol.empty
    in
    (
      myTable := Symbol.enter(!myTable,Symbol.symbol("string"),T.STRING);
      myTable := Symbol.enter(!myTable,Symbol.symbol("int"),T.INT);
      !myTable
    )
    end

  val base_env =
    let
      val myTable = ref Symbol.empty
    in
    (
      myTable := Symbol.enter(!myTable,Symbol.symbol("print"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("print")),
                                       formals=[T.STRING],
                                       result=T.UNIT});
      myTable := Symbol.enter(!myTable,Symbol.symbol("flush"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("flush")),
                                       formals=nil,
                                       result=T.UNIT});
      myTable := Symbol.enter(!myTable,Symbol.symbol("getchar"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("getch")),
                                       formals=nil,
                                       result=T.STRING});
      myTable := Symbol.enter(!myTable,Symbol.symbol("ord"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("ord")),
                                       formals=[T.STRING],
                                       result=T.INT});
      myTable := Symbol.enter(!myTable,Symbol.symbol("chr"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("chr")),
                                       formals=[T.INT],
                                       result=T.STRING});
      myTable := Symbol.enter(!myTable,Symbol.symbol("size"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("size")),
                                       formals=[T.STRING],
                                       result=T.INT});
      myTable := Symbol.enter(!myTable,Symbol.symbol("substring"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("substring")),
                                       formals=[T.STRING,T.INT,T.INT],
                                       result=T.STRING});
      myTable := Symbol.enter(!myTable,Symbol.symbol("concat"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("concat")),
                                       formals=[T.STRING,T.STRING],
                                       result=T.STRING});
      myTable := Symbol.enter(!myTable,Symbol.symbol("not"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("not")),
                                       formals=[T.INT],
                                       result=T.INT});
      myTable := Symbol.enter(!myTable,Symbol.symbol("exit"),
                              FUNentry{level=(Translate.outermost),label=(Temp.namedlabel("exit")),
                                       formals=[T.INT],
                                       result=T.UNIT});
      !myTable
    )
    end

end  (* structure Env *)

