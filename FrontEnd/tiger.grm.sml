functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
fun OP(e1, oper, e2, pos) = A.OpExp{left=e1, oper=oper, right=e2, pos=pos}

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\169\000\005\000\169\000\007\000\169\000\009\000\169\000\
\\011\000\169\000\013\000\169\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\169\000\026\000\169\000\
\\030\000\169\000\031\000\169\000\034\000\169\000\035\000\169\000\
\\037\000\169\000\038\000\169\000\042\000\169\000\043\000\169\000\
\\044\000\169\000\000\000\
\\001\000\001\000\170\000\005\000\170\000\007\000\170\000\009\000\170\000\
\\011\000\170\000\013\000\170\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\170\000\026\000\170\000\
\\030\000\170\000\031\000\170\000\034\000\170\000\035\000\170\000\
\\037\000\170\000\038\000\170\000\042\000\170\000\043\000\170\000\
\\044\000\170\000\000\000\
\\001\000\001\000\171\000\005\000\171\000\007\000\171\000\009\000\171\000\
\\011\000\171\000\013\000\171\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\171\000\026\000\171\000\
\\030\000\171\000\031\000\171\000\034\000\171\000\035\000\171\000\
\\037\000\171\000\038\000\171\000\042\000\171\000\043\000\171\000\
\\044\000\171\000\000\000\
\\001\000\001\000\172\000\005\000\172\000\007\000\172\000\009\000\172\000\
\\011\000\172\000\013\000\172\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\172\000\026\000\172\000\
\\030\000\172\000\031\000\172\000\034\000\172\000\035\000\172\000\
\\037\000\172\000\038\000\172\000\042\000\172\000\043\000\172\000\
\\044\000\172\000\000\000\
\\001\000\001\000\173\000\005\000\173\000\007\000\173\000\009\000\173\000\
\\011\000\173\000\013\000\173\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\173\000\026\000\173\000\
\\030\000\173\000\031\000\173\000\034\000\173\000\035\000\173\000\
\\037\000\173\000\038\000\173\000\042\000\173\000\043\000\173\000\
\\044\000\173\000\000\000\
\\001\000\001\000\174\000\005\000\174\000\007\000\174\000\009\000\174\000\
\\011\000\174\000\013\000\174\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\025\000\174\000\026\000\174\000\
\\030\000\174\000\031\000\174\000\034\000\174\000\035\000\174\000\
\\037\000\174\000\038\000\174\000\042\000\174\000\043\000\174\000\
\\044\000\174\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\009\000\050\000\016\000\014\000\029\000\013\000\032\000\012\000\
\\033\000\011\000\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\009\000\088\000\016\000\014\000\029\000\013\000\032\000\012\000\
\\033\000\011\000\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\029\000\013\000\032\000\012\000\033\000\011\000\
\\036\000\010\000\038\000\092\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\029\000\013\000\032\000\012\000\033\000\011\000\
\\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\055\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\075\000\000\000\
\\001\000\002\000\084\000\000\000\
\\001\000\002\000\084\000\013\000\083\000\000\000\
\\001\000\002\000\112\000\012\000\111\000\028\000\110\000\000\000\
\\001\000\002\000\115\000\000\000\
\\001\000\002\000\118\000\000\000\
\\001\000\002\000\127\000\000\000\
\\001\000\002\000\135\000\000\000\
\\001\000\006\000\095\000\027\000\094\000\000\000\
\\001\000\006\000\130\000\000\000\
\\001\000\006\000\137\000\000\000\
\\001\000\006\000\140\000\019\000\139\000\000\000\
\\001\000\007\000\081\000\009\000\080\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\029\000\020\000\028\000\
\\021\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\025\000\023\000\026\000\022\000\000\000\
\\001\000\007\000\081\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\038\000\108\000\000\000\
\\001\000\008\000\096\000\000\000\
\\001\000\009\000\079\000\000\000\
\\001\000\009\000\105\000\000\000\
\\001\000\009\000\129\000\000\000\
\\001\000\011\000\089\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\001\000\011\000\104\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\001\000\013\000\102\000\000\000\
\\001\000\013\000\136\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\030\000\078\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\034\000\119\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\035\000\077\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\035\000\142\000\000\000\
\\001\000\019\000\093\000\000\000\
\\001\000\019\000\103\000\000\000\
\\001\000\019\000\150\000\000\000\
\\001\000\027\000\076\000\000\000\
\\001\000\027\000\128\000\000\000\
\\001\000\037\000\072\000\000\000\
\\001\000\038\000\107\000\000\000\
\\001\000\039\000\124\000\000\000\
\\155\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\010\000\021\000\014\000\020\000\027\000\019\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\017\000\031\000\018\000\030\000\000\000\
\\164\000\017\000\031\000\018\000\030\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\000\000\
\\168\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\181\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\182\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\188\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\031\000\120\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\005\000\106\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\193\000\000\000\
\\194\000\005\000\133\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\195\000\000\000\
\\196\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\197\000\042\000\043\000\043\000\042\000\044\000\041\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\044\000\041\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\002\000\127\000\000\000\
\\209\000\000\000\
\\210\000\005\000\149\000\000\000\
\\211\000\000\000\
\\212\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\213\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\214\000\042\000\043\000\000\000\
\\215\000\000\000\
\\216\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\217\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\218\000\002\000\118\000\000\000\
\\219\000\000\000\
\\220\000\005\000\147\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\008\000\053\000\010\000\052\000\012\000\051\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\039\000\122\000\000\000\
\\227\000\007\000\081\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\228\000\000\000\
\"
val actionRowNumbers =
"\010\000\067\000\063\000\054\000\
\\053\000\049\000\050\000\071\000\
\\085\000\011\000\010\000\010\000\
\\010\000\007\000\052\000\051\000\
\\111\000\010\000\012\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\102\000\089\000\088\000\087\000\
\\090\000\085\000\046\000\013\000\
\\014\000\015\000\044\000\039\000\
\\037\000\064\000\030\000\027\000\
\\077\000\017\000\010\000\008\000\
\\068\000\112\000\033\000\062\000\
\\061\000\004\000\003\000\006\000\
\\005\000\002\000\001\000\060\000\
\\059\000\058\000\057\000\103\000\
\\091\000\086\000\009\000\041\000\
\\023\000\029\000\010\000\010\000\
\\010\000\055\000\056\000\010\000\
\\035\000\065\000\042\000\034\000\
\\031\000\080\000\078\000\113\000\
\\047\000\028\000\072\000\018\000\
\\010\000\019\000\106\000\038\000\
\\069\000\076\000\116\000\115\000\
\\066\000\010\000\114\000\079\000\
\\010\000\074\000\073\000\092\000\
\\048\000\096\000\093\000\100\000\
\\045\000\110\000\032\000\107\000\
\\024\000\010\000\010\000\082\000\
\\010\000\081\000\022\000\097\000\
\\036\000\025\000\010\000\026\000\
\\019\000\040\000\075\000\016\000\
\\084\000\095\000\094\000\019\000\
\\101\000\010\000\019\000\108\000\
\\010\000\083\000\098\000\104\000\
\\043\000\020\000\070\000\021\000\
\\010\000\109\000\099\000\105\000\
\\000\000"
val gotoT =
"\
\\001\000\005\000\002\000\152\000\003\000\004\000\006\000\003\000\
\\007\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\038\000\011\000\037\000\013\000\036\000\014\000\035\000\
\\015\000\034\000\016\000\033\000\017\000\032\000\000\000\
\\000\000\
\\001\000\043\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\044\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\045\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\047\000\003\000\004\000\005\000\046\000\006\000\003\000\
\\007\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\052\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\001\000\054\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\055\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\056\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\057\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\058\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\059\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\060\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\061\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\062\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\063\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\064\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\065\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\066\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\016\000\067\000\017\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\036\000\014\000\068\000\000\000\
\\010\000\069\000\011\000\037\000\013\000\036\000\014\000\035\000\
\\015\000\034\000\016\000\033\000\017\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\080\000\000\000\
\\001\000\083\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\085\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\012\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\089\000\003\000\004\000\005\000\088\000\006\000\003\000\
\\007\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\095\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\096\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\097\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\099\000\003\000\004\000\005\000\098\000\006\000\003\000\
\\007\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\107\000\000\000\
\\001\000\111\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\020\000\112\000\000\000\
\\018\000\115\000\019\000\114\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\119\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\085\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\012\000\121\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\124\000\023\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\129\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\001\000\130\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\001\000\132\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\136\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\020\000\139\000\000\000\
\\000\000\
\\000\000\
\\008\000\141\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\142\000\000\000\
\\000\000\
\\001\000\143\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\020\000\144\000\000\000\
\\000\000\
\\001\000\146\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\149\000\000\000\
\\000\000\
\\023\000\150\000\000\000\
\\001\000\151\000\003\000\004\000\006\000\003\000\007\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 153
val numrules = 74
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | one_or_more of unit ->  (A.tfield list)
 | tyfields of unit ->  (A.tfield list) | ty of unit ->  (A.ty)
 | tyid of unit ->  (string) | params of unit ->  (A.formals list)
 | params_one_or_more of unit ->  (A.formals list)
 | fundec of unit ->  (A.fundec)
 | fundec_list of unit ->  (A.fundec list)
 | vardec of unit ->  (A.dec)
 | tydec_list of unit ->  ({ name:Symbol.symbol,ty:A.ty,pos:A.pos }  list)
 | tydec of unit ->  ({ name:Symbol.symbol,ty:A.ty,pos:A.pos } )
 | args of unit ->  (A.exp list) | dec of unit ->  (A.dec)
 | decs of unit ->  (A.dec list) | arr_cre of unit ->  (A.exp)
 | rec_list of unit ->  (A.efield list) | funcal of unit ->  (A.exp)
 | nonvalue of unit ->  (A.exp)
 | expseq of unit ->  ( ( A.exp * A.pos )  list)
 | valueless_exp of unit ->  (A.exp) | lvalue of unit ->  (A.var)
 | program of unit ->  (A.exp) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change = 
(nil
,(T 29) :: nil
)::
(nil
,(T 30) :: nil
)::
(nil
,(T 7) :: nil
)::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UNARYMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms = (T 0) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: (T 8) :: (T 9
) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: (T 14) :: (T 15) :: (T 16
) :: (T 17) :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22) :: (T 23
) :: (T 24) :: (T 25) :: (T 26) :: (T 27) :: (T 28) :: (T 29) :: (T 30
) :: (T 31) :: (T 32) :: (T 33) :: (T 34) :: (T 35) :: (T 36) :: (T 37
) :: (T 38) :: (T 39) :: (T 40) :: (T 41) :: (T 42) :: (T 43) :: (T 44
) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.exp exp1,exp1left,exp1right))::rest671) => let val 
result=MlyValue.program(fn _ => let val exp as exp1=exp1 ()
 in (exp) end
)
 in (LrTable.NT 1,(result,exp1left,exp1right),rest671) end
| (1,(_,(_,NIL1left,NIL1right))::rest671) => let val result=
MlyValue.exp(fn _ => (A.NilExp))
 in (LrTable.NT 0,(result,NIL1left,NIL1right),rest671) end
| (2,(_,(MlyValue.INT INT1,INT1left,INT1right))::rest671) => let val 
result=MlyValue.exp(fn _ => let val INT as INT1=INT1 ()
 in (A.IntExp(INT)) end
)
 in (LrTable.NT 0,(result,INT1left,INT1right),rest671) end
| (3,(_,(MlyValue.STRING STRING1,STRINGleft as STRING1left,
STRING1right))::rest671) => let val result=MlyValue.exp(fn _ => let 
val STRING as STRING1=STRING1 ()
 in (A.StringExp(STRING, STRINGleft)) end
)
 in (LrTable.NT 0,(result,STRING1left,STRING1right),rest671) end
| (4,(_,(MlyValue.lvalue lvalue1,lvalue1left,lvalue1right))::rest671)
 => let val result=MlyValue.exp(fn _ => let val lvalue as lvalue1=
lvalue1 ()
 in (A.VarExp(lvalue)) end
)
 in (LrTable.NT 0,(result,lvalue1left,lvalue1right),rest671) end
| (5,(_,(MlyValue.nonvalue nonvalue1,nonvalue1left,nonvalue1right))::
rest671) => let val result=MlyValue.exp(fn _ => let val nonvalue as 
nonvalue1=nonvalue1 ()
 in (nonvalue) end
)
 in (LrTable.NT 0,(result,nonvalue1left,nonvalue1right),rest671) end
| (6,(_,(_,_,RPAREN1right))::(_,(MlyValue.expseq expseq1,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.exp(fn _ => let 
val expseq as expseq1=expseq1 ()
 in (A.SeqExp(expseq)) end
)
 in (LrTable.NT 0,(result,LPAREN1left,RPAREN1right),rest671) end
| (7,(_,(_,_,RPAREN1right))::(_,(MlyValue.exp exp1,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.exp(fn _ => let 
val exp as exp1=exp1 ()
 in (exp) end
)
 in (LrTable.NT 0,(result,LPAREN1left,RPAREN1right),rest671) end
| (8,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,PLUSleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.PlusOp, exp2, PLUSleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (9,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,MINUSleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.MinusOp, exp2, MINUSleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (10,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,TIMESleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.TimesOp, exp2, TIMESleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (11,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,DIVIDEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.DivideOp, exp2, DIVIDEleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (12,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ANDleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (
A.IfExp{test=exp1, then'=exp2, 
				 else'=SOME(A.IntExp(0)), 
				 pos=ANDleft}
) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (13,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ORleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (
A.IfExp{test=exp1, then'=A.IntExp(1),
				 else'= SOME(exp2), pos=ORleft}
) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (14,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,EQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.EqOp, exp2, EQleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (15,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,NEQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.NeqOp, exp2, NEQleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (16,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.GtOp, exp2, GTleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (17,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.GeOp, exp2, GEleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (18,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.LtOp, exp2, LTleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (19,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (OP(exp1, A.LeOp, exp2, LEleft)) end
)
 in (LrTable.NT 0,(result,exp1left,exp2right),rest671) end
| (20,(_,(MlyValue.funcal funcal1,funcal1left,funcal1right))::rest671)
 => let val result=MlyValue.exp(fn _ => let val funcal as funcal1=
funcal1 ()
 in (funcal) end
)
 in (LrTable.NT 0,(result,funcal1left,funcal1right),rest671) end
| (21,(_,(MlyValue.exp exp1,_,exp1right))::(_,(_,MINUSleft as 
MINUS1left,_))::rest671) => let val result=MlyValue.exp(fn _ => let 
val exp as exp1=exp1 ()
 in (OP(A.IntExp(0), A.MinusOp, 
				 exp, MINUSleft)) end
)
 in (LrTable.NT 0,(result,MINUS1left,exp1right),rest671) end
| (22,(_,(_,_,RBRACE1right))::_::(_,(MlyValue.ID ID1,IDleft as ID1left
,_))::rest671) => let val result=MlyValue.exp(fn _ => let val ID as 
ID1=ID1 ()
 in (A.RecordExp{typ=Symbol.symbol(ID), 
				 fields=nil, pos=IDleft})
 end
)
 in (LrTable.NT 0,(result,ID1left,RBRACE1right),rest671) end
| (23,(_,(_,_,RBRACE1right))::(_,(MlyValue.rec_list rec_list1,_,_))::_
::(_,(MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val 
result=MlyValue.exp(fn _ => let val ID as ID1=ID1 ()
val rec_list as rec_list1=rec_list1 ()
 in (
A.RecordExp{typ=Symbol.symbol(ID), 
				 fields=rec_list, pos=IDleft})
 end
)
 in (LrTable.NT 0,(result,ID1left,RBRACE1right),rest671) end
| (24,(_,(MlyValue.arr_cre arr_cre1,arr_cre1left,arr_cre1right))::
rest671) => let val result=MlyValue.exp(fn _ => let val arr_cre as 
arr_cre1=arr_cre1 ()
 in (arr_cre) end
)
 in (LrTable.NT 0,(result,arr_cre1left,arr_cre1right),rest671) end
| (25,(_,(MlyValue.exp exp1,_,exp1right))::(_,(_,ASSIGNleft,_))::(_,(
MlyValue.lvalue lvalue1,lvalue1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val lvalue as lvalue1=lvalue1 ()
val exp as exp1=exp1 ()
 in (A.AssignExp {var=lvalue, exp=exp1,
				 pos=ASSIGNleft}) end
)
 in (LrTable.NT 0,(result,lvalue1left,exp1right),rest671) end
| (26,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,_,
_))::(_,(_,WHILEleft as WHILE1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (A.WhileExp{test=exp1, body=exp2, 
				 pos=WHILEleft}) end
)
 in (LrTable.NT 0,(result,WHILE1left,exp2right),rest671) end
| (27,(_,(MlyValue.exp exp3,_,exp3right))::_::(_,(MlyValue.exp exp2,_,
_))::_::(_,(MlyValue.exp exp1,_,_))::_::(_,(MlyValue.ID ID1,_,_))::(_,
(_,FORleft as FOR1left,_))::rest671) => let val result=MlyValue.exp(
fn _ => let val ID as ID1=ID1 ()
val exp1=exp1 ()
val exp2=exp2 ()
val exp3=exp3 ()
 in (
A.ForExp {var={name=Symbol.symbol(ID),
				                escape=ref true},
					   lo=exp1,
					   hi=exp2,
					   body=exp3,
					   pos=FORleft}
) end
)
 in (LrTable.NT 0,(result,FOR1left,exp3right),rest671) end
| (28,(_,(_,BREAKleft as BREAK1left,BREAK1right))::rest671) => let 
val result=MlyValue.exp(fn _ => (A.BreakExp (BREAKleft)))
 in (LrTable.NT 0,(result,BREAK1left,BREAK1right),rest671) end
| (29,(_,(_,_,END1right))::_::(_,(MlyValue.decs decs1,_,_))::(_,(_,
LETleft as LET1left,_))::rest671) => let val result=MlyValue.exp(fn _
 => let val decs as decs1=decs1 ()
 in (A.LetExp {decs=decs, body=A.SeqExp([]), 
				 pos=LETleft}) end
)
 in (LrTable.NT 0,(result,LET1left,END1right),rest671) end
| (30,(_,(_,_,END1right))::(_,(MlyValue.exp exp1,_,_))::_::(_,(
MlyValue.decs decs1,_,_))::(_,(_,LETleft as LET1left,_))::rest671) => 
let val result=MlyValue.exp(fn _ => let val decs as decs1=decs1 ()
val exp as exp1=exp1 ()
 in (A.LetExp {decs=decs, body=exp,
				 pos=LETleft}) end
)
 in (LrTable.NT 0,(result,LET1left,END1right),rest671) end
| (31,(_,(_,_,END1right))::(_,(MlyValue.expseq expseq1,_,_))::_::(_,(
MlyValue.decs decs1,_,_))::(_,(_,LETleft as LET1left,_))::rest671) => 
let val result=MlyValue.exp(fn _ => let val decs as decs1=decs1 ()
val expseq as expseq1=expseq1 ()
 in (A.LetExp {decs=decs, body=A.SeqExp(expseq),
				 pos=LETleft})
 end
)
 in (LrTable.NT 0,(result,LET1left,END1right),rest671) end
| (32,(_,(MlyValue.exp exp3,_,exp3right))::_::(_,(MlyValue.exp exp2,_,
_))::_::(_,(MlyValue.exp exp1,_,_))::(_,(_,IFleft as IF1left,_))::
rest671) => let val result=MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
val exp3=exp3 ()
 in (
A.IfExp {test=exp1, then'=exp2, 
					  else'=SOME(exp3), pos=IFleft})
 end
)
 in (LrTable.NT 0,(result,IF1left,exp3right),rest671) end
| (33,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,_,
_))::(_,(_,IFleft as IF1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in (A.IfExp {test=exp1, then'=exp2,
					  else'=NONE, pos=IFleft})
 end
)
 in (LrTable.NT 0,(result,IF1left,exp2right),rest671) end
| (34,(_,(_,_,RPAREN1right))::(_,(_,LPAREN1left,_))::rest671) => let 
val result=MlyValue.nonvalue(fn _ => (A.SeqExp([])))
 in (LrTable.NT 5,(result,LPAREN1left,RPAREN1right),rest671) end
| (35,(_,(_,_,RPAREN1right))::_::(_,(MlyValue.ID ID1,IDleft as ID1left
,_))::rest671) => let val result=MlyValue.funcal(fn _ => let val ID
 as ID1=ID1 ()
 in (A.AppExp{func=Symbol.symbol(ID), 
					args=nil, pos=IDleft}) end
)
 in (LrTable.NT 6,(result,ID1left,RPAREN1right),rest671) end
| (36,(_,(_,_,RPAREN1right))::(_,(MlyValue.args args1,_,_))::_::(_,(
MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val result=
MlyValue.funcal(fn _ => let val ID as ID1=ID1 ()
val args as args1=args1 ()
 in (A.AppExp{func=Symbol.symbol(ID), 
					args=args, pos=IDleft})
 end
)
 in (LrTable.NT 6,(result,ID1left,RPAREN1right),rest671) end
| (37,(_,(MlyValue.exp exp1,exp1left,exp1right))::rest671) => let val 
result=MlyValue.args(fn _ => let val exp as exp1=exp1 ()
 in ([exp]) end
)
 in (LrTable.NT 11,(result,exp1left,exp1right),rest671) end
| (38,(_,(MlyValue.args args1,_,args1right))::_::(_,(MlyValue.exp exp1
,exp1left,_))::rest671) => let val result=MlyValue.args(fn _ => let 
val exp as exp1=exp1 ()
val args as args1=args1 ()
 in (exp::args) end
)
 in (LrTable.NT 11,(result,exp1left,args1right),rest671) end
| (39,(_,(MlyValue.exp exp1,_,exp1right))::_::(_,(MlyValue.ID ID1,
IDleft as ID1left,_))::rest671) => let val result=MlyValue.rec_list(
fn _ => let val ID as ID1=ID1 ()
val exp as exp1=exp1 ()
 in ([(Symbol.symbol(ID), exp, IDleft)]) end
)
 in (LrTable.NT 7,(result,ID1left,exp1right),rest671) end
| (40,(_,(MlyValue.rec_list rec_list1,_,rec_list1right))::_::(_,(
MlyValue.exp exp1,_,_))::_::(_,(MlyValue.ID ID1,IDleft as ID1left,_))
::rest671) => let val result=MlyValue.rec_list(fn _ => let val ID as 
ID1=ID1 ()
val exp as exp1=exp1 ()
val rec_list as rec_list1=rec_list1 ()
 in ([(Symbol.symbol(ID), exp, IDleft)]
				 @rec_list) end
)
 in (LrTable.NT 7,(result,ID1left,rec_list1right),rest671) end
| (41,(_,(MlyValue.exp exp2,_,exp2right))::_::_::(_,(MlyValue.exp exp1
,_,_))::_::(_,(MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let 
val result=MlyValue.arr_cre(fn _ => let val ID as ID1=ID1 ()
val exp1=exp1 ()
val exp2=exp2 ()
 in (
A.ArrayExp {typ=Symbol.symbol(ID),
					size=exp1, init=exp2, pos=IDleft}
) end
)
 in (LrTable.NT 8,(result,ID1left,exp2right),rest671) end
| (42,rest671) => let val result=MlyValue.decs(fn _ => (nil))
 in (LrTable.NT 9,(result,defaultPos,defaultPos),rest671) end
| (43,(_,(MlyValue.decs decs1,_,decs1right))::(_,(MlyValue.dec dec1,
dec1left,_))::rest671) => let val result=MlyValue.decs(fn _ => let 
val dec as dec1=dec1 ()
val decs as decs1=decs1 ()
 in (dec::decs) end
)
 in (LrTable.NT 9,(result,dec1left,decs1right),rest671) end
| (44,(_,(MlyValue.tydec_list tydec_list1,tydec_list1left,
tydec_list1right))::rest671) => let val result=MlyValue.dec(fn _ => 
let val tydec_list as tydec_list1=tydec_list1 ()
 in (A.TypeDec(tydec_list)) end
)
 in (LrTable.NT 10,(result,tydec_list1left,tydec_list1right),rest671)
 end
| (45,(_,(MlyValue.vardec vardec1,vardec1left,vardec1right))::rest671)
 => let val result=MlyValue.dec(fn _ => let val vardec as vardec1=
vardec1 ()
 in (vardec) end
)
 in (LrTable.NT 10,(result,vardec1left,vardec1right),rest671) end
| (46,(_,(MlyValue.fundec_list fundec_list1,fundec_list1left,
fundec_list1right))::rest671) => let val result=MlyValue.dec(fn _ => 
let val fundec_list as fundec_list1=fundec_list1 ()
 in (A.FunctionDec (fundec_list)) end
)
 in (LrTable.NT 10,(result,fundec_list1left,fundec_list1right),rest671
) end
| (47,(_,(MlyValue.tydec tydec1,tydec1left,tydec1right))::rest671) => 
let val result=MlyValue.tydec_list(fn _ => let val tydec as tydec1=
tydec1 ()
 in ([tydec]) end
)
 in (LrTable.NT 13,(result,tydec1left,tydec1right),rest671) end
| (48,(_,(MlyValue.tydec_list tydec_list1,_,tydec_list1right))::(_,(
MlyValue.tydec tydec1,tydec1left,_))::rest671) => let val result=
MlyValue.tydec_list(fn _ => let val tydec as tydec1=tydec1 ()
val tydec_list as tydec_list1=tydec_list1 ()
 in ([tydec]@tydec_list) end
)
 in (LrTable.NT 13,(result,tydec1left,tydec_list1right),rest671) end
| (49,(_,(MlyValue.ty ty1,_,ty1right))::_::(_,(MlyValue.ID ID1,_,_))::
(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val result=
MlyValue.tydec(fn _ => let val ID as ID1=ID1 ()
val ty as ty1=ty1 ()
 in ({name=Symbol.symbol(ID), ty=ty,
				 pos=TYPEleft}) end
)
 in (LrTable.NT 12,(result,TYPE1left,ty1right),rest671) end
| (50,(_,(MlyValue.ID ID1,IDleft as ID1left,ID1right))::rest671) => 
let val result=MlyValue.ty(fn _ => let val ID as ID1=ID1 ()
 in (A.NameTy (Symbol.symbol(ID), IDleft)) end
)
 in (LrTable.NT 20,(result,ID1left,ID1right),rest671) end
| (51,(_,(_,_,RBRACE1right))::(_,(MlyValue.tyfields tyfields1,_,_))::(
_,(_,LBRACE1left,_))::rest671) => let val result=MlyValue.ty(fn _ => 
let val tyfields as tyfields1=tyfields1 ()
 in (A.RecordTy (tyfields)) end
)
 in (LrTable.NT 20,(result,LBRACE1left,RBRACE1right),rest671) end
| (52,(_,(MlyValue.ID ID1,_,ID1right))::_::(_,(_,ARRAYleft as 
ARRAY1left,_))::rest671) => let val result=MlyValue.ty(fn _ => let 
val ID as ID1=ID1 ()
 in (A.ArrayTy (Symbol.symbol(ID), ARRAYleft)) end
)
 in (LrTable.NT 20,(result,ARRAY1left,ID1right),rest671) end
| (53,rest671) => let val result=MlyValue.tyfields(fn _ => ([]))
 in (LrTable.NT 21,(result,defaultPos,defaultPos),rest671) end
| (54,(_,(MlyValue.one_or_more one_or_more1,one_or_more1left,
one_or_more1right))::rest671) => let val result=MlyValue.tyfields(fn _
 => let val one_or_more as one_or_more1=one_or_more1 ()
 in (one_or_more) end
)
 in (LrTable.NT 21,(result,one_or_more1left,one_or_more1right),rest671
) end
| (55,(_,(MlyValue.tyid tyid1,_,tyid1right))::_::(_,(MlyValue.ID ID1,
IDleft as ID1left,_))::rest671) => let val result=MlyValue.one_or_more
(fn _ => let val ID as ID1=ID1 ()
val tyid as tyid1=tyid1 ()
 in (
[{name=Symbol.symbol(ID), 
				 typ=Symbol.symbol(tyid), 
				 pos=IDleft}]
) end
)
 in (LrTable.NT 22,(result,ID1left,tyid1right),rest671) end
| (56,(_,(MlyValue.one_or_more one_or_more1,_,one_or_more1right))::_::
(_,(MlyValue.tyid tyid1,_,_))::_::(_,(MlyValue.ID ID1,IDleft as 
ID1left,_))::rest671) => let val result=MlyValue.one_or_more(fn _ => 
let val ID as ID1=ID1 ()
val tyid as tyid1=tyid1 ()
val one_or_more as one_or_more1=one_or_more1 ()
 in (
[{name=Symbol.symbol(ID), 
				 	typ=Symbol.symbol(tyid), 
					pos=IDleft}]
					@one_or_more
) end
)
 in (LrTable.NT 22,(result,ID1left,one_or_more1right),rest671) end
| (57,(_,(MlyValue.exp exp1,_,exp1right))::_::(_,(MlyValue.ID ID1,_,_)
)::(_,(_,VARleft as VAR1left,_))::rest671) => let val result=
MlyValue.vardec(fn _ => let val ID as ID1=ID1 ()
val exp as exp1=exp1 ()
 in (
A.VarDec {var={name=Symbol.symbol(ID),
					   	escape=ref false},
					   typ=NONE, init=exp, pos=VARleft}
) end
)
 in (LrTable.NT 14,(result,VAR1left,exp1right),rest671) end
| (58,(_,(MlyValue.exp exp1,_,exp1right))::_::(_,(MlyValue.tyid tyid1,
tyidleft,_))::_::(_,(MlyValue.ID ID1,_,_))::(_,(_,VARleft as VAR1left,
_))::rest671) => let val result=MlyValue.vardec(fn _ => let val ID as 
ID1=ID1 ()
val tyid as tyid1=tyid1 ()
val exp as exp1=exp1 ()
 in (
A.VarDec {var={name=Symbol.symbol(ID),
					   	escape=ref false},
					   typ=SOME(Symbol.symbol(tyid),
						    tyidleft), 
					   init=exp, pos=VARleft}
) end
)
 in (LrTable.NT 14,(result,VAR1left,exp1right),rest671) end
| (59,(_,(MlyValue.fundec fundec1,fundec1left,fundec1right))::rest671)
 => let val result=MlyValue.fundec_list(fn _ => let val fundec as 
fundec1=fundec1 ()
 in ([fundec]) end
)
 in (LrTable.NT 15,(result,fundec1left,fundec1right),rest671) end
| (60,(_,(MlyValue.fundec_list fundec_list1,_,fundec_list1right))::(_,
(MlyValue.fundec fundec1,fundec1left,_))::rest671) => let val result=
MlyValue.fundec_list(fn _ => let val fundec as fundec1=fundec1 ()
val fundec_list as fundec_list1=fundec_list1 ()
 in ([fundec]@fundec_list) end
)
 in (LrTable.NT 15,(result,fundec1left,fundec_list1right),rest671) end
| (61,(_,(MlyValue.exp exp1,_,exp1right))::_::_::(_,(MlyValue.params 
params1,_,_))::_::(_,(MlyValue.ID ID1,_,_))::(_,(_,FUNCTIONleft as 
FUNCTION1left,_))::rest671) => let val result=MlyValue.fundec(fn _ => 
let val ID as ID1=ID1 ()
val params as params1=params1 ()
val exp as exp1=exp1 ()
 in (
{name=Symbol.symbol(ID),
				 params=params,
				 result= NONE,
				 body=exp,
				 pos=FUNCTIONleft}
) end
)
 in (LrTable.NT 16,(result,FUNCTION1left,exp1right),rest671) end
| (62,(_,(MlyValue.exp exp1,_,exp1right))::_::(_,(MlyValue.tyid tyid1,
tyidleft,_))::_::_::(_,(MlyValue.params params1,_,_))::_::(_,(
MlyValue.ID ID1,_,_))::(_,(_,FUNCTIONleft as FUNCTION1left,_))::
rest671) => let val result=MlyValue.fundec(fn _ => let val ID as ID1=
ID1 ()
val params as params1=params1 ()
val tyid as tyid1=tyid1 ()
val exp as exp1=exp1 ()
 in (
{name=Symbol.symbol(ID),
                                 params=params,
                                 result=SOME(Symbol.symbol(tyid),
                                             tyidleft),
                                 body=exp,
                                 pos=FUNCTIONleft}
) end
)
 in (LrTable.NT 16,(result,FUNCTION1left,exp1right),rest671) end
| (63,rest671) => let val result=MlyValue.params(fn _ => ([]))
 in (LrTable.NT 18,(result,defaultPos,defaultPos),rest671) end
| (64,(_,(MlyValue.params_one_or_more params_one_or_more1,
params_one_or_more1left,params_one_or_more1right))::rest671) => let 
val result=MlyValue.params(fn _ => let val params_one_or_more as 
params_one_or_more1=params_one_or_more1 ()
 in (params_one_or_more) end
)
 in (LrTable.NT 18,(result,params_one_or_more1left,
params_one_or_more1right),rest671) end
| (65,(_,(MlyValue.tyid tyid1,_,tyid1right))::_::(_,(MlyValue.ID ID1,
IDleft as ID1left,_))::rest671) => let val result=
MlyValue.params_one_or_more(fn _ => let val ID as ID1=ID1 ()
val tyid as tyid1=tyid1 ()
 in (
[{var={name=Symbol.symbol(ID), 
				       escape=ref true },
                                typ=Symbol.symbol(tyid),
                                pos=IDleft}]
) end
)
 in (LrTable.NT 17,(result,ID1left,tyid1right),rest671) end
| (66,(_,(MlyValue.params_one_or_more params_one_or_more1,_,
params_one_or_more1right))::_::(_,(MlyValue.tyid tyid1,_,_))::_::(_,(
MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val result=
MlyValue.params_one_or_more(fn _ => let val ID as ID1=ID1 ()
val tyid as tyid1=tyid1 ()
val params_one_or_more as params_one_or_more1=params_one_or_more1 ()
 in (
({var={name=Symbol.symbol(ID), 
				       escape=ref true },
                                typ=Symbol.symbol(tyid),
                                pos=IDleft})
                                ::params_one_or_more
) end
)
 in (LrTable.NT 17,(result,ID1left,params_one_or_more1right),rest671)
 end
| (67,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.tyid(fn _ => let val ID as ID1=ID1 ()
 in (ID) end
)
 in (LrTable.NT 19,(result,ID1left,ID1right),rest671) end
| (68,(_,(MlyValue.ID ID1,IDleft as ID1left,ID1right))::rest671) => 
let val result=MlyValue.lvalue(fn _ => let val ID as ID1=ID1 ()
 in (A.SimpleVar(Symbol.symbol(ID), IDleft)) end
)
 in (LrTable.NT 2,(result,ID1left,ID1right),rest671) end
| (69,(_,(MlyValue.ID ID1,_,ID1right))::(_,(_,DOTleft,_))::(_,(
MlyValue.lvalue lvalue1,lvalue1left,_))::rest671) => let val result=
MlyValue.lvalue(fn _ => let val lvalue as lvalue1=lvalue1 ()
val ID as ID1=ID1 ()
 in (
A.FieldVar(lvalue, 
				 	    Symbol.symbol(ID), 
					    DOTleft))
 end
)
 in (LrTable.NT 2,(result,lvalue1left,ID1right),rest671) end
| (70,(_,(_,_,RBRACK1right))::(_,(MlyValue.exp exp1,_,_))::(_,(_,
LBRACKleft,_))::(_,(MlyValue.lvalue lvalue1,lvalue1left,_))::rest671)
 => let val result=MlyValue.lvalue(fn _ => let val lvalue as lvalue1=
lvalue1 ()
val exp as exp1=exp1 ()
 in (A.SubscriptVar(lvalue, 
				 	    exp,
					    LBRACKleft)) end
)
 in (LrTable.NT 2,(result,lvalue1left,RBRACK1right),rest671) end
| (71,(_,(_,_,RBRACK1right))::(_,(MlyValue.exp exp1,_,_))::_::(_,(
MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val result=
MlyValue.lvalue(fn _ => let val ID as ID1=ID1 ()
val exp as exp1=exp1 ()
 in (
A.SubscriptVar (A.SimpleVar(Symbol.symbol(ID),
						   	     IDleft),
						 exp, IDleft)
) end
)
 in (LrTable.NT 2,(result,ID1left,RBRACK1right),rest671) end
| (72,(_,(MlyValue.exp exp2,exp2left,exp2right))::_::(_,(MlyValue.exp 
exp1,exp1left,_))::rest671) => let val result=MlyValue.expseq(fn _ => 
let val exp1=exp1 ()
val exp2=exp2 ()
 in ([(exp1, exp1left), (exp2, exp2left)]) end
)
 in (LrTable.NT 4,(result,exp1left,exp2right),rest671) end
| (73,(_,(MlyValue.expseq expseq1,_,expseq1right))::_::(_,(
MlyValue.exp exp1,expleft as exp1left,_))::rest671) => let val result=
MlyValue.expseq(fn _ => let val exp as exp1=exp1 ()
val expseq as expseq1=expseq1 ()
 in ([(exp, expleft)]@expseq) end
)
 in (LrTable.NT 4,(result,exp1left,expseq1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UNARYMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
