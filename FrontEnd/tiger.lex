type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue,pos) token

val MaxInt = 1

val trstr   = ref ""
val com_str_start = ref 0
val instring = ref false
val level  = ref 0

fun inc x = (x := !x + 1)
fun dec x = (x := !x - 1)

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1 p2

val makeint = valOf o Int.fromString

fun dolinepos(p, nil) = ()
  | dolinepos(p, "\n"::txtlist) =  (inc lineNum; linePos := p :: !linePos; dolinepos(p+1,txtlist))
  | dolinepos(p, c::txtlist) =  dolinepos(p+1,txtlist)

val eof = fn () => let 
			val pos = hd(!linePos) 
		   in 
			if (!level > 0) orelse !instring
			  then (err(!com_str_start,  "Unexpected EOF. Unmatched string or comment.");Tokens.EOF(pos,pos))
			else Tokens.EOF(pos,pos) 
	     	   end

%% 
%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s COMMENT STRING;
ANYPRCHAR	= ([\t\033\035-\091]|[\093-\126]);
SPACE		= [ \t\n\012];
BACKCHAR	= \\([0][0-9][0-9]|[1][0-2][0-7]);
CNTRLCHAR	= \\\^{ANYPRCHAR};

%%
<INITIAL>\n	=> (inc lineNum; linePos := yypos :: !linePos; continue());
<INITIAL>[\t\ ]	=> (continue());

<INITIAL>while	=> (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for	=> (Tokens.FOR(yypos,yypos+3));
<INITIAL>to	=> (Tokens.TO(yypos,yypos+2));
<INITIAL>break	=> (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let	=> (Tokens.LET(yypos,yypos+3));
<INITIAL>in	=> (Tokens.IN(yypos,yypos+2));
<INITIAL>end	=> (Tokens.END(yypos,yypos+3));
<INITIAL>function	=> (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>var	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>type	=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array	=> (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if	=> (Tokens.IF(yypos,yypos+2));
<INITIAL>then	=> (Tokens.THEN(yypos,yypos+4));
<INITIAL>else	=> (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do	=> (Tokens.DO(yypos,yypos+2));
<INITIAL>of	=> (Tokens.OF(yypos,yypos+2));
<INITIAL>nil	=> (Tokens.NIL(yypos,yypos+3));
 
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"	=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."	=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"	=> (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"	=> (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"	=> (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"	=> (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="	=> (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"	=> (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"	=> (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="	=> (Tokens.LE(yypos,yypos+2));
<INITIAL>">"	=> (Tokens.GT(yypos,yypos+1));
<INITIAL>">="	=> (Tokens.GE(yypos,yypos+2));
<INITIAL>"&"	=> (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"	=> (Tokens.OR(yypos,yypos+1));
<INITIAL>":="	=> (Tokens.ASSIGN(yypos,yypos+2));
 


<INITIAL>[a-zA-Z][a-zA-Z0-9_]*	=> (Tokens.ID(yytext, yypos, yypos + size(yytext)));
<INITIAL>[0-9]+			=> (Tokens.INT(makeint yytext handle Overflow => (err(yypos, "Overflow: integer " ^ yytext ^ " too large"); MaxInt), yypos, yypos + size(yytext)));


<INITIAL>\"		=> (YYBEGIN STRING; instring := true; trstr := ""; com_str_start := yypos; continue()); 
<STRING>{ANYPRCHAR}+ 	=> (trstr :=  !trstr ^ yytext;continue());  
<STRING>{CNTRLCHAR}+	=> (trstr :=  !trstr ^ yytext;continue()); 
<STRING>{BACKCHAR}+	=> (trstr :=  !trstr ^ yytext;continue()); 
<STRING>\\\\		=> (trstr :=  !trstr ^ "\\";continue()); 
<STRING>\\n 		=> (trstr :=  !trstr ^ "\n"; continue());
<STRING>\\t 		=> (trstr :=  !trstr ^ "\t"; continue());
<STRING>\\\" 		=> (trstr :=  !trstr ^ "\""; continue());
<STRING>" " 		=> (trstr :=  !trstr ^ " "; continue()); 
<STRING>\\{SPACE}+\\	=> (dolinepos(yypos,map str (explode(yytext)));continue());
<STRING>\"		=> (YYBEGIN INITIAL; instring := false; Tokens.STRING(!trstr,!com_str_start,size(!trstr)));
<STRING>\n|.		=> (err(yypos,"Syntax Error. Invalid character " ^ yytext ^ " in string literal \"" ^ !trstr);continue());


<INITIAL>"/*"		=> (YYBEGIN COMMENT; com_str_start := yypos; level := 1; continue());
<COMMENT>"/*"		=> (inc level; continue());
<COMMENT>"*/"		=> (dec level ; if !level = 0 
						then YYBEGIN INITIAL 
						else YYBEGIN COMMENT; 
		            continue());
<COMMENT>\n		=> (inc lineNum; linePos := yypos :: !linePos; continue());
<COMMENT>.		=> (continue());

<INITIAL>.		=> (err(yypos, "Syntax Error " ^ yytext);continue());
