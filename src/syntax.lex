type pos = string -> Coord.t
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type arg = string

val pos = ref Coord.init
val eof = fn (fname : string) => Tokens.EOF (!pos, !pos)

val between = fn yt => Coord.addchar (size yt) o (!pos)
val nextnext = Coord.nextchar o Coord.nextchar

exception LexerError of pos

%%
%arg (fname : string);
%header (functor RewriteLexFun (structure Tokens : Rewrite_TOKENS));
upper = [A-Z];
lower = [a-z];
digit = [0-9];
any   = [@a-zA-Z0-9];
whitespace = [\ \t];
%%

\n                 => (pos := (Coord.nextline o (!pos)); continue ());
{whitespace}+      => (pos := between yytext; continue ());

":rule"            => (Tokens.CMDRULE   (!pos, Coord.nextchar o (!pos)));
":norm"            => (Tokens.CMDNORM   (!pos, Coord.nextchar o (!pos)));

"=>"               => (Tokens.REWRITES  (!pos, nextnext o (!pos)));
","                => (Tokens.COMMA     (!pos, Coord.nextchar o (!pos)));

"("                => (Tokens.LPAREN  (!pos, Coord.nextchar o (!pos)));
")"                => (Tokens.RPAREN  (!pos, Coord.nextchar o (!pos)));

{lower}{any}*      => (Tokens.LIDENT (yytext, !pos, between yytext));
{upper}{any}*      => (Tokens.UIDENT (yytext, !pos, between yytext));
