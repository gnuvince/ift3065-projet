;;; File: "scanner.l"

sign   [-+]
digit  [0-9]
letter [a-zA-Z]
other  [_*/$!@<=>%]
ident  ({sign}|({letter}|{other})({letter}|{other}|{sign}|{digit})*)

%%

[ \n]+           (yycontinue)
{sign}?{digit}+  (cons 'INTEGER (string->number yytext))
{ident}          (cons 'IDENT   (string->symbol yytext))
"("              (cons '|(|     #f)
")"              (cons '|)|     #f)
<<EOF>>          (cons '*eoi*   #f)
<<ERROR>>        (error "invalid token")
