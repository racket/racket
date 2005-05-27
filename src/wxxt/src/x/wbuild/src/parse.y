/*
**  File: parse.y
**  Date: 28 Apr 1992
**  Description: YACC grammar for Widget Builder
**  Author: Bert Bos <bert@let.rug.nl>
**  Modified by: Joel N. Weber II <nemo@nautilus.sub>
*/

%{
#include <config.h>
#include <libit/alloca.h>
#include <libit/string.h>
#include <libit/unistd.h>

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>
#include <wbuild.h>

#include <libit/malloc.h>

#define new(ptr) ptr = calloc(1, sizeof(*(ptr))) /* set to zeros */
#ifdef YYBISON
#define yerr(m) if (! YYRECOVERING()) yyerror(m)
#else
#define yerr(m) yyerror(m)
#endif

/* extern int lineno; */
extern char *yytext;
extern int yyleng;
char *filename = "<undefined>";
time_t filetime;

/* #define alloca malloc */
/* void *alloca (); */

extern int yylex (void);
extern void err (int fatal, char *format,...);
static void yyerror (const char *s);
static void add_section (Section *psect, Section s);
static Section new_section (STRING, Decl, Section);
static char *no_quotes(char *s);

%}

%union {
  STRING string;
  Doctype doctype;
  Class class;
  Section section;
  Decl decl;
  taglist tag;
  int i;
  struct { char *name; char *open; char *close; } onetag;
  struct { int i; char *s; time_t t; } finfo;
}

%token_table

%token <finfo> ACTIONS
%token <finfo> CLASS
%token <finfo> CLASSVARS
%token <finfo> CONSTRAINTS
%token <finfo> DOCTYPE
%token <finfo> PRIVCONSTRAINTS
%token <finfo> DEF
%token <finfo> EXPORTS
%token <finfo> IMPORTS
%token <finfo> INCL
%token <finfo> METHODS
%token <finfo> PRIVATE
%token <finfo> PROC
%token <finfo> PUBLIC
%token <finfo> TAG
%token <finfo> TRANS
%token <finfo> TRANSLATIONS
%token <finfo> TYPE
%token <finfo> UTILITIES
%token <finfo> VAR
%token <string> BODY
%token <string> BRACKETED
%token <string> COMMENT
%token <string> CSTRING
%token <string> CTOK
%token <string> IDENT
%token <string> NUMBER
%token <string> TEXT
%token <string> DOTDOTDOT
%token COLON
%token COMMA
%token DIRECTORY
%token DOLLAR
%token EQUALS
%token EXCLAM
%token EXTENSION
%token FILE_OPT
%token GUARD
%token GUARDP
%token ILL_BODY
%token ILL_CHAR
%token INCL
%token LBRACK
%token LPAR
%token NOCODE
%token NODOC
%token PLUS
%token RBRACK
%token RPAR
%token SEMI
%token SHORTDOC
%token SLASH
%token STAR
%token TILDE
%token TYPE
%token UNKNOWN

%type <doctype> doctype
%type <tag> docopts
%type <onetag> docopt
%type <class> class
%type <class> sections
%type <decl> actiondef
%type <decl> classvardecl
%type <decl> constraintdef
%type <decl> privconstraintdef
%type <decl> exportdef
%type <decl> importdecl
%type <decl> methoddef
%type <decl> more_params
%type <decl> paramlist
%type <decl> params
%type <decl> privatedecl
%type <decl> pubvardef
%type <decl> transline
%type <decl> type_and_name
%type <decl> utildef
%type <section> actions
%type <section> actionsections
%type <section> actiontextsections
%type <section> classvars
%type <section> classvarsections
%type <section> classvartextsections
%type <section> constraints
%type <section> constraintsections
%type <section> constrainttextsections
%type <section> privconstraints
%type <section> privconstraintsections
%type <section> privconstrainttextsections
%type <section> exports
%type <section> exportsections
%type <section> exporttextsections
%type <section> imports
%type <section> importsections
%type <section> importtextsections
%type <section> methods
%type <section> methodsections
%type <section> methodtextsections
%type <section> privatesections
%type <section> privatetextsections
%type <section> privatevars
%type <section> publicvars
%type <section> pubvarsections
%type <section> pubvartextsections
%type <section> translations
%type <section> transsections
%type <section> transtextsections
%type <section> utilities
%type <section> utilsections
%type <section> utiltextsections
%type <string> C_stuff
%type <string> C_thing
%type <string> actioncall
%type <string> actioncalls
%type <string> arguments
%type <string> class_id
%type <string> count
%type <string> detail
%type <string> event
%type <string> macro_stuff
%type <string> mod_syms
%type <string> modifiers
%type <string> modifier
%type <string> more_arguments
%type <string> more_events
%type <string> stars
%type <string> superclass
%type <string> supername
%type <string> symbol

%%

program
 : class program		{add_class($1);}
 | doctype program		{add_doctype($1);}
 | myerror program		{yerr("error in class");}
 | /* empty */
 ;

doctype
 : DOCTYPE SHORTDOC docopts	{new($$); $$->shortdoc = 1;
					copy_taglist(&($$->tag), &($3));
					$$->next = 0;}
 | DOCTYPE docopts		{new($$); $$->shortdoc = 0;
					copy_taglist(&($$->tag), &($2));
					$$->next = 0;}
 ;

docopts
 : docopts docopt		{copy_taglist(&($$), &($1));
					set_doctag(($$), $2.name, $2.open,
					$2.close);}
 |				{zero_taglist(&($$));}
 ;

docopt
 : TAG IDENT CSTRING CSTRING	{$$.name = get($2);
					$$.open = get($3);
					$$.close = get($4); }
 ;

class
 : CLASS IDENT superclass
   options TEXT sections	{$$ = $6; $$->name = $2; $$->superclass = $3;
				$$->lineno = $1.i; $$->text = $5;
				$$->nocode = classnocode;
				$$->nodoc = classnodoc;
				$$->filename = hash($1.s);
				$$->filenamepart = classfilename;
				$$->filetime = $1.t;
				classnocode = classnodoc = 0;
				classfilename = 0;}
 | CLASS IDENT superclass
   options sections		{$$ = $5; $$->name = $2; $$->superclass = $3;
				$$->lineno = $1.i;
				$$->nocode = classnocode;
				$$->nodoc = classnodoc;
				$$->filename = hash($1.s);
				$$->filenamepart = classfilename;
				$$->filetime = $1.t;
				classnocode = classnodoc = 0;
				classfilename = 0;}
 ;
superclass
 : LPAR supername RPAR		{$$ = $2;}
 | LPAR myerror RPAR		{yerr("name of superclass expected");
				$$ = NULL;}
 | /* empty */			{$$ = NULL;}
 ;
supername
 : IDENT			{$$ = $1;}
 | supername SLASH IDENT	{$$ = catstr(3, get($1), "/", get($3));
				delete($1); delete($3);}
 ;
options
 : options option		{;}
 | /* empty */			{;}
 ;
option
 : NOCODE			{classnocode = 1; }
 | NODOC			{classnodoc = 1; }
 | FILE_OPT EQUALS IDENT	{classfilename = hdup($3); }
 | FILE_OPT EQUALS CSTRING	{char *s; s = no_quotes(get($3));
				classfilename = hash(s); free(s); delete($3);}
 ;


sections
 : sections classvars		{$$ = $1; add_section(&$$->classvars, $2);}
 | sections publicvars		{$$ = $1; add_section(&$$->publicvars, $2);}
 | sections privatevars		{$$ = $1; add_section(&$$->privatevars, $2);}
 | sections constraints		{$$ = $1; add_section(&$$->constraints, $2);}
 | sections privconstraints	{$$ = $1; add_section(&$$->privconstr, $2);}
 | sections methods		{$$ = $1; add_section(&$$->methods, $2);}
 | sections actions		{$$ = $1; add_section(&$$->actions, $2);}
 | sections translations	{$$ = $1; add_section(&$$->translations, $2);}
 | sections imports		{$$ = $1; add_section(&$$->imports, $2);}
 | sections exports		{$$ = $1; add_section(&$$->exports, $2);}
 | sections utilities		{$$ = $1; add_section(&$$->utilities, $2);}   
 | /* empty */			{new($$);}
 ;

publicvars
 : PUBLIC pubvarsections	{$$ = $2;}
 ;
pubvarsections
 : pubvardef pubvarsections	{$$ = new_section(0, $1, $2);}
 | pubvartextsections
 ;
pubvartextsections
 : TEXT pubvardef pubvarsections	{$$ = new_section($1, $2, $3);}
 | TEXT pubvartextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
pubvardef
 : VAR symbol type_and_name
   symbol EQUALS symbol
   C_stuff			{$$ = $3; $$->typesym = $2; $$->valuesym = $6;
				$$->value = $7; $$->namesym = $4; $$->lineno =
				$1.i; $$->tp = Var;}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;
opt_semi
 : SEMI
 | /* empty */
 ;
symbol
 : BRACKETED			{$$ = $1;}
 | /* empty */			{$$ = NULL;}
 ;


constraints
 : CONSTRAINTS
   constraintsections		{$$ = $2;}
 ;
constraintsections
 : constraintdef
   constraintsections		{$$ = new_section(0, $1, $2);}
 | constrainttextsections
 ;
constrainttextsections
 : TEXT constraintdef
   constraintsections		{$$ = new_section($1, $2, $3);}
 | TEXT constrainttextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
constraintdef
 : VAR symbol type_and_name
   symbol EQUALS symbol
   C_stuff	 		{$$ = $3; $$->typesym = $2; $$->valuesym = $6;
				$$->value = $7; $$->namesym = $4; $$->lineno =
				$1.i; $$->tp = Var;}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;


privconstraints
 : PRIVCONSTRAINTS
   privconstraintsections	{$$ = $2;}
 ;
privconstraintsections
 : privconstraintdef
   privconstraintsections	{$$ = new_section(0, $1, $2);}
 | privconstrainttextsections
 ;
privconstrainttextsections
 : TEXT privconstraintdef
   privconstraintsections	{$$ = new_section($1, $2, $3);}
 | TEXT
   privconstrainttextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
privconstraintdef
 : VAR type_and_name opt_semi	{$$ = $2; $$->lineno = $1.i; $$->tp = Var;}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;


actions
 : ACTIONS actionsections	{$$ = $2;}
 ;
actionsections
 : actiondef actionsections	{$$ = new_section(0, $1, $2);}
 | actiontextsections
 ;
actiontextsections
 : TEXT actiondef
   actionsections		{$$ = new_section($1, $2, $3);}
 | TEXT actiontextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
actiondef
 : PROC IDENT BODY		{new($$); $$->name = $2; $$->body = $3;
				$$->lineno = $1.i; $$->tp = Proc;}
 | PROC myerror BODY		{yerr("error in action name"); $$ = NULL;}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;


translations
 : TRANSLATIONS transsections	{$$ = $2;}
 ;
transsections
 : transline transsections	{$$ = new_section(0, $1, $2);}
 | transtextsections
 ;
transtextsections
 : TEXT transline transsections	{$$ = new_section($1, $2, $3);}
 | TEXT transtextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
transline
 : TRANS modifiers
   event more_events count
   COLON actioncalls opt_semi	{new($$); $$->tp = Trans; $$->type =
				catstr(4, get($2), get($3), get($4), get($5));
				$$->value = $7; $$->lineno = $1.i;
				delete($2); delete($3);
				delete($4); delete($5);}
 | TRANS myerror COLON
   actioncalls opt_semi		{yerr("error before ':' in translation"); $$ =
				NULL;}
 ;
mod_syms
 : COLON			{$$ = hash(":");}
 | EXCLAM			{$$ = hash("!");}
 | TILDE			{$$ = hash("~");}
 | /* empty */			{$$ = 0;}
 ;
modifiers
 : modifier modifiers		{$$ = catstr(2, get($1), get($2)); delete($1);
				delete($2);}
 | /* empty */			{$$ = 0;}
 ;
modifier
 : mod_syms IDENT		{$$ = catstr(2, get($1), get($2)); delete($1);
				delete($2);}
 ;
event
 : BRACKETED detail		{$$ = catstr(4, "<", get($1), ">", get($2));
				delete($1); delete($2);}
 | CSTRING			{$$ = $1;}
 ;
more_events
 : COMMA event more_events	{$$ = catstr(3, ",", get($2), get($3));
				delete($2); delete($3);}
 | /* empty */			{$$ = 0;}
 ;
count
 : LPAR NUMBER RPAR		{$$ = catstr(3, "(", get($2), ")");
				delete($2);}
 | LPAR NUMBER PLUS RPAR	{$$ = catstr(3, "(", get($2), "+)");
				delete($2);}
 | LPAR myerror RPAR		{yerr("count expected after '('");
				$$ = 0;}
 | /* empty */			{$$ = 0;}
 ;
detail
 : IDENT			{$$ = $1;}
 | /* empty */			{$$ = 0;}
 ;
actioncalls
 : actioncall actioncalls	{$$ = catstr(2, get($1), get($2)); delete($1);
				delete($2);}
 | actioncall			{$$ = $1;}
 ;
actioncall
 : IDENT LPAR arguments RPAR	{$$ = catstr(4, get($1), "(", get($3), ") ");
				delete($1); delete($3);}
 | IDENT LPAR myerror RPAR	{yerr("error in argument");
				$$ = NULL;}
 ;
arguments
 : CSTRING more_arguments	{char *s; s = no_quotes(get($1)); delete($1);
				$$ = catstr(2, s, get($2)); free(s);
				delete($2);}
 | /* empty */			{$$ = NULL;}
 ;
more_arguments
 : COMMA CSTRING more_arguments	{char *s; s = no_quotes(get($2)); delete($2);
				$$ = catstr(3, ",", s, get($3)); free(s);
				delete($3);}
				/* The arguments shouldn't be quoted... */
 | /* empty */			{$$ = NULL;}
 ;

methods
 : METHODS methodsections	{$$ = $2;}
 ;
methodsections
 : methoddef methodsections	{$$ = new_section(0, $1, $2);}
 | methodtextsections
 ;
methodtextsections
 : TEXT methoddef
   methodsections		{$$ = new_section($1, $2, $3);}
 | TEXT methodtextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
methoddef
 : PROC type_and_name
   params BODY			{$$ = $2; $$->params = $3; $$->body = $4;
				$$->lineno = $1.i; $$->tp = Proc;}
 | PROC myerror			{$$ = NULL; yerr("error in method");}
 | PROC type_and_name
   params ILL_BODY		{$$ = NULL; yerr("missing closing brace");}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;
type_and_name
 : type_and_name stars
   class_id IDENT 		{STRING h; $$ = $1;
				if ($$->type) {
				  h = catstr(4, get($$->type), " ",
				  get($$->name), get($2)); delete($$->type);
				  $$->type = h; delete($$->name);
				} else {
				  $$->type = catstr(2, get($$->name), get($2));
				  delete($$->name);
				}
				$$->class_id = $3; $$->name = $4;}
 | type_and_name LBRACK RBRACK	{STRING h; $$ = $1; h = catstr(2,
				get($$->suffix), "[]"); delete($$->suffix);
				$$->suffix = h;}
 | type_and_name
   LBRACK IDENT RBRACK		{STRING h; $$ = $1; h = catstr(4,
				get($$->suffix), "[", get($3), "]");
				delete($3); delete($$->suffix);
				$$->suffix = h;}
 | type_and_name
   LBRACK NUMBER RBRACK		{STRING h; $$ = $1; h = catstr(4,
				get($$->suffix), "[", get($3), "]");
				delete($3); delete($$->suffix);
				$$->suffix = h;}
 | class_id IDENT		{new($$); $$->name = $2; $$->class_id = $1;
				$$->suffix = 0;}
 | myerror IDENT		{yerr("error in type expression"); new($$);}
 ;
stars
 : stars STAR			{$$ = catstr(2, get($1), "*");}
 | /* empty */			{$$ = hash(" ");}
 ;
class_id
 : DOLLAR LPAR IDENT RPAR	{$$ = $3;}
 | DOLLAR			{$$ = hash("$");}
 | /* empty */			{$$ = 0;}
 ;

exports
 : EXPORTS exportsections	{$$ = $2;}
 ;
exportsections
 : exportdef exportsections	{$$ = new_section(0, $1, $2);}
 | exporttextsections
 ;
exporttextsections
 : TEXT exportdef
   exportsections		{$$ = new_section($1, $2, $3);}
 | TEXT exporttextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
exportdef
 : INCL BRACKETED		{new($$); $$->lineno = $1.i; $$->name =
				catstr(3, "<", get($2), ">"); $$->tp = Incl;
				delete($2);}
 | INCL CSTRING			{new($$); $$->lineno = $1.i; $$->name = $2;
				$$->tp = Incl;}
 | TYPE IDENT EQUALS C_stuff	{new($$); $$->lineno = $1.i; $$->type = $4;
				$$->name = $2; $$->tp = Type;}
 | TYPE myerror EQUALS C_stuff	{$4 = NULL;
				yerr("should be one identifier before '='");}
 | VAR type_and_name opt_semi	{$$ = $2; $$->lineno = $1.i; $$->tp = Var;}
 | VAR type_and_name EQUALS
   C_stuff			{$$ = $2; $$->lineno = $1.i; $$->tp = Var;
				$$->value = $4;}
 | PROC type_and_name
   params BODY			{$$ = $2; $$->params = $3; $$->body = $4;
				$$->lineno = $1.i; $$->tp = Proc;}
 | PROC myerror BODY		{$$ = NULL; yerr("error in function heading");}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;
macro_stuff
 : LBRACK macro_stuff		{$$ = catstr(2, "[", get($2)); delete($2);}
 | LPAR macro_stuff		{$$ = catstr(2, "(", get($2)); delete($2);}
 | C_stuff			{$$ = $1;}
 | /* empty */			{$$ = 0;}
 ;
C_stuff
 : C_stuff C_thing		{$$ = catstr(2, get($1), get($2)); delete($1);
				delete($2);}
 | C_thing			{$$ = $1;}
 ;
C_thing
 : C_thing LPAR			{$$ = catstr(2, get($1), "("); delete($1);}
 | IDENT LBRACK			{$$ = catstr(2, get($1), "["); delete($1);}
 | IDENT			{$$ = catstr(2, get($1), " "); delete($1);}
 | STAR				{$$ = hash("*");}
 | TILDE			{$$ = hash("~");}
 | EXCLAM			{$$ = hash("!");}
 | COLON			{$$ = hash(":");}
 | SEMI				{$$ = hash(";");}
 | RBRACK			{$$ = hash("]");}
 | BODY				{$$ = $1;}
 | RPAR				{$$ = hash(")");}
 | RPAR	LBRACK			{$$ = hash(")[");}
 | COMMA			{$$ = hash(",");}
 | NUMBER			{$$ = catstr(2, get($1), " "); delete($1);}
 | CSTRING			{$$ = $1;}
 | EQUALS			{$$ = hash("=");}
 | PLUS				{$$ = hash("+");}
 | SLASH			{$$ = hash("/");}
 | CTOK				{$$ = $1;}
 | COMMENT			{$$ = $1;}
 | DOLLAR			{$$ = hash("$");}
 ;
params
 : LPAR paramlist RPAR		{$$ = $2;}
 | LPAR myerror RPAR		{yerr("error in parameter list");$$ = NULL;}
 | /* empty */			{$$ = NULL;}
 ;
paramlist
 : type_and_name more_params	{$$ = $1; $$->next = $2;}
 | DOLLAR more_params		{new($$); $$->name = hash("$"); $$->next = $2;}
 | /* empty */			{$$ = NULL;}
 ;
more_params
 : COMMA type_and_name
   more_params			{$$ = $2; $$->next = $3;}
 | COMMA DOLLAR more_params	{new($$); $$->name = hash("$"); $$->next = $3;}
 | COMMA DOTDOTDOT		{new($$); $$->name = hash("...");}
 | /* empty */			{$$ = NULL;}
 ;


imports
 : IMPORTS importsections	{$$ = $2;}
 ;
importsections
 : importdecl importsections	{$$ = new_section(0, $1, $2);}
 | importtextsections
 ;
importtextsections
 : TEXT importdecl
   importsections		{$$ = new_section($1, $2, $3);}
 | TEXT importtextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
importdecl
 : INCL BRACKETED		{new($$); $$->lineno = $1.i; $$->name =
				catstr(3, "<", get($2), ">"); $$->tp = Incl;
				delete($2);}
 | INCL CSTRING			{new($$); $$->lineno = $1.i; $$->name = $2;
				$$->tp = Incl;}
 | VAR type_and_name opt_semi	{$$ = $2; $$->lineno = $1.i; $$->tp = Var;}
 | PROC type_and_name
   params opt_semi		{$$ = $2; $$->params = $3; $$->lineno = $1.i;
				$$->tp = Proc;}
 ;


utilities
 : UTILITIES utilsections	{$$ = $2;}
 ;
utilsections
 : utildef utilsections		{$$ = new_section(0, $1, $2);}
 | utiltextsections
 ;
utiltextsections
 : TEXT utildef utilsections	{$$ = new_section($1, $2, $3);}
 | TEXT utiltextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
utildef
 : TYPE IDENT EQUALS C_stuff	{new($$); $$->lineno = $1.i; $$->type = $4;
				$$->tp = Type; $$->name = $2;}
 | TYPE myerror EQUALS C_stuff	{$4 = NULL;
				yerr("should be one identifier before '='");}
 | VAR type_and_name opt_semi	{$$ = $2; $$->lineno = $1.i; $$->tp = Var;}
 | VAR type_and_name EQUALS
   C_stuff			{$$ = $2; $$->lineno = $1.i; $$->tp = Var;
				$$->value = $4;}
 | PROC type_and_name
   params BODY			{$$ = $2; $$->params = $3; $$->body = $4;
				$$->lineno = $1.i; $$->tp = Proc;}
 | PROC myerror BODY		{$$ = NULL; yerr("error in function heading");}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 ;


classvars
 : CLASSVARS classvarsections	{$$ = $2;}
 ;
classvarsections
 : classvardecl
   classvarsections		{$$ = new_section(0, $1, $2);}
 | classvartextsections
 ;
classvartextsections
 : TEXT classvardecl
   classvarsections		{$$ = new_section($1, $2, $3);}
 | TEXT classvartextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
classvardecl
 : VAR type_and_name EQUALS
   C_stuff			{$$ = $2; $$->value = $4; $$->lineno = $1.i;
				$$->tp = Var;}
 | VAR myerror EQUALS C_stuff	{$$ = NULL; yerr("error in class variable");}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 | TYPE IDENT EQUALS C_stuff	{new($$); $$->lineno = $1.i; $$->type = $4;
				$$->tp = Type; $$->name = $2;}
 ;


privatevars
 : PRIVATE privatesections	{$$ = $2;}
 ;
privatesections
 : privatedecl privatesections	{$$ = new_section(0, $1, $2);}
 | privatetextsections
 ;
privatetextsections
 : TEXT privatedecl
   privatesections		{$$ = new_section($1, $2, $3);}
 | TEXT privatetextsections	{$$ = new_section($1, (Decl) NULL, $2);}
 | /* empty */			{$$ = NULL;}
 ;
privatedecl
 : VAR type_and_name opt_semi	{$$ = $2; $$->lineno = $1.i; $$->tp = Var;}
 | DEF IDENT params EQUALS
   macro_stuff	    		{new($$); $$->name = $2; $$->tp = Def;
				$$->body = $5; $$->params = $3; $$->lineno =
				$1.i;}
 | DEF myerror macro_stuff	{$$ = NULL;yerr("incorrect macro definition");}
 | TYPE IDENT EQUALS C_stuff	{new($$); $$->lineno = $1.i; $$->type = $4;
				$$->tp = Type; $$->name = $2;}
 ;

myerror
 : error ILL_CHAR		{yerr("illegal character");}
 | error UNKNOWN		{yerr("unknown keyword");}
 | error ILL_BODY		{yerr("missing closing brace");}
 | error			{yerr("syntax error");}
 ;

%%

/*
 * new_section -- allocate a new section and initialize it
 */
static Section new_section(STRING text, Decl decl, Section next)
{
	Section h;

	new(h);
	h->text = text;
	h->decl = decl;
	h->next = next;
	return h;
}


/*
 * yyerror -- write error message to screen
 * for this to work, the parser must have been generated with option -t
 */
#ifdef __STDC__
static void yyerror(const char *s)
#else
static void yyerror(s)
  char *s;
#endif
{
# if defined(YYBISON) && defined(YYDEBUG)
  char temp[20];
  (void) strncpy(temp, yytext, 15);
  if (yyleng > 15) (void) strcpy(temp + 15, "...");
  err(0, "%s at `%s' (token = %s)\n",
    s, temp, yytname[YYTRANSLATE(yychar)]);
# else /* YYBISON && YYDEBUG*/
  err(0, "%s\n", s);
# endif /* YYBISON && YYDEBUG */
}

/*
 * add_section -- add a section to a class, append if already set
 */
static void add_section(psect, s)
  Section *psect;
  Section s;
{
  Section p;

  if (*psect == NULL) {
    *psect = s;
  } else {
    for (p = *psect; p->next; p = p->next) ; /* skip */
    p->next = s;
  }
}

/*
 * no_quotes -- make a copy of a string, but remove the quotes
 */
static char * no_quotes(s)
    char *s;
{
    char *t;

    t = strdup(s + 1);
    t[strlen(t)-1] = '\0';
    return t;
}
