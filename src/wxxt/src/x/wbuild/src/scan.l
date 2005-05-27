/*							-*-indented-text-*-
**  File: scan.l
**  Author: Bert Bos <bert@let.rug.nl>
**  Revised by Joel N. Weber II <nemo@nautilus.sub>
**  Description: (F)LEX definition for Widget Builder
**  $Header: /proj/scheme/cvs/plt/src/wxxt/src/x/wbuild/src/scan.l,v 1.1 2000/04/05 13:51:42 mflatt Exp $
*/

%k 2000

%{
#include <config.h>

#include <wbuild.h>
#include "parse.h"

#include <libit/ctype.h>
#include <libit/string.h>
#include <libit/unistd.h>

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>

#include <libit/malloc.h>

extern void err ();

int lineno;
extern char *filename;
extern time_t filetime;

#define FINFO \
	yylval.finfo.i = lineno; \
	yylval.finfo.s = filename; \
	yylval.finfo.t = filetime

static int countlines(s)
  char *s;
{
  int n = 0;
  for (; *s; s++)
    if (*s == '\n') n++;
  return n;
}

static void set_line(s)
  char *s;
{
  static char buf[1024];
  int n;

  while (! isdigit(*s)) s++;
  n = sscanf(s, "%d \"%s", &lineno, buf);
  buf[strlen(buf)-1] = '\0';			/* Remove " */
  if (n == 2) filename = buf;
}

static int get_body (void);

#undef yywrap

%}

a		[aA]
b		[bB]
c		[cC]
d		[dD]
e		[eE]
f		[fF]
g		[gG]
h		[hH]
i		[iI]
j		[jJ]
k		[kK]
l		[lL]
m		[mM]
n		[nN]
o		[oO]
p		[pP]
q		[qQ]
r		[rR]
s		[sS]
t		[tT]
u		[uU]
v		[vV]
w		[wW]
x		[xX]
y		[yY]
z		[zZ]

class		@{c}{l}{a}{s}{s}
classvars	@{c}{l}{a}{s}{s}{v}{a}{r}{s}
constraints	@{c}{o}{n}{s}{t}{r}{a}{i}{n}{t}{s}
privconstraints	@{p}{r}{i}{v}{a}{t}{e}-{c}{o}{n}{s}{t}{r}{a}{i}{n}{t}{s}
public		@{p}{u}{b}{l}{i}{c}
private		@{p}{r}{i}{v}{a}{t}{e}
methods		@{m}{e}{t}{h}{o}{d}{s}
actions		@{a}{c}{t}{i}{o}{n}{s}
translations	@{t}{r}{a}{n}{s}{l}{a}{t}{i}{o}{n}{s}
imports		@{i}{m}{p}{o}{r}{t}{s}
exports		@{e}{x}{p}{o}{r}{t}{s}
utilities	@{u}{t}{i}{l}{i}{t}{i}{e}{s}
proc		@{p}{r}{o}{c}
var		@{v}{a}{r}
def		@{d}{e}{f}
type		@{t}{y}{p}{e}
incl		@{i}{n}{c}{l}
trans		@{t}{r}{a}{n}{s}
nocode		@{n}{o}{c}{o}{d}{e}	
nodoc		@{n}{o}{d}{o}{c}
guard		@{g}{u}{a}{r}{d}
guardp		@{g}{u}{a}{r}{d}{p}
file		@{f}{i}{l}{e}
doctype		@{d}{o}{c}{t}{y}{p}{e}
tag		@{t}
directory	@{d}{i}{r}{e}{c}{t}{o}{r}{y}
shortdoc	@{s}{h}{o}{r}{t}{d}{o}{c}
extension	@{e}{x}{t}{e}{n}{s}{i}{o}{n}
line		"^#[ \t]*line[ \t]+[0-9]+.*\n"

%%

{class}			{ FINFO; return CLASS; }
{classvars}		{ FINFO; return CLASSVARS; }
{constraints}		{ FINFO; return CONSTRAINTS; }
{privconstraints}	{ FINFO; return PRIVCONSTRAINTS; }
{public}		{ FINFO; return PUBLIC; }
{private}		{ FINFO; return PRIVATE; }
{methods}		{ FINFO; return METHODS; }
{actions}		{ FINFO; return ACTIONS; }
{translations}		{ FINFO; return TRANSLATIONS; }
{imports}		{ FINFO; return IMPORTS; }
{exports}		{ FINFO; return EXPORTS; }
{utilities}		{ FINFO; return UTILITIES; }
{proc}			{ FINFO; return PROC; }
{var}			{ FINFO; return VAR; }
{def}			{ FINFO; return DEF; }
{type}			{ FINFO; return TYPE; }
{incl}			{ FINFO; return INCL; }
{trans}			{ FINFO; return TRANS; }
{tag}			{ FINFO; return TAG; }
{doctype}		{ FINFO; return DOCTYPE; }
{directory}		{ return DIRECTORY; }
{extension}		{ return EXTENSION; }
{shortdoc}		{ return SHORTDOC; }
{nocode}		{ return NOCODE; }
{nodoc}			{ return NODOC; }
{file}			{ return FILE_OPT; }
@[A-Za-z0-9_-]+		{ return UNKNOWN; }
@[ \t\n]([^@]|@@)*	{ lineno += countlines((char*)yytext); yylval.string
			  = hash((char*)yytext+2); return TEXT; }
^#line[ \t]+[0-9]+.*\n	{ set_line(yytext); }
^#.*$			{ /* ignore */ }
[ \t]*			{ /* ignore */ }
\n			{ lineno++; }
[A-Za-z_][A-Za-z0-9_.]*	{ yylval.string = hash((char*)yytext); return IDENT; }
"$"			{ return DOLLAR; }
[0-9]+			|
[0-9]*\.[0-9]+		|
0x[0-9A-F]+		{ yylval.string = hash((char*)yytext); return NUMBER; }
"/*"([^*]|"*"[^/])*"*/"	{ yylval.string = hash((char*)yytext); lineno +=
			  countlines((char*)yytext); return COMMENT; }
"<"[^ \t\n<>()]+">"	{ char c; c = yytext[yyleng-1]; yytext[yyleng-1]
			  = '\0'; yylval.string = hash((char*)yytext + 1);
			  yytext[yyleng-1] = c; return BRACKETED; }
"("			{ return LPAR; }
")"			{ return RPAR; }
"["			{ return LBRACK; }
"]"			{ return RBRACK; }
";"			{ return SEMI; }
"+"			{ return PLUS; }
"="			{ return EQUALS; }
"*"			{ return STAR; }
"!"			{ return EXCLAM; }
"~"			{ return TILDE; }
","			{ return COMMA; }
":"			{ return COLON; }
"..."			{ return DOTDOTDOT; }
"/"			{ return SLASH; }
"&"			|
"?"			|
">"			|
"<"			|
"-"			|
"|"			|
"%"			|
"^"			|
"."			|
\'[^']{1,4}\'		{ yylval.string = hash((char*)yytext); return CTOK; }
\"([^"]|\\\")*\"	{ lineno += countlines((char*)yytext); yylval.string =
                          hash((char*)yytext); return CSTRING; }
"{"			{ int save_lineno = lineno;
			  if (get_body()) return BODY;
			  else {lineno = save_lineno; return ILL_BODY;} }

.			{ return ILL_CHAR; }



%%

  /* this routine is called when a left brace has been seen */
static int get_body(void)
{
#define BUFSTEP 500
    int braces = 1, i = 0, len;
    int squote = 0, quote = 0, comment = 0;
    char c, prev = '{';
    char *buf;
  
    len = BUFSTEP;
    buf = malloc(len * sizeof(char));
    buf[0] = '{';
    i = 1;
    while (braces != 0 && (c = input()) != EOF) {
	if (i >= len) {
	    len += BUFSTEP;
	    buf = realloc(buf, len * sizeof(char));
	    if (! buf) { fprintf(stderr, "Out of memory\n"); exit(1); }
	}
	buf[i] = c;
	i++;
	switch (c) {
	case '{':
	    if (!squote && !quote && !comment) braces++;
	    break;
	case '}':
	    if (!squote && !quote && !comment) braces--;
	    break;
	case '"':
	    if (!squote && !comment && prev != '\\') quote = !quote;
	    break;
	case '\'':
	    if (!quote && !comment && prev != '\\') squote = !squote;
	    break;
	case '*':
	    if (!squote && !quote && !comment && prev == '/') comment = 1;
	    break;
	case '/':
	    if (comment && prev == '*') comment = 0;
	    break;
	case '\n':
	    if ((squote || quote) && prev != '\\') {
		err(0, "missing quote (\" or ')\n");
		squote = quote = 0;
	    }
	    lineno++;
	    break;
	}
	prev = prev == '\\' ? '\0' : c;
    }
    if (braces != 0) return 0;
    if (i >= len) {
	len += BUFSTEP;
	buf = realloc(buf, len * sizeof(char));
    }
    buf[i] = '\0';
    yylval.string = hash(buf);
    free(buf);
    return 1;
}
