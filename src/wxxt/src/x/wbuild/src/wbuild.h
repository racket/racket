/*   wbuild
     Copyright (C) 1996  Joel N. Weber II <nemo@koa.iolani.honolulu.hi.us>
     
     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License
     as published by the Free Software Foundation; either version 2
     of the License, or (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* time_t is needed below, so include the appropriate headers here */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

/* Types and constants. A number of types are defined here that are
 * also used by the parser to store the `parse tree' in. |Class| is a
 * structure that holds all the available information about a single
 * class. Classes are linked into a list. A |Section| is a structure that
 * holds information about a `section' of a class, i.e., tex plus macros
 * plus a piece of C code.  A |Decl| holds information about a single
 * variable declaration, typedef, function, etc.
 *
 * A |STRING| is an opaque type, implemented in `util' as a pointer in a
 * string buffer (see the module `util').
 */

typedef struct _STRING {
	char *s;
	int linkcount;
	struct _STRING *next;
} *STRING;


/* The |Decl| type contains a variable declaration, a typedef, a
 * procedure, etc. The fields are used in different ways for different
 * types of declarations. |tp| is always used, and so is |lineno|.
 *
 * In the section for the public instance variables, |tp| is always
 * |Var|, |type| holds the type of the variable, |name| holds the name
 * and |value| holds the default value. These fields should not be empty.
 * The |suffix| optionally holds a suffix like |[4]|.  The |typesym|
 * field --- if not empty --- holds an identifier that will be used for
 * the symbolic name of the type. The generator will capitalize it and
 * prefix |XtR|. Similarly, |namesym| --- if not empty --- is used for
 * the symbolic name of the varaible, when used as a resource. |XtN| and
 * |XtC| will be prefixed to it. |valuesym| must be used if the initial
 * value (in |value|) needs to be converted. It contains the symbolic
 * name of the type of |value|, if that type is different from |type|.
 * The fields |params| and |body| are not used.
 *
 * E.g., the input
 * %
 * $$|@@var Pixel shadow_color = XtDefaultBackground|$$
 * %
 * will result in a resource wih symbolic names |XtNshadow_color| (|=
 * "shadow_color"|) and |XtCShadow_color| (|= "Shadow_color"|) and type
 * |XtRPixel| (|= "Pixel"|), but
 * %
 * $$|@@var <shadowColor> Pixel shadow_color <shadowColor>
 * = <String> "black"|$$
 * %
 * creates a resource with symbolic names |XtNshadowColor| and
 * |XtCShadowColor|. The default value will be converted from |XtRString|
 * to |XtRShadowColor|.
 *
 * The field |force_new| is set to |TRUE| if the name of the variable is
 * prefixed wit a `\$'. This means that the name should be declared in
 * this class, regardless of the existence of variables of the same name
 * in superclasses. If the \$ is omitted, the variable is only declared
 * if the name is not already in use.
 *
 * For class variables, the fields |type|, |name| |force_new| and |value|
 * should be non-empty and |tp| should be |Var|. |suffix| may hold an
 * array part. The other fields are not used.
 *
 * Methods use |type| (the function's return type), |name|, |force_new|,
 * |body| and |params|, but |params| may be left empty. If |type| is
 * empty, the generator will provide a return type of |void|. |tp| should
 * be |Proc|.
 *
 * Private variables have |tp = Var| and use |type|, |force_new| and
 * |name|. |suffix| is again optional.
 *
 * Exports may be variables (|tp = Var|), procedures (|tp = Proc|), type
 * definitions (|tp = Type|) and macro definitions (|tp = Def|).
 * Variables use |type|, |name| and optionally |suffix|; procedures use
 * |type| (may be empty), |name|, |params| (may be empty), and |body|;
 * type definitions use |name| and |type|; macros use |name|, |body| and
 * optionally |params|.
 *
 * Imports may be include files, variables or procedures. Include files
 * have |tp = Incl| and use only the |name| field.
 *
 * Utilities can be procedures, types, macros, or variables.
 *
 * The |next| field is only used when the |Decl| is used as a parameter
 * list in the field |params| of another |Decl|.
 */

typedef enum {Undefined, Proc, Var, Type, Def, Incl, Trans} DeclTp;
typedef struct _Decl {
	DeclTp tp;
	int lineno;
	STRING class_id;
	STRING typesym, type;
	STRING namesym, name;
	STRING suffix;
	STRING valuesym, value;
	struct _Decl *params;
	STRING body;
	struct _Decl *next;
} *Decl;

/* A |Section| contains a ``section'' of the spec file, i.e., from one
 * `@@' to the next, including a piece of text, some macros and a piece
 * of C code.
 */

typedef struct _Section {
	STRING text;
	Decl decl;
	struct _Section *next;
} *Section;

/* A |Class| contains the complete information about a single class.
 * The |superclass| may be empty, but only for the X Toolkit's intrinsic
 * classes. The |options| contain directions for the generator, if the
 * class has special conventions, e.g., it doesn't produce any C files.
 */

typedef struct _Class {
	int lineno; /* starting line of the class */
	STRING name, superclass, filename, filenamepart;
	STRING text; /* purpose of the class */
	Section classvars, publicvars, privatevars, methods, actions,
	translations, imports, exports, utilities,
	constraints, privconstr;
	int nodoc, nocode;
	struct _Class *daughters, *sister, *super; /* hierarchy */
	struct _Class *next; /* in spec file */
	time_t filetime;
} *Class;


typedef enum {
	t_start, t_class, t_name, t_name2, t_filename, t_superclass,
	t_publicvars,
	t_privatevars, t_actions, t_translations, t_exports, t_methods,
	t_imports, t_utilities, t_classvars, t_section, t_macro,
	t_publicvar, t_action, t_code,
	t_table, t_tablehead, t_row, t_resname, t_resclass, t_restype,
	t_resdefault, t_inline, t_underline, t_backslash, t_tilde,
	t_hashmark, t_dollar, t_less, t_greater,
	t_percent, t_caret, t_ampersand, t_lbrace, t_rbrace, t_bar, t_at,
	t_type, t_incl, t_constraints, t_constraint, t_privconstraints,
	t_privconstraint
} tagtype;

typedef char *tagpair[2];
typedef char *taglist[t_privconstraint + 1][2];

typedef struct _Doctype {
	int shortdoc;
	taglist tag;
	struct _Doctype *next;
} *Doctype;

/* Function prototypes
 */

STRING hash(char *s);
char *get(STRING s);
STRING get_headerPname(char *dir, Class c);
STRING get_classname(STRING s);
STRING get_word(char *s);
STRING get_instname(STRING s);
Class find_instvar_class(Class c, STRING name);
Section find_pubvar(Class c, STRING m);
Section find_pubvar_back(Class c, STRING m);
Class find_constr_class(Class c, STRING name);
Section find_constraint(Class c, STRING m);
Section find_method(Class c, STRING m);
Class find_superclass(Class c, STRING name);
Class find_method_class(Class c, STRING name);
Class find_classvar_class(Class c, STRING name);
int strneq(char *a, char *b, int n);
size_t Strlen(STRING s);
void delete(STRING s);
int has_method(Class c, STRING m);
STRING find_classvar_value(Class c, STRING name);
STRING hdup(STRING s);
void declare_function(int export, STRING name, Decl d, FILE *f);
void wbuild_comment(FILE *f);
void print_body(FILE *f, STRING body, Class class, STRING procname,
	int lineno, int ismacro);
int set_hierarchy(void);
void public_header(FILE *f, Class c, char *prefix, char *guard);
void generate_private_header(FILE *f, Class c, char *prefix, char *guard);
void symbol_init(void);
void generatec(FILE *f, Class c, char *prefix, int lines);
void generate_doc(FILE *f, tagpair *tag, Class c, int shortdoc);
void add_class(Class c);
void add_doctype(Doctype d);
void copy_taglist(taglist *dest, taglist *src);
void zero_taglist(taglist *dest);
void set_doctag(taglist tag, char *name, char *open, char *close);


#if HAVE_STDARG_H
#ifdef __STDC__
STRING catstr(int n,...);
#else /* not __STDC__ */
STRING catstr(n);
#endif /* not __STDC__ */
#else /* not HAVE_STDARG_H */
STRING catstr(va_alist);
#endif /* not HAVE_STDARG_H */



#define lined(lineno, file)                                             \
	if (lines) fprintf(f, "#line %d \"%s\"\n", lineno, get(file))

#define warn_if_params(params, filename, lineno, procname)		\
	if (params != NULL)						\
		nerrors++,						\
		fprintf(stderr,						\
			_("%s:%d: Parameter list of `%s' ignored\n"),	\
			get(filename), lineno, get(procname))


extern int nerrors;
extern Doctype doctypes;
extern Class classes;
extern int classnodoc, classnocode, doctypeshort;
extern STRING classfilename, doctypeext, doctypedir;
