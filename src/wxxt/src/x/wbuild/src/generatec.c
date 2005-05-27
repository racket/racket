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

/* This module contains the code that generates C and
 * documentation files from the specification of the widgets in a |Class|
 * datastructure. The only exported function is |generate|, which uses the
 * list of class specifications.
 */

#define fputS(s, f) fputs(get(s), f)

#include <config.h>
#include <stdio.h>

#include <libit/unistd.h>

#include <libit/string.h>

#include <libit/ctype.h>
#include <wbuild.h>
#include <wsym.h>

#include <libintl.h>
#define _(String) gettext(String)

#include <libit/malloc.h>

/* Implementation file. The implementation file contains the
 * definitions of all methods, actions and utility functions. It
 * initializes the class record and the translations.
 */

/* inlining philosophy: nautilus has fast processor, slow disks.  Don't
 * significantly increase size of executable.
 *
 * Actually, I'm probably wasting too much effort on this...
 */

/* |define_function| creates the function's header (taking care of
 * substituting `self' for `\$') and the function's body (again
 * substituting `self' where appropriate).
 *
 * The function header is defined twice, separated with |#if
 * NeedFunctionPrototypes|, echoing the forward declaration that was
 * created earlier.
 */

static void define_header(int export, Decl d, FILE *f, int lines, int lineno,
	STRING sourcefile)
{
	Decl d1;

	fputs("/*ARGSUSED*/\n", f);
	fprintf(f, "#if NeedFunctionPrototypes\n");
	lined(lineno, sourcefile);
	if (!export)
		fputs("static ", f);
	if (d->type)
		fputS(d->type, f);
	else
		fputs("void", f);
	fprintf(f, " %s(", get(d->name));
	if (d->params == NULL) {
		fputs("void", f);
	} else {
		for (d1 = d->params; d1; d1 = d1->next) {
			if (d1->name == DOLLAR)
				fprintf(f, "Widget self");
			else if (d1->name == DOTDOTDOT)
				fprintf(f, "...");
			else
				fprintf(f, "%s %s", get(d1->type),
					get(d1->name));
			if (d1->suffix)
				/* An array? */
				fputS(d1->suffix, f);
			if (d1->next)
				putc(',', f);
		}
	}
	fprintf(f, ")\n#else\n");
	lined(lineno, sourcefile);
	if (! export)
		fputs("static ", f);
	if (d->type)
		fputS(d->type, f);
	else
		fputs("void", f);
	fprintf(f, " %s(", get(d->name));
	for (d1 = d->params; d1; d1 = d1->next) {
		if (d1->name == DOLLAR)
			fputs("self", f);
		else if (d1->name == DOTDOTDOT)
			fputs("va_alist", f);
		else
			fputS(d1->name, f);
		if (d1->next)
			putc(',', f);
	}
	putc(')', f);
	for (d1 = d->params; d1; d1 = d1->next) {
		if (d1->name == DOLLAR)
			fprintf(f, "Widget self");
		else if (d1->name == DOTDOTDOT)
			fprintf(f, "va_dcl");
		else
			fprintf(f, "%s %s", get(d1->type), get(d1->name));
		if (d1->suffix)
			/* An array? */
			fputS(d1->suffix, f);
		if (d1->name != DOTDOTDOT)
			putc(';', f);
	}
	fprintf(f, "\n#endif\n");
}


/* The number of declarations in a list of sections is counted,
 * omitting the macro definitions, since they are not declarations in the
 * C sense.
 */

static int number_of_decls(Section sect)
{
	Section s;
	int n;

	n = 0;
	for (s = sect; s; s = s->next) {
		if (s->decl && (s->decl->tp != Def)) n++;
	}
	return n;
}

static void define_function(int export, int lines, Decl d, STRING body, Class c,
	FILE *f, int lineno)
{
	define_header(export, d, f, lines, lineno, c->filename);
	lined(lineno + 1, c->filename);
	print_body(f, body, c, d->name, lineno, 0);
}

static void define_macro(FILE *f, Decl d, Class c)
{
	Decl d1;

	fputs("#define ", f);
	fputS(d->name, f);
	if (d->params) {
		putc('(', f);
		for (d1 = d->params; d1; d1 = d1->next) {
			fputS(d1->name, f);
			if (d1->next)
				fputs(", ", f);
		}
		putc(')', f);
	}
	putc(' ', f);
	print_body(f, d->body, c, d->name, d->lineno, 1);
	fputs("\n\n", f);
}

#if 0
/* |declare_function| creates a prototype for a function |d|. If
 * |export| is |False|, the prototype will be preceded by
 * |"static"|.\mkref{declare-function}
 */

static void declare_function(int export, STRING name, Decl d, FILE *f)
{
	Decl d1;

	if (!export)
		fputs("static ", f);

	if (d->type)
		fputS(d->type, f);
	else
		fputs("void", f);

	putc(' ', f);
	fputS(name, f);
	fputs("(\n#if NeedFunctionPrototypes\n", f);
	if (!d->params) {
		fputs("void", f);
	} else {
		for (d1 = d->params; d1; d1 = d1->next) {
			if (d1->name == DOLLAR)
				fputs("Widget", f);
			else
				fputS(d1->type, f);
			if (d1->suffix)
				fprintf(f, " %s%s", get(d1->name),
					get(d1->suffix));
			if (d1->next) (void) putc(',', f);
		}
	}
	fputs("\n#endif\n);\n", f);
}
#endif

/* The are two type of included files: system header files and local
 * header files. System header files are files whose names are bracketed
 * by $<$ and $>$.  Local header files have quotes.
 */

inline static void generate_import_file(FILE *f, Section s)
{
	fprintf(f, "#include %s\n", get(s->decl->name));
}

inline static void generate_import_var(FILE *f, Section s)
{
	fprintf(f, "extern %s %s", get(s->decl->type), get(s->decl->name));
	if (s->decl->suffix)
		fputS(s->decl->suffix, f);
	fputs(";\n", f);
}

inline static void generate_import_func(FILE *f, Section s)
{
	declare_function(1, s->decl->name, s->decl, f);
}

/* The implementation file starts with a set of included files. The
 * intrinsics and string definitions are always included. Next is the
 * private header file of the widget itself. After that there may be
 * additional inclusions.
 */

inline static void generate_imports(FILE *f, Class c, char *prefix, int lines)
{
	Section s;

	fprintf(f, "#include <X11/IntrinsicP.h>\n");
	fprintf(f, "#include <X11/StringDefs.h>\n");
	for (s = c->imports; s; s = s->next) {
		if (s->decl) {
			lined(s->decl->lineno, c->filename);
			switch (s->decl->tp) {
				case Proc: generate_import_func(f, s); break;
				case Var: generate_import_var(f, s); break;
				case Incl: generate_import_file(f, s); break;
				/* RMS, you don't see the following:
				 * to placate -Wall
				 * (it should be optomized away)
				 */
				case Undefined: break;
				case Type: break;
				case Def: break;
				case Trans: break;
			}
		}
	}
	fprintf(f, "#include <%s>\n", get(get_headerPname(prefix, c)));
}

/* The resource list might also initialize resources from superclasses.
 * The offset has to be calculated carefully. |s1| is the declaration, it is
 * different from |s|, if the variable is inherited.
 *
 * Macros are skipped, since they have already been defined in the
 * private header file.
 */

inline static void res_name_class(FILE *f, Section s1)
{
	fprintf(f, "{XtN%s,XtC%s,",
		s1->decl->namesym
			? get(s1->decl->namesym) : get(s1->decl->name),
		s1->decl->namesym
			? get(get_classname(s1->decl->namesym))
			: get(get_classname(s1->decl->name)));
}


inline static void res_type(FILE *f, Section s1)
{
	fprintf(f, "XtR%s,", s1->decl->typesym ? get(s1->decl->typesym)
		: get(get_classname(get_word(get(s1->decl->type)))));
}

inline static void res_size_offset(FILE *f, Class c, Section s1)
{
	fprintf(f, "sizeof(((%sRec*)NULL)->%s.%s),", get(c->name),
		get(get_instname(find_instvar_class(c, s1->decl->name)->name)),
		get(s1->decl->name));
	fprintf(f, "XtOffsetOf(%sRec,%s.%s),", get(c->name),
		get(get_instname(find_instvar_class(c, s1->decl->name)->name)),
		get(s1->decl->name));
}

inline static void res_initial_type(FILE *f, Section s)
{
	if (s->decl->valuesym)
		fprintf(f, "XtR%s,", get(s->decl->valuesym));
	else
		fprintf(f, "XtRImmediate,");
}

inline static void res_initial_value(FILE *f, Section s)
{
	fprintf(f, "(XtPointer)%s},\n", get(s->decl->value));
}

inline static void res_list(FILE *f, Class c, int lines)
{
	Section s, s1;

	if (number_of_decls(c->publicvars) != 0) {
		fprintf(f, "\nstatic XtResource resources[] = {\n");
		for (s = c->publicvars; s; s = s->next) {
			if (s->decl && (s->decl->tp == Var)) {
				s1 = find_pubvar(c, s->decl->name);
				lined(s->decl->lineno, c->filename);
				res_name_class(f, s1);
				res_type(f, s1);
				res_size_offset(f, c, s1);
				res_initial_type(f, s1);
				s1 = find_pubvar_back(c, s->decl->name);
				res_initial_value(f, s1);
			}
		}
		fprintf(f, "};\n");
	}
}

/* The constraint resource list is similar to the resource list.
 */
inline static void res_constraint_size_offset(FILE *f, Class c, Section s1)
{
	fprintf(f, "sizeof(((%sConstraintRec*)NULL)->%s.%s),", get(c->name),
		get(get_instname(find_constr_class(c, s1->decl->name)->name)),
		get(s1->decl->name));
	fprintf(f, "XtOffsetOf(%sConstraintRec,%s.%s),", get(c->name),
		get(get_instname(find_constr_class(c, s1->decl->name)->name)),
		get(s1->decl->name));
}

inline static void generate_constraint_res_list(FILE *f, Class c, int lines)
{
	Section s, s1;
	if (number_of_decls(c->constraints) != 0) {
		fprintf(f, "\nstatic XtResource constraint_resources[] = {\n");
		for (s = c->constraints; s; s = s->next) {
			if (s->decl && (s->decl->tp == Var)) {
				s1 = find_constraint(c, s->decl->name);
				lined(s->decl->lineno, c->filename);
				res_name_class(f, s1);
				res_type(f, s1);
				res_constraint_size_offset(f, c, s1);
				res_initial_type(f, s1);
				res_initial_value(f, s1);
			}
		}
		fprintf(f, "};\n");
	}
}


/* All action functions have the same parameters; they should be
 * omitted in the spec files. The variable |action_proto| holds a
 * declaration of a prototypical action function. The parameters are
 * taken from that variable.
 */

inline static void action_func_decl(FILE *f, Class c, int lines)
{
	Section s;

	for (s = c->actions; s; s = s->next) {
		if (s->decl && (s->decl->tp == Proc)) {
			warn_if_params(s->decl->params, c->filename,
				s->decl->lineno, s->decl->name);
			lined(s->decl->lineno, c->filename);
			declare_function(0, s->decl->name,
				action_proto.decl, f);
		} else if (s->decl && (s->decl->tp == Def)) {
			define_macro(f, s->decl, c);
		}
	}
}

/* The action table will be called |actionsList| and the translation
 * table |defaultTranslations|. Both may be empty.
 */

inline static void action_table_trans(FILE *f, Class c)
{
	Section s;

	if (number_of_decls(c->actions) != 0) {
		fprintf(f, "\nstatic XtActionsRec actionsList[] = {\n");
		for (s = c->actions; s; s = s->next) {
			if (s->decl && s->decl->tp == Proc) {
				fprintf(f, "{\"%s\", %s},\n",
					get(s->decl->name), get(s->decl->name));
			}
		}
		fprintf(f, "};\n");
	}
	if (number_of_decls(c->translations) != 0) {
		fprintf(f, "\nstatic char defaultTranslations[] = \"\\\n");
		for (s = c->translations; s; s = s->next) {
			if (s->decl) {
				fprintf(f, "%s: %s\\n\\\n",
					get(s->decl->type),
					get(s->decl->value));
			}
		}
		fprintf(f, "\";\n");
	}
}

/* The methods all have forward declarations. The code here includes
 * ANSI style prototypes, protected with a macro. The method definitions
 * further on have old style K\&R headers.
 *
 * Wbuild generates a function called |"_resolve_inheritance"| that takes
 * care of replacing `XtInherit' by to appropriate methods in the final
 * widget. This function will be used as the |class_part_initialize|
 * method, unless the specificationfile already has such a method; in
 * that case the function will still be generated, but not installed in
 * the class record.
 */


/* All methods have forward declaration. If the method is inherited,
 * there should be no parameter list, as the inherited parameter list
 * will be used. If |class_id| is |DOLLAR|, it is forced to be a new
 * method, if |class_id| is anything else (except |NIL|), it must be an
 * inherited method, from the given class.
 */

inline static void declare_method(FILE *f, Class c, Section s)
{
	Section s2;

	if (s->decl->class_id == DOLLAR)
		/* Force a new method */
		s2 = s;
	else if (!s->decl->class_id)
		/* Search for it */
		s2 = find_method(c, s->decl->name);
	else
		/* Search from given class */
		s2 = find_method(find_superclass(c, s->decl->class_id),
			s->decl->name);
	if (s2 == NULL) {
		fprintf(stderr,
			_("%s:%d: method \"%s\" or class \"%s\" not found\n"),
			get(c->filename), s->decl->lineno, get(s->decl->name),
			get(s->decl->class_id));
			nerrors++;
	} else {
		if (s2 != s)
			warn_if_params(s->decl->params, c->filename,
				s->decl->lineno, s->decl->name);
		declare_function(0, s->decl->name, s2->decl, f);
	}
}

inline static void method_func_decl(FILE *f, Class c, int lines)
{
	Section s;

	fprintf(f, "static void _resolve_inheritance(\n");
	fprintf(f, "#if NeedFunctionPrototypes\n");
	fprintf(f, "WidgetClass\n");
	fprintf(f, "#endif\n");
	fprintf(f, ");\n");
	for (s = c->methods; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc) {
			lined(s->decl->lineno, c->filename);
			declare_method(f, c, s);
		}
	}
}

/* The work of initializing the class record is done by a recursive
 * procedure. The WidgetClass identifier is defined at the end of the
 * structure.
 */

static void init_class_parts(FILE *f, Class self, Class part);

inline static void class_record_instantation(FILE *f, Class c)
{
	STRING instname;

	instname = get_instname(c->name);
	fprintf(f, "\n%sClassRec %sClassRec = {\n",
		get(c->name), get(instname));
	init_class_parts(f, c, c);
	fprintf(f, "};\n");
	fprintf(f,
		"WidgetClass %sWidgetClass = (WidgetClass) &%sClassRec;\n",
		get(instname), get(instname));
}


static void initialize_core_class_part(FILE *f, Class self, Class part)
{
	char *p, *q;
	int nd;

	fprintf(f, "{ /* core_class part */\n");
	q = get(self->superclass);
	p = strrchr(q, '/');
	if (p)
		fprintf(f,
			"/* superclass   \t*/  (WidgetClass) &%sClassRec,\n",
			get(get_instname(hash(p + 1))));
	else
		fprintf(f,
			"/* superclass   \t*/  (WidgetClass) &%sClassRec,\n",
			get(get_instname(self->superclass)));
	fprintf(f, "/* class_name   \t*/  \"%s\",\n", get(self->name));
	fprintf(f, "/* widget_size  \t*/  sizeof(%sRec),\n", get(self->name));
	fprintf(f, "/* class_initialize \t*/  %s,\n",
		has_method(self, CLASS_INITIALIZE)
		? "class_initialize" : "NULL");
	fprintf(f, "/* class_part_initialize*/  %s,\n",
		has_method(self, CLASS_PART_INITIALIZE)
		? "class_part_initialize" : "_resolve_inheritance");
	fprintf(f, "/* class_inited \t*/  FALSE,\n");
	fprintf(f, "/* initialize   \t*/  %s,\n",
		has_method(self, INITIALIZE) ? "initialize" : "NULL");
	fprintf(f, "/* initialize_hook \t*/  %s,\n",
		has_method(self, INITIALIZE_HOOK) ? "initialize_hook" : "NULL");
	fprintf(f, "/* realize      \t*/  %s,\n",
		has_method(self, REALIZE) ? "realize" : "XtInheritRealize");

	nd = number_of_decls(self->actions);
	if (nd)
		fprintf(f, "/* actions      \t*/  actionsList,\n");
	else
		 fprintf(f, "/* actions      \t*/  NULL,\n");
	fprintf(f, "/* num_actions  \t*/  %d,\n", nd);

	nd = number_of_decls(self->publicvars);
	if (nd)
		fprintf(f, "/* resources    \t*/  resources,\n");
	else
		fprintf(f, "/* resources    \t*/  NULL,\n");
	fprintf(f, "/* num_resources \t*/  %d,\n", nd);

	fprintf(f, "/* xrm_class    \t*/  NULLQUARK,\n");
	fprintf(f, "/* compres_motion \t*/  %s,\n",
		get(find_classvar_value(self, COMPRESS_MOTION)));
	fprintf(f, "/* compress_exposure \t*/  %s,\n",
		get(find_classvar_value(self, COMPRESS_EXPOSURE)));
	fprintf(f, "/* compress_enterleave \t*/  %s,\n",
		get(find_classvar_value(self, COMPRESS_ENTERLEAVE)));
	fprintf(f, "/* visible_interest \t*/  %s,\n",
		get(find_classvar_value(self, VISIBLE_INTEREST)));
	fprintf(f, "/* destroy      \t*/  %s,\n",
		has_method(self, DESTROY) ? "destroy" : "NULL");
	fprintf(f, "/* resize       \t*/  %s,\n",
		has_method(self, RESIZE) ? "resize" : "XtInheritResize");
	fprintf(f, "/* expose       \t*/  %s,\n",
		has_method(self, EXPOSE) ? "expose" : "XtInheritExpose");
	fprintf(f, "/* set_values   \t*/  %s,\n",
		has_method(self, SET_VALUES) ? "set_values" : "NULL");
	fprintf(f, "/* set_values_hook \t*/  %s,\n",
		has_method(self, SET_VALUES_HOOK) ? "set_values_hook" : "NULL");
	fprintf(f, "/* set_values_almost \t*/  %s,\n",
		has_method(self, SET_VALUES_ALMOST) ?
		"set_values_almost" : "XtInheritSetValuesAlmost");
	fprintf(f, "/* get_values+hook \t*/  %s,\n",
		has_method(self, GET_VALUES_HOOK) ? "get_values_hook" : "NULL");
	fprintf(f, "/* accept_focus \t*/  %s,\n",
		has_method(self, ACCEPT_FOCUS)
		? "accept_focus" : "XtInheritAcceptFocus");
	fprintf(f, "/* version      \t*/  XtVersion,\n");
	fprintf(f, "/* callback_private \t*/  NULL,\n");

	if (number_of_decls(self->translations))
		fprintf(f, "/* tm_table      \t*/  defaultTranslations,\n");
	else
		fputs("/* tm_table      \t*/  NULL,\n", f);

	fprintf(f, "/* query_geometry \t*/  %s,\n",
		has_method(self, QUERY_GEOMETRY) ?
		"query_geometry" : "XtInheritQueryGeometry");
	fprintf(f, "/* display_acceleator \t*/  %s,\n",
		has_method(self, DISPLAY_ACCELERATOR) ?
		"display_accelerator" : "XtInheritDisplayAccelerator");
	fprintf(f, "/* extension    \t*/  %s\n",
		get(find_classvar_value(self, EXTENSION)));
	fprintf(f, "},\n");
}

inline static void initialize_object_class_part(FILE *f, Class self, Class part)
{
	int nd;

	fprintf(f, "{ /* object class part */\n");
	fprintf(f, "(WidgetClass) &%sClassRec,\n",
		get(get_instname(self->superclass)));
	fprintf(f, "\"%s\",\n", get(self->name));
	fprintf(f, "sizeof(%sRec),\n", get(self->name));
	fprintf(f, "%s,\n", has_method(self, CLASS_INITIALIZE) ?
		"class_initialize" : "NULL");
	fprintf(f, "%s,\n", has_method(self, CLASS_PART_INITIALIZE) ?
		"class_part_initialize" : "_resolve_inheritance");
	fprintf(f, "FALSE,\n");
	fprintf(f, "%s,\n", has_method(self, INITIALIZE) ?
		"initialize" : "NULL");
	fprintf(f, "%s,\n", has_method(self, INITIALIZE_HOOK) ?
		"initialize_hook" : "NULL");
	fprintf(f, "NULL,\n"); /* obj1 <- `realize' */
	fprintf(f, "NULL,\n"); /* obj2 <- `actions' */
	fprintf(f, "0,\n"); /* obj3 <- `num_actions' */

	nd = number_of_decls(self->publicvars);
	if (nd == 0)
		fprintf(f, "NULL,\n");
	else
		fprintf(f, "resources,\n");
	fprintf(f, "%d,\n", nd);

	fprintf(f, "NULLQUARK,\n");
	fprintf(f, "False,\n"); /* obj4 <- `compress_motion' */
	fprintf(f, "False,\n"); /* obj5 <- `compress_exposure' */
	fprintf(f, "False,\n"); /* obj6 <- `compress_enterleave' */
	fprintf(f, "False,\n"); /* obj7 <- `visible_interest' */
	fprintf(f, "%s,\n", has_method(self, DESTROY) ? "destroy" : "NULL");
	fprintf(f, "NULL,\n"); /* obj8 <- `resize' */
	fprintf(f, "NULL,\n"); /* obj9 <- `expose' */
	fprintf(f, "%s,\n",
		has_method(self, SET_VALUES) ? "set_values" : "NULL");
	fprintf(f, "%s,\n",
		has_method(self, SET_VALUES_HOOK) ? "set_values_hook" : "NULL");
	fprintf(f, "NULL,\n"); /* obj10 <- `set_values_almost' */
	fprintf(f, "%s,\n",
		has_method(self, GET_VALUES_HOOK) ? "get_values_hook" : "NULL");
	fprintf(f, "NULL,\n"); /* obj11 <- `accept_focus' */
	fprintf(f, "XtVersion,\n");
	fprintf(f, "NULL,\n");
	fprintf(f, "NULL,\n"); /* obj12 <- `tm_table' */
	fprintf(f, "NULL,\n"); /* obj13 <- `query_geometry' */
	fprintf(f, "NULL,\n"); /* obj14 <- `display_accelerator' */
	fprintf(f, "%s\n", get(find_classvar_value(self, EXTENSION)));
	fprintf(f, "},\n");
}

inline static void initialize_rectobj_class_part(FILE *f, Class self,
	Class part)
{
	int nd;

	fprintf(f, "{ /* rectobj class part */\n");
	fprintf(f, "(WidgetClass) &%sClassRec,\n",
		get(get_instname(self->superclass)));
	fprintf(f, "\"%s\",\n", get(self->name));
	fprintf(f, "sizeof(%sRec),\n", get(self->name));
	fprintf(f, "%s,\n", has_method(self, CLASS_INITIALIZE) ?
		"class_initialize" : "NULL");
	fprintf(f, "%s,\n", has_method(self, CLASS_PART_INITIALIZE) ?
		"class_part_initialize" : "_resolve_inheritance");
	fprintf(f, "FALSE,\n");
	fprintf(f, "%s,\n", has_method(self, INITIALIZE) ?
		"initialize" : "NULL");
	fprintf(f, "%s,\n", has_method(self, INITIALIZE_HOOK) ?
		"initialize_hook" : "NULL");
	fprintf(f, "NULL,\n"); /* rect1 <- `realize' */
	fprintf(f, "NULL,\n"); /* rect2 <- `actions' */
	fprintf(f, "0,\n"); /* rect3 <- `num_actions' */

	nd = number_of_decls(self->publicvars);
	if (nd)
		fprintf(f, "resources,\n");
	else
		fprintf(f, "NULL,\n");
	fprintf(f, "%d,\n", nd);

	fprintf(f, "NULLQUARK,\n");
	fprintf(f, "False,\n"); /* rect4 <- `compress_motion' */
	fprintf(f, "False,\n"); /* rect5 <- `compress_exposure' */
	fprintf(f, "False,\n"); /* rect6 <- `compress_enterleave' */
	fprintf(f, "False,\n"); /* rect7 <- `visible_interest' */
	fprintf(f, "%s,\n",
	has_method(self, DESTROY) ? "destroy" : "NULL");
	fprintf(f, "%s,\n",
	has_method(self, RESIZE) ? "resize" : "XtInheritResize");
	fprintf(f, "%s,\n",
		has_method(self, EXPOSE) ? "expose" : "XtInheritExpose");
	fprintf(f, "%s,\n",
		has_method(self, SET_VALUES) ? "set_values" : "NULL");
	fprintf(f, "%s,\n",
		has_method(self, SET_VALUES_HOOK) ? "set_values_hook" : "NULL");
	fprintf(f, "%s,\n", has_method(self, SET_VALUES_ALMOST) ?
		"set_values_almost" : "XtInheritSetValuesAlmost");
	fprintf(f, "%s,\n",
		has_method(self, GET_VALUES_HOOK) ? "get_values_hook" : "NULL");
	fprintf(f, "NULL,\n"); /* rect9 <- `accept_focus' */
	fprintf(f, "XtVersion,\n");
	fprintf(f, "NULL,\n");
	fprintf(f, "NULL,\n"); /* rect10 <- `tm_table' */
	fprintf(f, "%s,\n", has_method(self, QUERY_GEOMETRY) ?
		"query_geometry" : "XtInheritQueryGeometry");
	fprintf(f, "NULL,\n"); /* rect11 <- `display_accelerator' */
	fprintf(f, "%s\n", get(find_classvar_value(self, EXTENSION)));
	fprintf(f, "},\n");
}

static void initialize_composite_class_part(FILE *f, Class self, Class part)
{
	initialize_core_class_part(f, self, part);

	fprintf(f, "{ /* composite_class part */\n");
	fprintf(f, "%s,\n", has_method(self, GEOMETRY_MANAGER) ?
		"geometry_manager" : "XtInheritGeometryManager");
	fprintf(f, "%s,\n", has_method(self, CHANGE_MANAGED) ?
		"change_managed" : "XtInheritChangeManaged");
	fprintf(f, "%s,\n", has_method(self, INSERT_CHILD) ?
		"insert_child" : "XtInheritInsertChild");
	fprintf(f, "%s,\n", has_method(self, DELETE_CHILD) ?
		"delete_child" : "XtInheritDeleteChild");
	fprintf(f, "NULL\n");
	fprintf(f, "},\n");
}

static void initialize_constraint_class_part(FILE *f, Class self, Class part)
{
	int nd;

	initialize_composite_class_part(f, self, part);

	fprintf(f, "{ /* constraint_class part */\n");

	nd = number_of_decls(self->constraints);
	fprintf(f, "/* constraint_resources     */  %s,\n",
		nd == 0 ? "NULL" : "constraint_resources");
	fprintf(f, "/* num_constraint_resources */  %d,\n", nd);

	fprintf(f,"/* constraint_size          */  sizeof(%sConstraintRec),\n",
		get(self->name));
	fprintf(f, "/* constraint_initialize    */  %s,\n",
		has_method(self, CONSTRAINT_INITIALIZE) ?
		"constraint_initialize" : "NULL");
	fprintf(f, "/* constraint_destroy       */  %s,\n",
		has_method(self, CONSTRAINT_DESTROY) ?
		"constraint_destroy" : "NULL");
	fprintf(f, "/* constraint_set_values    */  %s,\n",
		has_method(self, CONSTRAINT_SET_VALUES) ?
		"constraint_set_values" : "NULL");
	fprintf(f, "/* constraint_extension     */  %s\n",
		get(find_classvar_value(self, CONSTRAINT_EXTENSION)));
	fprintf(f, "},\n");
}

inline static void initialize_xmprimitive_class_part(FILE *f, Class self,
	Class part)
{
	initialize_core_class_part(f, self, part);

	fprintf(f, "{ /* XmPrimitive class part */\n");
	fprintf(f, "%s,\t/* border_highlight */\n",
		has_method(self, BORDER_HIGHLIGHT) ?
		"border_highlight" : "XmInheritBorderHighlight");
	fprintf(f, "%s,\t/* border_unhighlight */\n",
		has_method(self, BORDER_UNHIGHLIGHT) ?
		"border_unhighlight" : "XmInheritBorderUnhighlight");
	fprintf(f, "%s,\t/* translations */\n",
		get(find_classvar_value(self, TRANSLATIONS)));
	fprintf(f, "%s,\t/* arm_and_activate */\n",
		has_method(self, ARM_AND_ACTIVATE) ?
		"arm_and_activate" : "XmInheritArmAndActivate");
	fprintf(f, "%s,\t/* syn_resources */\n",
		get(find_classvar_value(self, SYN_RESOURCES)));
	fprintf(f, "%s,\t/* num_syn_resources */\n",
		get(find_classvar_value(self, NUM_SYN_RESOURCES)));
	fprintf(f, "#define primitive_extension extension\n");
	fprintf(f, "%s,\t/* primitive_extension */\n",
		get(find_classvar_value(self, PRIMITIVE_EXTENSION)));
	fprintf(f, "},\n");
}

inline static void initialize_xmmanager_class_part(FILE *f, Class self,
	Class part)
{
	initialize_constraint_class_part(f, self, part);

	fprintf(f, "{ /* XmManager class part */\n");
	fprintf(f, "#define manager_extension extension\n");
	fprintf(f, "/* translations                 */  %s,\n",
		get(find_classvar_value(self, TRANSLATIONS)));
	fprintf(f, "/* syn_resources                */  %s,\n",
		get(find_classvar_value(self, SYN_RESOURCES)));
	fprintf(f, "/* num_syn_resources            */  %s,\n",
		get(find_classvar_value(self, NUM_SYN_RESOURCES)));
	fprintf(f, "/* syn_constraint_resources     */  %s,\n",
		get(find_classvar_value(self, SYN_CONSTRAINT_RESOURCES)));
	fprintf(f, "/* num_syn_constraint_resources */  %s,\n",
		get(find_classvar_value(self, NUM_SYN_CONSTRAINT_RESOURCES)));
	fprintf(f, "/* parent_process               */  %s,\n",
		has_method(self, PARENT_PROCESS) ?
		"parent_process" : "XmInheritParentProcess");
	fprintf(f, "/* manager_extension            */  %s,\n",
		get(find_classvar_value(self, MANAGER_EXTENSION)));
	fprintf(f, "},\n");
}


/* Since the intrinsic classes do not conform to the layout as used by
 * this widget builder (i.e., methods first, then class variables) they
 * are treated separately. The intrinsic classes handled here are Core,
 * RectObj, Composite and Shell.
 */

static void init_class_parts(FILE *f, Class self, Class part)
{
	int n;
	Section s;
	STRING h;

	if (part->superclass == CORE) {
		initialize_core_class_part(f, self, part);
	} else if (part->superclass == OBJECT) {
		initialize_object_class_part(f, self, part);
	} else if (part->superclass == RECTOBJ) {
		initialize_rectobj_class_part(f, self, part);
	} else if (part->superclass == COMPOSITE) {
		initialize_composite_class_part(f, self, part);
	} else if (part->superclass == CONSTRAINT) {
		initialize_constraint_class_part(f, self, part);
	} else if (part->superclass == PRIMITIVE
			|| part->superclass==XMPRIMITIVE) {
		initialize_xmprimitive_class_part(f, self, part);
	} else if (part->superclass == MANAGER
			|| part->superclass == XMMANAGER) {
		initialize_xmmanager_class_part(f, self, part);
	} else if (! part->super) {
		fprintf(stderr, _("superclass %s not found\n"),
			get(part->superclass));
		nerrors++;
	} else {
		init_class_parts(f, self, part->super); /* Recursively */
	}
	fprintf(f, "{ /* %s_class part */\n", get(part->name));
	n = 0;
	for (s = part->methods; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc
			&& find_method_class(part, s->decl->name) == part) {
			if (has_method(self, s->decl->name))
				fprintf(f, "%s,\n", get(s->decl->name));
			else
				fprintf(f, "XtInherit_%s,\n",
					get(s->decl->name));
			n++;
		}
	}
	for (s = part->classvars; s; s = s->next) {
		if (s->decl && s->decl->tp == Var
			&& find_classvar_class(part, s->decl->name) == part) {
			h = find_classvar_value(self, s->decl->name);
			fprintf(f, "/* %s */  %s,\n", get(s->decl->name),
				get(h));
			n++;
		}
	}
	if (n == 0)
		fprintf(f, " /* dummy */  0\n");
	fprintf(f, "},\n");
}

/* The local functions are also declared with a function prototype
 * before they are defined, because they may be mutually recursive.
 */
inline static void define_utility_type(FILE *f, Section s)
{
	fprintf(f, "typedef %s %s;\n", get(s->decl->type), get(s->decl->name));
}


inline static void define_utility_var(FILE *f, Section s)
{
	fprintf(f, "static %s %s", get(s->decl->type), get(s->decl->name));
	if (s->decl->suffix)
		/* An array? */
		fputS(s->decl->suffix, f);
	if (s->decl->value)
		fprintf(f, " = %s", get(s->decl->value));
	fputs(";\n", f);
}

inline static void generate_local_functions(FILE *f, Class c, int lines)
{
	Section s;

	for (s = c->utilities; s; s = s->next) {
		if (s->decl) {
			lined(s->decl->lineno, c->filename);
			switch (s->decl->tp) {
				case Proc:
					declare_function(0, s->decl->name,
						s->decl, f);
					break;
				case Var:
					define_utility_var(f, s);
					break;
				case Type:
					define_utility_type(f, s);
					break;
				case Def:
					define_macro(f, s->decl, c);
					break;
				/* In the continuing fight to make
				 * gcc -Wall be quiet, we have the following:
				 */
				case Undefined: break;
				case Incl: break;
				case Trans: break;
			}
		}
	}
	for (s = c->utilities; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc) {
			lined(s->decl->lineno, c->filename);
			define_function(0, lines, s->decl, s->decl->body,
			c, f, s->decl->lineno);
		}
	}
}




static void generate_action_function_defs(FILE *f, Class c, int lines)
{
	Section s;

	for (s = c->actions; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc) {
			fputs("/*ARGSUSED*/\n", f);
			lined(s->decl->lineno, c->filename);
			fprintf(f,
				"static void %s(self,event,params,num_params)",
				get(s->decl->name));
			fprintf(f,
				"Widget self;XEvent*event;String*params;"
				"Cardinal*num_params;\n");
			print_body(f, s->decl->body, c, s->decl->name,
				s->decl->lineno, 0);
			putc('\n', f);
		}
	}
}

/* The |_resolve_inheritance| method contains two lines for each method
 * that is new in this class. The lines contain code that check for the
 * constant |XtInherit|-something and put the method from the superclass
 * in its place. The routine returns immediately when the |class| is
 * |c| itself, since a class doesn't inherit from itself.
 *
 * When the class is a subclass of Composite, the extension record that
 * tells Composite to accept gadgets as children is automatically added.
 * This means that the X Toolkit will not be able to issue a warning when
 * gadgets are added as children to a composite widget that can't handle
 * them. But on the other hand, having to specify explicitly whether a
 * widget accepts gadgets is a nuisance, and it doesn't even guarantee
 * that the widget will accept a particular type of gadget.
 */


inline static void generate_resolve_inheritance_defs(FILE *f, Class c)
{
	STRING instname;
	Section s;

	instname = get_instname(c->name);

	fprintf(f, "static void _resolve_inheritance(class)\n");
	fprintf(f, "WidgetClass class;\n");
	fprintf(f, "{\n");
	fprintf(f, "  %sWidgetClass c = (%sWidgetClass) class;\n",
		get(c->name), get(c->name));
	fprintf(f, "  %sWidgetClass super;\n", get(c->name));
	if (find_superclass(c, COMPOSITE)) {
		fprintf(f, "  static CompositeClassExtensionRec "
		"extension_rec = {\n");
		fprintf(f,
			"    NULL, NULLQUARK, XtCompositeExtensionVersion,\n");
		fprintf(f, "    sizeof(CompositeClassExtensionRec), True};\n");
		fprintf(f, "  CompositeClassExtensionRec *ext;\n");
		fprintf(f, "  ext = (XtPointer)XtMalloc(sizeof(*ext));\n");
		fprintf(f, "  *ext = extension_rec;\n");
		fprintf(f, "  ext->next_extension = "
			"c->composite_class.extension;\n");
		fprintf(f, "  c->composite_class.extension = ext;\n");
	}
	fprintf(f, "  if (class == %sWidgetClass) return;\n", get(instname));
	fprintf(f, "  super = (%sWidgetClass)class->core_class.superclass;\n",
		get(c->name));
	for (s = c->methods; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc
			&& find_method_class(c, s->decl->name) == c) {
			fprintf(f, "  if (c->%s_class.%s == XtInherit_%s)\n",
				get(instname), get(s->decl->name),
				get(s->decl->name));
			fprintf(f,
				"    c->%s_class.%s = super->%s_class.%s;\n",
				get(instname), get(s->decl->name),
				get(instname), get(s->decl->name));
		}
	}
	fprintf(f, "}\n");
}


/* The |insert_child|, |delete_child| and |geometry_manager| methods
 * are treated specially, because we want to ensure that there is a
 * `self' declared in them, even though their only parameter is |child|.
 * Directly after the opening brace we insert the line |Widget self =
 * XtParent(child);|.
 */

inline static void generate_method_synthetic_self(FILE *f, Class c,
	Section s, Section s2, int lines)
{
	define_header(0, s2->decl, f, lines, s2->decl->lineno, c->filename);
	fprintf(f, "{ Widget self = XtParent(%s); ",
		get(s2->decl->params->name));
	print_body(f, s->decl->body, c, s2->decl->name, s->decl->lineno, 0);
	fputs("}\n", f);
}

inline static void generate_method_def(FILE *f, Class c, Section s, int lines)
{
	Section s2;

	s2 = find_method(c, s->decl->name);
	if (find_superclass(c, COMPOSITE) && s->decl->name == INSERT_CHILD) {
		generate_method_synthetic_self(f, c, s, s2, lines);
	} else if (find_superclass(c, COMPOSITE)
			&& s->decl->name == DELETE_CHILD) {
		generate_method_synthetic_self(f, c, s, s2, lines);
	} else if (find_superclass(c, COMPOSITE)
			&& s->decl->name == GEOMETRY_MANAGER) {
		generate_method_synthetic_self(f, c, s, s2, lines);
	} else {
		define_function(0, lines, s2->decl, s->decl->body, c, f,
			s->decl->lineno);
	}
}

inline static void generate_method_function_defs(FILE *f, Class c, int lines)
{
	Section s;

	generate_resolve_inheritance_defs(f, c);
	for (s = c->methods; s; s = s->next) {
		if (s->decl && s->decl->tp == Proc) {
			lined(s->decl->lineno, c->filename);
			generate_method_def(f, c, s, lines);
		} else if (s->decl && s->decl->tp == Def) {
			define_macro(f, s->decl, c);
		}
	}
}

/* If the exported variable is not initialized, there is no need to
 * define it again, since the declaration is already put in the public
 * header file.
 */

inline static void define_exported_variable(FILE *f, Class c,
	Section s, int lines)
{
	if (s->decl->value) {
		lined(s->decl->lineno, c->filename);
		fprintf(f, "%s %s%s = %s;\n",
			get(s->decl->type), get(s->decl->name),
			get(s->decl->suffix), get(s->decl->value));
	}
}

inline static void generate_exported_function_defs(FILE *f, Class c, int lines)
{
	Section s;

	for (s = c->exports; s; s = s->next) {
		if (s->decl) {
			lined(s->decl->lineno, c->filename);
			switch (s->decl->tp) {
				case Proc:
					define_function(1, lines, s->decl,
						s->decl->body, c, f,
						s->decl->lineno);
					break;
				case Var:
					define_exported_variable(f, c, s,
						lines);
					break;
				case Undefined: break;
				case Trans: break;
				case Type: break;
				case Def: break;
				case Incl: break;
			}
		}
	}
}

void generatec(FILE *f, Class c, char *prefix, int lines)
{
	wbuild_comment(f);
	generate_imports(f, c, prefix, lines);
	action_func_decl(f, c, lines);
	action_table_trans(f, c);
	method_func_decl(f, c, lines);
	generate_local_functions(f, c, lines);
	res_list(f, c, lines);
	generate_constraint_res_list(f, c, lines);
	class_record_instantation(f, c);
	generate_action_function_defs(f, c, lines);
	generate_method_function_defs(f, c, lines);
	generate_exported_function_defs(f, c, lines);
}
