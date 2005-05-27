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


#define fputS(s, f) = fputs(get(s), f)

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

/* Document tags. In the documentation file various tags indicate the
 * sections of the text. The tags can be in four formats (|DOCTYPES = 3|):
 * \TeX, Nroff, {\sc sgml}, or TexInfo. The following table contains all
 * the tags. The * enumerated type defines symbolic names for the tags.
 * The tags come in * pairs, like opening and closing brackets.
 *
 * The second half of the tags contains symbolic names for characters
 * that are special to \TeX.
 *
 * |doctp| is the type of documentation that will be generated: 0
 * for \TeX, 2 for Nroff, 1 for {\sc sgml}, 3 for TeXinfo.
 */

#define topen(tg) (tag[tg][0])
#define tclose(tg) (tag[tg][1])

/* |putcode| is used to write C code that might contain
 * characters that are special to \TeX. It also expands tabs.
 */

static void putcode(STRING text, FILE *f, tagpair *tag)
{
	char *p;
	int col = 0;
	if (text) {
		for (p = get(text); *p; p++) {
			switch (*p) {
				case '_': fputs(topen(t_underline), f);
					col++; break;
				case '\\': fputs(topen(t_backslash), f);
					col++; break;
				case '~': fputs(topen(t_tilde), f);
					col++; break;
				case '#': fputs(topen(t_hashmark), f);
					col++; break;
				case '$': fputs(topen(t_dollar), f);
					col++; break;
				case '%': fputs(topen(t_percent), f);
					col++; break;
				case '^': fputs(topen(t_caret), f);
					col++; break;
				case '<': fputs(topen(t_less), f);
					col++; break;
				case '>': fputs(topen(t_greater), f);
					col++; break;
				case '&': fputs(topen(t_ampersand), f);
					col++; break;
				case '{': fputs(topen(t_lbrace), f);
					col++; break;
				case '}': fputs(topen(t_rbrace), f);
					col++; break;
				case '|': fputs(topen(t_bar), f);
					col++; break;
        			case '@': fputs(topen(t_at), f);
					col++; break;
				case '\n': putc('\n', f);
					col = 0; break;
				case '\t':
					do {
						putc(' ', f);
					} while (++col % 8 != 0);
					break;
				default: putc(*p, f); col++;
			}
		}
	}
}


/* The function |print_text_par| copies text to the documentation file
 * and ends it with an empty line. Text between vertical bars is set in
 * typewriter type with special characters quoted.
 *
 * The function checks for an even number of vertical bars and inserts an
 * extra closing brace if the final bar is missing.
 */

static void print_text_par(FILE *f, tagpair *tag, STRING text,
	STRING file, int lineno)
{
	char *p;
	int between_bars;

	if (!text)
		return;
	between_bars = 0;
	for (p = get(text); *p; p++) {
		if (between_bars) {
			switch (*p) {
				case '_': fputs(topen(t_underline), f); break;
				case '\\': fputs(topen(t_backslash), f); break;
				case '~': fputs(topen(t_tilde), f); break;
				case '#': fputs(topen(t_hashmark), f); break;
				case '$': fputs(topen(t_dollar), f); break;
				case '%': fputs(topen(t_percent), f); break;
				case '^': fputs(topen(t_caret), f); break;
				case '&': fputs(topen(t_ampersand), f); break;
				case '{': fputs(topen(t_lbrace), f); break;
				case '}': fputs(topen(t_rbrace), f); break;
				case '@': fputs(topen(t_at), f); break;
				case '|':
					fputs(tclose(t_inline), f);
					between_bars = 0;
					break;
				default: putc(*p, f);
			}
		} else if (*p == '|') {
			between_bars = 1;
			fputs(topen(t_inline), f);
		} else {
			putc(*p, f);
		}
	}
	if (between_bars) {
		fprintf(stderr, _("%s:%d: unmatched \"|\" in text\n"),
			get(file), lineno);
		nerrors++;
		fputs(tclose(t_inline), f); /* make TeX happy */
	}
	putc('\n', f); (void) putc('\n', f);
}

inline static void import_file(FILE *f, tagpair *tag, Section s)
{
	fputs(topen(t_code), f);
	fputs(topen(t_incl), f);
	putc(' ', f);
	putcode(s->decl->name, f, tag);
	fputs(tclose(t_code), f);
}

/* The function name and body are taken from |s|, but the type and
 * parameters are taken from |s2|. |s2| may be the same as |s|, but for
 * inherited methods, it should point to the first definition of the
 * function.
 */

inline static void function(FILE *f, tagpair *tag, Section s, Section s2)
{
	Decl d1;

	fputs(topen(t_code), f);
	if (s2->decl->type) {
		putcode(s2->decl->type, f, tag);
		putc(' ', f);
	}
	putcode(s->decl->name, f, tag);
	putc('(', f);
	if (s2->decl->params) {
		for (d1 = s2->decl->params; d1; d1 = d1->next) {
			if (d1->type) {
				putcode(d1->type, f, tag);
				putc(' ', f);
			}
			putcode(d1->name, f, tag);
			if (d1->suffix)
				putcode(d1->suffix, f, tag);
			if (d1->next)
				fputs(", ", f);
		}
	}
	putc(')', f);
	putc('\n', f);
	if (s->decl->body)
		putcode(s->decl->body, f, tag);
	fputs(tclose(t_code), f);
}

inline static void variable(FILE *f, tagpair *tag, Section s)
{
	fputs(topen(t_code), f);
	putcode(s->decl->type, f, tag);
	if (s->decl->type)
		putc(' ', f);
	putcode(s->decl->name, f, tag);
	if (s->decl->suffix)
		putcode(s->decl->suffix, f, tag);
	if (s->decl->value) {
		fputs(" = ", f);
		putcode(s->decl->value, f, tag);
	}
	fputs(tclose(t_code), f);
}

inline static void type(FILE *f, tagpair *tag, Section s)
{
	fputs(topen(t_code), f);
	fputs(topen(t_type), f);
	putc(' ', f);
	putcode(s->decl->name, f, tag);
	fputs(" = ", f);
	putcode(s->decl->type, f, tag);
	fputs(tclose(t_code), f);
}

inline static void macro(FILE *f, tagpair *tag, Section s)
{
	Decl d1;

	fputs(topen(t_macro), f);
	putcode(s->decl->name, f, tag);
	if (s->decl->params) {
		putc('(', f);
		for (d1 = s->decl->params; d1; d1 = d1->next) {
			putcode(d1->name, f, tag);
			if (d1->next)
				fputs(", ", f);
		}
		fputs(") =\n", f);
	} else {
		fputs(" = ", f);
	}
	putcode(s->decl->body, f, tag);
	fputs(tclose(t_macro), f);
}
inline static void chapter_title_intro(FILE *f, tagpair *tag, Class c)
{
	fputs(topen(t_class), f);
	fputs(topen(t_name), f);
	putcode(c->name, f, tag);
	fputs(tclose(t_name), f);
	if (topen(t_name2)) {
		fputs(topen(t_name2), f);
		putcode(c->name, f, tag);
		fputs(tclose(t_name2), f);
	}
	print_text_par(f, tag, c->text, c->filename, c->lineno);
}


/* The table is started with |\restable{name}| and ends with
 * |\endrestable|. The `name' is the name of the class in which the
 * public variables (resources) are defined.
 */


inline static void public_variables_table(FILE *f, tagpair *tag, Class c)
{
	Section s;

	fputs(topen(t_table), f);
	fputs(topen(t_tablehead), f);
	putcode(c->name, f, tag);
	fputs(tclose(t_tablehead), f);
	for (s = c->publicvars; s; s = s->next) {
		if (s->decl && find_instvar_class(c, s->decl->name) == c) {
			fputs(topen(t_row), f);
			fputs(topen(t_resname), f);
			if (s->decl->namesym) {
				putcode(s->decl->namesym, f, tag);
			} else {
				fputs("XtN", f);
				putcode(s->decl->name, f, tag);
			}
			fputs(tclose(t_resname), f);
			fputs(topen(t_resclass), f);
			fputs("XtC", f);
			if (s->decl->namesym)
				putcode(get_classname(s->decl->namesym),
					f, tag);
			else
				putcode(get_classname(s->decl->name), f, tag);
			fputs(tclose(t_resclass), f);
			fputs(topen(t_restype), f);
			/* (void) fputs("XtR", f); */
			if (s->decl->typesym)
				putcode(s->decl->typesym, f, tag);
			else
				putcode(s->decl->type, f, tag);
			fputs(tclose(t_restype), f);
			fputs(topen(t_resdefault), f);
			putcode(s->decl->value, f, tag);
			fputs(tclose(t_resdefault), f);
			fputs(tclose(t_row), f);
		}
	}
	fputs(tclose(t_table), f);
}

/* The section on public instance variables (resources) contains only
 * the variables that are new in this widget. The inherited resources
 * will be listed in the next section. However, if an inherited resource
 * is give a new default value (and an explanatory text), that variable
 * will be given here also.
 */


inline static void public_variables(FILE *f, tagpair *tag, Class c,
	int shortdoc)
{
	Section s;

	if (c->publicvars != NULL) {
		fputs(topen(t_publicvars), f);
		public_variables_table(f, tag, c);
		for (s = c->publicvars; s; s = s->next) {
			fputs(topen(t_section), f);
			if (s->decl) {
				fputs(topen(t_publicvar), f);
				if (s->decl->namesym) {
					putcode(s->decl->namesym, f, tag);
				} else {
					fputs("XtN", f);
					putcode(s->decl->name, f, tag);
				}
				fputs(tclose(t_publicvar), f);
			}
			print_text_par(f, tag, s->text, c->filename, c->lineno);
			if (!shortdoc && s->decl) {
				/* (void) fputs(topen(t_code), f); */
				if (s->decl->typesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->typesym, f, tag);
					fputs(topen(t_greater), f);
					putc(' ', f);
				}
				putcode(s->decl->type, f, tag);
				putc(' ', f);
				putcode(s->decl->name, f, tag);
				if (s->decl->suffix)
					putcode(s->decl->suffix, f, tag);
				if (s->decl->namesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->namesym, f, tag);
					fputs(topen(t_greater), f);
					putc(' ', f);
				}
				fputs(" = ", f);
				if (s->decl->valuesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->valuesym, f, tag);
					fputs(topen(t_greater), f);
				}
				putcode(s->decl->value, f, tag);
				/* (void) fputs(tclose(t_code), f); */
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_publicvars), f);
	}
}

inline static void constraint_resources_table(FILE *f, tagpair *tag, Class c)
{
	Section s;

	fputs(topen(t_table), f);
	fputs(topen(t_tablehead), f);
	putcode(c->name, f, tag);
	fputs(tclose(t_tablehead), f);
	for (s = c->constraints; s; s = s->next) {
		if (s->decl && find_constr_class(c, s->decl->name) == c) {
			fputs(topen(t_row), f);
			fputs(topen(t_resname), f);
			if (s->decl->namesym) {
				putcode(s->decl->namesym, f, tag);
			} else {
				fputs("XtN", f);
				putcode(s->decl->name, f, tag);
			}
			fputs(tclose(t_resname), f);
			fputs(topen(t_resclass), f);
			fputs("XtC", f);
			if (s->decl->namesym)
				putcode(get_classname(s->decl->namesym),
					f, tag);
			else
				putcode(get_classname(s->decl->name), f, tag);
			fputs(tclose(t_resclass), f);
			fputs(topen(t_restype), f);
			/* (void) fputs("XtR", f); */
			if (s->decl->typesym)
				putcode(s->decl->typesym, f, tag);
			else
				putcode(s->decl->type, f, tag);
			fputs(tclose(t_restype), f);
			fputs(topen(t_resdefault), f);
			putcode(s->decl->value, f, tag);
			fputs(tclose(t_resdefault), f);
			fputs(tclose(t_row), f);
		}
	}
	fputs(tclose(t_table), f);
}


inline static void constraint_resources(FILE *f, tagpair *tag, Class c,
	int shortdoc)
{
	Section s;

	if (c->constraints != NULL) {
		fputs(topen(t_constraints), f);
		constraint_resources_table(f, tag, c);
		for (s = c->constraints; s; s = s->next) {
			fputs(topen(t_section), f);
			if (s->decl) {
				fputs(topen(t_constraint), f);
				if (s->decl->namesym) {
					putcode(s->decl->namesym, f, tag);
				} else {
					fputs("XtN", f);
					putcode(s->decl->name, f, tag);
				}
				fputs(tclose(t_constraint), f);
			}
			print_text_par(f, tag, s->text, c->filename, c->lineno);
			if (!shortdoc && s->decl) {
				/* (void) fputs(topen(t_code), f); */
				if (s->decl->typesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->typesym, f, tag);
					fputs(topen(t_greater), f);
					putc(' ', f);
				}
				putcode(s->decl->type, f, tag);
				putc(' ', f);
				putcode(s->decl->name, f, tag);
				if (s->decl->suffix)
					putcode(s->decl->suffix, f, tag);
				if (s->decl->namesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->namesym, f, tag);
					fputs(topen(t_greater), f);
					putc(' ', f);
				}
				fputs(" = ", f);
				if (s->decl->valuesym) {
					fputs(topen(t_less), f);
					putcode(s->decl->valuesym, f, tag);
					fputs(topen(t_greater), f);
				}
				putcode(s->decl->value, f, tag);
				/* (void) fputs(tclose(t_code), f); */
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_constraints), f);
	}
}



inline static void export_declaration(FILE *f, tagpair *tag, Section s,
	int shortdoc)
{
	Decl d1;

	switch (s->decl->tp) {
		case Proc:
			fputs(topen(t_code), f);
			putcode(s->decl->type, f, tag);
			if (s->decl->type)
				putc(' ', f);
			putcode(s->decl->name, f, tag);
			if (s->decl->suffix)
				putcode(s->decl->suffix, f, tag);
			if (s->decl->params) {
				putc('(', f);
				for (d1 = s->decl->params; d1; d1 = d1->next) {
					putcode(d1->type, f, tag);
					putc(' ', f);
					putcode(d1->name, f, tag);
					if (d1->suffix)
						putcode(d1->suffix, f, tag);
					if (d1->next)
						fputs(", ", f);
				}
				putc(')', f);
			}
			fputs(tclose(t_code), f);
			if (!shortdoc)
				putcode(s->decl->body, f, tag);
			break;
		case Var: variable(f, tag, s); break;
		case Type: type(f, tag, s); break;
		case Def: macro(f, tag, s); break;
		case Incl: import_file(f, tag, s); break;
		default: ; /* Cannot happen! */
	}
}

inline static void exports(FILE *f, tagpair *tag, Class c, int shortdoc)
{
	Section s;

	if (c->exports != NULL) {
		fputs(topen(t_exports), f);
		for (s = c->exports; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				export_declaration(f, tag, s, shortdoc);
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_exports), f);
	}
}

/* A recursive procedure |tabulate_inherited_resources| creates tables
 * for the public variables of all superclasses, with the most recent
 * initial values.
 */

static void tabulate_inherited_resources(FILE *f, tagpair *tag, Class self,
	Class part)
{
	Section s;
	if (!part)
		return;
	if (part->publicvars) {
		fputs(topen(t_table), f);
		fputs(topen(t_tablehead), f);
		putcode(part->name, f, tag);
		fputs(tclose(t_tablehead), f);
		for (s = part->publicvars; s; s = s->next) {
			if (s->decl && (find_instvar_class(part, s->decl->name)
					== part)) {
				fputs(topen(t_row), f);
				fputs(topen(t_resname), f);
				if (s->decl->namesym) {
					putcode(s->decl->namesym, f, tag);
				} else {
					fputs("XtN", f);
					putcode(s->decl->name, f, tag);
				}
				fputs(tclose(t_resname), f);
				fputs(topen(t_resclass), f);
				fputs("XtC", f);
				if (s->decl->namesym)
					putcode(get_classname(s->decl->namesym),
						f, tag);
				else
					putcode(get_classname(s->decl->name),
						f, tag);
				fputs(tclose(t_resclass), f);
				fputs(topen(t_restype), f);
				/* (void) fputs("XtR", f); */
				if (s->decl->typesym)
					putcode(s->decl->typesym, f, tag);
				else
					putcode(get_classname(s->decl->type),
						f, tag);
				fputs(tclose(t_restype), f);
				fputs(topen(t_resdefault), f);
				putcode(s->decl->value, f, tag);
				fputs(tclose(t_resdefault), f);
				fputs(tclose(t_row), f);
			}
		}
		fputs(tclose(t_table), f);
	}
	tabulate_inherited_resources(f, tag, self, part->super);
}

inline static void summary_inherited_resources(FILE *f, tagpair *tag, Class c)
{
	if (c->superclass)
		tabulate_inherited_resources(f, tag, c, c->super);
}


inline static void actions(FILE *f, tagpair *tag, Class c, int shortdoc)
{
	Section s;

	if (c->actions) {
		fputs(topen(t_actions), f);
		for (s = c->actions; s; s = s->next) {
			fputs(topen(t_section), f);
			if (s->decl && s->decl->tp == Proc) {
				fputs(topen(t_action), f);
				putcode(s->decl->name, f, tag);
				fputs(tclose(t_action), f);
			}
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (!shortdoc && s->decl) {
				switch (s->decl->tp) {
					case Proc:
						function(f, tag, s,
							&action_proto);
						break;
					case Def:
						macro(f, tag, s);
						break;
					default: break;
				}
			}
		fputs(tclose(t_section), f);
		}
	fputs(tclose(t_actions), f);
	}
}

inline static void default_translations(FILE *f, tagpair *tag, Class c)
{
	Section s;

	if (c->translations) {
		fputs(topen(t_translations), f);
		for (s = c->translations; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				fputs(topen(t_code), f);
				putcode(s->decl->type, f, tag);
				fputs(": ", f);
				putcode(s->decl->value, f, tag);
				fputs(tclose(t_code), f);
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_translations), f);
	}
}

inline static void imports(FILE *f, tagpair *tag, Class c)
{
	Section s;
	if (c->imports) {
		fputs(topen(t_imports), f);
		for (s = c->imports; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				switch (s->decl->tp) {
					case Proc:
						function(f, tag, s, s);
						break;
					case Var:
						variable(f, tag, s);
						break;
					case Incl:
						import_file(f, tag, s);
						break;
					default: ; /* Cannot happen! */
				}
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_imports), f);
	}
}


inline static void private_variables(FILE *f, tagpair *tag, Class c)
{
	Section s;

	if (c->privatevars) {
		fputs(topen(t_privatevars), f);
		for (s = c->privatevars; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				fputs(topen(t_code), f);
				putcode(s->decl->type, f, tag);
				putc(' ', f);
				putcode(s->decl->name, f, tag);
				if (s->decl->suffix)
					putcode(s->decl->suffix, f, tag);
				fputs(tclose(t_code), f);
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_privatevars), f);
	}
}

inline static void private_constraint_variables(FILE *f, tagpair *tag, Class c)
{
	Section s;

	if (c->privconstr) {
		fputs(topen(t_privconstraints), f);
		for (s = c->privconstr; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				fputs(topen(t_code), f);
				putcode(s->decl->type, f, tag);
				putc(' ', f);
				putcode(s->decl->name, f, tag);
				if (s->decl->suffix)
					putcode(s->decl->suffix, f, tag);
				fputs(tclose(t_code), f);
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_privconstraints), f);
	}
}

inline static void private_class_variables(FILE *f, tagpair *tag, Class c)
{
	Section s;

	if (c->classvars != NULL) {
		fputs(topen(t_classvars), f);
		for (s = c->classvars; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl)
				variable(f, tag, s);
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_classvars), f);
	}
}

inline static void utilities(FILE *f, tagpair *tag, Class c)
{
	Section s;

	if (c->utilities) {
		fputs(topen(t_utilities), f);
		for (s = c->utilities; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				switch (s->decl->tp) {
					case Proc: function(f, tag, s, s);
						break;
					case Var: variable(f, tag, s); break;
					case Type: type(f, tag, s); break;
					case Def: macro(f, tag, s); break;
					default: ; /* Cannot happen! */
				}
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_utilities), f);
	}
}


inline static void private_methods(FILE *f, tagpair *tag, Class c)
{
	Section s, s2;

	if (c->methods) {
		fputs(topen(t_methods), f);
		for (s = c->methods; s; s = s->next) {
			fputs(topen(t_section), f);
			print_text_par(f, tag, s->text, c->filename,
				s->decl ? s->decl->lineno : -1);
			if (s->decl) {
				switch (s->decl->tp) {
					case Proc:
						s2 = find_method(c,
							s->decl->name);
						function(f, tag, s, s2);
						break;
					case Def:
						macro(f, tag, s);
						break;
					default:
				}
			}
			fputs(tclose(t_section), f);
		}
		fputs(tclose(t_methods), f);
	}
}

void generate_doc(FILE *f, tagpair *tag, Class c, int shortdoc)
{
	fputs(topen(t_start), f);
	chapter_title_intro(f, tag, c);
	public_variables(f, tag, c, shortdoc);
	summary_inherited_resources(f, tag, c);
	constraint_resources(f, tag, c, shortdoc);
	exports(f, tag, c, shortdoc);
	default_translations(f, tag, c);
	actions(f, tag, c, shortdoc);
	if (!shortdoc) {
		imports(f, tag, c);
		private_variables(f, tag, c);
		private_constraint_variables(f, tag, c);
		private_class_variables(f, tag, c);
		private_methods(f, tag, c);
		utilities(f, tag, c);
	}
	fputs(tclose(t_start), f);
}
