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

#include <config.h>

#include <stdio.h>
#include <wbuild.h>
#include <wsym.h>
#include <ctype.h>

#include <libintl.h>
#define _(String) gettext(String)

#include <libit/malloc.h>

/* The following cases are possible:
 *
 * \item{1.} |$|: replaced by |self|.
 *
 * \item{2.} |$$|: replaced by a single \$.
 *
 * \item{3.} |$id|: replaced by |self->part.id| plus some typecasts.
 *
 * \item{4.} |$id1$id2|: replaced by |id1->part.id2| plus some
 * typecasts.
 *
 * \item{5.} |$(id1)id2|: replaced by |self->part.id2| plus typecasts,
 * where |part| is the class |id2| or a superclass.
 *
 * \item{6.} |$id1$(id2)id3|: replaced by |id1->part.id3| plus typecasts,
 * where |part| is the class |id2| or a superclass.
 */


inline static char *expand_hash_word_p(FILE *f, char *p, Class class,
	int lineno, STRING procname)
{
	STRING word;
	Class part;

	if (*(p+1) == '#') {
		p++;
		putc('#', f);
		return p;
	}
	if (!class->super) {
		fprintf(stderr, _("%s:%d: (in %s) no superclass (#) defined\n"),
			get(class->filename), lineno, get(procname));
		nerrors++;
		return p;
	}
	word = get_word(p + 1);
	if (!word) {
		fputs("XtSuperclass(self)", f);
		return p;
	}
	p += Strlen(word);
	if ((part = find_method_class(class->super, word)) != NULL
		|| (part = find_classvar_class(class->super, word)) != NULL) {
		fprintf(f, "%sClassRec.%s_class.%s",
			get(get_instname(class->super->name)),
			get(get_instname(part->name)), get(word));
	} else {
		fprintf(stderr,
			_("%s:%d: (in %s) identifier `#%s' is unknown\n"),
			get(class->filename), lineno, get(procname),
			get(word));
		nerrors++;
	}
	delete(word);
	return p;
}

static void expand_word_part1(FILE *f, Class class, Class part1,
	STRING word, STRING word1, int lineno, STRING procname)
{
	Class part;

	if ((part = find_instvar_class(part1, word)) != NULL)
		fprintf(f, "((%sWidget)%s)->%s.%s", get(class->name),
			get(word1), get(get_instname(part->name)), get(word));
	else if ((part = find_method_class(part1, word)) != NULL
		|| (part = find_classvar_class(part1, word)) != NULL)
		fprintf(f,
			"((%sWidgetClass)%s->core.widget_class)->%s_class.%s",
			get(class->name), get(word1),
			get(get_instname(part->name)), get(word));
	else if ((part = find_constr_class(part1, word)) != NULL)
		fprintf(f,
			"((%sConstraintRec*)((%sWidget)%s)->"
			"core.constraints)->%s.%s",
			get(class->name), get(class->name), get(word1),
			get(get_instname(part->name)), get(word));
	else {
		fprintf(stderr,
			_("%s:%d: (in %s) identifier `$%s' is unknown\n"),
			get(class->filename), lineno, get(procname), get(word));
		nerrors++;
	}
}


inline static char *expand_word_p(FILE *f, char *p, Class class, int lineno,
	STRING procname)
{
	STRING word1, word;
	Class part1;

	if (*(p+1) == '$') {
		/* case |$$| */
		p++;
	} else if (*(p+1) == '(') {
		/* case |$(id1)id2| */
		word1 = SELF;
		word = get_word(p + 2);
		p += Strlen(word) + 1;
		if (*(p+1) != ')') {
			fprintf(stderr,
				_("%s:%d: (in %s) missing `)' after "
				"`$%s(%s'\n"),
				get(class->filename), lineno, get(procname),
				get(word1), get(word));
			nerrors++;
			return p;
		}
		part1 = find_superclass(class, word);
		if (!part1) {
			fprintf(stderr,
				_("%s:%d: (in %s) class `%s' is not a "
				"superclass\n"),
				get(class->filename), lineno, get(procname),
				get(word));
			nerrors++;
			return p;
		}
		word = get_word(p + 2);
		if (!word) {
			fprintf(stderr,
				_("%s:%d: (in %s) missing identifier after "
				"`$(%s)'\n"),
				get(class->filename), lineno, get(procname),
				get(word));
			nerrors++;
			return p;
		}
		p += Strlen(word) + 1;
		expand_word_part1(f, class, part1, word, word1, lineno,
			procname);
	} else if (!(word = get_word(p + 1))) {
		/* case |$| */
		fputs("self", f);
	} else if (p += Strlen(word), *(p+1) != '$') {
		/* case |$id| */
		word1 = SELF;
		part1 = class;
		expand_word_part1(f, class, part1, word, word1, lineno,
			procname);
	} else if (*(p+2) != '(') {
		/* case |$id1$id2| */
		word1 = word;
		word = get_word(p + 2);
		p += Strlen(word) + 1;
		part1 = class;
		expand_word_part1(f, class, part1, word, word1, lineno,
			procname);
	} else {
		/* case |$id1$(id2)id3| */
		word1 = word;
		word = get_word(p + 3);
		p += Strlen(word) + 2;
		if (*(p+1) != ')') {
			fprintf(stderr, 
				_("%s:%d: (in %s) missing `)' after `$%s(%s'\n"),
				get(class->filename), lineno, get(procname),
				get(word1), get(word));
			nerrors++;
			return p;
		}
		part1 = find_superclass(class, word);
		if (!part1) {
			fprintf(stderr,
				_("%s:%d: (in %s) class `%s' is not a "
				"superclass\n"),
				get(class->filename), lineno, get(procname),
				get(word));
			nerrors++;
			return p;
		}
		word = get_word(p + 2);
		if (!word) {
			fprintf(stderr,
				_("%s:%d: (in %s) missing identifier after "
				"`$%s()'\n"),
				get(class->filename), lineno, get(procname),
				get(word1));
			nerrors++;
			return p;
		}
		p += Strlen(word) + 1;
		expand_word_part1(f, class, part1, word, word1, lineno,
			procname);
	}
	return p;
}

void print_body(FILE *f, STRING body, Class class, STRING procname,
	int lineno, int ismacro)
{
	char *p, prev = '\0';
	int quoted = 0, squoted = 0, comment = 0;

	for (p = get(body); *p; prev = prev == '\\' ? '\0' : *p, p++)
		switch (*p) {
			case '"':
				putc('"', f);
				if (! squoted && ! comment && prev != '\\')
					quoted = ! quoted;
				break;
			case '\'':
				putc('\'', f);
				if (! quoted && ! comment && prev != '\\')
					squoted = ! squoted;
				break;
			case '*':
				putc('*', f);
				if (! quoted && ! squoted && ! comment &&
					(prev == '/'))
						comment = 1;
				break;
			case '/':
				putc('/', f);
				if (comment && prev == '*')
					comment = 0;
				break;
			case '$':
				if (quoted || squoted || comment ||
					(prev == '\\'))
						putc('$', f);
				else {
					p = expand_word_p(f, p, class, lineno,
						procname);
				}
				break;
			case '#':
				if (quoted || squoted || comment ||
					(prev == '\\'))
						putc('#', f);
				else if (prev == '\n'
					&& ((strneq(p, "#ifdef", 6) && !isalnum(p[6]))
					|| (strneq(p, "#ifndef", 7) && !isalnum(p[7]))
					|| (strneq(p, "#if", 3) && !isalnum(p[3]))
					|| (strneq(p, "#define", 7) && !isalnum(p[7]))
					|| (strneq(p, "#include", 8) && !isalnum(p[8]))
					|| (strneq(p, "#undef", 6) && !isalnum(p[6]))
					|| (strneq(p, "#else", 5) && !isalnum(p[5]))
					|| (strneq(p, "#endif", 6) && !isalnum(p[6]))))
						putc('#', f);
				else {
					p = expand_hash_word_p(f, p, class,
						lineno, procname);
				}
				break;
			case '\n':
				if (ismacro)
					putc('\\', f);
				putc('\n', f);
				break;
			default:
				putc(*p, f);
	}
	putc('\n', f);
}
