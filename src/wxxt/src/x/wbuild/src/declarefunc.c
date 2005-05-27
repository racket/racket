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

/* |declare_function| creates a prototype for a function |d|. If
 * |export| is |False|, the prototype will be preceded by
 * |"static"|.\mkref{declare-function}
 */

void declare_function(int export, STRING name, Decl d, FILE *f)
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
