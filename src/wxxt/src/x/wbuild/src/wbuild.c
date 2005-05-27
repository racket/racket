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
#include <sys/types.h>
#include <sys/stat.h>

#include <libit/stat.h>
#include <libit/unistd.h>
#include <libit/string.h>

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>

#include <libit/getopt.h>
#include <wbuild.h>
#include <wsym.h>
#include <libit/ctype.h>

#include <libintl.h>
#define _(String) gettext(String)

#include <libit/malloc.h>

extern int yyparse();

/* The global variables are used to hold the program's command line
 * arguments (file names), which will be read one by one by the scanner.
 */

extern char *optarg;
extern int optind;
static char **arguments;
static int nrarguments;
extern char *filename;
extern FILE *yyin;
extern int lineno;
int nerrors = 0;
extern time_t filetime;

void get_filetime(char *filename, time_t *result)
{
	struct stat s;
	int status;

	*result = (time_t) 0;
	status = stat(filename, &s);
	if (status == 0)
		*result = s.st_mtime;
}

int yywrap()
{
	fclose(yyin);
	lineno = 1;
	if (optind < nrarguments) {
		if (arguments[optind][0] == '-' && arguments[optind][1] == '\0') {
			filename = "<stdin>";
			filetime = 0;
			yyin = stdin;
			optind++;
			return 0;
		} else if (! (yyin = fopen(arguments[optind], "r"))) {
			perror(arguments[optind]);
			return 1;
		} else {
			filename = arguments[optind];
			get_filetime(filename, &filetime);
			optind++;
			return 0;
		}
	} else {
		return 1;
	}
}

char *build_filename(char *dir, char *name, char *ext)
{
	char *tmp;
	int dirlen, namelen, extlen;

	dirlen = strlen(dir);
	namelen = strlen(name);
	extlen = strlen(ext);

	tmp = malloc(dirlen + namelen + extlen + 2);
	strcpy(tmp, dir);
	tmp[dirlen] = '/';
	strcpy(tmp + dirlen + 1, name);
	strcpy(tmp + dirlen + namelen + 1, ext);
	return tmp;
}

char *build_guard(char *prefix, char *filename, char *ending)
{
	char *tmp, *s;
	int prefixlen, filenamelen, endinglen;

	prefixlen = strlen(prefix);
	filenamelen = strlen(filename);
	endinglen = strlen(ending);

	tmp = malloc(prefixlen + filenamelen + endinglen + 3);
	tmp[0] = '_';
	strcpy(tmp + 1, prefix);
	tmp[prefixlen + 1] = '_';
	strcpy(tmp + prefixlen + 2, filename);
	strcpy(tmp + prefixlen + filenamelen + 2, ending);

	s = tmp;
	while (*s) {
		if (ISALPHA(*s)) {
			*s = toupper(*s);
		} else if (!isdigit(*s)) {
			*s = '_';
		}
		s++;
	}
	return tmp;
}

void process_file(Class c, char *include_dir, char *c_dir, char *doc_dir,
	char *prefix, int lines, int force)
{
	char *name, *filename, *guard;
	time_t desttime;
	Doctype doctype;
	FILE *f;

	if (c->filenamepart)
		name = get(c->filenamepart);
	else
		name = get(c->name);

	if (!c->nocode) {
		filename = build_filename(include_dir, name, ".h");
		get_filetime(filename, &desttime);
		if (force || (desttime < c->filetime)) {
			guard = build_guard(prefix, name, "_H");
			f = fopen(filename, "w");
			public_header(f, c, prefix, guard);
			fclose(f);
			free(guard);
		}
		free(filename);

		filename = build_filename(include_dir, name, "P.h");
		get_filetime(filename, &desttime);
		if (force || (desttime < c->filetime)) {
			guard = build_guard(prefix, name, "P_H");
			f = fopen(filename, "w");
			generate_private_header(f, c, prefix, guard);
			fclose(f);
			free(guard);
		}
		free(filename);

		filename = build_filename(c_dir, name, ".c");
		get_filetime(filename, &desttime);
		if (force || (desttime < c->filetime)) {
			f = fopen(filename, "w");
			generatec(f, c, prefix, lines);
			fclose(f);
		}
		free(filename);
	}

	if (c->nodoc)
		return;

	for (doctype = doctypes; doctype; doctype = doctype->next) {
		if (doc_dir)
			filename = build_filename(doc_dir, name,
				doctype->tag[t_filename][1]);
		else
			filename = build_filename(
				doctype->tag[t_filename][0],
				name, doctype->tag[t_filename][1]);
		get_filetime(filename, &desttime);
		if (force || (desttime < c->filetime)) {
			f = fopen(filename, "w");
			generate_doc(f, &(doctype->tag), c, doctype->shortdoc);
			fclose(f);
		}
		free(filename);
	}
}

struct option const longopts[] = {
	{"version",		no_argument,		0,	'v'},
	{"help",		no_argument,		0,	'h'},
	{"no-lines",		no_argument,		0,	'l'},
	{"include-prefix",	required_argument,	0,	'p'},
	{"only",		required_argument,	0,	'O'},
	{"include-dir",		required_argument,	0,	'i'},
	{"c-dir",		required_argument,	0,	'c'},
	{"doc-dir",		required_argument,	0,	'd'},
	{"documentation-dir",	required_argument,	0,	'd'},
	{"force",		no_argument,		0,	258},
	{"no-init-file",	no_argument,		0,	257},
	{0,			0,			0,	0}
};

/* The main program invokes the parser on all command line arguments
 * and then calls the generator to create all the C and \TeX\ files
 * (unless the parser reported errors).
 *
 * The intrinsic classes are read from a configuration file, which must
 * be present.
 */

int main(int argc, char *argv[])
{
	char *include_dir = "../include/Xfwf";
	char *c_dir = "../lib";
	char *doc_dir = 0;
	char *prefix = "Xfwf";
    	char *only = 0;
	int lines = 1;
	int help = 0, version = 0;
	int force = 0;
	int init_file = 1;
	int c;
	Class class;

	symbol_init();

	arguments = argv;
	nrarguments = argc;

	while ((c = getopt_long(argc, argv, "lhvp:O:i:c:d:", longopts,
			/*& optind */ 0)) != -1)
		switch (c) {
			case 'l': lines = 0; break;
			case 'h': help = 1; break;
			case 'v': version = 1; break;
			case 'p': prefix = optarg; break;
			case 'O': only = optarg; break;
			case 'i': include_dir = optarg; break;
			case 'c': c_dir = optarg; break;
			case 'd': doc_dir = optarg; break;
			case 257: init_file = 0; break;
			case 258: force = 1; break;
			case '?': help = 1; break;
		}
	if (help) {
		fprintf(stderr, _("wbuild usage:\n"
			"wbuild [options] input-files\n"
			"-v, --version\t\tPrint version and exit\n"
			"-h, --help\t\tPrint this message and exit\n"
			"-l, --no-lines\t\tDon't generate #line directives\n"
			"-p, --include-prefix\tSpecify prefix for generated #include directives\n"
			"-O, --only\t\tSet class to output code for\n"
			"-i, --include-dir\tdirectory for generated include files\n"
			"-c, --c-dir\t\tdirectory for generated c files\n"
			"-d, --doc-dir, --documentation-dir\n"
			"\t\t\tdirectory for generated documentation files\n"
			"    --force\t\tForce regeneration of all files, ignoring timestamps\n"
			"    --no-init-file\tDo not read initialization file\n"
			"\n"
			"Report bugs to free-widgets-bugs@let.rug.nl\n"
			));
		return 0;
	} else if (version) {
	fprintf (stderr, "wbuild - FWF %s %s\n", "???", "3.2");
	fprintf (stderr, _("\
Copyright (C) %s Joel N. Weber II\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\
"), "1996");
	fprintf (stderr, _("Written by %s\n"),
		"Joel N. Weber II <nemo@koa.iolani.honolulu.hi.us>");
		return 0;
	}
	if (init_file) {
		yyin = fopen(PKGDATADIR "/init.w", "r");
		if (!yyin) {
			perror(PKGDATADIR "/init.w");
			return 1;
		}
	} else {
		yyin = fopen("/dev/null", "r");
		if (!yyin) {
			perror("/dev/null");
			return 1;
		}
		yywrap();
	}
	lineno = 1;
	yyparse();
	if (nerrors)
		return 1;
	nerrors = set_hierarchy();
	if (nerrors)
		return 1;
	for (class = classes; class; class = class->next)
		if (!only || (!strcmp(only, get(class->name))))
			process_file(class, include_dir, c_dir, doc_dir,
				prefix, lines, force);
	if (nerrors)
		return 1;
	return 0;
}

/* The |yywrap| function is called by the scanner when the end of a
 * file is reached. The function sets up a new file and returns zero. If
 * all files have been read, the function returns 1.
 */


/* The |err| function is used by the parser to display error messages.
 */

#if !HAVE_STDARG_H
void err(va_alist) va_dcl
{
	va_list ap;
	int fatal;  
	char *format;
	va_start(ap);
	fatal = va_arg(ap, int);
	format = va_arg(ap, char*);
#else /* HAVE_STDARG_H */
#ifdef __STDC__
void err(int fatal, char *format,...)
{
	va_list ap;
	va_start(ap, format);
#else /* not __STDC__ */
void err(fatal, format) int fatal; char *format;
{
	va_list ap;
	va_start(ap, format);
#endif /* not __STDC__ */
#endif /* HAVE_STDARG_H */
	nerrors++;
	(void) fprintf(stderr, "%s:%d: ", filename, lineno);
	(void) vfprintf(stderr, format, ap);
	if (fatal) exit(1);
	va_end(ap);
}  
