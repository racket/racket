/*
Copyright (c) 2002 Jorge Acereda <jacereda@users.sourceforge.net>


Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

static void * dlopen(const char *path, int mode);
static void * dlsym(void * handle, const char *symbol);
static const char * dlerror(void);
static int dlclose(void * handle);

#define RTLD_LAZY	0x1
#define RTLD_NOW	0x2
#define RTLD_LOCAL	0x4
#define RTLD_GLOBAL	0x8
#define RTLD_NOLOAD	0x10
#define RTLD_NODELETE	0x80

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <mach-o/dyld.h>

/* Define this to make dlcompat reuse data block. This way in theory we save
 * a little bit of overhead. However we then couldn't correctly catch excess
 * calls to dlclose(). Hence we don't use this feature
 */
#undef REUSE_STATUS

/* Size of the internal error message buffer (used by dlerror()) */
#define ERR_STR_LEN			256

/* Maximum number of search paths supported by getSearchPath */
#define MAX_SEARCH_PATHS	32


#define MAGIC_DYLIB_OFI ((NSObjectFileImage) 'DYOF')
#define MAGIC_DYLIB_MOD ((NSModule) 'DYMO')

/* internal flags */
#define DL_IN_LIST 0x01

/* This is our central data structure. Whenever a module is loaded via
 * dlopen(), we create such a struct.
 */
struct dlstatus
{
	struct dlstatus * next;	/* pointer to next element in the linked list */
	NSModule module;
	const struct mach_header *lib;
	int refs;				/* reference count */
	int mode;				/* mode in which this module was loaded */
	dev_t device;
	ino_t inode;
	int flags; 				/* Any internal flags we may need */
};

/* Head node of the dlstatus list */
static struct dlstatus mainStatus =
	{ 0, MAGIC_DYLIB_MOD,NULL, -1, RTLD_GLOBAL, 0, 0 };
static struct dlstatus * stqueue = &mainStatus;


/* Storage for the last error message (used by dlerror()) */
static char err_str[ERR_STR_LEN];
static int err_filled = 0;


/* Prototypes to internal functions */
static void debug(const char * fmt, ...);
static void error(const char * str, ...);
static const char * safegetenv(const char * s);
static const char * searchList(void);
static const char * getSearchPath(int i);
static const char * getFullPath(int i, const char * file);
static const struct stat * findFile(const char * file, const char ** fullPath);
static int isValidStatus(struct dlstatus * status);
static int isFlagSet(int mode, int flag);
static struct dlstatus * lookupStatus(const struct stat * sbuf);
static void insertStatus(struct dlstatus * dls, const struct stat * sbuf);
static int promoteLocalToGlobal(struct dlstatus * dls);
static void * reference (struct dlstatus * dls, int mode);
static void * dlsymIntern(struct dlstatus * dls, const char *symbol, int canSetError);
static struct dlstatus * allocStatus(void);
static struct dlstatus * loadModule(const char * path, const struct stat * sbuf, int mode);

/* Functions */

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void debug(const char * fmt, ...)
{
#if DEBUG > 1
	va_list arg;
	va_start(arg, fmt);
	fprintf(stderr, "DLDEBUG: ");
	vfprintf(stderr, fmt, arg);
	fprintf(stderr, "\n");
	fflush(stderr);
	va_end(arg);
#endif
}

static void error(const char * str, ...)
{
	va_list arg;
	va_start(arg, str);
	strncpy(err_str, "dlcompat: ", ERR_STR_LEN);
	vsnprintf(err_str+10, ERR_STR_LEN-10, str, arg);
	va_end(arg);
	debug("ERROR: %s\n", err_str);
	err_filled = 1;
}

static void warning(const char * str)
{
#if DEBUG > 0
	fprintf(stderr, "WARNING: dlcompat: %s\n", str);
#endif
}

static const char * safegetenv(const char * s)
{
	const char * ss = getenv(s);
	return ss? ss : "";
}

/* Compute and return a list of all directories that we should search when
 * trying to locate a module. We first look at the values of LD_LIBRARY_PATH
 * and DYLD_LIBRARY_PATH, and then finally fall back to looking into
 * /usr/lib and /lib. Since both of the environments variables can contain a
 * list of colon seperated paths, we simply concat them and the two other paths
 * into one big string, which we then can easily parse.
 * Splitting this string into the actual path list is done by getSearchPath()
 */
static const char * searchList()
{
	char * buf;
	const char * ldlp = safegetenv("LD_LIBRARY_PATH");
	const char * dyldlp = safegetenv("DYLD_LIBRARY_PATH");
	size_t	buf_size = strlen(ldlp) + strlen(dyldlp) + 20;
	buf = malloc(buf_size);
	snprintf(buf, buf_size, "%s:%s:/usr/lib:/lib", dyldlp, ldlp);
	return buf;
}

/* Returns the ith search path from the list as computed by searchList() */
static const char * getSearchPath(int i)
{
	static const char * list = 0;
	static const char * path[MAX_SEARCH_PATHS] = {0};
	static int end = 0;
	if (!list && !end) 
		list = searchList();
	while (!path[i] && !end) 
	{
		path[i] = strsep((char**)&list, ":");
		if (path[i][0] == 0)
			path[i] = 0;
		end = list == 0;
	}
	return path[i];
}

static const char * getFullPath(int i, const char * file)
{
	static char buf[PATH_MAX];
	const char * path = getSearchPath(i);
	if (path)
		snprintf(buf, PATH_MAX, "%s/%s", path, file);
	return path? buf : 0;
}

/* Given a file name, try to determine the full path for that file. Starts
 * its search in the current directory, and then tries all paths in the 
 * search list in the order they are specified there.
 */
static const struct stat * findFile(const char * file, const char ** fullPath)
{
	int i = 0;
	static struct stat sbuf;
	debug("finding file %s",file);
	*fullPath = file;
	do
	{
		if (0 == stat(*fullPath, &sbuf))
			return &sbuf;
	}
	while ((*fullPath = getFullPath(i++, file)));
	return 0;
}

/* Determine whether a given dlstatus is valid or not */
static int isValidStatus(struct dlstatus * status)
{
	/* Walk the list to verify status is contained in it */
	struct dlstatus * dls = stqueue;
	while (dls && status != dls)
		dls = dls->next;

	if (dls == 0)
		error("invalid handle");
	else if (dls->module == 0)
		error("handle to closed library");
	else 
		return TRUE;
	return FALSE;
}

static int isFlagSet(int mode, int flag)
{
	return (mode & flag) == flag;
}

static struct dlstatus * lookupStatus(const struct stat * sbuf)
{
	struct dlstatus * dls = stqueue;
	debug("looking for status");
	while (dls && (	 /* isFlagSet(dls->mode, RTLD_UNSHARED) */ 0
			|| sbuf->st_dev != dls->device
			|| sbuf->st_ino != dls->inode) && dls->refs)
		dls = dls->next;
	return dls;
}

static void insertStatus(struct dlstatus * dls, const struct stat * sbuf)
{
	debug("inserting status");
	dls->inode = sbuf->st_ino;
	dls->device = sbuf->st_dev;
	dls->refs = 0;
	dls->mode = 0;
	if ((dls->flags & DL_IN_LIST) == 0) {
		dls->next = stqueue;
		stqueue = dls;
		dls->flags |= DL_IN_LIST;
	}	
}

static struct dlstatus * allocStatus()
{
	struct dlstatus * dls;
#ifdef REUSE_STATUS
	dls = stqueue;
	while (dls && dls->module)
		dls = dls->next;
	if (!dls)
#endif
	dls = malloc(sizeof(*dls));
	dls->flags = 0;
	return dls;
}

static int promoteLocalToGlobal(struct dlstatus * dls)
{
	static int (*p)(NSModule module) = 0;
	debug("promoting");
	if(!p)
		_dyld_func_lookup("__dyld_NSMakePrivateModulePublic", 
			  (unsigned long *)&p);
	return (dls->module == MAGIC_DYLIB_MOD) || (p && p(dls->module));
}

static void * reference (struct dlstatus * dls, int mode)
{
	if (dls)
	{
		if (dls->module == MAGIC_DYLIB_MOD && isFlagSet(mode, RTLD_LOCAL))
		{
			warning("trying to open a .dylib with RTLD_LOCAL");
			error("unable to open a .dylib with RTLD_LOCAL");
			return NULL;
		}
		if (isFlagSet(mode, RTLD_GLOBAL) &&
			!isFlagSet(dls->mode, RTLD_GLOBAL) &&
			!promoteLocalToGlobal(dls))
		{
			error("unable to promote local module to global");
			return NULL;
		}
		dls->mode |= mode;
		dls->refs++;
	}
	else
		debug("reference called with NULL argument");

	return dls;
}

static void * dlsymIntern(struct dlstatus * dls, const char *symbol,int canSetError)
{
	NSSymbol * nssym = 0;
	/* If it is a module - use NSLookupSymbolInModule */
	if (dls->module != MAGIC_DYLIB_MOD) {
	
		nssym = NSLookupSymbolInModule(dls->module, symbol);
		if (!nssym && canSetError)
			error("unable to find symbol \"%s\"", symbol);
	}		
	else {
		if (dls->lib != NULL) {
			/* dylib, use NSIsSymbolNameDefinedInImage */
			if (NSIsSymbolNameDefinedInImage(dls->lib,symbol)) {
				nssym = NSLookupSymbolInImage(dls->lib,
								  symbol,
								  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND 
								  | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR
								  );
			}
		}
		else {
			/* Global context, use NSLookupAndBindSymbol */
			nssym = NSLookupAndBindSymbol(symbol);
		}						  
		if (!nssym) {
			NSLinkEditErrors ler;
			int lerno;
			const char* errstr;
			const char* file;		
			NSLinkEditError(&ler,&lerno,&file,&errstr);	
			if (errstr && strlen(errstr)) {	
				if (canSetError)	
					error(errstr);
				debug("%s",errstr);	
			}	
			
		}
	}			      
	if (!nssym)
		return NULL;
	return NSAddressOfSymbol(nssym);
}

static struct dlstatus * loadModule(const char * path, 
					const struct stat * sbuf, int mode)
{
	NSObjectFileImage ofi = 0;
	NSObjectFileImageReturnCode ofirc;
	struct dlstatus * dls; 
	NSLinkEditErrors ler;
	int lerno;
	const char* errstr;
	const char* file;
	void (*init)(void);

	ofirc = NSCreateObjectFileImageFromFile(path, &ofi);
	switch (ofirc)
	{
		case NSObjectFileImageSuccess:
			break;
		case NSObjectFileImageInappropriateFile:
			if (isFlagSet(mode, RTLD_LOCAL))
			{
				warning("trying to open a .dylib with RTLD_LOCAL");
				error("unable to open this file with RTLD_LOCAL");
				return NULL;
			}				
			break;
		case NSObjectFileImageFailure:
			error("object file setup failure");
			return NULL;
		case NSObjectFileImageArch:
			error("no object for this architecture");
			return NULL;
		case NSObjectFileImageFormat:
			error("bad object file format");
			return NULL;
		case NSObjectFileImageAccess:
			error("can't read object file");
			return NULL;
		default:
			error("unknown error from NSCreateObjectFileImageFromFile()");
			return NULL;
	}
	dls = lookupStatus(sbuf);
	if (!dls) {
		dls = allocStatus();
	}	
	if (!dls)
	{
		error("unable to allocate memory");
		return NULL;
	}
	dls->lib=0;
	if (ofirc == NSObjectFileImageInappropriateFile)
	{
		if ((dls->lib = NSAddImage(path, NSADDIMAGE_OPTION_RETURN_ON_ERROR)))
		{
			debug("Dynamic lib loaded at %ld",dls->lib);
			ofi = MAGIC_DYLIB_OFI;
			dls->module = MAGIC_DYLIB_MOD;
			ofirc = NSObjectFileImageSuccess;
		}
	}
	else
	{
		/* Should change this to take care of RLTD_LAZY etc */
		dls->module = NSLinkModule(ofi, path,
					NSLINKMODULE_OPTION_RETURN_ON_ERROR
					 | NSLINKMODULE_OPTION_PRIVATE
					| NSLINKMODULE_OPTION_BINDNOW);
		NSDestroyObjectFileImage(ofi);
	}
	if (!dls->module)
	{
		NSLinkEditError(&ler,&lerno,&file,&errstr);			
		free(dls); 
		error(errstr);
		return NULL;
	}
	insertStatus(dls, sbuf);
	
	if ((init = dlsymIntern(dls, "__init",0)))
	{
		debug("calling _init()");
		init();
	}

	return dls;
}

static void * dlopen(const char * path, int mode)
{
	const struct stat * sbuf;
	struct dlstatus * dls;
	const char * fullPath;
	if (!path)
	{
		return &mainStatus;
	}	
	if (!(sbuf = findFile(path, &fullPath)))
	{
		error("file \"%s\" not found", path);
		return NULL;
	}
	/* Now checks that it hasn't been closed already */
	if ((dls = lookupStatus(sbuf)) && (dls->refs > 0))
	{
		/* debug("status found"); */
		return reference(dls, mode);	
	}
	if (isFlagSet(mode, RTLD_NOLOAD))
	{
		error("no existing handle and RTLD_NOLOAD specified");
		return NULL;
	}
	return reference(loadModule(fullPath, sbuf, mode),mode);
}

static void * dlsym(void * handle, const char *symbol)
{
	struct dlstatus * dls = handle;
	void * addr = 0;

	if (!isValidStatus(dls))
		return NULL;
	addr = dlsymIntern(dls, symbol,1);
	return addr;
}

static int dlclose(void * handle)
{
	struct dlstatus * dls = handle;
	if (!isValidStatus(dls))
		return 1;
	if (dls->module == MAGIC_DYLIB_MOD)
	{
		warning("trying to close a .dylib!");
		error("dynamic libraries cannot be closed");
		return 1;
	}
	if (!dls->module)
	{
		error("module already closed");
		return 1;
	}
	dls->refs--;
	if (!dls->refs)
	{
		unsigned long options = 0;
		void (*fini)(void);
		if ((fini = dlsymIntern(dls, "__fini",0)))
		{
			debug("calling _fini()");
			fini();
		}
		if (isFlagSet(dls->mode, RTLD_NODELETE))
			options |= NSUNLINKMODULE_OPTION_KEEP_MEMORY_MAPPED;
		if (!NSUnLinkModule(dls->module, options))
		{
			error("unable to unlink module");
			return 1;
		}
		dls->module = 0;
		/* Note: the dlstatus struct dls is neither removed from the list
		 * nor is the memory it occupies freed. This shouldn't pose a 
		 * problem in mostly all cases, though.
		 */
	}
	return 0;
}

static const char * dlerror(void)
{
	const char * e = err_filled ? err_str : 0;
	err_filled = 0;
	return e;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif
