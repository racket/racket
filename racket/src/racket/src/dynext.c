/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
  Copyright (c) 1995-2002 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  Thanks to Patrick Barta for the WINDOWS_DYNAMIC_LOAD code.
  Thanks to William Ng for the CODEFRAGMENT_DYNAMIC_LOAD code.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include "schvers.h"
#include "schgc.h"

#ifdef UNIX_DYNAMIC_LOAD
# ifdef OS_X_NO_DLFCN
#  include "dlcompat.inc"
# else
#  include <dlfcn.h>
# endif
#endif
#if defined(WINDOWS_DYNAMIC_LOAD)
# include <windows.h>
#endif
#if defined(CODEFRAGMENT_DYNAMIC_LOAD)
# include <CodeFragments.h>
static Boolean get_ext_file_spec(FSSpec *spec, const char *filename );
static Boolean load_ext_file_spec(FSSpec *spec, CFragConnectionID *connID);
#endif

#if defined(RTLD_NOW)
# define DLOPEN_MODE (RTLD_NOW)
#elif defined(RTLD_LAZY)
# define DLOPEN_MODE (RTLD_LAZY)
#else
# define DLOPEN_MODE (1)
#endif

#ifdef SHL_DYNAMIC_LOAD
#include <dl.h>
#include <errno.h>
#define dlopen(file, flag) ((void *)shl_load(file, BIND_IMMEDIATE, 0L))
#define dlclose(dl) (shl_unload((shl_t)dl))
void *dlsym(void *_handle, const char *name)
{
  void *result;
  shl_t handle = (shl_t)_handle;

  if (!shl_findsym(&handle, name, TYPE_PROCEDURE, (void *)&result))
    return result;
  else
    return NULL;
}
static char *dlerror(void) {
  static char errbuf[20];
  sprintf(errbuf, "%d", errno);
  return errbuf;
}
#define UNIX_DYNAMIC_LOAD
#endif

#ifdef LINK_EXTENSIONS_BY_TABLE
# undef SCHEME_NO_GC_PROTO
# include "schemex.h"
#endif

static Scheme_Object *load_extension(int argc, Scheme_Object **argv);
static Scheme_Object *current_load_extension(int argc, Scheme_Object *argv[]);

#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Extension_Table *scheme_extension_table;

#define SSI_ARG_TYPES Scheme_Extension_Table *
#define SSI_ARGS scheme_extension_table
#else
#define SSI_ARG_TYPES
#define SSI_ARGS
#endif

#ifndef UNIX_DYNAMIC_LOAD
# ifndef WINDOWS_DYNAMIC_LOAD
#  ifndef CODEFRAGMENT_DYNAMIC_LOAD
#   define NO_DYNAMIC_LOAD
#  endif
# endif
#endif

#ifndef NO_DYNAMIC_LOAD
THREAD_LOCAL_DECL(static Scheme_Hash_Table *loaded_extensions;) /* hash on scheme_initialize pointer */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *fullpath_loaded_extensions;) /* hash on full path name */
#endif

#ifdef MZ_PRECISE_GC 
# define VERSION_AND_VARIANT MZSCHEME_VERSION "@3m"
#else
# define VERSION_AND_VARIANT MZSCHEME_VERSION
#endif

/* For precise GC, make a proc ptr look like a fixnum: */
#define mzPROC_TO_HASH_OBJ(f) ((Scheme_Object *)(((intptr_t)f) | 0x1))

#define BAD_VERSION_STR "found version does not match the expected version"

void scheme_init_dynamic_extension(Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifdef LINK_EXTENSIONS_BY_TABLE
    REGISTER_SO(scheme_extension_table);
    
    scheme_extension_table = 
      (Scheme_Extension_Table *)scheme_malloc_atomic(sizeof(Scheme_Extension_Table));
#include "schemex.inc"
#endif
  }

  GLOBAL_PRIM_W_ARITY2("load-extension", load_extension, 1, 1, 0, -1, env);
  GLOBAL_PARAMETER("current-load-extension", current_load_extension, MZCONFIG_LOAD_EXTENSION_HANDLER, env);
}

static Scheme_Object *
current_load_extension(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load-extension", 
			     scheme_make_integer(MZCONFIG_LOAD_EXTENSION_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

#ifndef NO_DYNAMIC_LOAD

typedef Scheme_Object *(*Init_Procedure)(Scheme_Env *);
typedef Scheme_Object *(*Reload_Procedure)(Scheme_Env *);
typedef Scheme_Object *(*Modname_Procedure)(void);

typedef struct {
  void *handle;
  Init_Procedure init_f;
  Reload_Procedure reload_f;
  Modname_Procedure modname_f;
} ExtensionData;

#endif

static char *copy_vers(char *vers)
{
  if (vers) {
    intptr_t len = strlen(vers);
    char *vcopy;
    vcopy = (char *)scheme_malloc_atomic(len + 1);
    memcpy(vcopy, vers, len + 1);
    return vcopy;
  } else
    return NULL;
}

typedef char *(*Setup_Procedure)(SSI_ARG_TYPES);

static Scheme_Object *do_load_extension(const char *filename,
					Scheme_Object *expected_module, Scheme_Env *env)
{
#ifndef NO_DYNAMIC_LOAD
  Init_Procedure init_f; /* set by platform-specific code */
  Reload_Procedure reload_f; /* set by platform-specific code */
  Modname_Procedure modname_f; /* set by platform-specific code */
  ExtensionData *ed;
  void *handle;
  int comppath;

#ifndef NO_DYNAMIC_LOAD
  if (!loaded_extensions) {
    REGISTER_SO(loaded_extensions);
    REGISTER_SO(fullpath_loaded_extensions);
    loaded_extensions = scheme_make_hash_table(SCHEME_hash_ptr);
    fullpath_loaded_extensions = scheme_make_hash_table(SCHEME_hash_string);
  }
#endif

  comppath = scheme_is_complete_path(filename, strlen(filename), SCHEME_PLATFORM_PATH_KIND);

  reload_f = NULL;
  modname_f = NULL;
  handle = NULL;

  if (comppath)
    init_f = (Init_Procedure)scheme_hash_get(fullpath_loaded_extensions, (Scheme_Object *)filename);
  else
    init_f = NULL;

  if (!init_f) {
#endif

#ifdef UNIX_DYNAMIC_LOAD
    void *dl;
    Setup_Procedure f;
    char *vers;
    
    /* Make sure that filename is not a pathless filename.
       Some Unix systems don't search as a relative path
       otherwise. */
    if (filename[0] != '/') {
      int l = strlen(filename);
      char *s;

      s = (char *)scheme_malloc_atomic(l + 3);

      s[0] = '.';
      s[1] = '/';
      memcpy(s + 2, filename, l + 1);
      filename = s;
    }
    
    dl = dlopen((char *)filename, DLOPEN_MODE);
    if (!dl)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: couldn't open file\n"
                       "  path: %q\n"
                       "  system error: %s",
		       filename, dlerror());

    handle = dl;
    
#ifdef UNDERSCORE_DYNLOAD_SYMBOL_PREFIX
# define SO_SYMBOL_PREFIX "_"
#else
# define SO_SYMBOL_PREFIX
#endif

    f = (Setup_Procedure)dlsym(dl, SO_SYMBOL_PREFIX "scheme_initialize_internal");

    if (!f) {
      const char *err;
      err = dlerror();
      dlclose(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: file is not an extension\n"
                       "  path %q\n"
                       "  system error: %s", 
		       filename, err);
    }

    vers = f(SSI_ARGS);
    if (!vers || strcmp(vers, VERSION_AND_VARIANT)) {
      /* Copy, because we're going to unload the extension: */
      vers = copy_vers(vers);
      dlclose(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_VERSION,
		       "load-extension: " BAD_VERSION_STR "\n"
                       "  found: %s\n"
                       "  expected: %s\n"
                       "  path: %s",
		       vers, VERSION_AND_VARIANT, filename);
    }

    init_f = (Init_Procedure)dlsym(dl, SO_SYMBOL_PREFIX "scheme_initialize");
    if (init_f) {
      reload_f = (Reload_Procedure)dlsym(dl, SO_SYMBOL_PREFIX "scheme_reload");
      if (reload_f)
	modname_f = (Modname_Procedure)dlsym(dl, SO_SYMBOL_PREFIX "scheme_module_name");
    }

    if (!init_f || !reload_f || !modname_f) {
      const char *err;
      err = dlerror();
      dlclose(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: no %s\n"
                       "  path: %q\n"
                       "  system error: %s",
		       (init_f 
			? (reload_f
			   ? "scheme_module_name"
			   : "scheme_reload")
			: "scheme_initialize"),
		       filename, err);
    }
#endif
#if defined(WINDOWS_DYNAMIC_LOAD)
    HINSTANCE dl;
    Setup_Procedure f;
    char *vers;
  
    dl = LoadLibraryW(WIDE_PATH(filename));
    if (!dl) {
      long err;
      err = GetLastError();
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: could not load file\n"
                       "  path: %q\n"
                       "  system error: %E",
		       filename, err);
    }
    
    handle = (void *)dl;
    
    f = (Setup_Procedure)GetProcAddress(dl, "scheme_initialize_internal");
    
    if (!f) {
      long err;
      err = GetLastError();
      FreeLibrary(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: file is not an extension\n"
                       "  path: %q\n"
                       "  system error: %E",
		       filename, err);
    }
    
    vers = f(SSI_ARGS);
    if (!vers || strcmp(vers, VERSION_AND_VARIANT)) {
      /* Copy, because we're going to unload the extension: */
      vers = copy_vers(vers);
      FreeLibrary(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_VERSION,
		       "load-extension: " BAD_VERSION_STR "\n"
                       "  found: %s\n"
                       "  expected: %s\n"
                       "  path: %s",
		       vers, VERSION_AND_VARIANT, filename);
    }
    
    init_f = (Init_Procedure)GetProcAddress(dl,"scheme_initialize");
    if (init_f) {
      reload_f = (Reload_Procedure)GetProcAddress(dl,"scheme_reload");
      if (reload_f)
	modname_f = (Modname_Procedure)GetProcAddress(dl,"scheme_module_name");
    }
    
    if (!init_f || !reload_f || !modname_f) {
      FreeLibrary(dl);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: no %s\n"
                       "  path: %q", 
		       (init_f 
			? (reload_f
			   ? "scheme_module_name"
			   : "scheme_reload")
			: "scheme_initialize"),
		       filename);
    }
#endif
#if defined(CODEFRAGMENT_DYNAMIC_LOAD)
    FSSpec spec;
    Setup_Procedure f;
    char *vers;
    CFragConnectionID connID;

    if (get_ext_file_spec( &spec, filename ) && load_ext_file_spec( &spec, &connID ) )
      {
	OSErr err;
	handle = (void *)connID;
	
	err = FindSymbol( connID, "\pscheme_initialize_internal", ( Ptr * )&f, 0 );
	if ( err != noErr )
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "load-extension: file is not an extension\n"
                           "  path: %q",
			   filename);
	
	vers = f(SSI_ARGS);
	
	if (!vers || strcmp(vers, VERSION_AND_VARIANT))
          scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_VERSION,
                           "load-extension: " BAD_VERSION "\n"
                           "  found: %s\n"
                           "  expected: %s\n"
                           "  path: %s",
                           vers, VERSION_AND_VARIANT, filename);
	
	err = FindSymbol( connID, "\pscheme_initialize", ( Ptr * )&init_f, 0 );
	if ( err != noErr )
	  init_f = NULL;
	else {
	  err = FindSymbol( connID, "\pscheme_reload", ( Ptr * )&reload_f, 0 );
	  if ( err != noErr )
	    reload_f = NULL;
	  else {
	    err = FindSymbol( connID, "\pscheme_module_name", ( Ptr * )&modname_f, 0 );
	    if ( err != noErr )
	      modname_f = NULL;
	  }
	}

	if ( err != noErr )
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "load-extension: no %s\n"
                           "  path: %q", 
			   (init_f 
			    ? (reload_f
			       ? "scheme_module_name"
			       : "scheme_reload")
			    : "scheme_initialize"),
			   filename);
	

      }
    else
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: could not load extension"
                       "  path: %q",
		       filename);
#endif
#ifdef NO_DYNAMIC_LOAD
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		     "load-extension: " NOT_SUPPORTED_STR);
    return NULL;
#else

    if (comppath)
      scheme_hash_set(fullpath_loaded_extensions, (Scheme_Object *)filename, mzPROC_TO_HASH_OBJ(init_f));
  }
#endif

#ifndef NO_DYNAMIC_LOAD
  ed = (ExtensionData *)scheme_hash_get(loaded_extensions, mzPROC_TO_HASH_OBJ(init_f));

  if (ed) {
    init_f = ed->reload_f;
    modname_f = ed->modname_f;
  } else {
    ed = MALLOC_ONE_ATOMIC(ExtensionData);
    ed->handle = handle;
    ed->init_f = init_f;
    ed->reload_f = reload_f;
    ed->modname_f = modname_f;
    scheme_hash_set(loaded_extensions, mzPROC_TO_HASH_OBJ(init_f), (Scheme_Object *)ed);
  }

  if (SCHEME_SYMBOLP(expected_module)) {
    Scheme_Object *n;
    n = modname_f();
    if (!SAME_OBJ(expected_module, n)) {
      Scheme_Object *other;

      if (n && SCHEME_SYMBOLP(n)) {
	char *s, *t;
	intptr_t len, slen;
	
	t = "module `";
	len = strlen(t);
	slen = SCHEME_SYM_LEN(n);
	
	s = (char *)scheme_malloc_atomic(len + slen + 2);

	memcpy(s, t, len);
	memcpy(s + len, SCHEME_SYM_VAL(n), slen);
	s[len + slen] = '\'';
	s[len + slen + 1]= 0;
	
	other = scheme_make_sized_byte_string(s, len + slen + 1, 0);
      } else
	other = scheme_make_byte_string("non-module");

      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "load-extension: expected module not found\n"
                       "  expected: %S\n"
                       "  found: %T\n"
                       "  path: %q", 
		       expected_module,
		       other,
		       filename);

      return NULL;
    }
  }

  return init_f(env);
#endif
}

void scheme_register_extension_global(void *ptr, intptr_t size)
  XFORM_SKIP_PROC
{
  GC_add_roots((char *)ptr, (char *)(((char *)ptr) + size + 1));
}

static Scheme_Object *load_extension(int argc, Scheme_Object **argv)
{
  return scheme_load_with_clrd(argc, argv, "load-extension", MZCONFIG_LOAD_EXTENSION_HANDLER);
}

Scheme_Object *scheme_default_load_extension(int argc, Scheme_Object **argv)
{
  char *filename;
  Scheme_Object *expected_module;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("default-load-extension-handler", "path-string?", 0, argc, argv);
  expected_module = argv[1];
  if (!SCHEME_FALSEP(expected_module) && !SCHEME_SYMBOLP(expected_module))
    scheme_wrong_contract("default-load-extension-handler", "(or/c symbol? #f)", 1, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   "default-load-extension-handler",
					   NULL,
					   SCHEME_GUARD_FILE_EXECUTE);

  return scheme_force_value(do_load_extension(filename, expected_module, scheme_get_env(NULL)));
}

Scheme_Object *scheme_load_extension(const char *filename, Scheme_Env *env)
{
  Scheme_Object *a[1];

  a[0] = scheme_make_byte_string(filename);
  return load_extension(1, a);
}

void scheme_free_dynamic_extensions()
{
  if (loaded_extensions) {
    int i;
    ExtensionData *ed;
    for (i = 0; i < loaded_extensions->size; i++) {
      if (loaded_extensions->vals[i]) {
        ed = (ExtensionData *)loaded_extensions->vals[i];
#       ifdef UNIX_DYNAMIC_LOAD
        dlclose(ed->handle);
#       endif
#       ifdef WINDOWS_DYNAMIC_LOAD
        FreeLibrary(ed->handle);
#       endif
      }
    }
  }
}

#if defined(CODEFRAGMENT_DYNAMIC_LOAD)

static Boolean get_ext_file_spec(FSSpec *spec, const char *filename)
{
#ifndef EXTENSIONS_WITHOUT_PATH
	return scheme_mac_path_to_spec(filename, spec);
#else
	/* William Ng's code for always finding an extension in a particular place. */
	/* This is a very Mac-like idea, but not Racket-like. */
    ProcessSerialNumber currentPSN;
    ProcessInfoRec info;
	Boolean ret = false;
    currentPSN.highLongOfPSN = 0;
    currentPSN.lowLongOfPSN = kCurrentProcess;
    info.processInfoLength = sizeof(ProcessInfoRec);
    info.processName = NULL;
    info.processAppSpec = spec;
    
	if ( GetProcessInformation(&currentPSN, &info)==noErr )
	{
#ifdef EXTENSION_IN_SEPARATE_FOLDER
		/* call PBGetCatInfoSync to get the folder par id */
		#define EXTENSION_FOLDER_NAME "\pextensions"
		HFileInfo file_info = {0};
		CInfoPBPtr	myCPBPtr;           /* for the PBGetCatInfo call */
		myCPBPtr = (CInfoPBRec*)&file_info;
	
		myCPBPtr->hFileInfo.ioNamePtr 	= EXTENSION_FOLDER_NAME;
		myCPBPtr->hFileInfo.ioVRefNum 	= spec->vRefNum;
		myCPBPtr->hFileInfo.ioFDirIndex	= 0;
		myCPBPtr->hFileInfo.ioDirID		= spec->parID;
		
		if (PBGetCatInfoSync(myCPBPtr) == noErr) 
		{
			if ((myCPBPtr->hFileInfo.ioFlAttrib & ioDirMask) != 0) 
			{   /* we have a directory */
				spec->parID   = myCPBPtr->hFileInfo.ioDirID;
				c2pstrcpy(spec->name,filename);
				ret = true;
			}
		}
#else
		/* copy the extension filename to the FSSpec */
		c2pstrcpy(spec->name,filename);
		ret = true;

#endif
	}
			
	return ret;
#endif
}

static Boolean load_ext_file_spec(FSSpec *spec, CFragConnectionID *connID)
{
	OSErr err = GetDiskFragment(spec, 0, 0, 0, kPrivateCFragCopy, connID, 0, NULL);
	return err==noErr;
}

#endif

