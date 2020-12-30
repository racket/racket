#lang scribble/doc
@(require "utils.rkt"
          scribble/bnf)

@title[#:tag "cs-embedding"]{Embedding into a Program}

@section-index["embedding Racket CS"]

To embed Racket CS in a program, follow these steps:

@itemize[

 @item{Locate or @seclink["src-build"]{build} the Racket CS library.

  On Unix, the library is @as-index{@filepath{libracketcs.a}}.
  Building from source and installing places the libraries into the
  installation's @filepath{lib} directory.

  On Windows, link to @filepath{libracketcs@italic{x}.dll} (where
  @italic{x} represents the version number). At run time, either
  @filepath{libracketcs@italic{x}.dll} must be moved to a location in
  the standard DLL search path, or your embedding application must
  ``delayload'' link the DLLs and explicitly load them before use.
  (@filepath{Racket.exe} uses the latter strategy.)

  On Mac OS, besides @as-index{@filepath{libracketcs.a}} for static
  linking, a dynamic library is provided by the @filepath{Racket}
  framework, which is typically installed in @filepath{lib}
  sub-directory of the installation. Supply @exec{-framework Racket}
  to @exec{gcc} when linking, along with @exec{-F} and a path to the
  @filepath{lib} directory. At run time, either
  @filepath{Racket.framework} must be moved to a location in the
  standard framework search path, or your embedding executable must
  provide a specific path to the framework (possibly an
  executable-relative path using the Mach-O @tt["@executable_path"]
  prefix). When targeting the Hardened Runtime, you must enable the
  ``Allow Unsigned Executable Memory'' entitlement, otherwise you will
  run into ``out of memory'' errors when calling @cppi{racket_boot}.}

 @item{For each C file that uses Racket library functions,
  @cpp{#include} the files @as-index{@filepath{chezscheme.h}}
  and @as-index{@filepath{racketcs.h}}.

  The @filepath{chezscheme.h} and @filepath{racketcs.h} files are
  distributed with the Racket software in the installation's
  @filepath{include} directory. Building and installing from source
  also places this file in the installation's @filepath{include}
  directory.}

 @item{In your program, call @cppi{racket_boot}. The
  @cppi{racket_boot} function takes a pointer to a
  @cpp{racket_boot_arguments_t} for configuring the Racket instance.
  After zeroing out the @cpp{racket_boot_arguments_t} value
  (typicially with @cpp{memset}), only the following fields are
  required to be set:

  @itemlist[

     @item{@cpp{exec_file} --- a path to be reported by
           @racket[(find-system-path 'exec-file)], usually
           @cpp{argv[0]} for the @cpp{argv} received by your program's
           @cpp{main}.}

     @item{@cpp{boot1_path} --- a path to @filepath{petite.boot}. Use
     a path that includes at least one directory separator.}

     @item{@cpp{boot2_path} --- a path to @filepath{scheme.boot} (with
     a separator).}
     
     @item{@cpp{boot3_path} --- a path to @filepath{racket.boot}
     (which a separator).}

  ]

  The @filepath{petite.boot}, @filepath{scheme.boot}, and
  @filepath{racket.boot} files are distributed with the Racket
  software in the installation's @filepath{lib} directory for Windows,
  and they are distributed within the @filepath{Racket} framework on
  Mac OS X; they must be @seclink["src-build"]{built} from source on Unix.
  These files can be combined into a single file---or even
  embedded into the executable---as long as the @cpp{boot1_offset},
  @cpp{boot2_offset}, and @cpp{boot3_offset} fields of
  @cpp{racket_boot_arguments_t} are set to identify the starting
  offset of each boot image in the file.

  See @secref["segment-ideas"] for advice on embedding files like
  @filepath{petite.boot} in an executable.}

 @item{Configure the main thread's namespace by adding module
  declarations. The initial namespace contains declarations only for a
  few primitive modules, such as @racket['#%kernel], and no bindings
  are imported into the top-level environment.

  To embed a module like @racketmodname[racket/base] (along with all
  its dependencies), use
  @seclink["c-mods" #:doc raco-doc]{@exec{raco ctool --c-mods @nonterm{dest}}},
  which generates a C file @nonterm{dest}
  that contains modules in compiled form as encapsulated in a static
  array. The generated C file defines a @cppi{declare_modules}
  function that takes no arguments and installs the modules into
  the environment, and it adjusts the module name resolver to access the
  embedded declarations. If embedded modules refer to runtime files
  that need to be carried along, supply @DFlag{runtime} to
  @exec{raco ctool --c-mods} to collect the runtime files into a
  directory; see @secref[#:doc raco-doc "c-mods"] for more information.

  Alternatively, set fields like @cpp{collects_dir}, @cpp{config_dir},
  and/or @cpp{argv} in the @cpp{racket_boot_arguments_t} passed to
  @cppi{racket_boot} to locate collections/packages and initialize the
  namespace the same way as when running the @exec{racket} executable.

  On Windows, @exec{raco ctool --c-mods @nonterm{dest} --runtime
  @nonterm{dest-dir}} includes in @nonterm{dest-dir} optional DLLs
  that are referenced by the Racket library to support
  @racket[bytes-open-converter]. Set @cpp{dll_dir} in
  @cpp{racket_boot_arguments_t} to register @nonterm{dest-dir} so that
  those DLLs can be found at run time.

  Instead of using @DFlag{c-mods} with @exec{raco ctool}, you can use
  @DFlag{mods}, embed the file content (see @secref["segment-ideas"])
  and load the content with @cppi{racket_embedded_load_file_region}.}

 @item{Access Racket through @cppi{racket_dynamic_require},
  @cppi{racket_eval}, and/or other functions described in this manual.

  If the embedding program configures built-in parameters in a way
  that should be considered part of the default configuration, then
  call the @racketidfont{seal} function provided by the primitive
  @racketidfont{#%boot} module afterward. The snapshot of parameter
  values taken by @racketidfont{seal} is used for certain
  privileged operations, such as installing a @|PLaneT| package.}

 @item{Compile the program and link it with the Racket libraries.}

]

@index['("allocation")]{Racket} values may be moved or garbage
collected any time that @cpp{racket_...} functions are used to run
Racket code. Do not retain a reference to any Racket value across such
a call.

For example, the following is a simple embedding program that runs a
module @filepath{run.rkt}, assuming that @filepath{run.c} is created
as

@commandline{raco ctool --c-mods run.c "run.rkt"}

to generate @filepath{run.c}, which encapsulates the compiled form of
@filepath{run.rkt} and all of its transitive imports (so that they
need not be found separately a run time). Copies of
@filepath{petite.boot}, @filepath{scheme.boot}, and
@filepath{racket.boot} must be in the current directory on startup.

@filebox["main.c"]{
@verbatim[#:indent 2]{
#include <string.h>
#include "chezscheme.h"
#include "racketcs.h"

#include "run.c"

int main(int argc, char *argv[])
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));

  ba.boot1_path = "./petite.boot";
  ba.boot2_path = "./scheme.boot";
  ba.boot3_path = "./racket.boot";
  
  ba.exec_file = argv[0];

  racket_boot(&ba);

  declare_modules();

  ptr mod = Scons(Sstring_to_symbol("quote"),
                  Scons(Sstring_to_symbol("run"),
                        Snil));

  racket_dynamic_require(mod, Sfalse);

  return 0;
}
}}

As another example, the following is a simple embedding program that
evaluates all expressions provided on the command line and displays
the results, then runs a @racket[read]-@racket[eval]-@racket[print]
loop, all using @racketmodname[racket/base]. Run

@commandline{raco ctool --c-mods base.c ++lib racket/base}

to generate @filepath{base.c}, which encapsulates @racket[racket/base]
and all of its transitive imports.

@filebox["main.c"]{
@verbatim[#:indent 2]{
#include <string.h>
#include "chezscheme.h"
#include "racketcs.h"

#include "base.c"

static ptr to_bytevector(char *s);

int main(int argc, char *argv[])
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));

  ba.boot1_path = "./petite.boot";
  ba.boot2_path = "./scheme.boot";
  ba.boot3_path = "./racket.boot";
  
  ba.exec_file = argv[0];

  racket_boot(&ba);

  declare_modules();

  racket_namespace_require(Sstring_to_symbol("racket/base"));

  {
    int i;
    for (i = 1; i < argc; i++) {
      ptr e = to_bytevector(argv[i]);
      e = Scons(Sstring_to_symbol("open-input-bytes"),
                Scons(e, Snil));
      e = Scons(Sstring_to_symbol("read"), Scons(e, Snil));
      e = Scons(Sstring_to_symbol("eval"), Scons(e, Snil));
      e = Scons(Sstring_to_symbol("println"), Scons(e, Snil));

      racket_eval(e);
    }
  }

  {
    ptr rbase_sym = Sstring_to_symbol("racket/base");
    ptr repl_sym = Sstring_to_symbol("read-eval-print-loop");
  
    racket_apply(Scar(racket_dynamic_require(rbase_sym,
                                             repl_sym)),
                 Snil);
  }

  return 0;
}

static ptr to_bytevector(char *s)
{
  iptr len = strlen(s);
  ptr bv = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bv), s, len);
  return bv;
}
}}

If modules embedded in the executable need to access runtime files
(via @racketmodname[racket/runtime-path] forms), supply the
@DFlag{runtime} flag to @seclink["ctool" #:doc raco-doc]{@exec{raco ctool}}, specifying a directory
where the runtime files are to be gathered. The modules in the
generated @filepath{.c} file will then refer to the files in that
directory.
