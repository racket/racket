#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          "common.rkt"
          (for-label racket/gui
                     compiler/compiler
                     compiler/sig
                     compiler/compiler-unit
                     compiler/option-unit
                     compiler/distribute
                     compiler/bundle-dist
                     compiler/embed
                     compiler/embed-sig
                     compiler/embed-unit
                     racket/runtime-path
                     launcher/launcher
                     compiler/find-exe))

@title{API for Creating Executables}

@defmodule[compiler/embed]{

The @racketmodname[compiler/embed] library provides a function to
embed Racket code into a copy of Racket or GRacket, thus creating a
stand-alone Racket executable. To package the executable into a
distribution that is independent of your Racket installation, use
@racket[assemble-distribution] from
@racketmodname[compiler/distribute].}

Embedding walks the module dependency graph to find all modules needed
by some initial set of top-level modules, compiling them if needed,
and combining them into a ``module bundle.'' In addition to the module
code, the bundle extends the module name resolver, so that modules can
be @racket[require]d with their original names, and they will be
retrieved from the bundle instead of the filesystem.

The @racket[create-embedding-executable] function combines the bundle
with an executable (Racket or GRacket). The
@racket[write-module-bundle] function prints the bundle to the current
output port, instead; this stream can be @racket[load]ed directly by a
running program, as long as the @racket[read-accept-compiled]
parameter is true.

@defproc[(create-embedding-executable [dest path-string?]
                               [#:modules mod-list 
                                         (listof (or/c (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?))
                                                       (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?)
                                                               (listof symbol?))))]
                               [#:configure-via-first-module? config-via-first? 
                                                              any/c 
                                                              #f]
                               [#:literal-files literal-files
                                                (listof path-string?)
                                                null]
                               [#:literal-expression literal-sexp
                                                     any/c
                                                     #f]
                               [#:literal-expressions literal-sexps
                                                      list?
                                                      (if literal-sexp
                                                          (list literal-sexp)
                                                          null)]
                               [#:cmdline cmdline (listof string?)
                                                  null]
                               [#:gracket? gracket? any/c #f]
                               [#:mred? mred? any/c #f]
                               [#:variant variant (or/c 'cgc '3m)
                                                  (system-type 'gc)]
                               [#:aux aux (listof (cons/c symbol? any/c)) null]
                               [#:collects-path collects-path
                                                (or/c #f
                                                      path-string? 
                                                      (listof path-string?))
                                                #f]
                               [#:collects-dest collects-dest
                                                (or/c #f path-string?)
                                                #f]
                               [#:launcher? launcher? any/c #f]
                               [#:verbose? verbose? any/c #f]
                               [#:expand-namespace expand-namespace namespace? (current-namespace)]
                               [#:compiler compile-proc (any/c . -> . compiled-expression?) 
                                           (lambda (e)
                                             (parameterize ([current-namespace 
                                                             expand-namespace])
                                               (compile e)))]
                               [#:src-filter src-filter (path? . -> . any) (lambda (p) #t)]
                               [#:on-extension ext-proc
                                               (or/c #f (path-string? boolean? . -> . any))
                                               #f]
                               [#:get-extra-imports extras-proc 
                                                    (path? compiled-module-expression? 
                                                     . -> . (listof module-path?))
                                                    (lambda (p m) null)])
         void?]{

Copies the Racket (if @racket[gracket?] and @racket[mred?] are
@racket[#f]) or GRacket (otherwise) binary, embedding code into the
copied executable to be loaded on startup.  On Unix, the binary is
actually a wrapper executable that execs the original; see also the
@racket['original-exe?] tag for @racket[aux].

The embedding executable is written to @racket[dest], which is
overwritten if it exists already (as a file or directory).

The embedded code consists of module declarations followed by
additional (arbitrary) code. When a module is embedded, every module
that it imports is also embedded. Library modules are embedded so that
they are accessible via their @racket[lib] paths in the initial
namespace except as specified in @racket[mod-list], other modules
(accessed via local paths and absolute paths) are embedded with a
generated prefix, so that they are not directly accessible.

The @racket[#:modules] argument @racket[mod-list] designates modules
to be embedded, as described below. The @racket[#:literal-files] and
@racket[#:literal-expressions] arguments specify literal code to be
copied into the executable: the content of each file in
@racket[literal-files] is copied in order (with no intervening space),
followed by each element of @racket[literal-sexps]. The
@racket[literal-files] files or @racket[literal-sexps] list can
contain compiled bytecode, and it's possible that the content of the
@racket[literal-files] files only parse when concatenated; the files
and expression are not compiled or inspected in any way during the
embedding process. Beware that the initial namespace contains no
bindings; use compiled expressions to bootstrap the namespace. If
@racket[literal-sexp] is @racket[#f], no literal expression is
included in the executable. The @racket[#:literal-expression]
(singular) argument is for backward compatibility.

If the @racket[#:configure-via-first-module?] argument is specified as
true, then the language of the first module in @racket[mod-list] is
used to configure the run-time environment before the expressions
added by @racket[#:literal-files] and @racket[#:literal-expressions]
are evaluated. See also @secref[#:doc '(lib
"scribblings/reference/reference.scrbl") "configure-runtime"].

The @racket[#:cmdline] argument @racket[cmdline] contains command-line
strings that are prefixed onto any actual command-line arguments that
are provided to the embedding executable. A command-line argument that
evaluates an expression or loads a file will be executed after the
embedded code is loaded.

Each element of the @racket[#:modules] argument @racket[mod-list] is a
two- or three-item list, where the first item is a prefix for the
module name, and the second item is a module path datum (that's in the
format understood by the default module name resolver), and the third
is a list of submodule names to be included if they are available. The
prefix can be a symbol, @racket[#f] to indicate no prefix, or
@racket[#t] to indicate an auto-generated prefix. For example,

@racketblock['((#f "m.rkt"))]

embeds the module @racket[m] from the file @filepath{m.rkt}, without
prefixing the name of the module; the @racket[literal-sexpr] argument
to go with the above might be @racket['(require m)]. When submodules
are available and included, the submodule is given a name by
symbol-appending the @racket[write] form of submodule path to the
enclosing module's name.

Modules are normally compiled before they are embedded into the target
executable; see also @racket[#:compiler] and @racket[#:src-filter]
below. When a module declares run-time paths via
@racket[define-runtime-path], the generated executable records the
path (for use both by immediate execution and for creating a
distribution that contains the executable).

If @racket[collects-dest] is a path instead of @racket[#f], then
instead of embedding collection-based modules into the executable, the
modules (in compiled form, only) are copied into collections in the
@racket[collects-dest] directory.

The optional @racket[#:aux] argument is an association list for
platform-specific options (i.e., it is a list of pairs where the first
element of the pair is a key symbol and the second element is the
value for that key). See also @racket[build-aux-from-path]. The
currently supported keys are as follows:

@itemize[

  @item{@racket['icns] (Mac OS X) : An icon file path (suffix
        @filepath{.icns}) to use for the executable's desktop icon.}

  @item{@racket['ico] (Windows) : An icon file path (suffix
        @filepath{.ico}) to use for the executable's desktop icon;
        the executable will have 16x16, 32x32, and 48x48 icons at
        4-bit, 8-bit, and 32-bit (RGBA) depths; the icons are copied
        and generated from any 16x16, 32x32, and 48x48 icons in the
        @filepath{.ico} file.}

  @item{@racket['creator] (Mac OS X) : Provides a 4-character string
        to use as the application signature.}

  @item{@racket['file-types] (Mac OS X) : Provides a list of
        association lists, one for each type of file handled by the
        application; each association is a two-element list, where the
        first (key) element is a string recognized by Finder, and the
        second element is a plist value (see
        @racketmodname[xml/plist]). See @filepath{drracket.filetypes}
        in the @filepath{drracket} collection for an example.}

  @item{@racket['uti-exports] (Mac OS X) : Provides a list of
        association lists, one for each @as-index{Uniform Type
        Identifier} (UTI) exported by the executable; each association
        is a two-element list, where the first (key) element is a
        string recognized in a UTI declaration, and the second element
        is a plist value (see @racketmodname[xml/plist]). See
        @filepath{drracket.utiexports} in the @filepath{drracket}
        collection for an example.}

  @item{@racket['resource-files] (Mac OS X) : extra files to copy into
        the @filepath{Resources} directory of the generated
        executable.}

  @item{@racket['framework-root] (Mac OS X) : A string to prefix the
        executable's path to the Racket and GRacket frameworks
        (including a separating slash); note that when the prefix
        starts @filepath{@"@"executable_path/} works for a
        Racket-based application, the corresponding prefix start for
        a GRacket-based application is
        @filepath{@"@"executable_path/../../../}; if @racket[#f] is
        supplied, the executable's framework path is left as-is,
        otherwise the original executable's path to a framework is
        converted to an absolute path if it was relative.}

  @item{@racket['dll-dir] (Windows) : A string/path to a directory
        that contains Racket DLLs needed by the executable, such as
        @filepath{racket@nonterm{version}.dll}, or a boolean; a path
        can be relative to the executable; if @racket[#f] is supplied,
        the path is left as-is; if @racket[#t] is supplied, the path
        is dropped (so that the DLLs must be in the system directory
        or the user's @envvar{PATH}); if no value is supplied the
        original executable's path to DLLs is converted to an absolute
        path if it was relative.}

  @item{@racket['subsystem] (Windows) : A symbol, either
        @racket['console] for a console application or
        @racket['windows] for a consoleless application; the default
        is @racket['console] for a Racket-based application and
        @racket['windows] for a GRacket-based application; see also
        @racket['single-instance?], below.}

  @item{@racket['single-instance?] (Windows) : A boolean for
        GRacket-based apps; the default is @racket[#t], which means that
        the app looks for instances of itself on startup and merely
        brings the other instance to the front; @racket[#f] means that
        multiple instances are expected.}

  @item{@racket['forget-exe?] (Windows, Mac OS X) : A boolean;
        @racket[#t] for a launcher (see @racket[launcher?] below) does
        not preserve the original executable name for
        @racket[(find-system-path 'exec-file)]; the main consequence
        is that library collections will be found relative to the
        launcher instead of the original executable.}

  @item{@racket['original-exe?] (Unix) : A boolean; @racket[#t] means
        that the embedding uses the original Racket or GRacket
        executable, instead of a wrapper binary that execs the
        original; the default is @racket[#f].}

  @item{@racket['relative?] (Unix, Windows, Mac OS X) : A boolean;
        @racket[#t] means that, to the degree that the generated
        executable must refer to another, it can use a relative path
        (so the executables can be moved together, but not
        separately); a @racket[#f] value (the default) means that
        absolute paths should be used (so the generated executable can
        be moved).}

  @item{@racket['wm-class] (Unix) : A string; used as the default
        @tt{WM_CLASS} program class for the program's windows.}

]

If the @racket[#:collects-path] argument is @racket[#f], then the
created executable maintains its built-in (relative) path to the main
@filepath{collects} directory---which will be the result of
@racket[(find-system-path 'collects-dir)] when the executable is
run---plus a potential list of other directories for finding library
collections---which are used to initialize the
@racket[current-library-collection-paths] list in combination with
@envvar{PLTCOLLECTS} environment variable.  Otherwise, the argument
specifies a replacement; it must be either a path, string, or
list of paths and strings. In the last case, the first path
or string specifies the main collection directory, and the rest are
additional directories for the collection search path (placed, in
order, after the user-specific @filepath{collects} directory, but
before the main @filepath{collects} directory; then the search list is
combined with @envvar{PLTCOLLECTS}, if it is defined). If the list
is empty, then @racket[(find-system-path 'collects-dir)] will return
the directory of the executable, but @racket[current-library-collection-paths] 
is initialized to an empty list and
@racket[use-collection-link-paths] is set to false to disable the
use of @tech[#:doc reference-doc]{collection links files}.

If the @racket[#:launcher?] argument is @racket[#t], then
@racket[lid-list] should be null, @racket[literal-files] should be
null, @racket[literal-sexp] should be @racket[#f], and the platform
should be Windows or Mac OS X. The embedding executable is created in
such a way that @racket[(find-system-path 'exec-file)] produces the
source Racket or GRacket path instead of the embedding executable (but
the result of @racket[(find-system-path 'run-file)] is still the
embedding executable).

The @racket[#:variant] argument indicates which variant of the
original binary to use for embedding. The default is
@racket[(system-type 'gc)]; see also
@racket[current-launcher-variant].

The @racket[#:compiler] argument is used to compile the source of
modules to be included in the executable (when a compiled form is not
already available). It should accept a single argument that is a
syntax object for a @racket[module] form. The default procedure uses
@racket[compile] parameterized to set the current namespace to
@racket[expand-namespace].

The @racket[#:expand-namespace] argument selects a namespace for
expanding extra modules (and for compiling using the default
@racket[compile-proc]).  Extra-module expansion is needed to detect
run-time path declarations in included modules, so that the path
resolutions can be directed to the current locations (and, ultimately,
redirected to copies in a distribution).

The @racket[#:src-filter] @racket[src-filter] argument takes a path and returns true if
the corresponding file source should be included in the embedding
executable in source form (instead of compiled form), @racket[#f]
otherwise. The default returns @racket[#f] for all paths. Beware that
the current output port may be redirected to the result executable
when the filter procedure is called. Each path given to
@racket[src-filter] corresponds to the actual file name (e.g.,
@filepath{.ss}/@filepath{.rkt} conversions have been applied as needed
to refer to the existing file).

If the @racket[#:on-extension] argument is a procedure, the procedure
is called when the traversal of module dependencies arrives at an
extension (i.e., a DLL or shared object). The default, @racket[#f],
causes a reference to a single-module extension (in its current
location) to be embedded into the executable. The procedure is called
with two arguments: a path for the extension, and a @racket[#f] (for
historical reasons).
  
The @racket[#:get-extra-imports] @racket[extras-proc] argument takes a source pathname and
compiled module for each module to be included in the executable. It
returns a list of quoted module paths (absolute, as opposed to
relative to the module) for extra modules to be included in the
executable in addition to the modules that the source module
@racket[require]s. For example, these modules might correspond to
reader extensions needed to parse a module that will be included as
source, as long as the reader is referenced through an absolute module
path. Each path given to @racket[extras-proc] corresponds to the
actual file name (e.g., @filepath{.ss}/@filepath{.rkt} conversions
have been applied as needed to refer to the existing file).}


@defproc[(make-embedding-executable [dest path-string?]
                               [mred? any/c]
                               [verbose? any/c]
                               [mod-list (listof (or/c (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?))
                                                       (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?)
                                                               (listof symbol?))))]
                               [literal-files (listof path-string?)]
                               [literal-sexp any/c]
                               [cmdline (listof string?)]
                               [aux (listof (cons/c symbol? any/c)) null]
                               [launcher? any/c #f]
                               [variant (one-of/c 'cgc '3m) (system-type 'gc)]
                               [collects-path (or/c #f
                                                    path-string? 
                                                    (listof path-string?))
                                              #f])
         void?]{

Old (keywordless) interface to @racket[create-embedding-executable].}


@defproc[(write-module-bundle [verbose? any/c]
                               [mod-list (listof (or/c (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?))
                                                       (list/c (or/c symbol? (one-of/c #t #f)) 
                                                               (or/c module-path? path?)
                                                               (listof symbol?))))]
                              [literal-files (listof path-string?)]
                              [literal-sexp any/c])
         void?]{

Like @racket[make-embedding-executable], but the module bundle is
written to the current output port instead of being embedded into an
executable.  The output of this function can be @racket[read] to load
and instantiate @racket[mod-list] and its dependencies, adjust the
module name resolver to find the newly loaded modules, evaluate the
forms included from @racket[literal-files], and finally evaluate
@racket[literal-sexpr]. The @racket[read-accept-compiled] parameter
must be true to read the stream.}


@defproc[(embedding-executable-is-directory? [mred? any/c]) boolean]{

Indicates whether Racket/GRacket executables for the current platform
correspond to directories from the user's perspective. The result is
currently @racket[#f] for all platforms.}


@defproc[(embedding-executable-is-actually-directory? [mred? any/c])
         boolean?]{

Indicates whether Racket/GRacket executables for the current platform
actually correspond to directories. The result is @racket[#t] on
Mac OS X when @racket[mred?] is @racket[#t], @racket[#f] otherwise.}


@defproc[(embedding-executable-put-file-extension+style+filters [mred? any/c])
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Returns three values suitable for use as the @racket[extension],
@racket[style], and @racket[filters] arguments to @racket[put-file],
respectively.

If Racket/GRacket launchers for the current platform were directories
form the user's perspective, the @racket[style] result is suitable for
use with @racket[get-directory], and the @racket[extension] result may
be a string indicating a required extension for the directory name. }


@defproc[(embedding-executable-add-suffix [path path-string?] [mred? any/c])
         path-string?]{

Adds a suitable executable suffix, if it's not present already.}


@; ----------------------------------------

@section{Executable Creation Signature}

@defmodule[compiler/embed-sig]

@defsignature/splice[compiler:embed^ ()]{

Includes the identifiers provided by @racketmodname[compiler/embed].}

@; ----------------------------------------

@section{Executable Creation Unit}

@defmodule[compiler/embed-unit]

@defthing[compiler:embed@ unit?]{

A unit that imports nothing and exports @racket[compiler:embed^].}

@section{Finding the Racket Executable}

@defmodule[compiler/find-exe]

@defproc[(find-exe [gracket? any/c #f]
                   [variant (or/c 'cgc '3m) (system-type 'gc)])
         path?]{

  Finds the path to the @exec{racket} or @exec{gracket} (when
  @racket[gracket?] is true) executable.}
