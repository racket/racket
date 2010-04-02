#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label scheme/gui
                     compiler/compiler
                     compiler/sig
                     compiler/compiler-unit
                     compiler/option-unit
                     compiler/comp-unit
                     compiler/distribute
                     compiler/bundle-dist
                     compiler/embed
                     compiler/embed-sig
                     compiler/embed-unit
                     scheme/runtime-path
                     launcher/launcher))

@title{Scheme API for Creating Executables}

@defmodule[compiler/embed]{

The @schememodname[compiler/embed] library provides a function to
embed Scheme code into a copy of MzScheme or MrEd, thus creating a
stand-alone Scheme executable. To package the executable into a
distribution that is indpendent of your PLT installation, use
@scheme[assemble-distribution] from
@schememodname[compiler/distribute].}

Embedding walks the module dependency graph to find all modules needed
by some initial set of top-level modules, compiling them if needed,
and combining them into a ``module bundle.'' In addition to the module
code, the bundle extends the module name resolver, so that modules can
be @scheme[require]d with their original names, and they will be
retrieved from the bundle instead of the filesystem.

The @scheme[create-embedding-executable] function combines the bundle
with an executable (MzScheme or MrEd). The
@scheme[write-module-bundle] function prints the bundle to the current
output port, instead; this stream can be @scheme[load]ed directly by a
running program, as long as the @scheme[read-accept-compiled]
parameter is true.

@defproc[(create-embedding-executable [dest path-string?]
                               [#:modules mod-list 
                                          (listof (list/c (or/c symbol? (one-of/c #t #f)) 
                                                          module-path?))
                                          null]
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
                               [#:mred? mred? any/c #f]
                               [#:variant variant (one-of/c 'cgc '3m)
                                                  (system-type 'gc)]
                               [#:aux aux (listof (cons/c symbol? any/c)) null]
                               [#:collects-path collects-path
                                                (or/c false/c 
                                                      path-string? 
                                                      (listof path-string?))
                                                #f]
                               [#:launcher? launcher? any/c #f]
                               [#:verbose? verbose? any/c #f]
                               [#:compiler compile-proc (any/c . -> . compiled-expression?) 
                                           (lambda (e)
                                             (parameterize ([current-namespace 
                                                             expand-namespace])
                                               (compile e)))]
                               [#:expand-namespace expand-namespace namespace? (current-namespace)]
                               [#:src-filter src-filter (path? . -> . any) (lambda (p) #t)]
                               [#:on-extension ext-proc
                                               (or/c false/c (path-string? boolean? . -> . any))
                                               #f]
                               [#:get-extra-imports extras-proc 
                                                    (path? compiled-module? 
                                                     . -> . (listof module-path?))
                                                    (lambda (p m) null)])
         void?]{

Copies the MzScheme (if @scheme[mred?] is @scheme[#f]) or MrEd
(otherwise) binary, embedding code into the copied executable to be
loaded on startup.  Under Unix, the binary is actually a wrapper
executable that execs the original; see also the
@scheme['original-exe?] tag for @scheme[aux].

The embedding executable is written to @scheme[dest], which is
overwritten if it exists already (as a file or directory).

The embedded code consists of module declarations followed by
additional (arbitrary) code. When a module is embedded, every module
that it imports is also embedded. Library modules are embedded so that
they are accessible via their @scheme[lib] paths in the initial
namespace except as specified in @scheme[mod-list], other modules
(accessed via local paths and absolute paths) are embedded with a
generated prefix, so that they are not directly accessible.

The @scheme[#:modules] argument @scheme[mod-list] designates modules
to be embedded, as described below. The @scheme[#:literal-files] and
@scheme[#:literal-expressions] arguments specify literal code to be
copied into the executable: the content of each file in
@scheme[literal-files] is copied in order (with no intervening space),
followed by each element of @scheme[literal-sexps]. The
@scheme[literal-files] files or @scheme[literal-sexps] list can
contain compiled bytecode, and it's possible that the content of the
@scheme[literal-files] files only parse when concatenated; the files
and expression are not compiled or inspected in any way during the
embedding process. Beware that the initial namespace contains no
bindings; use compiled expressions to bootstrap the namespace. If
@scheme[literal-sexp] is @scheme[#f], no literal expression is
included in the executable. The @scheme[#:literal-expression]
(singular) argument is for backward compatibility.

If the @scheme[#:configure-via-first-module?] argument is specified as
true, then the language of the first module in @scheme[mod-list] is
used to configure the run-time environment before the expressions
added by @scheme[#:literal-files] and @scheme[#:literal-expressions]
are evaluated.

The @scheme[#:cmdline] argument @scheme[cmdline] contains command-line
strings that are prefixed onto any actual command-line arguments that
are provided to the embedding executable. A command-line argument that
evaluates an expression or loads a file will be executed after the
embedded code is loaded.

Each element of the @scheme[#:modules] argument @scheme[mod-list] is a
two-item list, where the first item is a prefix for the module name,
and the second item is a module path datum (that's in the format
understood by the default module name resolver). The prefix can be a
symbol, @scheme[#f] to indicate no prefix, or @scheme[#t] to indicate
an auto-generated prefix. For example,

@schemeblock['((#f "m.ss"))]

embeds the module @scheme[m] from the file @filepath{m.ss}, without
prefixing the name of the module; the @scheme[literal-sexpr] argument
to go with the above might be @scheme['(require m)].

Modules are normally compiled before they are embedded into the target
executable; see also @scheme[#:compiler] and @scheme[#:src-filter]
below. When a module declares run-time paths via
@scheme[define-runtime-path], the generated executable records the
path (for use both by immediate execution and for creating a
distribution that contains the executable).

The optional @scheme[#:aux] argument is an association list for
platform-specific options (i.e., it is a list of pairs where the first
element of the pair is a key symbol and the second element is the
value for that key). See also @scheme[build-aux-from-path]. The
currently supported keys are as follows:

@itemize[

  @item{@scheme['icns] (Mac OS X) : An icon file path (suffix
        @filepath{.icns}) to use for the executable's desktop icon.}

  @item{@scheme['ico] (Windows) : An icon file path (suffix
        @filepath{.ico}) to use for the executable's desktop icon;
        the executable will have 16x16, 32x32, and 48x48 icons at
        4-bit, 8-bit, and 32-bit (RBBA) depths; the icons are copied
        and generated from any 16x16, 32x32, and 48x48 icons in the
        @filepath{.ico} file.}

  @item{@scheme['creator] (Mac OS X) : Provides a 4-character string
        to use as the application signature.}

  @item{@scheme['file-types] (Mac OS X) : Provides a list of
        association lists, one for each type of file handled by the
        application; each association is a two-element list, where the
        first (key) element is a string recognized by Finder, and the
        second element is a plist value (see
        @schememodname[xml/plist]). See @filepath{drscheme.filetypes}
        in the @filepath{drscheme} collection for an example.}

  @item{@scheme['uti-exports] (Mac OS X) : Provides a list of
        association lists, one for each @as-index{Uniform Type
        Identifier} (UTI) exported by the executable; each association
        is a two-element list, where the first (key) element is a
        string recognized in a UTI declaration, and the second element
        is a plist value (see @schememodname[xml/plist]). See
        @filepath{drscheme.utiexports} in the @filepath{drscheme}
        collection for an example.}

  @item{@scheme['resource-files] (Mac OS X) : extra files to copy into
        the @filepath{Resources} directory of the generated
        executable.}

  @item{@scheme['framework-root] (Mac OS X) : A string to prefix the
        executable's path to the MzScheme and MrEd frameworks
        (including a separating slash); note that when the prefix
        starts @filepath{@"@"executable_path/} works for a
        MzScheme-based application, the corresponding prefix start for
        a MrEd-based application is
        @filepath{@"@"executable_path/../../../}; if @scheme[#f] is
        supplied, the executable's framework path is left as-is,
        otherwise the original executable's path to a framework is
        converted to an absolute path if it was relative.}

  @item{@scheme['dll-dir] (Windows) : A string/path to a directory
        that contains PLT DLLs needed by the executable, such as
        @filepath{pltmzsch@nonterm{version}.dll}, or a boolean; a path
        can be relative to the executable; if @scheme[#f] is supplied,
        the path is left as-is; if @scheme[#t] is supplied, the path
        is dropped (so that the DLLs must be in the system directory
        or the user's @envvar{PATH}); if no value is supplied the
        original executable's path to DLLs is converted to an absolute
        path if it was relative.}

  @item{@scheme['subsystem] (Windows) : A symbol, either
        @scheme['console] for a console application or
        @scheme['windows] for a consoleless application; the default
        is @scheme['console] for a MzScheme-based application and
        @scheme['windows] for a MrEd-based application; see also
        @scheme['single-instance?], below.}

  @item{@scheme['single-instance?] (Windows) : A boolean for
        MrEd-based apps; the default is @scheme[#t], which means that
        the app looks for instances of itself on startup and merely
        brings the other instance to the front; @scheme[#f] means that
        multiple instances are expected.}

  @item{@scheme['forget-exe?] (Windows, Mac OS X) : A boolean;
        @scheme[#t] for a launcher (see @scheme[launcher?] below) does
        not preserve the original executable name for
        @scheme[(find-system-path 'exec-file)]; the main consequence
        is that library collections will be found relative to the
        launcher instead of the original executable.}

  @item{@scheme['original-exe?] (Unix) : A boolean; @scheme[#t] means
        that the embedding uses the original MzScheme or MrEd
        executable, instead of a wrapper binary that execs the
        original; the default is @scheme[#f].}

]

If the @scheme[#:collects-path] argument is @scheme[#f], then the
created executable maintains its built-in (relative) path to the main
@filepath{collects} directory---which will be the result of
@scheme[(find-system-path 'collects-dir)] when the executable is
run---plus a potential list of other directories for finding library
collections---which are used to initialize the
@scheme[current-library-collection-paths] list in combination with
@envvar{PLTCOLLECTS} environment variable.  Otherwise, the argument
specifies a replacement; it must be either a path, string, or
non-empty list of paths and strings. In the last case, the first path
or string specifies the main collection directory, and the rest are
additional directories for the collection search path (placed, in
order, after the user-specific @filepath{collects} directory, but
before the main @filepath{collects} directory; then the search list is
combined with @envvar{PLTCOLLECTS}, if it is defined).

If the @scheme[#:launcher?] argument is @scheme[#t], then no
@scheme[module]s should be null, @scheme[literal-files] should be
null, @scheme[literal-sexp] should be @scheme[#f], and the platform
should be Windows or Mac OS X. The embedding executable is created in
such a way that @scheme[(find-system-path 'exec-file)] produces the
source MzScheme or MrEd path instead of the embedding executable (but
the result of @scheme[(find-system-path 'run-file)] is still the
embedding executable).

The @scheme[#:variant] argument indicates which variant of the
original binary to use for embedding. The default is
@scheme[(system-type 'gc)]; see also
@scheme[current-launcher-variant].

The @scheme[#:compiler] argument is used to compile the source of
modules to be included in the executable (when a compiled form is not
already available). It should accept a single argument that is a
syntax object for a @scheme[module] form. The default procedure uses
@scheme[compile] parameterized to set the current namespace to
@scheme[expand-namespace].

The @scheme[#:expand-namespace] argument selects a namespace for
expanding extra modules (and for compiling using the default
@scheme[compile-proc]).  Extra-module expansion is needed to detect
run-time path declarations in included modules, so that the path
resolutions can be directed to the current locations (and, ultimately,
redirected to copies in a distribution).

The @scheme[#:src-filter] argument takes a path and returns true if
the corresponding file source should be included in the embedding
executable in source form (instead of compiled form), @scheme[#f]
otherwise. The default returns @scheme[#f] for all paths. Beware that
the current output port may be redirected to the result executable
when the filter procedure is called.

If the @scheme[#:on-extension] argument is a procedure, the procedure
is called when the traversal of module dependencies arrives at an
extension (i.e., a DLL or shared object). The default, @scheme[#f],
causes a reference to a single-module extension (in its current
location) to be embedded into the executable. The procedure is called
with two arguments: a path for the extension, and a @scheme[#f] (for
historical reasons).
  
The @scheme[#:get-extra-imports] argument takes a source pathname and
compiled module for each module to be included in the executable. It
returns a list of quoted module paths (absolute, as opposed to
relative to the module) for extra modules to be included in the
executable in addition to the modules that the source module
@scheme[require]s. For example, these modules might correspond to
reader extensions needed to parse a module that will be included as
source, as long as the reader is referenced through an absolute module
path.}


@defproc[(make-embedding-executable [dest path-string?]
                               [mred? any/c]
                               [verbose? any/c]
                               [mod-list (listof (list/c (or/c symbol? (one-of/c #t #f)) 
                                                         module-path?))]
                               [literal-files (listof path-string?)]
                               [literal-sexp any/c]
                               [cmdline (listof string?)]
                               [aux (listof (cons/c symbol? any/c)) null]
                               [launcher? any/c #f]
                               [variant (one-of/c 'cgc '3m) (system-type 'gc)])
         void?]{

Old (keywordless) interface to @scheme[create-embedding-executable].}


@defproc[(write-module-bundle [verbose? any/c]
                              [mod-list (listof (list/c (or/c symbol? (one-of/c #t #f)) 
                                                        module-path?))]
                              [literal-files (listof path-string?)]
                              [literal-sexp any/c])
         void?]{

Like @scheme[make-embedding-executable], but the module bundle is
written to the current output port instead of being embedded into an
executable.  The output of this function can be @scheme[read] to load
and instantiate @scheme[mod-list] and its dependencies, adjust the
module name resolver to find the newly loaded modules, evaluate the
forms included from @scheme[literal-files], and finally evaluate
@scheme[literal-sexpr]. The @scheme[read-accept-compiled] parameter
must be true to read the stream.}


@defproc[(embedding-executable-is-directory? [mred? any/c]) boolean]{

Indicates whether MzScheme/MrEd executables for the current platform
correspond to directories from the user's perspective. The result is
currently @scheme[#f] for all platforms.}


@defproc[(embedding-executable-is-actually-directory? [mred? any/c])
         boolean?]{

Indicates whether MzScheme/MrEd executables for the current platform
actually correspond to directories. The result is @scheme[#t] under
Mac OS X when @scheme[mred?] is @scheme[#t], @scheme[#f] otherwise.}


@defproc[(embedding-executable-put-file-extension+style+filters [mred? any/c])
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Returns three values suitable for use as the @scheme[extension],
@scheme[style], and @scheme[filters] arguments to @scheme[put-file],
respectively.

If MzScheme/MrEd launchers for the current platform were directories
form the user's perspective, the @scheme[style] result is suitable for
use with @scheme[get-directory], and the @scheme[extension] result may
be a string indicating a required extension for the directory name. }


@defproc[(embedding-executable-add-suffix [path path-string?] [mred? any/c])
         path-string?]{

Adds a suitable executable suffix, if it's not present already.}


@; ----------------------------------------

@section{Executable Creation Signature}

@defmodule[compiler/embed-sig]

@defsignature/splice[compiler:embed^ ()]{

Includes the identifiers provided by @schememodname[compiler/embed].}

@; ----------------------------------------

@section{Executable Creation Unit}

@defmodule[compiler/embed-unit]

@defthing[compiler:embed@ unit?]{

A unit that imports nothing and exports @scheme[compiler:embed^].}
