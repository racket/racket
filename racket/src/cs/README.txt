The implementation of Racket CS (Racket on Chez Scheme) in this
directory is organized into two layers:

 * The immediate directory contains Scheme sources to implement Racket
   functionality on top of Chez Scheme. It references sibling
   directories like "expander" and "io", which contain Racket code
   that is compiled to Chez Scheme to implement Racket.

 * The "c" subdirectory contains C sources and build scripts to create
   wrapper executables that combine Chez Scheme with the Racket
   functionality implemented in this immediate directory.

You will need to use `zuo` instead of `make` for working at the level
of this directory. See "../zuo".

========================================================================
 Bootstrapping
========================================================================

The Racket source distribution includes already-bootstrapped files to
build Racket CS. Some of those files are checked into the Racket Git
repository, and the rest are in a sibling "pb" repository that that
Racket repository's top-level makefile checks out.

Building Racket CS from original sources requires an existing Racket
or Chez Scheme build:

 * Chez Scheme (v9.5.3 and up) is typically used to compile Chez
   Scheme boot files, but any recent version of Racket (v7.1 and up)
   can generate boot files for Chez Scheme.

   When you use `configure` as described in "../README.txt", supply
   `--enable-scheme=...` to select a Scheme implementation to build
   boot files, or supply `--enable-racket=...` to select a Racket
   implementation to build boot files. A Racket implementation could
   be one that is built by first using `configure --enable-bc`.

   Alternatively, boot files for the pb (portable bytecode) Chez
   Scheme variant can be used to compile Chez Scheme on any supported
   platform. The pb boot files must correspond to practically the same
   version of Chez Scheme as being built, though. The should be
   installed in the "../ChezScheme/boot/pb" directory as described by
   "../ChezScheme/BUILDING".

   Supplying `--enable-scheme=...` is also an option if you already have
   Chez Scheme built on the current platform; it does not need to match
   the Chez Scheme version as used in the Racket being built; a "reboot"
   bootstrapping path is able to reconstruct boot files even across
   versions. Another build will be created anyway, but more quickly
   than without Chez Scheme.

 * Racket is needed to generate the files in the "schemified"
   directory from the sources in sibling directories like "../io". The
   Racket version must be practically the same as the current Racket
   version, although it can be the Racket BC implementation (instead of
   the Racket CS implementation).

   Unlike Chez Scheme boot files, the files generated in "schemified"
   are human-readable and -editable Scheme code. That provides a way
   out of bootstrapping black holes, even without BC.


========================================================================
 Development versus Build
========================================================================

The Racket CS implementation can be built and run in two different
ways: development mode for running directly using a Chez Scheme
installation, and build mode for creating a `racket` executable that
combines Chez Scheme and Racket functionality into a single
executable.

Development Mode
----------------

The "main.zuo" in this directory is set up for modifying the
implementation of Racket functionality (including in the sibling
directories, like "io") and testing it out on a Chez Scheme
installation.

For this development mode, either Chez Scheme needs to be installed as
`scheme`, or you must use `zuo . SCHEME=...` to set the command for
`scheme`.

Development mode also needs a Racket installation with at least the
"compiler-lib" package installed. By default, the build script looks
for Racket installed as "../../bin/racket"; if this directory is in a
clone of the Git repository for Racket, you can get "../../bin/racket"
with

      make PKGS="compiler-lib"

in the clone's root directory. Alternatively, use `make RACKET=...`
to set the command for `racket`.

The use of development mode is described in more detail further below.

Development mode currently doesn't work on Windows, because the
build script makes too many Unix-ish assumptions.

Build Mode
----------

To build a Racket CS executable, the `configure` script and makefile
in the "c" subdirectory are normally used via `configure` and `make`
in the parent directory of this one, as described in "../README.txt".
However, you can use them directly with something like

   cd [build]
   mkdir cs
   cd cs
   [here]/c/configure
   zuo
   zuo . install

where [here] is the directory containing this "README.txt" and [build]
is a build directory (usually "../build" relative to [here]).

The resulting Racket CS executable is named "racket" by default. To
generate an executable with a "cs" suffix, supply `--enable-bcdefault`
to `configure` (which means that the name "racket" should be reserved
for Racket BC). The option to select the presence or absence of "cs"
also affects the location of ".zo" files, where they are written to a
subdirectory of "compiled" if a "cs" suffix is used.

On Windows, use `[here]\c\winfig.bat` instead of `[here]/c/configure`.


========================================================================
 Machine Code versus JIT
========================================================================

Racket CS currently supports three compilation modes:

 * Machine-code mode --- The compiled form of a module is machine code
   generated by compiling either whole linklets (for small enough
   linklets) or functions within linklets (with a "bytecode"
   interpreter around the compiled parts).

   Select this mode by setting the `PLT_CS_MACH` environment variable,
   but it's currently the default. When this mode is selected,
   interpreter mode is still used for compile-time code that does not
   span a module (or, more generally, for the 'quick option to
   `compile-linklet` and similar functions).

   When the "cs" suffix is used for build mode, compiled ".zo" files
   in this mode are written to a subdirectory of "compiled" using the
   Chez Scheme platform name (e.g., "ta6osx").

   Set `PLT_CS_COMPILE_LIMIT` to set the maximum size of forms to
   compile before falling back to interpreted "bytecode". The default
   is 10000. Setting `PLT_CS_COMPILE_LIMIT` to 0 effectively turns
   the implementation into a pure interpreter.

 * Interpreter mode --- The compiled form of a module is a "bytecode"
   tree (not unlike Racket BC's bytecode) that is interpreted.

   Select this mode by setting the `PLT_CS_INTERP` environment
   variable. Alternatively, set `PLT_LINKLET_COMPILE_QUICK` when
   otherwise using machine-code mode (where the difference has to do
   with where compiled file are read and written in development mode).
   At the linklet API level, this mode implements the 'quick option to
   `compile-linklet` and similar functions.

   When the "cs" suffix is used for build mode, compiled ".zo" files
   in this mode are written to a "cs" subdirectory of "compiled".

   Interpreter mode is used automatically for large modules in
   machine-code mode, as controlled by `PLT_CS_COMPILE_LIMIT`. It is
   also used by default for compile-time code within a module while
   that same module is being expanded.

 * JIT mode --- The compiled form of a module is an S-expression where
   individual `lambda`s are compiled on demand.

   JIT mode does not perform well and probably should be discontinued.

   Select this mode by setting the `PLT_CS_JIT` environment variable.
   When this mode is selected, interpreter mode is still used for
   compile-time code that does not span a module (or, more generally,
   for the 'quick option to `compile-linklet` and similar functions).

   When the "cs" suffix is used for build mode, compiled ".zo" files
   in this mode are written to a "cs" subdirectory of "compiled".

Set the `PLT_ZO_PATH` environment variable to override the path used
for ".zo" files. For example, you may want to preserve a normal build
while also building in machine-code mode with `PLT_CS_DEBUG` set, in
which case setting `PLT_ZO_PATH` to something like "compiled/debug"
could be a good idea.


========================================================================
 Development Mode
========================================================================

Development mode is driven by the build script in this directory.

Building
--------

Running `zuo` will build the Racket CS implementation. You can use
`zuo . expander-demo` to run a demo that loads `racket/base` from
source.

Use `zuo . setup` (or `zuo . setup-v` for a verbose version) to build
".zo" files for collection-based libraries. You can supply additional
arguments to `zuo . setup` to be passed along to `raco setup`.

   zuo . setup ARGS="-l typed/racket"  # only sets up TR
   zuo . setup ARGS="--clean -Dd"      # clears ".zo" files
   zuo . setup ARGS="--fail-fast"      # stop at the first error

Running
-------

Use `zuo . run` to run Racket on Chez Scheme analogous to running
plain `racket`, where extra command-line arguments after `run` are
passed along to Racket.

Structure
---------

The Racket CS implementation is organized in layers. The immediate
layer over Chez Scheme is called "Rumble", and it implements delimited
continuations, structures, chaperones and impersonators, engines (for
threads), and similar base functionality. The Rumble layer is
implemented in Chez Scheme.

The rest of the layers are implemented in Racket:

   thread
   io
   regexp
   schemify
   expander

Each of those layers is implemented in a sibling directory of this
one. Each layer is expanded (using "expander", of course) and then
compiled to Chez Scheme (using "schemify") to implement Racket.

The fully expanded form of each layer must not refer to any
functionality of previous layers. For example, the expander "thread"
must not refer to functionality implemented by "io", "regexp", or
"expander", while the expanded form of "io" must not refer to "regexp"
or "expander" functionality. Each layer can use `racket/base`
functionality, but beware that code from `racket/base` will be
duplicated in each layer.

The "io" layer relies on a shared library, rktio, to provide a uniform
interface to OS resources. The rktio source is in a "rktio" sibling
directory to this one.

Files in this directory:

 *.sls - Chez Scheme libraries that provide implementations of Racket
         primitives, building up to the Racket expander. The
         "rumble.sls" library is implemented directly in Chez Scheme.
         For most other cases, a corresponding "compiled/*.scm" file
         contains the implementation extracted from from expanded and
         flattened Racket code. Each "*.sls" file is built to "*.so".

 rumble/*.ss - Parts of "rumble.sls" (via `include`) to implement data
         structures, immutable hash tables, structs, delimited
         continuations, engines, impersonators, etc.

 linklet/*.ss - Parts of "linklet.sls" (via `include`).

 compiled/*.rktl (generated) - A Racket library (e.g., to implement
         regexps) that has been fully macro expanded and flattened
         into a linklet from its source in "../*". A linklet's only
         free variables are primitives that will be implemented by
         various ".sls" libraries in lower layers.

         For example, "../thread" contains the implementation (in
         Racket) of the thread and event subsystem.

 schemified/*.scm (generated) - A conversion from a ".rktl" file to be
         `include`d into an ".sls" library.

 ../build/so-rktio/rktio.rktl (generated) and
 ../../lib/librktio.{so,dylib,dll} (generated) - Created when building
         the "io" layer, the "rktio.rktl" file contains FFI descriptions
         that are `include`d by "io.sls" and "librktio.{so,dylib,dll}"
         is the shared library that implements rktio.

         CAUTION: The build script here doesn't track all dependencies
         for rktio, and you may need use `zuo build.zuo rktio-rktl` in
         the "rktio" directory if you change its implementation.

 primitive/*.ss - for "expander.sls", tables of bindings for
         primitive linklet instances; see "From primitives to modules"
         below for more information.

 convert.rkt - A "schemify"-based linklet-to-library-body compiler,
         which is used to convert a ".rktl" file to a ".scm" file to
         inclusion in an ".sls" library.

 demo/*.ss - Chez Scheme scripts to check that a library basically
         works. For example "demo/regexp.ss" runs the regexp matcher
         on a few examples. To run "demo/*.ss", use `zuo . *-demo`.

 other *.rkt - Racket scripts like "convert.rkt" or comparisons like
         "demo/regexp.rkt". For example, you can run "demo/regexp.rkt"
         and compare the reported timing to "demo/regexp.ss".

From Primitives to Modules
--------------------------

The "expander" layer, as turned into a Chez Scheme library by
"expander.sls", synthesizes primitive Racket modules such as
`'#%kernel` and `'#%network`. The content of those primitive _modules_
at the expander layer is based on primitive _instances_ (which are just
hash tables) as populated by tables in the "primitive" directory. For
example, "primitive/network.scm" defines the content of the
`'#network` primitive instance, which is turned into the primitive
`'#%network` module by the expander layer, which is reexported by the
`racket/network` module that is implemented as plain Racket code. The
Racket implementation in "../racket" provides those same primitive
instances to the macro expander.

Running "demo/expander.ss"
--------------------------

A `make expander-demo` builds and tries the expander on simple
examples, including loading `racket/base` from source.

Dumping Linklets and Schemified Linklets
----------------------------------------

See "Inspecting Compiler Passes" in the Racket reference manual.

Safety and Debugging Mode
-------------------------

If you make changes to files in "rumble", you should turn off
sunsafe mode by provided `UNSAFE_COMP=no` to `zuo`.

You may want to supply `DEBUG_COMP=yes` so that backtraces provide
expression-specific source locations instead of just
procedure-specific source locations. Enabling `DEBUG_COMP` makes the
Racket CS implementation take up twice as much memory and take twice
as long to load.

Turning on `DEBUG_COMP` affects only the Racket CS implementation. To
preserve per-expression locations on compiled Racket code, set
`PLT_CS_DEBUG`. See also "JIT versus Machine Code" for a suggestion on
setting `PLT_ZO_PATH`.

When you change "rumble" or other layers, you can continue to use
Racket modules that were previously compiled to ".zo" form... usually,
but inlining optimizations and similar compiler choices can break
compatibility.

FFI Differences
---------------

Compared to the Racket BC implementation, Racket CS's FFI behaves in
several different ways:

 * The `make-sized-byte-string` function always raises an exception,
   because a foreign address cannot be turned into a byte string whose
   content is stored in the foreign address. The options are to copy
   the foreign content to/from a byte string or use `ptr-ref` and
   `ptr-set!` to read and write at the address.

 * When `_bytes` is used as an argument type, beware that a byte
   string is not implicitly terminated with a NUL byte. When `_bytes`
   is used as a result type, the C result is copied into a fresh byte
   string. See also `_bytes/nul-terminated`.

 * A `_gcpointer` can only refer to the start of an allocated object,
   and never the interior of an 'atomic-interior allocation. Like
   Racket BC, `_gcpointer` is equivalent to `_pointer` for
   sending values to a foreign procedure, return values from a
   callback that is called from foreign code, or for `ptr-set!`. For
   the other direction (receiving a foreign result, `ptr-ref`, and
   receiving values in a callback), the received pointer must
   correspond to the content of a byte string or vector.

 * An immobile cell must be modified only through its original pointer
   or a reconstructed `_gcpointer`. If it is cast or reconstructed as
   a `_pointer`, setting the cell will not cooperate correctly with
   the garbage collector.

 * Callbacks are always in atomic mode (i.e., the `#:atomic?` option
   in `_fun` and `_cprocedure` is ignored). A callback must be
   declared with `#:callback-exns?` to raise an exception that escapes
   to an enclosing foreign callout.

Threads, Threads, Atomicity, Atomicity, and Atomicity
-----------------------------------------------------

Racket's thread layer does not use Chez Scheme threads. Chez Scheme
threads correspond to OS threads. Racket threads are implemented in
terms of engines at the Rumble layer. At the same time, futures and
places use Chez Scheme threads, and so parts of Rumble are meant to be
thread-safe with respect to Chez Scheme and OS threads. The FFI also
exposes elements of Chez Scheme / OS threads.

As a result of these layers, there are multiple ways to implement
atomic regions:

 * For critical sections with respect to Chez Scheme / OS threads, use
   a mutex or a spinlock.

   For example, `eq?` and `eqv?`-based hash tables can be accessed
   concurrently from Chez Scheme threads as long as they are guarded
   by a mutex or spinlock. In contrast, `equal?`-based hash table
   operations are not atomic from the Racket perspective, so they
   can't be locked by a mutex or spinlock; they use Racket-thread
   locks, instead.

   Chez Scheme deactivates a thread that is blocked on a mutex, as
   long as interrupts are not disabled, so you don't have to worry
   about waiting on a mutex blocking GCs. However, if a lock guards a
   value that is also used by a GC callback, then interrupts should be
   disabled before taking the lock to avoid deadlock.

 * For critical sections at the Racket level, there are multiple
   possibilities:

     - The Racket "thread" layer provides `start-atomic` and
       `end-atomic` to prevent Racket-thread swaps.

       These are the same operations as provided by
       `ffi/unsafe/atomic`.

     - Disabling Chez Scheme interrupts will also disable Racket
       thread swaps, since a thread swap via engines depends on a
       timer interrupt --- unless something explicitly blocks via the
       Racket thread scheduler, such as with `(sleep)`.

       Disable interrupts for atomicity only at the Rumble level where
       no Racket-level callbacks are not involved. Also, beware that
       disabling interrupts will prevent GC interrupts.

       The Racket "thread" layer provides `start-atomic/no-interrupts`
       and `end-atomic/no-interrupts` for both declaring atomicity at
       the Racket level and turning off Chez Scheme interrupts. The
       combination is useful for implementing functionality that might
       be called in response to a GC and might also be called by
       normal (non-atomic) code; the implementation of logging at the
       "io" layer might be the only use case.

     - The implementation of engines and continuations uses its own
       flag to protect regions where an engine timeout should not
       happen, such as when the metacontinuation is being manipulated.
       That flag is managed by `start-uninterrupted` and
       `end-uninterrupted` in "rumble/interrupt.ss".

       It may be tempting to use that flag for other purposes, as a
       cheap way to disable thread swaps. For now, don't do that.

Performance Notes
-----------------

The best-case scenario for performance is currently the default
configuration:

 * `UNSAFE_COMP` is enabled in "main.zuo" --- currently on by default.

   Effectiveness: Can mean a 10-20% improvement in loading
   `racket/base` from source. Since the implementation is in pretty
   good shape, `UNSAFE_COMP` is enabled by default.

 * `DEBUG_COMP` not enabled --- or, if you enable it, run `zuo .
   strip`, but note that `zuo . run` will detect the changed files and
   recompile.

   Effectiveness: Avoids increasing the load time for the Rumble and
   other layers by 30-50%.

 * `PLT_CS_DEBUG` not set --- an environment variable similar to
   `DEBUG_COMP`, but applies to code compiled by Racket CS.

   Effectiveness: Avoids improvement to stack traces, but also avoids
   increases load time and memory use of Racket programs by as much as
   50%.

Structure Types
---------------

See the note in "../expander/README.txt" about structure types. That
applies for all of layers. So, for example,

   (struct-predicate-procedure? thread?) ; => #f

Beware, however, that if schemify is not able to optimize a
structure-type creation, then the current implementation will end up
exposing structure procedures as such.

Inlining Expectations
---------------------

Chez Scheme will inline small Rumble functions as long as the inlined
function body refers only to primitives and other identifiers that are
explicitly defined in the Rumble library body. The "explicitly defined"
part can be tricky, particularly since inlining will consider the
function body after other inlining is already applied. For example,
given

  (define (list? v)
    (or (null? v)
        (and (pair? v)
             (slow-list? v))))

  (define (slow-list? v)
    (let loop ([v v] [depth 0])
      ....))

then `list?` probably will not be inlined, because the call to
`slow-list?` within `list?` is likely to be inlined, so that `list?`
ends up calling the `loop` function nested within `slow-list?` (and
`loop` is not defined at the level of the library body).

The `$app/no-inline` primitive is useful to prevent unproductive
inlining, particularly to enable other, productive inlining. For
example,

  (define (list? v)
    (or (null? v)
        (and (pair? v)
             (#%$app/no-inline slow-list? v))))

is likely to make `list?` inlinable, since the reference to
`slow-list?` is preserved.

Chez Scheme will inline small Rumble functions independent of the
amount of inlining that has already happened at the call site of the
Rumble function. To prevent code explosion and endless inlining
cycles, however, it will not perform further inlining of a Rumble
function that is referenced by code introduced by inlining another
Rumble function.

Using a macro to force inlining can be ok, but that technique should
be used sparingly. Note that a reference to a primitive in a macro
will be compiled as a safe reference to the primitive, since the
conversion of a primitive reference to unsafe or not based on
`(optimize-level)` is an expansion-time operation. So, macros that are
meant to expand to uses of unsafe operations should refer to the
operations using `#3%`; beware that such a reference will stay unsafe,
even if `UNSAFE_COMP` is disabled in "main.zuo".


========================================================================
 Modifying Racket
========================================================================

If you modify Racket in a way that changes compiled code, including
changing the set of primitives, be sure to update the version number
in "../version/racket_version.h", so that various tools know to
rebuild bytecode.

Modifying "thread", "io", "regexp", "schemify", or "expander"
-------------------------------------------------------------

If you modify one of the layers in "../thread", "../io", "../regexp",
"../schemify", or "../expander", then a `zuo . run` here in development
mode will pick up those changes. Even if you don't use development
mode here, though, use `zuo . schemified` to compile the changes into a
"schemified/*.scm" form. The bootstrapped forms in "schemified/*.scm"
should be committed to the Racket repo along with the source-file
changes.

If you're working in a Racket repo checkout, and if you have a working
`racket` in your `PATH`, `make derived` in the checkout's top-level
directory includes a `zuo . schemified` in this directory.

If you modify the "thread", "io", or "regexp" layer to add new
bindings, you will also need to modify "primitive/kernel.ss" (or, less
commonly, one of the other files in "primitive"). For example,
"../io/main.rkt" needs to `provide` everything that should be exported
from that layer, but any provided name also needs to be added to
"primitive/kernel.ss" to make that exported to the `#%kernel` instance
(a hash table) that the expander layer uses to create primitive
modules like `#%kernel`. The entries in "primitive" files use the
constructors defined in "../schemify/known.rkt" to describe
primitives; note that most of the numbers you see are arity masks in
the sense of `procedure-arity-mask`.

For some additions to rktio with corresponding changes to the io
layer, you may need to modify "io.sls" to fill in some conversion
glue. For example, "io.sls" defines `rktio_identity_to_vector`, which
unpacks a C-level structure from rktio into a Scheme value for
consumption by the io layer.

Modifying Chez Scheme
---------------------

If you modify the Chez Scheme implementation in "../ChezScheme" in a
way that changes compiled code, then you should also update the Chez
Scheme version number in "../ChezScheme/s/cmacro.ss". For more about
Chez Scheme's implementation and bootstrap, see
"../ChezScheme/IMPLEMENTATION.md".

If you're working in a checkout of the Racket Git repo, then when you
update Chez Scheme in a way that needs new pb bootfiles, the updated
bootfiles should be pushed to a new branch of the Racket pb repo and
the Racket repo's top-level makefile should be updated to refer to the
branch. Assuming that a working `racket` is in your path:

 * Update "Makefile" in the checkout root to set `PB_BRANCH` to a
   fresh branch name, typically based on the Racket version number.

 * Use `make pb-build` in the checkout root to build pb bootfiles
   using `racket`.

 * Use `make pb-stage` in the checkout root to set up the new branch
   locally. You could check that "../ChezScheme/boot/pb" looks
   sensible at this point. The local branch checkout should have a
   single commit in its history.

 * Use `make pb-push` to push the new branch to the Racket pb repo.
