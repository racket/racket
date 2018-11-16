This package contains the implementation of Racket's front-end: macro
expander, reader, and module systems. A copy of this implementation is
extracted and built into the Racket executable, so normally this
package's modules are not run directly. The expander or reader can be
run separately, however, and the Racket expander is updated by
modifying this package as it exists in the main Racket Git repository.

Running:

 % racket demo.rkt
 or
 % racket bootstrap-demo.rkt

   Runs the examples/tests in "demo.rkt". The tests are not remotely
   complete, but they're a quick and useful sanity check. The
   "demo.rkt" module uses the somewhat internal interface exported by
   `main`, where the expansion, compilation, and evaluation are less
   overloaded and more controllable.

   Use the "bootstrap-demo.rkt" when running in an older version of
   Racket that is not built with this expander (but that version of
   Racket must be new enough to provide a primitive '#%linklet module
   as a bootstrapping hook).

 % racket run.rkt -c <dir>
 or
 % racket bootstrap-run.rkt -c <dir>

   Runs the expander to load itself from source. Expanded and compiled
   modules are stored in <dir>, somewhat like bytecode files.
   Dependency tracking doesn't take into account the expander itself,
   so throw away <dir> if the expander changes in a way that you want
   reflected in compilation results.

 % racket run.rkt -c <dir> -l <module-path>
 % racket run.rkt -c <dir> -t <file-path-for-module>

   Runs the expander to load the specified module (instead of the
   default module, which is the expander itself).

   When running with a new enough version of Racket that "run.rkt"
   works (as opposed to "bootstrap-run.rkt"), the performance of the
   expander in this mode should be close to the performance when the
   expander is built into the Racket executable. Beware, however, that
   "run.rkt" implements just enough of the module loader protocol to
   work as a bridge, so module loading and caching can have very
   different performance than in an embedding build.

   Beware also that the flags above cause bytecode for the target
   module to be cached, so running a second time will not test the
   expander a second time. Prime the cache directory with modules that
   don't change, and then use `-r` to load a module with a read-only
   cache.

 % racket run.rkt -c <dir> -f <file-path-for-top-level>

   Loads the given file as a sequence of top-level forms.

 % racket run.rkt -c <dir> -e -l <module-path>

   Expands the given file, instead of compiling and running it.

 % racket bootstrap-run.rkt -s -c <dir> --linklets -l <module-path>

   Compiles the given file to a set of linklets in S-expression form,
   instead of compiling and running it.

 % racket bootstrap-run.rkt -s -c <dir> -x

   Checks possibility of converting a module to a stand-alone linklet
   with no imports --- used mainly to extract the expander itself.

 % racket bootstrap-run.rkt -c <dir> -sx -t <file-path> -o <outfile-path>

   Expands and extracts <file-path> as a single linklet to
   <outfile-path>.

   Unless the `--local-rename` flag is also provided to
   "bootstrap-run.rkt", an extracted linklet preserves a naming
   property of the expander's compilation to linklets, which is that
   it uses a distinct symbol for every binding. The symbol--binding
   correspondence is useful for some further compiler passes, but
   `--local-rename` is useful to minimize syntactic diffs.

 % racket bootstrap-run.rkt -c <dir> -sx -D -t <file-path> -o <outfile-path>

   Expands and extracts <file-path> as a single linklet, compiles and
   decompiles it, then writes the s-expression into <outfile-path>.

 % racket bootstrap-run.rkt -c <dir> -sx -B -t <file-path> -o <outfile-path>

   Expands and extracts <file-path> as a single linklet, compiles it
   and then writes the bytecode into <outfile-path>.

 % racket bootstrap-run.rkt -c <dir> -O <checkout-dir>/racket

   Compiles the expander to source files in <dir> --- note that
   "bootstrap-run.rkt" must be used to get source compiles --- and
   writes the flattened linklet to "startup.inc" in a Git checkout of
   a linklet-based Racket. Be sure to increment the target Racket
   version if you change the serialization of syntax objects or the
   linklet protocol.

   When you `make`, then "startup.inc" will be automatically compiled
   to bytecode for for embedding into the Racket executable. If you
   change the expander in a way that makes existing compiled files
   invalid, be sure to update "schvers.h". (Updating "schvers.h" is
   important both for bytecode files and the makefile/preprocessor
   dance that generates the bytecode version of the expander itself.)

 % make

   A shortcut for the above: When this package resides in an existing
   in-place build from the main Racket repo, then the makefile uses
   that copy of Racket to build the expander and drop a replacement
   into the "src" directory. Re-making the Racket tree will then use
   the updated expander. You may have to manually discard
   "compiled/cache-src" when things change.

 % make demo
 % make run ARGS="<arg> ..."

   More shortcuts. Use `make run ARGS="<arg> ..."` as a shorthand for `racket
   run.rkt -c compiled/cache <arg> ...`.

   See "Makefile" for more information and other shortcuts.

----------------------------------------

Roadmap to the implementation:

 read/ - the readers
   demo.rkt - simple examples/tests for the reader

 syntax/ - syntax-object and binding representation
   syntax.rkt - syntax-object structure
   scope.rkt - scope sets and binding
   binding.rkt - binding representations
   binding-table.rkt - managing sets of bindings

 namespace/ - namespaces and module instances

 expand/ - expander loop and core forms

 common/ - utilities
   module-path.rkt - [resolved] module path [indexes]

 compile/ - from expanded to S-expression linklet
   main.rkt - compiler functions called from "eval/main.rkt"

 eval/ - evaluation
   main.rkt - top-level evaluation, with top-level `module` forms as
              an important special case; the `compile-to-linklets`
              function compiles to a set of S-expression linklets
   api.rkt - wrappers that implement `eval`, `compile`, and `expand`
             for `racket/base`

 boot/ - internal initialization
   handler.rkt - implements the default module name resolver, eval
                 handler, and compiler handler
   ...-primitive.rkt - export built-in functions as modules

 run/ - helpers to drive the expander; not part of the resulting
        expander's implementation
   linklet.rkt - a bootstrapping implementation of `linklet` by
                 compilation into `lambda` plus primitives

 extract/ - extracts a module and its dependencies to a single
            linklet, especially for extracting the compiler itself
            (via "run.rkt"); not part of the resulting expander's
            implementation

 main.rkt - installs eval handler, etc.; entry point for directly
            running the expander/compiler/evaluator, and the provided
            variables of this module become the entry points for the
            embedded expander

 demo.rkt - exercises the expander and compiler (uses "main.rkt")

 run.rkt - starts a Racket replacement (uses "main.rkt")

 bootstrap-run.rkt - like "run.rkt", but for a host Racket that
                     does not include linklet support

 bootstrap-demo.rkt - like "demo.rkt", but for a host Racket that
                      does not include linklet support

Beware that names are routinely shadowed when they are provided by
`racket/base` but replaced by the expander's implementation. For
example, `syntax?` is shadowed, and any part of the expander that
needs `syntax?` must import "syntax/syntax.rkt" or
"syntax/checked-syntax.rkt".

----------------------------------------

Performance measurements:

Set the `PLT_EXPANDER_TIMES` environment variable for a summary of
performance information (written via `log-error`, which normally goes
to stderr) on exit. In the output, a category that is nested under
another category contributes to the latter's recorded time and memory
use, but not to its counts. Beware that taking measurements can slow
down the expander slightly.

Set the `PLT_LINKLET_TIMES` environment variable to get similar
information from the underlying runtime system. Except for compile
times, linklet-level times generally will be disjoint from the times
reported by the expander.

----------------------------------------

Implementation guidelines:

 * Do not rely on more than `racket/base` for code that will be
   extracted as the compiler implementation. (Relying on more in
   "run/" or "extract/" is allowed.)

 * The runtime implementation of the expander must not itself use any
   syntax objects or syntax function as provided by the Racket
   implementation used to compile the expander. That means, for
   example, that the contract system cannot be used in the
   implementation of the expander, since the contract system manages
   some information with syntax objects at run time. The
   expander-extraction process double-checks that the expander is
   independent of its host in this way.

 * The runtime implementation of the expander can refer (via
   `#%kernel`) to reader primitives that are to be implemented by the
   reader that is bundled with the expander. The extraction process
   simply redirects those references to the implemented variants.
   Beware that adjusting parameters from `#%kernel` will not change
   the behavior of the bundled reader during bootrstapping of the
   expander (i.e., for bootstrapping, always refer to the parameters
   from the implementation in the "read" directory).

----------------------------------------

Some naming conventions:

 s or stx - a syntax object

 sc - a scope

 scs - a set or list of scopes

 id - an identifier (obviously)

 b - a binding; sometimes spelled out as `binding`

 m - a result of syntax matching

 m - a module

 ns - a namespace

 ctx - an expansion context (including the expand-time environment)

 cctx - a compilation context (including a compile-time environment)

 insp - an inspector

 mpi - a module path index

 mod-name - a resolved module path, usually; sometimes used for other
  forms of module reference

 c and ec - character and "effective" character (after readtable
  mapping) in the reader

 <subscript>-<something> - like <something>, but specifically one for
   <subscript>; for example, `m-ns` is a namespace for some module
