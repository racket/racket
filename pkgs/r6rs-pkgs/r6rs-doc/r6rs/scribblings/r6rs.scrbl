#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (only-in scribble/core style)
          scribble/html-properties
          (for-label setup/dirs
                     rnrs/programs-6
                     rnrs/base-6
                     rnrs/unicode-6
                     rnrs/exceptions-6
                     rnrs/conditions-6
                     rnrs/io/simple-6
                     rnrs/r5rs-6
                     rnrs/hashtables-6
                     r6rs
                     (only-in racket/base
                              module lib
                              current-library-collection-paths
                              parameterize
                              uncaught-exception-handler
                              make-hash)))

@(define guide-src '(lib "scribblings/guide/guide.scrbl"))

@(define r6rs @elem{R@superscript{6}RS})
@(define r5rs @elem{R@superscript{5}RS})

@(define r6rs-std (style #f (list (install-resource "r6rs-std")
                                  (install-resource "r6rs-lib-std"))))

@title{R6RS: Scheme}

The @link[#:style r6rs-std "r6rs-std/index.html"]{The Revised@superscript{6} Report
on the Algorithmic Language Scheme} defines a dialect of Scheme. We
use @defterm{@|r6rs|} to refer to both the standard and the language
defined by the standard.

@margin-note{See @seclink[#:doc '(lib "scribblings/guide/guide.scrbl")
             "dialects"] for general information about different
             dialects of Scheme within Racket.}

@|r6rs| defines both @defterm{libraries} and @defterm{top-level
programs}. Both correspond to Racket @defterm{modules} (see
@secref[#:doc guide-src "modules"]). That is, although @|r6rs| defines
top-level programs as entry points, you can just as easily treat a
library as an entry point when using Racket. The only difference
is that an @|r6rs| top-level program cannot export any bindings to
other modules.

@table-of-contents[]

@; ----------------------------------------

@section[#:tag "Using R6RS with DrRacket"]{Using @|r6rs| with DrRacket}

To run an @|r6rs| program with DrRacket choose @onscreen{Use language declared in source}
from the language dialog box and add the following line to the top of your program.
@racketmetafont{#!r6rs}.

Here is a small example @|r6rs| program that will work in DrRacket.
@racketblock[
#,(racketmetafont "#!r6rs")
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))
(display (find even? '(3 1 4 1 5 9)))]

@section[#:tag "Running Top-Level Programs"]{Running Top-Level Programs}

To run a top-level program, either:

@itemize[

 @item{Use the @exec{plt-r6rs} executable, supplying the file that
       contains the program on the command line:

       @commandline{plt-r6rs @nonterm{program-file}}

       Additional command-line arguments are propagated as
       command-line arguments to the program (accessed via 
       @racket[command-line]).

       To compile the file to bytecode (to speed future runs of the
       program), use @exec{plt-r6rs} with the @DFlag{compile} flag:

       @commandline{plt-r6rs --compile @nonterm{program-file}}
  
       The bytecode file is written in a @filepath{compiled}
       sub-directory next to @nonterm{program-file}.

       For example, if @filepath{hi.sps} contains

       @racketblock[
       (import (rnrs))
       (display "hello\n")
       ]

       then

       @commandline{plt-r6rs hi.sps}

       prints ``hello.''}

 @item{Prefix the program with @racketmetafont{#!r6rs}, which counts
       as a comment from the @|r6rs| perspective, but is a synonym for
       @racket[#,(hash-lang) r6rs] from the Racket perspective.
       Such files can be run like any other Racket module, such as
       using @exec{racket}:

       @commandline{racket @nonterm{program-file}}

       or using DrRacket. The file can also be compiled to bytecode
       using @exec{raco make}:

       @commandline{raco make @nonterm{program-file}}

       For example, if @filepath{hi.sps} contains

       @racketblock[
       #,(racketmetafont "#!r6rs")
       (import (rnrs))
       (display "hello\n")
       ]

       then

       @commandline{racket hi.sps}

       prints ``hello.'' Similarly, opening @filepath{hi.sps} in
       DrRacket and clicking @onscreen{Run} prints ``hello'' within
       the DrRacket interactions window.}

]

@; ----------------------------------------

@section[#:tag "Installing Libraries"]{Installing Libraries}
       
To reference an @|r6rs| library from a top-level program or another
library, it must be installed as a collection-based library in
Racket.

One way to produce an @|r6rs| installed library is to create in
a @techlink[#:doc guide-src]{collection} a file that starts with
@racketmetafont{#!r6rs} and that contains a @racket[library] form. For
example, the following file might be created in a @filepath{hello.sls}
file within a @filepath{examples} collection directory:

       @racketblock[
       #,(racketmetafont "#!r6rs")
       (library (examples hello)
         (export greet)
         (import (rnrs))
         
         (define (greet)
           (display "hello\n")))
       ]

Alternately, the @exec{plt-r6rs} executable with the @DFlag{install}
flag accepts a sequence of @racket[library] declarations and installs
them into separate files in a collection directory, based on the
declared name of each library:

 @commandline{plt-r6rs --install @nonterm{libraries-file}}

By default, libraries are installed into the user-specific collection
directory (see @racket[find-user-collects-dir]). The @DFlag{all-users}
flag causes the libraries to be installed into the main installation,
instead (see @racket[find-collects-dir]):

 @commandline{plt-r6rs --install --all-users @nonterm{libraries-file}}

You may as well specify an arbitrary collections directory by using
the @DFlag{collections} flag:

 @commandline{plt-r6rs --install --collections @nonterm{directory} @nonterm{libraries-file}}

See @secref["libpaths"] for information on how @|r6rs| library names
are turned into collection-based module paths, which determines where
the files are written. Libraries installed by @exec{plt-r6rs
@DFlag{install}} are automatically compiled to bytecode form.

One final option is to supply a @as-index{@DPFlag{path}} flag to
@exec{plt-r6rs}. A path added with @DPFlag{path} extends the set of
directories that are searched to find a collection (i.e., it sets
@racket[current-library-collection-paths]). If @nonterm{dir} contains
@filepath{duck} and @filepath{cow} sub-directories with
@filepath{duck/feather.sls} and @filepath{cow/bell.sls}, and if each
file is an @|r6rs| library prefixed with @racketmetafont{#!r6rs}, then
@exec{plt-r6rs ++path @nonterm{dir}} directs the @|r6rs| library
references @racket[(duck feather)] and @racket[(cow bell)] to the
files. Note that this technique does not support accessing
@filepath{duck.sls} directly within @nonterm{dir}, since the library
reference @racket[(duck)] is treated like @racket[(duck main)] for
finding the library, as explained in @secref["libpaths"]. Multiple
paths can be provided with multiple uses of @DPFlag{path}; the paths
are searched in order, and before the installation's collections.

@; ----------------------------------------

@section[#:tag "r6rs-mod"]{@|r6rs| Module Language}

@defmodulelang[r6rs]

The @racketmodname[r6rs] language is usually used in the form
@racketmetafont{#!}@racketmodname[r6rs], which is equivalent to
@racket[@#,hash-lang[] @#,racketmodname[r6rs]] and is also valid
@|r6rs| syntax. 

@subsection[#:tag "using-r6rs"]{Using @|r6rs|}

See @secref["Using\x20R6RS\x20with\x20DrRacket"],
@secref["Running\x20Top-Level\x20Programs"], and
@secref["Installing\x20Libraries"] for more information on writing and
running @|r6rs| programs with Racket.

@subsection{The Implementation of @|r6rs|}

The @|r6rs| language is itself implemented as a module within
Racket. The details of that implementation, as provided in this
section, are not normally relevant to programmers using @|r6rs|; see
the links in @secref["using-r6rs"], instead. The details may be
relevant to programmers who are developing new tools or deriving
variants of @|r6rs| within Racket.

As a Racket module, the @racketmodname[r6rs] module language provides
only a @racket[#%module-begin] binding, which is used to process the
entire body of a Racket module (see @racket[module]). The
@racket[#%module-begin] binding from @racketmodname[r6rs] allows the
body of a module to use the syntax of either a @|r6rs| library or a
@|r6rs| top-level program.

@defform*[#:literals (library import export)
          [(#%module-begin 
            (library library-name 
              (export export-spec ...)
              (import import-spec ...)
              library-body ...))
           (#%module-begin
            (import import-spec ...)
            program-body ...)]]{

An @racketmodname[r6rs] module that contains a single @racket[library]
form defines an @|r6rs| library, while a module body that starts with
an @racket[import] form defined an @|r6rs| top-level program.

The @racket[library], @racket[export], and @racket[import] identifiers
are not exported by the @racketmodname[r6rs] library; they are
recognized through equivalence to unbound identifiers.}

@; ----------------------------------------

@section[#:tag "libpaths"]{Libraries and Collections}

An @|r6rs| library name is sequence of symbols, optionally followed by
a version as a sequence of exact, non-negative integers. Roughly, such
a name is converted to a Racket module pathname (see @secref[#:doc
guide-src "module-paths"]) by concatenating the symbols with a
@litchar{/} separator, and then appending the version integers each
with a preceding @litchar{-}. As a special case, when an @|r6rs| path
contains a single symbol (optionally followed by a version), a
@racketidfont{main} symbol is effectively inserted after the initial
symbol. See below for further encoding considerations.

When an @|r6rs| library or top-level program refers to another
library, it can supply version constraints rather than naming a
specific version. Version constraints are always resolved at compile
time by searching the set of installed files.

In addition, when an @|r6rs| library path is converted, a file
extension is selected at compile time based on installed files. The
search order for file extensions is @filepath{.mzscheme.ss},
@filepath{.mzscheme.sls}, @filepath{.ss}, @filepath{.sls}, and
@filepath{.rkt}.  When resolving version constraints, these extensions
are all tried when looking for matches.



To ensure that all @|r6rs| library names can be converted to a unique
and distinct library module path, the following conversions are
applied to each symbol before concatenating them:

@itemize[

 @item{The symbol is encoded using UTF-8, and the resulting bytes are
 treated as Latin-1 encoded characters. ASCII letters, digits,
 @litchar{+}, @litchar{-}, and @litchar{_} are left as-is; other
 characters are replaced by @litchar{%} followed by two lowercase
 hexadecimal digits. Note that UTF-8 encodes ASCII letters, digits,
 @|etc| as themselves, so typical library names correspond to readable
 module paths.}

 @item{If the @|r6rs| library reference has two symbol elements and
 the second one is @racketidfont{main} followed by any number of
 underscores, then an extra underscore is added to that symbol. This
 conversion avoids a collision between an explicit @racketidfont{main}
 and the implicit @racketidfont{main} when a library path has a single
 symbol element.}

]

Examples (assuming a typical Racket installation):

@racketblock[
(rnrs io simple (6))  @#,elem{means}  (lib "rnrs/io/simple-6.rkt")
(rnrs)                @#,elem{means}  (lib "rnrs/main-6.rkt")
(rnrs main)           @#,elem{means}  (lib "rnrs/main_.rkt")
(rnrs (6))            @#,elem{means}  (lib "rnrs/main-6.rkt")
(racket base)         @#,elem{means}  (lib "racket/base.rkt")
(achtung!)            @#,elem{means}  (lib "achtung%21/main.rkt")
(funco new-λ)         @#,elem{means}  (lib "funco/new-%ce%bb.rkt")
]


@; ----------------------------------------

@section{Language Interoperability}

Using the conversion rules in @secref["libpaths"], and @r6rs library
can refer to modules that are implemented in other dialects supported
by Racket, and other Racket modules can refer to libraries
that are implemented in @|r6rs|.

Beware that a @defterm{pair} in @|r6rs| corresponds to a
@defterm{mutable pair} in @racketmodname[racket/base]. Otherwise,
@|r6rs| libraries and @racketmodname[racket/base] share the same
datatype for numbers, characters, strings, bytevectors (a.k.a. byte
strings), vectors, and so on. Hash tables are different. Input and
output ports from @racketmodname[racket/base] can be used directly as
binary ports with @|r6rs| libraries, and all @|r6rs| ports can be used
as ports in @racketmodname[racket/base] programs, but only textual
ports created via @|r6rs| libraries can be used by other @|r6rs|
operations that expect textual ports.

@; ----------------------------------------------------------------------

@section[#:tag "conformance"]{@|r6rs| Conformance}

Racket's @|r6rs| support does not conform with the standard in
several known ways:

@itemize[

 @item{When @racket[guard] catches an exception that no clause
       matches, the exception is re-@racket[raise]ed without restoring
       the continuation to the one that raised the exception.

       This difference can be made visible using
       @racket[dynamic-wind]. According to @|r6rs|, the following
       program should print ``in'' and ``out'' twice, but each prints
       once using Racket:

      @racketblock[
        (guard (exn [(equal? exn 5) 'five])
           (guard (exn [(equal? exn 6) 'six])
             (dynamic-wind
               (lambda () (display "in") (newline))
               (lambda () (raise 5))
               (lambda () (display "out") (newline)))))

       ]

       Along similar lines, continuation capture and invocation within
       an exception handler is restricted. Unless the exception is
       raised through @racket[raise-continuable], a handler can escape
       only through a continuation that is a tail of the current
       continuation, and a continuation captured within the handler
       cannot be invoked after control escapes from the raise.

       The initial exception handler does not return for
       non-@racket[&serious] conditions, but @racket[raise] and
       @racket[raise-continuable] both install an uncaught-exception
       handler (via @racket[parameterize] and
       @racket[uncaught-exception-handler]) to one that returns for
       non-@racket[&serious] conditions.}

 @item{Inexact numbers are printed without a precision indicator, and
       precision indicators are ignored on input (e.g.,
       @racketvalfont{0.5|7} is read the same as @racket[0.5]).}

 @item{Word boundaries for @racket[string-downcase],
       @racket[string-upcase], and @racket[string-titlecase] are not
       determined as specified by Unicode Standard Annex #29.}

 @item{When an identifier bound by @racket[letrec] or @racket[letrec*]
       is referenced before it is bound, an exception is not raised;
       instead, the reference produces @|undefined-const|.}

 @item{A custom textual port must represent positions using integers,
       and the positions must correspond to bytes in a UTF-8 encoding
       of the port's data. For custom ports (byte or character) that
       support both input and output, beware that buffered input can
       create a mismatch between the position implemented by the
       custom procedures and the port's current position; the result
       from a custom position procedure is automatically adjusted to
       account for buffering, and setting the port's position flushes
       all buffered bytes, but writing after a read does @emph{not}
       automatically reset the port's position to counteract the
       effects of buffering.}

 @item{The bindings in a namespace produced by @racket[null-environment]
       or @racket[scheme-report-environment] correspond to @|r5rs| bindings
       instead of @|r6rs| bindings. In particular, @racket[=>], @racket[else],
       @racket[_], and @racket[...] are not bound.}

 @item{Bindings for @racketidfont{#%datum}, @racketidfont{#%app},
       @racketidfont{#%top}, and @racketidfont{#%top-interaction} are
       imported into every library and program, and at every phase
       level for which the library or program has imports.}

]


@; ----------------------------------------------------------------------

@section{@|r6rs| Libraries}

@(define (dir-of s)
   (string-append (cadr (regexp-match "^(.*)-Z-H" s)) "-std"))

@(define (cvt-elem i)
   (list (car i)
         (cadr i)
         (build-path (dir-of (caddr i))
                     (caddr i))
         (cadddr i)))

@(define-syntax-rule (r6rs-module mod-path lib html anchor title elem ...)
  (begin
    (subsection #:tag (format "~a" 'mod-path) 
             (racket lib) ": " title)
    (defmodule mod-path)
    "Original specification: " (link (format "~a/~a#~a" (dir-of html) html anchor)
                                     title)
    (make-binding-redirect-elements
     'mod-path
     (map cvt-elem '(elem ...)))))

@r6rs-module[rnrs/base-6 (rnrs base (6))
             "r6rs-Z-H-14.html" "node_sec_11.4" "Base"
       ;; Generated with makeindex.rkt --r6rs, then further converted and 
       ;; filtered by hand
         (* #f "r6rs-Z-H-14.html" "node_idx_496")
         (+ #f "r6rs-Z-H-14.html" "node_idx_494")
         (- #f "r6rs-Z-H-14.html" "node_idx_498")
         (/ #f "r6rs-Z-H-14.html" "node_idx_502")
         (< #f "r6rs-Z-H-14.html" "node_idx_466")
         (<= #f "r6rs-Z-H-14.html" "node_idx_470")
         (= #f "r6rs-Z-H-14.html" "node_idx_464")
         (=> #t "r6rs-Z-H-14.html" "node_idx_378")
         (> #f "r6rs-Z-H-14.html" "node_idx_468")
         (>= #f "r6rs-Z-H-14.html" "node_idx_472")
         (abs #f "r6rs-Z-H-14.html" "node_idx_506")
         (acos #f "r6rs-Z-H-14.html" "node_idx_554")
         (and #t "r6rs-Z-H-14.html" "node_idx_388")
         (angle #f "r6rs-Z-H-14.html" "node_idx_576")
         (append #f "r6rs-Z-H-14.html" "node_idx_634")
         (apply #f "r6rs-Z-H-14.html" "node_idx_752")
         (asin #f "r6rs-Z-H-14.html" "node_idx_552")
         (assert #t "r6rs-Z-H-14.html" "node_idx_750")
         (assertion-violation #f "r6rs-Z-H-14.html" "node_idx_748")
         (atan #f "r6rs-Z-H-14.html" "node_idx_556")
         (begin #t "r6rs-Z-H-14.html" "node_idx_418")
         (boolean=? #f "r6rs-Z-H-14.html" "node_idx_598")
         (boolean? #f "r6rs-Z-H-14.html" "node_idx_596")
         (caar #f "r6rs-Z-H-14.html" "node_idx_616")
         (cadr #f "r6rs-Z-H-14.html" "node_idx_618")
         (call-with-current-continuation #f "r6rs-Z-H-14.html" "node_idx_754")
         (call-with-values #f "r6rs-Z-H-14.html" "node_idx_762")
         (call/cc #f "r6rs-Z-H-14.html" "node_idx_756")
         (car #f "r6rs-Z-H-14.html" "node_idx_612")
         (case #f "r6rs-Z-H-14.html" "node_idx_384")
         (cdddar #f "r6rs-Z-H-14.html" "node_idx_620")
         (cddddr #f "r6rs-Z-H-14.html" "node_idx_622")
         (cdr #f "r6rs-Z-H-14.html" "node_idx_614")
         (ceiling #f "r6rs-Z-H-14.html" "node_idx_530")
         (char->integer #f "r6rs-Z-H-14.html" "node_idx_670")
         (char<=? #f "r6rs-Z-H-14.html" "node_idx_680")
         (char<? #f "r6rs-Z-H-14.html" "node_idx_676")
         (char=? #f "r6rs-Z-H-14.html" "node_idx_674")
         (char>=? #f "r6rs-Z-H-14.html" "node_idx_682")
         (char>? #f "r6rs-Z-H-14.html" "node_idx_678")
         (char? #f "r6rs-Z-H-14.html" "node_idx_668")
         (complex? #f "r6rs-Z-H-14.html" "node_idx_442")
         (cond #f "r6rs-Z-H-14.html" "node_idx_376")
         (cons #f "r6rs-Z-H-14.html" "node_idx_610")
         (cos #f "r6rs-Z-H-14.html" "node_idx_548")
         (define #t "r6rs-Z-H-14.html" "node_idx_352")
         (define-syntax #t "r6rs-Z-H-14.html" "node_idx_358")
         (denominator #f "r6rs-Z-H-14.html" "node_idx_526")
         (div #f "r6rs-Z-H-14.html" "node_idx_510")
         (div-and-mod #f "r6rs-Z-H-14.html" "node_idx_508")
         (div0 #f "r6rs-Z-H-14.html" "node_idx_516")
         (div0-and-mod0 #f "r6rs-Z-H-14.html" "node_idx_514")
         (dynamic-wind #f "r6rs-Z-H-14.html" "node_idx_764")
         (else #t "r6rs-Z-H-14.html" "node_idx_380")
         (eq? #f "r6rs-Z-H-14.html" "node_idx_434")
         (equal? #f "r6rs-Z-H-14.html" "node_idx_436")
         (eqv? #f "r6rs-Z-H-14.html" "node_idx_428")
         (error #f "r6rs-Z-H-14.html" "node_idx_746")
         (even? #f "r6rs-Z-H-14.html" "node_idx_482")
         (exact #f "r6rs-Z-H-14.html" "node_idx_462")
         (exact-integer-sqrt #f "r6rs-Z-H-14.html" "node_idx_562")
         (exact? #f "r6rs-Z-H-14.html" "node_idx_456")
         (exp #f "r6rs-Z-H-14.html" "node_idx_540")
         ;; (export #t "r6rs-Z-H-10.html" "node_idx_270")
         (expt #f "r6rs-Z-H-14.html" "node_idx_564")
         (finite? #f "r6rs-Z-H-14.html" "node_idx_484")
         (floor #f "r6rs-Z-H-14.html" "node_idx_528")
         (for-each #f "r6rs-Z-H-14.html" "node_idx_644")
         (gcd #f "r6rs-Z-H-14.html" "node_idx_520")
         (if #t "r6rs-Z-H-14.html" "node_idx_366")
         (imag-part #f "r6rs-Z-H-14.html" "node_idx_572")
         ;; (import #t "r6rs-Z-H-10.html" "node_idx_268")
         (inexact #f "r6rs-Z-H-14.html" "node_idx_460")
         (inexact? #f "r6rs-Z-H-14.html" "node_idx_458")
         (infinite? #f "r6rs-Z-H-14.html" "node_idx_486")
         (integer->char #f "r6rs-Z-H-14.html" "node_idx_672")
         (integer-valued? #f "r6rs-Z-H-14.html" "node_idx_454")
         (integer? #f "r6rs-Z-H-14.html" "node_idx_448")
         (lambda #t "r6rs-Z-H-14.html" "node_idx_364")
         (lcm #f "r6rs-Z-H-14.html" "node_idx_522")
         (length #f "r6rs-Z-H-14.html" "node_idx_632")
         (let #t "r6rs-Z-H-14.html" "node_idx_394")
         (let* #t "r6rs-Z-H-14.html" "node_idx_398")
         (let*-values #t "r6rs-Z-H-14.html" "node_idx_414")
         (let-syntax #t "r6rs-Z-H-14.html" "node_idx_776")
         (let-values #t "r6rs-Z-H-14.html" "node_idx_410")
         (letrec #t "r6rs-Z-H-14.html" "node_idx_402")
         (letrec* #t "r6rs-Z-H-14.html" "node_idx_406")
         (letrec-syntax #t "r6rs-Z-H-14.html" "node_idx_782")
         ;; (library #t "r6rs-Z-H-10.html" "node_idx_266")
         (list #f "r6rs-Z-H-14.html" "node_idx_630")
         (list->string #f "r6rs-Z-H-14.html" "node_idx_714")
         (list->vector #f "r6rs-Z-H-14.html" "node_idx_738")
         (list-ref #f "r6rs-Z-H-14.html" "node_idx_640")
         (list-tail #f "r6rs-Z-H-14.html" "node_idx_638")
         (list? #f "r6rs-Z-H-14.html" "node_idx_628")
         (log #f "r6rs-Z-H-14.html" "node_idx_542")
         (magnitude #f "r6rs-Z-H-14.html" "node_idx_574")
         (make-polar #f "r6rs-Z-H-14.html" "node_idx_568")
         (make-rectangular #f "r6rs-Z-H-14.html" "node_idx_566")
         (make-string #f "r6rs-Z-H-14.html" "node_idx_688")
         (make-vector #f "r6rs-Z-H-14.html" "node_idx_724")
         (map #f "r6rs-Z-H-14.html" "node_idx_642")
         (max #f "r6rs-Z-H-14.html" "node_idx_490")
         (min #f "r6rs-Z-H-14.html" "node_idx_492")
         (mod #f "r6rs-Z-H-14.html" "node_idx_512")
         (mod0 #f "r6rs-Z-H-14.html" "node_idx_518")
         (nan? #f "r6rs-Z-H-14.html" "node_idx_488")
         (negative? #f "r6rs-Z-H-14.html" "node_idx_478")
         (not #f "r6rs-Z-H-14.html" "node_idx_594")
         (null? #f "r6rs-Z-H-14.html" "node_idx_624")
         (number->string #f "r6rs-Z-H-14.html" "node_idx_578")
         (number? #f "r6rs-Z-H-14.html" "node_idx_440")
         (numerator #f "r6rs-Z-H-14.html" "node_idx_524")
         (odd? #f "r6rs-Z-H-14.html" "node_idx_480")
         (or #t "r6rs-Z-H-14.html" "node_idx_390")
         (pair? #f "r6rs-Z-H-14.html" "node_idx_608")
         (positive? #f "r6rs-Z-H-14.html" "node_idx_476")
         (procedure? #f "r6rs-Z-H-14.html" "node_idx_342")
         (quasiquote #t "r6rs-Z-H-14.html" "node_idx_768")
         (quote #t "r6rs-Z-H-14.html" "node_idx_362")
         (rational-valued? #f "r6rs-Z-H-14.html" "node_idx_452")
         (rational? #f "r6rs-Z-H-14.html" "node_idx_446")
         (rationalize #f "r6rs-Z-H-14.html" "node_idx_536")
         (real-part #f "r6rs-Z-H-14.html" "node_idx_570")
         (real-valued? #f "r6rs-Z-H-14.html" "node_idx_450")
         (real? #f "r6rs-Z-H-14.html" "node_idx_444")
         (reverse #f "r6rs-Z-H-14.html" "node_idx_636")
         (round #f "r6rs-Z-H-14.html" "node_idx_534")
         (set! #f "r6rs-Z-H-14.html" "node_idx_372")
         (sin #f "r6rs-Z-H-14.html" "node_idx_546")
         (sqrt #f "r6rs-Z-H-14.html" "node_idx_560")
         (string #f "r6rs-Z-H-14.html" "node_idx_692")
         (string->list #f "r6rs-Z-H-14.html" "node_idx_712")
         (string->number #f "r6rs-Z-H-14.html" "node_idx_584")
         (string->symbol #f "r6rs-Z-H-14.html" "node_idx_652")
         (string-append #f "r6rs-Z-H-14.html" "node_idx_710")
         (string-copy #f "r6rs-Z-H-14.html" "node_idx_718")
         (string-for-each #f "r6rs-Z-H-14.html" "node_idx_716")
         (string-length #f "r6rs-Z-H-14.html" "node_idx_694")
         (string-ref #f "r6rs-Z-H-14.html" "node_idx_696")
         (string<=? #f "r6rs-Z-H-14.html" "node_idx_704")
         (string<? #f "r6rs-Z-H-14.html" "node_idx_700")
         (string=? #f "r6rs-Z-H-14.html" "node_idx_698")
         (string>=? #f "r6rs-Z-H-14.html" "node_idx_706")
         (string>? #f "r6rs-Z-H-14.html" "node_idx_702")
         (string? #f "r6rs-Z-H-14.html" "node_idx_686")
         (substring #f "r6rs-Z-H-14.html" "node_idx_708")
         (symbol->string #f "r6rs-Z-H-14.html" "node_idx_648")
         (symbol=? #f "r6rs-Z-H-14.html" "node_idx_650")
         (symbol? #f "r6rs-Z-H-14.html" "node_idx_646")
         (tan #f "r6rs-Z-H-14.html" "node_idx_550")
         (truncate #f "r6rs-Z-H-14.html" "node_idx_532")
         (unquote #t "r6rs-Z-H-14.html" "node_idx_770")
         (unquote-splicing #t "r6rs-Z-H-14.html" "node_idx_772")
         (values #f "r6rs-Z-H-14.html" "node_idx_760")
         (vector #f "r6rs-Z-H-14.html" "node_idx_728")
         (vector->list #f "r6rs-Z-H-14.html" "node_idx_736")
         (vector-fill! #f "r6rs-Z-H-14.html" "node_idx_740")
         (vector-for-each #f "r6rs-Z-H-14.html" "node_idx_744")
         (vector-length #f "r6rs-Z-H-14.html" "node_idx_730")
         (vector-map #f "r6rs-Z-H-14.html" "node_idx_742")
         (vector-ref #f "r6rs-Z-H-14.html" "node_idx_732")
         (vector-set! #f "r6rs-Z-H-14.html" "node_idx_734")
         (vector? #f "r6rs-Z-H-14.html" "node_idx_722")
         (zero? #f "r6rs-Z-H-14.html" "node_idx_474")]

@(make-binding-redirect-elements
  'r6rs/private/base-for-syntax
  (map cvt-elem
       '((identifier-syntax ! "r6rs-Z-H-14.html" "node_idx_796")
         (... ! "r6rs-Z-H-14.html" "node_idx_784")
         (_ ! "r6rs-Z-H-14.html" "node_idx_788")
         (syntax-rules ! "r6rs-Z-H-14.html" "node_idx_786"))))

@r6rs-module[rnrs/unicode-6 (rnrs unicode (6))
             "r6rs-lib-Z-H-2.html" "node_idx_2" "Unicode"
             (string-upcase #f "r6rs-lib-Z-H-2.html" "node_idx_36")
             (string-titlecase #f "r6rs-lib-Z-H-2.html" "node_idx_40")
             (string-normalize-nfkd #f "r6rs-lib-Z-H-2.html" "node_idx_56")
             (string-normalize-nfkc #f "r6rs-lib-Z-H-2.html" "node_idx_60")
             (string-normalize-nfd #f "r6rs-lib-Z-H-2.html" "node_idx_54")
             (string-normalize-nfc #f "r6rs-lib-Z-H-2.html" "node_idx_58")
             (string-foldcase #f "r6rs-lib-Z-H-2.html" "node_idx_42")
             (string-downcase #f "r6rs-lib-Z-H-2.html" "node_idx_38")
             (string-ci>? #f "r6rs-lib-Z-H-2.html" "node_idx_48")
             (string-ci>=? #f "r6rs-lib-Z-H-2.html" "node_idx_52")
             (string-ci=? #f "r6rs-lib-Z-H-2.html" "node_idx_44")
             (string-ci<? #f "r6rs-lib-Z-H-2.html" "node_idx_46")
             (string-ci<=? #f "r6rs-lib-Z-H-2.html" "node_idx_50")
             (char-whitespace? #f "r6rs-lib-Z-H-2.html" "node_idx_26")
             (char-upper-case? #f "r6rs-lib-Z-H-2.html" "node_idx_28")
             (char-upcase #f "r6rs-lib-Z-H-2.html" "node_idx_4")
             (char-titlecase #f "r6rs-lib-Z-H-2.html" "node_idx_8")
             (char-title-case? #f "r6rs-lib-Z-H-2.html" "node_idx_32")
             (char-numeric? #f "r6rs-lib-Z-H-2.html" "node_idx_24")
             (char-lower-case? #f "r6rs-lib-Z-H-2.html" "node_idx_30")
             (char-general-category #f "r6rs-lib-Z-H-2.html" "node_idx_34")
             (char-foldcase #f "r6rs-lib-Z-H-2.html" "node_idx_10")
             (char-downcase #f "r6rs-lib-Z-H-2.html" "node_idx_6")
             (char-ci>? #f "r6rs-lib-Z-H-2.html" "node_idx_16")
             (char-ci>=? #f "r6rs-lib-Z-H-2.html" "node_idx_20")
             (char-ci=? #f "r6rs-lib-Z-H-2.html" "node_idx_12")
             (char-ci<? #f "r6rs-lib-Z-H-2.html" "node_idx_14")
             (char-ci<=? #f "r6rs-lib-Z-H-2.html" "node_idx_18")
             (char-alphabetic? #f "r6rs-lib-Z-H-2.html" "node_idx_22")]

@r6rs-module[rnrs/bytevectors-6 (rnrs bytevectors (6))
             "r6rs-lib-Z-H-3.html" "node_idx_62" "Bytevectors"
             (utf8->string #f "r6rs-lib-Z-H-3.html" "node_idx_194")
             (utf32->string #f "r6rs-lib-Z-H-3.html" "node_idx_198")
             (utf16->string #f "r6rs-lib-Z-H-3.html" "node_idx_196")
             (uint-list->bytevector #f "r6rs-lib-Z-H-3.html" "node_idx_120")
             (u8-list->bytevector #f "r6rs-lib-Z-H-3.html" "node_idx_106")
             (string->utf8 #f "r6rs-lib-Z-H-3.html" "node_idx_184")
             (string->utf32 #f "r6rs-lib-Z-H-3.html" "node_idx_190")
             (string->utf16 #f "r6rs-lib-Z-H-3.html" "node_idx_186")
             (sint-list->bytevector #f "r6rs-lib-Z-H-3.html" "node_idx_122")
             (native-endianness #f "r6rs-lib-Z-H-3.html" "node_idx_78")
             (make-bytevector #f "r6rs-lib-Z-H-3.html" "node_idx_82")
             (endianness #t "r6rs-lib-Z-H-3.html" "node_idx_76")
             (bytevector? #f "r6rs-lib-Z-H-3.html" "node_idx_80")
             (bytevector=? #f "r6rs-lib-Z-H-3.html" "node_idx_88")
             (bytevector-uint-set! #f "r6rs-lib-Z-H-3.html" "node_idx_112")
             (bytevector-uint-ref #f "r6rs-lib-Z-H-3.html" "node_idx_108")
             (bytevector-u8-set! #f "r6rs-lib-Z-H-3.html" "node_idx_100")
             (bytevector-u8-ref #f "r6rs-lib-Z-H-3.html" "node_idx_96")
             (bytevector-u64-set! #f "r6rs-lib-Z-H-3.html" "node_idx_164")
             (bytevector-u64-ref #f "r6rs-lib-Z-H-3.html" "node_idx_156")
             (bytevector-u64-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_168")
             (bytevector-u64-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_160")
             (bytevector-u32-set! #f "r6rs-lib-Z-H-3.html" "node_idx_148")
             (bytevector-u32-ref #f "r6rs-lib-Z-H-3.html" "node_idx_140")
             (bytevector-u32-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_152")
             (bytevector-u32-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_144")
             (bytevector-u16-set! #f "r6rs-lib-Z-H-3.html" "node_idx_132")
             (bytevector-u16-ref #f "r6rs-lib-Z-H-3.html" "node_idx_124")
             (bytevector-u16-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_136")
             (bytevector-u16-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_128")
             (bytevector-sint-set! #f "r6rs-lib-Z-H-3.html" "node_idx_114")
             (bytevector-sint-ref #f "r6rs-lib-Z-H-3.html" "node_idx_110")
             (bytevector-s8-set! #f "r6rs-lib-Z-H-3.html" "node_idx_102")
             (bytevector-s8-ref #f "r6rs-lib-Z-H-3.html" "node_idx_98")
             (bytevector-s64-set! #f "r6rs-lib-Z-H-3.html" "node_idx_166")
             (bytevector-s64-ref #f "r6rs-lib-Z-H-3.html" "node_idx_158")
             (bytevector-s64-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_170")
             (bytevector-s64-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_162")
             (bytevector-s32-set! #f "r6rs-lib-Z-H-3.html" "node_idx_150")
             (bytevector-s32-ref #f "r6rs-lib-Z-H-3.html" "node_idx_142")
             (bytevector-s32-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_154")
             (bytevector-s32-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_146")
             (bytevector-s16-set! #f "r6rs-lib-Z-H-3.html" "node_idx_134")
             (bytevector-s16-ref #f "r6rs-lib-Z-H-3.html" "node_idx_126")
             (bytevector-s16-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_138")
             (bytevector-s16-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_130")
             (bytevector-length #f "r6rs-lib-Z-H-3.html" "node_idx_86")
             (bytevector-ieee-single-ref #f "r6rs-lib-Z-H-3.html" "node_idx_174")
             (bytevector-ieee-single-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_180")
             (bytevector-ieee-single-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_172")
             (bytevector-ieee-double-ref #f "r6rs-lib-Z-H-3.html" "node_idx_178")
             (bytevector-ieee-double-native-set! #f "r6rs-lib-Z-H-3.html" "node_idx_182")
             (bytevector-ieee-double-native-ref #f "r6rs-lib-Z-H-3.html" "node_idx_176")
             (bytevector-fill! #f "r6rs-lib-Z-H-3.html" "node_idx_90")
             (bytevector-copy! #f "r6rs-lib-Z-H-3.html" "node_idx_92")
             (bytevector-copy #f "r6rs-lib-Z-H-3.html" "node_idx_94")
             (bytevector->uint-list #f "r6rs-lib-Z-H-3.html" "node_idx_116")
             (bytevector->u8-list #f "r6rs-lib-Z-H-3.html" "node_idx_104")
             (bytevector->sint-list #f "r6rs-lib-Z-H-3.html" "node_idx_118")]

@r6rs-module[rnrs/lists-6 (rnrs lists (6))
             "r6rs-lib-Z-H-4.html" "node_idx_200" "List utilities"
             (remv #f "r6rs-lib-Z-H-4.html" "node_idx_220")
             (remq #f "r6rs-lib-Z-H-4.html" "node_idx_222")
             (remp #f "r6rs-lib-Z-H-4.html" "node_idx_216")
             (remove #f "r6rs-lib-Z-H-4.html" "node_idx_218")
             (partition #f "r6rs-lib-Z-H-4.html" "node_idx_210")
             (memv #f "r6rs-lib-Z-H-4.html" "node_idx_228")
             (memq #f "r6rs-lib-Z-H-4.html" "node_idx_230")
             (memp #f "r6rs-lib-Z-H-4.html" "node_idx_224")
             (member #f "r6rs-lib-Z-H-4.html" "node_idx_226")
             (for-all #f "r6rs-lib-Z-H-4.html" "node_idx_204")
             (fold-right #f "r6rs-lib-Z-H-4.html" "node_idx_214")
             (fold-left #f "r6rs-lib-Z-H-4.html" "node_idx_212")
             (find #f "r6rs-lib-Z-H-4.html" "node_idx_202")
             (filter #f "r6rs-lib-Z-H-4.html" "node_idx_208")
             (exists #f "r6rs-lib-Z-H-4.html" "node_idx_206")
             (cons* #f "r6rs-lib-Z-H-4.html" "node_idx_240")
             (assv #f "r6rs-lib-Z-H-4.html" "node_idx_236")
             (assq #f "r6rs-lib-Z-H-4.html" "node_idx_238")
             (assp #f "r6rs-lib-Z-H-4.html" "node_idx_232")
             (assoc #f "r6rs-lib-Z-H-4.html" "node_idx_234")]

@r6rs-module[rnrs/sorting-6 (rnrs sorting (6))
             "r6rs-lib-Z-H-5.html" "node_idx_244" "Sorting"
             (vector-sort! #f "r6rs-lib-Z-H-5.html" "node_idx_250")
             (vector-sort #f "r6rs-lib-Z-H-5.html" "node_idx_248")
             (list-sort #f "r6rs-lib-Z-H-5.html" "node_idx_246")]

@r6rs-module[rnrs/control-6 (rnrs control (6))
             "r6rs-lib-Z-H-6.html" "node_idx_252" "Control Structures"
             (when #t "r6rs-lib-Z-H-6.html" "node_idx_254")
             (unless #t "r6rs-lib-Z-H-6.html" "node_idx_256")
             (do #t "r6rs-lib-Z-H-6.html" "node_idx_258")
             (case-lambda #t "r6rs-lib-Z-H-6.html" "node_idx_262")]

@r6rs-module[rnrs/records/syntactic-6 (rnrs records syntactic (6))
             "r6rs-lib-Z-H-7.html" "node_idx_286" "Records: Syntactic"
             (sealed #t "r6rs-lib-Z-H-7.html" "node_idx_300")
             (record-type-descriptor #t "r6rs-lib-Z-H-7.html" "node_idx_308")
             (record-constructor-descriptor #t "r6rs-lib-Z-H-7.html" "node_idx_310")
             (protocol #t "r6rs-lib-Z-H-7.html" "node_idx_298")
             (parent-rtd #t "r6rs-lib-Z-H-7.html" "node_idx_306")
             (parent #t "r6rs-lib-Z-H-7.html" "node_idx_296")
             (opaque #t "r6rs-lib-Z-H-7.html" "node_idx_302")
             (nongenerative #t "r6rs-lib-Z-H-7.html" "node_idx_304")
             (mutable #t "r6rs-lib-Z-H-7.html" "node_idx_292")
             (immutable #t "r6rs-lib-Z-H-7.html" "node_idx_294")
             (fields #t "r6rs-lib-Z-H-7.html" "node_idx_290")
             (define-record-type #t "r6rs-lib-Z-H-7.html" "node_idx_288")]

@r6rs-module[rnrs/records/procedural-6 (rnrs records procedural (6))
             "r6rs-lib-Z-H-7.html" "node_idx_312" "Records: Procedural"
             (record-type-descriptor? #f "r6rs-lib-Z-H-7.html" "node_idx_320")
             (record-predicate #f "r6rs-lib-Z-H-7.html" "node_idx_334")
             (record-mutator #f "r6rs-lib-Z-H-7.html" "node_idx_338")
             (record-constructor #f "r6rs-lib-Z-H-7.html" "node_idx_332")
             (record-accessor #f "r6rs-lib-Z-H-7.html" "node_idx_336")
             (make-record-type-descriptor #f "r6rs-lib-Z-H-7.html" "node_idx_314")
             (make-record-constructor-descriptor #f "r6rs-lib-Z-H-7.html" "node_idx_322")]

@r6rs-module[rnrs/records/inspection-6 (rnrs records inspection (6))
             "r6rs-lib-Z-H-7.html" "node_idx_340" "Records: Inspection"
             (record? #f "r6rs-lib-Z-H-7.html" "node_idx_342")
             (record-type-uid #f "r6rs-lib-Z-H-7.html" "node_idx_350")
             (record-type-sealed? #f "r6rs-lib-Z-H-7.html" "node_idx_354")
             (record-type-parent #f "r6rs-lib-Z-H-7.html" "node_idx_348")
             (record-type-opaque? #f "r6rs-lib-Z-H-7.html" "node_idx_356")
             (record-type-name #f "r6rs-lib-Z-H-7.html" "node_idx_346")
             (record-type-generative? #f "r6rs-lib-Z-H-7.html" "node_idx_352")
             (record-type-field-names #f "r6rs-lib-Z-H-7.html" "node_idx_358")
             (record-rtd #f "r6rs-lib-Z-H-7.html" "node_idx_344")
             (record-field-mutable? #f "r6rs-lib-Z-H-7.html" "node_idx_360")]

@r6rs-module[rnrs/exceptions-6 (rnrs exceptions (6))
             "r6rs-lib-Z-H-8.html" "node_idx_364" "Exceptions"
             (with-exception-handler #f "r6rs-lib-Z-H-8.html" "node_idx_368")
             (raise-continuable #f "r6rs-lib-Z-H-8.html" "node_idx_378")
             (raise #f "r6rs-lib-Z-H-8.html" "node_idx_376")
             (guard #t "r6rs-lib-Z-H-8.html" "node_idx_370")
             (else #t "r6rs-lib-Z-H-8.html" "node_idx_374")
             (=> #t "r6rs-lib-Z-H-8.html" "node_idx_372")]

See also @secref["conformance"].

@r6rs-module[rnrs/conditions-6 (rnrs conditions (6))
             "r6rs-lib-Z-H-8.html" "node_idx_382" "Conditions"
             (who-condition? #f "r6rs-lib-Z-H-8.html" "node_idx_456")
             (warning? #f "r6rs-lib-Z-H-8.html" "node_idx_418")
             (violation? #f "r6rs-lib-Z-H-8.html" "node_idx_436")
             (undefined-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_492")
             (syntax-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_482")
             (syntax-violation-subform #f "r6rs-lib-Z-H-8.html" "node_idx_486")
             (syntax-violation-form #f "r6rs-lib-Z-H-8.html" "node_idx_484")
             (simple-conditions #f "r6rs-lib-Z-H-8.html" "node_idx_396")
             (serious-condition? #f "r6rs-lib-Z-H-8.html" "node_idx_424")
             (non-continuable-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_464")
             (message-condition? #f "r6rs-lib-Z-H-8.html" "node_idx_410")
             (make-who-condition #f "r6rs-lib-Z-H-8.html" "node_idx_454")
             (make-warning #f "r6rs-lib-Z-H-8.html" "node_idx_416")
             (make-violation #f "r6rs-lib-Z-H-8.html" "node_idx_434")
             (make-undefined-violation #f "r6rs-lib-Z-H-8.html" "node_idx_490")
             (make-syntax-violation #f "r6rs-lib-Z-H-8.html" "node_idx_480")
             (make-serious-condition #f "r6rs-lib-Z-H-8.html" "node_idx_422")
             (make-non-continuable-violation #f "r6rs-lib-Z-H-8.html" "node_idx_462")
             (make-message-condition #f "r6rs-lib-Z-H-8.html" "node_idx_408")
             (make-lexical-violation #f "r6rs-lib-Z-H-8.html" "node_idx_474")
             (make-irritants-condition #f "r6rs-lib-Z-H-8.html" "node_idx_446")
             (make-implementation-restriction-violation #f "r6rs-lib-Z-H-8.html" "node_idx_468")
             (make-error #f "r6rs-lib-Z-H-8.html" "node_idx_428")
             (make-assertion-violation #f "r6rs-lib-Z-H-8.html" "node_idx_440")
             (lexical-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_476")
             (irritants-condition? #f "r6rs-lib-Z-H-8.html" "node_idx_448")
             (implementation-restriction-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_470")
             (error? #f "r6rs-lib-Z-H-8.html" "node_idx_430")
             (define-condition-type #t "r6rs-lib-Z-H-8.html" "node_idx_404")
             (condition? #f "r6rs-lib-Z-H-8.html" "node_idx_398")
             (condition-who #f "r6rs-lib-Z-H-8.html" "node_idx_458")
             (condition-predicate #f "r6rs-lib-Z-H-8.html" "node_idx_400")
             (condition-message #f "r6rs-lib-Z-H-8.html" "node_idx_412")
             (condition-irritants #f "r6rs-lib-Z-H-8.html" "node_idx_450")
             (condition-accessor #f "r6rs-lib-Z-H-8.html" "node_idx_402")
             (condition #f "r6rs-lib-Z-H-8.html" "node_idx_394")
             (assertion-violation? #f "r6rs-lib-Z-H-8.html" "node_idx_442")
             (&who #f "r6rs-lib-Z-H-8.html" "node_idx_452")
             (&warning #f "r6rs-lib-Z-H-8.html" "node_idx_414")
             (&violation #f "r6rs-lib-Z-H-8.html" "node_idx_432")
             (&undefined #f "r6rs-lib-Z-H-8.html" "node_idx_488")
             (&syntax #f "r6rs-lib-Z-H-8.html" "node_idx_478")
             (&serious #f "r6rs-lib-Z-H-8.html" "node_idx_420")
             (&non-continuable #f "r6rs-lib-Z-H-8.html" "node_idx_460")
             (&message #f "r6rs-lib-Z-H-8.html" "node_idx_406")
             (&lexical #f "r6rs-lib-Z-H-8.html" "node_idx_472")
             (&irritants #f "r6rs-lib-Z-H-8.html" "node_idx_444")
             (&implementation-restriction #f "r6rs-lib-Z-H-8.html" "node_idx_466")
             (&error #f "r6rs-lib-Z-H-8.html" "node_idx_426")
             (&condition #f "r6rs-lib-Z-H-8.html" "node_idx_392")
             (&assertion #f "r6rs-lib-Z-H-8.html" "node_idx_438")]

@r6rs-module[rnrs/io/ports-6 (rnrs io ports (6))
             "r6rs-lib-Z-H-9.html" "node_idx_560" "I/O: Ports"
             (utf-8-codec #f "r6rs-lib-Z-H-9.html" "node_idx_592")
             (utf-16-codec #f "r6rs-lib-Z-H-9.html" "node_idx_594")
             (transcoder-error-handling-mode #f "r6rs-lib-Z-H-9.html" "node_idx_628")
             (transcoder-eol-style #f "r6rs-lib-Z-H-9.html" "node_idx_626")
             (transcoder-codec #f "r6rs-lib-Z-H-9.html" "node_idx_624")
             (transcoded-port #f "r6rs-lib-Z-H-9.html" "node_idx_650")
             (textual-port? #f "r6rs-lib-Z-H-9.html" "node_idx_646")
             (string->bytevector #f "r6rs-lib-Z-H-9.html" "node_idx_632")
             (standard-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_730")
             (standard-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_676")
             (standard-error-port #f "r6rs-lib-Z-H-9.html" "node_idx_732")
             (set-port-position! #f "r6rs-lib-Z-H-9.html" "node_idx_658")
             (put-u8 #f "r6rs-lib-Z-H-9.html" "node_idx_742")
             (put-string #f "r6rs-lib-Z-H-9.html" "node_idx_750")
             (put-datum #f "r6rs-lib-Z-H-9.html" "node_idx_756")
             (put-char #f "r6rs-lib-Z-H-9.html" "node_idx_748")
             (put-bytevector #f "r6rs-lib-Z-H-9.html" "node_idx_744")
             (port? #f "r6rs-lib-Z-H-9.html" "node_idx_642")
             (port-transcoder #f "r6rs-lib-Z-H-9.html" "node_idx_644")
             (port-position #f "r6rs-lib-Z-H-9.html" "node_idx_654")
             (port-has-set-port-position!? #f "r6rs-lib-Z-H-9.html" "node_idx_656")
             (port-has-port-position? #f "r6rs-lib-Z-H-9.html" "node_idx_652")
             (port-eof? #f "r6rs-lib-Z-H-9.html" "node_idx_666")
             (output-port? #f "r6rs-lib-Z-H-9.html" "node_idx_710")
             (output-port-buffer-mode #f "r6rs-lib-Z-H-9.html" "node_idx_714")
             (open-string-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_726")
             (open-string-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_674")
             (open-file-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_716")
             (open-file-input/output-port #f "r6rs-lib-Z-H-9.html" "node_idx_758")
             (open-file-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_668")
             (open-bytevector-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_720")
             (open-bytevector-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_672")
             (native-transcoder #f "r6rs-lib-Z-H-9.html" "node_idx_622")
             (native-eol-style #f "r6rs-lib-Z-H-9.html" "node_idx_598")
             (make-transcoder #f "r6rs-lib-Z-H-9.html" "node_idx_616")
             (make-i/o-write-error #f "r6rs-lib-Z-H-9.html" "node_idx_508")
             (make-i/o-read-error #f "r6rs-lib-Z-H-9.html" "node_idx_502")
             (make-i/o-port-error #f "r6rs-lib-Z-H-9.html" "node_idx_554")
             (make-i/o-invalid-position-error #f "r6rs-lib-Z-H-9.html" "node_idx_514")
             (make-i/o-filename-error #f "r6rs-lib-Z-H-9.html" "node_idx_522")
             (make-i/o-file-protection-error #f "r6rs-lib-Z-H-9.html" "node_idx_530")
             (make-i/o-file-is-read-only-error #f "r6rs-lib-Z-H-9.html" "node_idx_536")
             (make-i/o-file-does-not-exist-error #f "r6rs-lib-Z-H-9.html" "node_idx_548")
             (make-i/o-file-already-exists-error #f "r6rs-lib-Z-H-9.html" "node_idx_542")
             (make-i/o-error #f "r6rs-lib-Z-H-9.html" "node_idx_496")
             (make-i/o-encoding-error #f "r6rs-lib-Z-H-9.html" "node_idx_608")
             (make-i/o-decoding-error #f "r6rs-lib-Z-H-9.html" "node_idx_602")
             (make-custom-textual-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_740")
             (make-custom-textual-input/output-port #f "r6rs-lib-Z-H-9.html" "node_idx_764")
             (make-custom-textual-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_682")
             (make-custom-binary-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_738")
             (make-custom-binary-input/output-port #f "r6rs-lib-Z-H-9.html" "node_idx_762")
             (make-custom-binary-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_680")
             (lookahead-u8 #f "r6rs-lib-Z-H-9.html" "node_idx_686")
             (lookahead-char #f "r6rs-lib-Z-H-9.html" "node_idx_698")
             (latin-1-codec #f "r6rs-lib-Z-H-9.html" "node_idx_590")
             (input-port? #f "r6rs-lib-Z-H-9.html" "node_idx_664")
             (i/o-write-error? #f "r6rs-lib-Z-H-9.html" "node_idx_510")
             (i/o-read-error? #f "r6rs-lib-Z-H-9.html" "node_idx_504")
             (i/o-port-error? #f "r6rs-lib-Z-H-9.html" "node_idx_556")
             (i/o-invalid-position-error? #f "r6rs-lib-Z-H-9.html" "node_idx_516")
             (i/o-filename-error? #f "r6rs-lib-Z-H-9.html" "node_idx_524")
             (i/o-file-protection-error? #f "r6rs-lib-Z-H-9.html" "node_idx_532")
             (i/o-file-is-read-only-error? #f "r6rs-lib-Z-H-9.html" "node_idx_538")
             (i/o-file-does-not-exist-error? #f "r6rs-lib-Z-H-9.html" "node_idx_550")
             (i/o-file-already-exists-error? #f "r6rs-lib-Z-H-9.html" "node_idx_544")
             (i/o-error? #f "r6rs-lib-Z-H-9.html" "node_idx_498")
             (i/o-error-position #f "r6rs-lib-Z-H-9.html" "node_idx_518")
             (i/o-error-port #f "r6rs-lib-Z-H-9.html" "node_idx_558")
             (i/o-error-filename #f "r6rs-lib-Z-H-9.html" "node_idx_526")
             (i/o-encoding-error? #f "r6rs-lib-Z-H-9.html" "node_idx_610")
             (i/o-encoding-error-char #f "r6rs-lib-Z-H-9.html" "node_idx_612")
             (i/o-decoding-error? #f "r6rs-lib-Z-H-9.html" "node_idx_604")
             (get-u8 #f "r6rs-lib-Z-H-9.html" "node_idx_684")
             (get-string-n! #f "r6rs-lib-Z-H-9.html" "node_idx_702")
             (get-string-n #f "r6rs-lib-Z-H-9.html" "node_idx_700")
             (get-string-all #f "r6rs-lib-Z-H-9.html" "node_idx_704")
             (get-line #f "r6rs-lib-Z-H-9.html" "node_idx_706")
             (get-datum #f "r6rs-lib-Z-H-9.html" "node_idx_708")
             (get-char #f "r6rs-lib-Z-H-9.html" "node_idx_696")
             (get-bytevector-some #f "r6rs-lib-Z-H-9.html" "node_idx_692")
             (get-bytevector-n! #f "r6rs-lib-Z-H-9.html" "node_idx_690")
             (get-bytevector-n #f "r6rs-lib-Z-H-9.html" "node_idx_688")
             (get-bytevector-all #f "r6rs-lib-Z-H-9.html" "node_idx_694")
             (flush-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_712")
             (file-options #t "r6rs-lib-Z-H-9.html" "node_idx_574")
             (error-handling-mode #t "r6rs-lib-Z-H-9.html" "node_idx_614")
             (eol-style #t "r6rs-lib-Z-H-9.html" "node_idx_596")
             (eof-object? #f "r6rs-lib-Z-H-9.html" "node_idx_638")
             (eof-object #f "r6rs-lib-Z-H-9.html" "node_idx_636")
             (current-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_734")
             (current-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_678")
             (current-error-port #f "r6rs-lib-Z-H-9.html" "node_idx_736")
             (close-port #f "r6rs-lib-Z-H-9.html" "node_idx_660")
             (call-with-string-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_728")
             (call-with-port #f "r6rs-lib-Z-H-9.html" "node_idx_662")
             (call-with-bytevector-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_724")
             (bytevector->string #f "r6rs-lib-Z-H-9.html" "node_idx_630")
             (buffer-mode? #f "r6rs-lib-Z-H-9.html" "node_idx_578")
             (buffer-mode #t "r6rs-lib-Z-H-9.html" "node_idx_576")
             (binary-port? #f "r6rs-lib-Z-H-9.html" "node_idx_648")
             (&i/o-write #f "r6rs-lib-Z-H-9.html" "node_idx_506")
             (&i/o-read #f "r6rs-lib-Z-H-9.html" "node_idx_500")
             (&i/o-port #f "r6rs-lib-Z-H-9.html" "node_idx_552")
             (&i/o-invalid-position #f "r6rs-lib-Z-H-9.html" "node_idx_512")
             (&i/o-filename #f "r6rs-lib-Z-H-9.html" "node_idx_520")
             (&i/o-file-protection #f "r6rs-lib-Z-H-9.html" "node_idx_528")
             (&i/o-file-is-read-only #f "r6rs-lib-Z-H-9.html" "node_idx_534")
             (&i/o-file-does-not-exist #f "r6rs-lib-Z-H-9.html" "node_idx_546")
             (&i/o-file-already-exists #f "r6rs-lib-Z-H-9.html" "node_idx_540")
             (&i/o-encoding #f "r6rs-lib-Z-H-9.html" "node_idx_606")
             (&i/o-decoding #f "r6rs-lib-Z-H-9.html" "node_idx_600")
             (&i/o #f "r6rs-lib-Z-H-9.html" "node_idx_494")]

@r6rs-module[rnrs/io/simple-6 (rnrs io simple (6))
             "r6rs-lib-Z-H-9.html" "node_idx_766" "I/O: Simple"
             (write-char #f "r6rs-lib-Z-H-9.html" "node_idx_820")
             (write #f "r6rs-lib-Z-H-9.html" "node_idx_832")
             (with-output-to-file #f "r6rs-lib-Z-H-9.html" "node_idx_798")
             (with-input-from-file #f "r6rs-lib-Z-H-9.html" "node_idx_796")
             (read-char #f "r6rs-lib-Z-H-9.html" "node_idx_808")
             (read #f "r6rs-lib-Z-H-9.html" "node_idx_816")
             (peek-char #f "r6rs-lib-Z-H-9.html" "node_idx_814")
             (open-output-file #f "r6rs-lib-Z-H-9.html" "node_idx_802")
             (open-input-file #f "r6rs-lib-Z-H-9.html" "node_idx_800")
             (newline #f "r6rs-lib-Z-H-9.html" "node_idx_824")
             (display #f "r6rs-lib-Z-H-9.html" "node_idx_828")
             (close-output-port #f "r6rs-lib-Z-H-9.html" "node_idx_806")
             (close-input-port #f "r6rs-lib-Z-H-9.html" "node_idx_804")
             (call-with-output-file #f "r6rs-lib-Z-H-9.html" "node_idx_778")
             (call-with-input-file #f "r6rs-lib-Z-H-9.html" "node_idx_776")]

@r6rs-module[rnrs/files-6 (rnrs files (6))
             "r6rs-lib-Z-H-10.html" "node_idx_836" "File System"
             (file-exists? #f "r6rs-lib-Z-H-10.html" "node_idx_838")
             (delete-file #f "r6rs-lib-Z-H-10.html" "node_idx_840")]

@r6rs-module[rnrs/programs-6 (rnrs programs (6))
             "r6rs-lib-Z-H-11.html" "node_idx_842" "Command-line Access and Exit Values"
             (exit #f "r6rs-lib-Z-H-11.html" "node_idx_846")
             (command-line #f "r6rs-lib-Z-H-11.html" "node_idx_844")]

@r6rs-module[rnrs/arithmetic/fixnums-6 (rnrs arithmetic fixnums (6))
             "r6rs-lib-Z-H-12.html" "node_idx_854" "Arithmetic: Fixnums"
             (least-fixnum #f "r6rs-lib-Z-H-12.html" "node_idx_860")
             (greatest-fixnum #f "r6rs-lib-Z-H-12.html" "node_idx_862")
             (fxzero? #f "r6rs-lib-Z-H-12.html" "node_idx_874")
             (fxxor #f "r6rs-lib-Z-H-12.html" "node_idx_920")
             (fxrotate-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_944")
             (fxreverse-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_946")
             (fxpositive? #f "r6rs-lib-Z-H-12.html" "node_idx_876")
             (fxodd? #f "r6rs-lib-Z-H-12.html" "node_idx_880")
             (fxnot #f "r6rs-lib-Z-H-12.html" "node_idx_914")
             (fxnegative? #f "r6rs-lib-Z-H-12.html" "node_idx_878")
             (fxmod0 #f "r6rs-lib-Z-H-12.html" "node_idx_906")
             (fxmod #f "r6rs-lib-Z-H-12.html" "node_idx_900")
             (fxmin #f "r6rs-lib-Z-H-12.html" "node_idx_886")
             (fxmax #f "r6rs-lib-Z-H-12.html" "node_idx_884")
             (fxlength #f "r6rs-lib-Z-H-12.html" "node_idx_926")
             (fxior #f "r6rs-lib-Z-H-12.html" "node_idx_918")
             (fxif #f "r6rs-lib-Z-H-12.html" "node_idx_922")
             (fxfirst-bit-set #f "r6rs-lib-Z-H-12.html" "node_idx_928")
             (fxeven? #f "r6rs-lib-Z-H-12.html" "node_idx_882")
             (fxdiv0-and-mod0 #f "r6rs-lib-Z-H-12.html" "node_idx_902")
             (fxdiv0 #f "r6rs-lib-Z-H-12.html" "node_idx_904")
             (fxdiv-and-mod #f "r6rs-lib-Z-H-12.html" "node_idx_896")
             (fxdiv #f "r6rs-lib-Z-H-12.html" "node_idx_898")
             (fxcopy-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_936")
             (fxcopy-bit #f "r6rs-lib-Z-H-12.html" "node_idx_932")
             (fxbit-set? #f "r6rs-lib-Z-H-12.html" "node_idx_930")
             (fxbit-field #f "r6rs-lib-Z-H-12.html" "node_idx_934")
             (fxbit-count #f "r6rs-lib-Z-H-12.html" "node_idx_924")
             (fxarithmetic-shift-right #f "r6rs-lib-Z-H-12.html" "node_idx_942")
             (fxarithmetic-shift-left #f "r6rs-lib-Z-H-12.html" "node_idx_940")
             (fxarithmetic-shift #f "r6rs-lib-Z-H-12.html" "node_idx_938")
             (fxand #f "r6rs-lib-Z-H-12.html" "node_idx_916")
             (fx>? #f "r6rs-lib-Z-H-12.html" "node_idx_866")
             (fx>=? #f "r6rs-lib-Z-H-12.html" "node_idx_870")
             (fx=? #f "r6rs-lib-Z-H-12.html" "node_idx_864")
             (fx<? #f "r6rs-lib-Z-H-12.html" "node_idx_868")
             (fx<=? #f "r6rs-lib-Z-H-12.html" "node_idx_872")
             (fx-/carry #f "r6rs-lib-Z-H-12.html" "node_idx_910")
             (fx- #f "r6rs-lib-Z-H-12.html" "node_idx_892")
             (fx+/carry #f "r6rs-lib-Z-H-12.html" "node_idx_908")
             (fx+ #f "r6rs-lib-Z-H-12.html" "node_idx_888")
             (fx*/carry #f "r6rs-lib-Z-H-12.html" "node_idx_912")
             (fx* #f "r6rs-lib-Z-H-12.html" "node_idx_890")
             (fixnum? #f "r6rs-lib-Z-H-12.html" "node_idx_856")
             (fixnum-width #f "r6rs-lib-Z-H-12.html" "node_idx_858")]

@r6rs-module[rnrs/arithmetic/flonums-6 (rnrs arithmetic flonums (6))
             "r6rs-lib-Z-H-12.html" "node_idx_948" "Arithmetic: Flonums"
             (real->flonum #f "r6rs-lib-Z-H-12.html" "node_idx_952")
             (no-nans-violation? #f "r6rs-lib-Z-H-12.html" "node_idx_1058")
             (no-infinities-violation? #f "r6rs-lib-Z-H-12.html" "node_idx_1052")
             (make-no-nans-violation #f "r6rs-lib-Z-H-12.html" "node_idx_1056")
             (make-no-infinities-violation #f "r6rs-lib-Z-H-12.html" "node_idx_1050")
             (flzero? #f "r6rs-lib-Z-H-12.html" "node_idx_966")
             (fltruncate #f "r6rs-lib-Z-H-12.html" "node_idx_1020")
             (fltan #f "r6rs-lib-Z-H-12.html" "node_idx_1034")
             (flsqrt #f "r6rs-lib-Z-H-12.html" "node_idx_1044")
             (flsin #f "r6rs-lib-Z-H-12.html" "node_idx_1030")
             (flround #f "r6rs-lib-Z-H-12.html" "node_idx_1022")
             (flpositive? #f "r6rs-lib-Z-H-12.html" "node_idx_968")
             (flonum? #f "r6rs-lib-Z-H-12.html" "node_idx_950")
             (flodd? #f "r6rs-lib-Z-H-12.html" "node_idx_972")
             (flnumerator #f "r6rs-lib-Z-H-12.html" "node_idx_1012")
             (flnegative? #f "r6rs-lib-Z-H-12.html" "node_idx_970")
             (flnan? #f "r6rs-lib-Z-H-12.html" "node_idx_980")
             (flmod0 #f "r6rs-lib-Z-H-12.html" "node_idx_1010")
             (flmod #f "r6rs-lib-Z-H-12.html" "node_idx_1004")
             (flmin #f "r6rs-lib-Z-H-12.html" "node_idx_984")
             (flmax #f "r6rs-lib-Z-H-12.html" "node_idx_982")
             (fllog #f "r6rs-lib-Z-H-12.html" "node_idx_1026")
             (flinteger? #f "r6rs-lib-Z-H-12.html" "node_idx_964")
             (flinfinite? #f "r6rs-lib-Z-H-12.html" "node_idx_978")
             (flfloor #f "r6rs-lib-Z-H-12.html" "node_idx_1016")
             (flfinite? #f "r6rs-lib-Z-H-12.html" "node_idx_976")
             (flexpt #f "r6rs-lib-Z-H-12.html" "node_idx_1046")
             (flexp #f "r6rs-lib-Z-H-12.html" "node_idx_1024")
             (fleven? #f "r6rs-lib-Z-H-12.html" "node_idx_974")
             (fldiv0-and-mod0 #f "r6rs-lib-Z-H-12.html" "node_idx_1006")
             (fldiv0 #f "r6rs-lib-Z-H-12.html" "node_idx_1008")
             (fldiv-and-mod #f "r6rs-lib-Z-H-12.html" "node_idx_1000")
             (fldiv #f "r6rs-lib-Z-H-12.html" "node_idx_1002")
             (fldenominator #f "r6rs-lib-Z-H-12.html" "node_idx_1014")
             (flcos #f "r6rs-lib-Z-H-12.html" "node_idx_1032")
             (flceiling #f "r6rs-lib-Z-H-12.html" "node_idx_1018")
             (flatan #f "r6rs-lib-Z-H-12.html" "node_idx_1040")
             (flasin #f "r6rs-lib-Z-H-12.html" "node_idx_1036")
             (flacos #f "r6rs-lib-Z-H-12.html" "node_idx_1038")
             (flabs #f "r6rs-lib-Z-H-12.html" "node_idx_998")
             (fl>? #f "r6rs-lib-Z-H-12.html" "node_idx_960")
             (fl>=? #f "r6rs-lib-Z-H-12.html" "node_idx_962")
             (fl=? #f "r6rs-lib-Z-H-12.html" "node_idx_954")
             (fl<? #f "r6rs-lib-Z-H-12.html" "node_idx_956")
             (fl<=? #f "r6rs-lib-Z-H-12.html" "node_idx_958")
             (fl/ #f "r6rs-lib-Z-H-12.html" "node_idx_994")
             (fl- #f "r6rs-lib-Z-H-12.html" "node_idx_990")
             (fl+ #f "r6rs-lib-Z-H-12.html" "node_idx_986")
             (fl* #f "r6rs-lib-Z-H-12.html" "node_idx_988")
             (fixnum->flonum #f "r6rs-lib-Z-H-12.html" "node_idx_1060")
             (&no-nans #f "r6rs-lib-Z-H-12.html" "node_idx_1054")
             (&no-infinities #f "r6rs-lib-Z-H-12.html" "node_idx_1048")]

@r6rs-module[rnrs/arithmetic/bitwise-6 (rnrs arithmetic bitwise (6))
             "r6rs-lib-Z-H-12.html" "node_idx_1062" "Arithmetic: Bitwise"
             (bitwise-xor #f "r6rs-lib-Z-H-12.html" "node_idx_1070")
             (bitwise-rotate-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_1094")
             (bitwise-reverse-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_1096")
             (bitwise-not #f "r6rs-lib-Z-H-12.html" "node_idx_1064")
             (bitwise-length #f "r6rs-lib-Z-H-12.html" "node_idx_1076")
             (bitwise-ior #f "r6rs-lib-Z-H-12.html" "node_idx_1068")
             (bitwise-if #f "r6rs-lib-Z-H-12.html" "node_idx_1072")
             (bitwise-first-bit-set #f "r6rs-lib-Z-H-12.html" "node_idx_1078")
             (bitwise-copy-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_1086")
             (bitwise-copy-bit #f "r6rs-lib-Z-H-12.html" "node_idx_1082")
             (bitwise-bit-set? #f "r6rs-lib-Z-H-12.html" "node_idx_1080")
             (bitwise-bit-field #f "r6rs-lib-Z-H-12.html" "node_idx_1084")
             (bitwise-bit-count #f "r6rs-lib-Z-H-12.html" "node_idx_1074")
             (bitwise-arithmetic-shift-right #f "r6rs-lib-Z-H-12.html" "node_idx_1092")
             (bitwise-arithmetic-shift-left #f "r6rs-lib-Z-H-12.html" "node_idx_1090")
             (bitwise-arithmetic-shift #f "r6rs-lib-Z-H-12.html" "node_idx_1088")
             (bitwise-and #f "r6rs-lib-Z-H-12.html" "node_idx_1066")]

@r6rs-module[rnrs/syntax-case-6 (rnrs syntax-case (6))
             "r6rs-lib-Z-H-13.html" "node_idx_1098" "Syntax-Case"
             (with-syntax #t "r6rs-lib-Z-H-13.html" "node_idx_1152")
             (unsyntax-splicing #t "r6rs-lib-Z-H-13.html" "node_idx_1158")
             (unsyntax #t "r6rs-lib-Z-H-13.html" "node_idx_1156")
             (syntax-violation #f "r6rs-lib-Z-H-13.html" "node_idx_1160")
             (syntax-case #t "r6rs-lib-Z-H-13.html" "node_idx_1124")
             (syntax->datum #f "r6rs-lib-Z-H-13.html" "node_idx_1144")
             (syntax #t "r6rs-lib-Z-H-13.html" "node_idx_1134")
             (quasisyntax #t "r6rs-lib-Z-H-13.html" "node_idx_1154")
             (make-variable-transformer #f "r6rs-lib-Z-H-13.html" "node_idx_1120")
             (identifier? #f "r6rs-lib-Z-H-13.html" "node_idx_1138")
             (generate-temporaries #f "r6rs-lib-Z-H-13.html" "node_idx_1150")
             (free-identifier=? #f "r6rs-lib-Z-H-13.html" "node_idx_1142")
             (datum->syntax #f "r6rs-lib-Z-H-13.html" "node_idx_1146")
             (bound-identifier=? #f "r6rs-lib-Z-H-13.html" "node_idx_1140")
             (_ #t "r6rs-lib-Z-H-13.html" "node_idx_1126")
             (... #t "r6rs-lib-Z-H-13.html" "node_idx_1128")]

@r6rs-module[rnrs/hashtables-6 (rnrs hashtables (6))
             "r6rs-lib-Z-H-14.html" "node_idx_1164" "Hashtables"
             (symbol-hash #f "r6rs-lib-Z-H-14.html" "node_idx_1224")
             (string-hash #f "r6rs-lib-Z-H-14.html" "node_idx_1220")
             (string-ci-hash #f "r6rs-lib-Z-H-14.html" "node_idx_1222")
             (make-hashtable #f "r6rs-lib-Z-H-14.html" "node_idx_1182")
             (make-eqv-hashtable #f "r6rs-lib-Z-H-14.html" "node_idx_1178")
             (make-eq-hashtable #f "r6rs-lib-Z-H-14.html" "node_idx_1174")
             (hashtable? #f "r6rs-lib-Z-H-14.html" "node_idx_1186")
             (hashtable-update! #f "r6rs-lib-Z-H-14.html" "node_idx_1198")
             (hashtable-size #f "r6rs-lib-Z-H-14.html" "node_idx_1188")
             (hashtable-set! #f "r6rs-lib-Z-H-14.html" "node_idx_1192")
             (hashtable-ref #f "r6rs-lib-Z-H-14.html" "node_idx_1190")
             (hashtable-mutable? #f "r6rs-lib-Z-H-14.html" "node_idx_1216")
             (hashtable-keys #f "r6rs-lib-Z-H-14.html" "node_idx_1208")
             (hashtable-hash-function #f "r6rs-lib-Z-H-14.html" "node_idx_1214")
             (hashtable-equivalence-function #f "r6rs-lib-Z-H-14.html" "node_idx_1212")
             (hashtable-entries #f "r6rs-lib-Z-H-14.html" "node_idx_1210")
             (hashtable-delete! #f "r6rs-lib-Z-H-14.html" "node_idx_1194")
             (hashtable-copy #f "r6rs-lib-Z-H-14.html" "node_idx_1200")
             (hashtable-contains? #f "r6rs-lib-Z-H-14.html" "node_idx_1196")
             (hashtable-clear! #f "r6rs-lib-Z-H-14.html" "node_idx_1204")
             (equal-hash #f "r6rs-lib-Z-H-14.html" "node_idx_1218")]

A hashtable is a dictionary in the sense of
@racketmodname[racket/dict], and hash table operations interact with
threads in the same way for hash tables created with
@racket[make-hash] (e.g., @racket[hashtable-ref] and
@racket[hashtable-set!] are thread-safe).

@r6rs-module[rnrs/enums-6 (rnrs enums (6))
             "r6rs-lib-Z-H-15.html" "node_idx_1226" "Enumerations"
             (make-enumeration #f "r6rs-lib-Z-H-15.html" "node_idx_1236")
             (enum-set=? #f "r6rs-lib-Z-H-15.html" "node_idx_1250")
             (enum-set-universe #f "r6rs-lib-Z-H-15.html" "node_idx_1238")
             (enum-set-union #f "r6rs-lib-Z-H-15.html" "node_idx_1252")
             (enum-set-subset? #f "r6rs-lib-Z-H-15.html" "node_idx_1248")
             (enum-set-projection #f "r6rs-lib-Z-H-15.html" "node_idx_1260")
             (enum-set-member? #f "r6rs-lib-Z-H-15.html" "node_idx_1246")
             (enum-set-intersection #f "r6rs-lib-Z-H-15.html" "node_idx_1254")
             (enum-set-indexer #f "r6rs-lib-Z-H-15.html" "node_idx_1240")
             (enum-set-difference #f "r6rs-lib-Z-H-15.html" "node_idx_1256")
             (enum-set-constructor #f "r6rs-lib-Z-H-15.html" "node_idx_1242")
             (enum-set-complement #f "r6rs-lib-Z-H-15.html" "node_idx_1258")
             (enum-set->list #f "r6rs-lib-Z-H-15.html" "node_idx_1244")
             (define-enumeration #t "r6rs-lib-Z-H-15.html" "node_idx_1262")]

@r6rs-module[rnrs/eval-6 (rnrs eval (6))
             "r6rs-lib-Z-H-17.html" "node_idx_1265.5" "Eval"
             (eval #f "r6rs-lib-Z-H-17.html" "node_idx_1266")
             (environment #f "r6rs-lib-Z-H-17.html" "node_idx_1270")]

@r6rs-module[rnrs/mutable-pairs-6 (rnrs mutable-pairs (6))
             "r6rs-lib-Z-H-18.html" "node_idx_1272" "Mutable Pairs"
             (set-cdr! #f "r6rs-lib-Z-H-18.html" "node_idx_1276")
             (set-car! #f "r6rs-lib-Z-H-18.html" "node_idx_1274")]

@r6rs-module[rnrs/mutable-strings-6 (rnrs mutable-strings (6))
             "r6rs-lib-Z-H-19.html" "node_idx_1278" "Mutable Strings"
             (string-set! #f "r6rs-lib-Z-H-19.html" "node_idx_1280")
             (string-fill! #f "r6rs-lib-Z-H-19.html" "node_idx_1282")]

@r6rs-module[rnrs/r5rs-6 (rnrs r5rs (6))
             "r6rs-lib-Z-H-20.html" "node_idx_1284" "R5RS Compatibility"
             (scheme-report-environment #f "r6rs-lib-Z-H-20.html" "node_idx_1308")
             (remainder #f "r6rs-lib-Z-H-20.html" "node_idx_1292")
             (quotient #f "r6rs-lib-Z-H-20.html" "node_idx_1290")
             (null-environment #f "r6rs-lib-Z-H-20.html" "node_idx_1306")
             (modulo #f "r6rs-lib-Z-H-20.html" "node_idx_1294")
             (inexact->exact #f "r6rs-lib-Z-H-20.html" "node_idx_1288")
             (force #f "r6rs-lib-Z-H-20.html" "node_idx_1304")
             (exact->inexact #f "r6rs-lib-Z-H-20.html" "node_idx_1286")
             (delay #t "r6rs-lib-Z-H-20.html" "node_idx_1296")]

See also @secref["conformance"].

@; ----------------------------------------

@index-section[]

