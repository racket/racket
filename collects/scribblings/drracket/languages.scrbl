#lang scribble/doc
@(require "common.rkt"
          (for-label errortrace/errortrace-lib compiler/cm planet/config))

@title[#:tag "languages" #:style 'toc]{Languages}

This chapter describes some of the languages that are available for
use within DrRacket. The list here is potentially incomplete, because
new languages can be added through DrRacket plug-ins.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "module"]{Language Declared in Source}

The @as-index{@drlang{Use the language declared in the source} mode}
in DrRacket is a kind of meta-language, where the program itself
specifies its language, usually through a @hash-lang[] line.

More generally, when using the declared-in-source mode, the
@tech{definitions window} must contain a module in some form. Besides
@hash-lang[], a Racket module can be written as @racket[(module
...)]. In any case, aside from comments, the @tech{definitions window}
must contain exactly one module.

In the details pane of the module language, some of the configuration
options correspond to using various libraries and thus can be used
without DrRacket.  Here's how, for the ones that are straightforward
(the ones not mentioned here require more sophisticated configuration
of various libraries).

@itemize[
 @item{@bold{Dynamic Properties}: 
        The radio buttons corresond to various uses of the @racketmodname[errortrace/errortrace-lib] library.
        
        The @italic{No Debugging or profiling} option means not to use the library at all.
        
        The @italic{Debugging} option means @racket[(current-compile (make-errortrace-compile-handler))] as well as
        adding @racket[(build-path "compiled" "errortrace")] to @racket[use-compiled-file-paths].
        
        The @italic{Debugging and profiling} option means to use @racketmodname[errortrace/errortrace-lib] library's 
        @racket[profiling-enabled] in conjunction with @racket[current-eval].
        
        The @italic{Syntactic test suite coverage} option means to use @racket[test-coverage-enabled]
        in conjunction with @racket[current-eval].

        The other two checkboxes save compiled @tt{.zo} files and adjust the JIT compiler. 
        
        The @italic{populate compiled/ directories} option corresponds to 
        @racketblock[(current-load/use-compiled 
                      (make-compilation-manager-load/use-compiled-handler))
                     (manager-skip-file-handler
                      (λ (p) 
                        (file-date-in-paths 
                         p
                         (cons (CACHE-DIR) (current-library-collection-paths)))))]
        plus adding either @racket[(build-path "compiled" "drracket")] or 
        @racket[(build-path "compiled" "drracket" "errortrace")]
        to the front of @racket[use-compiled-file-paths], depending if the
        @italic{Debugging} option is set or not.
        
        The @italic{Preserve stacktrace} option corresponds to 
        @racketblock[(compile-context-preservation-enabled #t)]
        }
 @item{@bold{Output Syntax}: The output syntax options correspond to settings in the @racketmodname[racket/pretty] library 
       and the @racketmodname[mzlib/pconvert] library.}
 @item{@bold{Collection Paths}: This corresponds to setting the @racket[current-library-collection-paths] parameter.}
 @item{@bold{Command-line arguments}: This corresponds to setting the @racket[current-command-line-arguments] parameter.}
]

@; ----------------------------------------

@section[#:tag "legacy"]{Legacy Languages}

DrRacket supports several historically useful variants of Scheme
without a @hash-lang[] prefix:

@itemize[

 @item{The @as-index{@drlang{R5RS} language} contains those
  primitives and syntax defined in the R@superscript{5}RS Scheme
  standard. See the @racketmodname[r5rs] library for details.}

 @item{The @as-index{@defterm{PLT Pretty Big} language} provides a
  language roughly compatible with a language in earlier versions of
  DrRacket. It evaluates a program in the same way as @racket[load],
  and it starts by importing the following modules:
  @racketmodname[mzscheme], @racketmodname[racket/gui/base],
  @racketmodname[mzlib/class], @racketmodname[mzlib/etc],
  @racketmodname[mzlib/file], @racketmodname[mzlib/list],
  @racketmodname[mzlib/unit], @racketmodname[mzlib/include],
  @racketmodname[mzlib/defmacro], @racketmodname[mzlib/pretty],
  @racketmodname[mzlib/string], @racketmodname[mzlib/thread],
  @racketmodname[mzlib/math], @racketmodname[mzlib/match], and
  @racketmodname[mzlib/shared].}

 @item{The @as-index{@drlang{Swindle} language} starts with the same
  bindings as @racketmodname[swindle], and evaluates the program like
  @racket[load].}

]

@; ----------------------------------------

@section[#:tag "htdp-langs"]{@|HtDP| Teaching Languages}

Five of DrRacket's languages are specifically designed for teaching:

@itemize[

 @item{The @as-index{@drlang{Beginning Student} language} is a small
       version of Racket that is tailored for beginning computer
       science students.}
       
 @item{The @as-index{@drlang{Beginning Student with List
       Abbreviations} languages} is an extension to Beginning Student
       that prints lists with @racket[list] instead of @racket[cons],
       and accepts @racket[quasiquote]d input.}

 @item{The @as-index{@drlang{Intermediate Student} language} adds
       local bindings and higher-order functions.}

 @item{The @as-index{@drlang{Intermediate Student with Lambda}
       language} adds anonymous functions.}

 @item{The @as-index{@drlang{Advanced Student} language} adds mutable
       state.}

]

The teaching languages are different from conventional Racket in a number
of ways:

@itemize[

 @item{@defterm{Case-sensitive identifiers and symbols} --- In a
       case-sensitive language, the variable names @racket[x] and
       @racket[X] are distinct, and the symbols @racket['x] and
       @racket['X] are also distinct. In a case-insensitive language,
       @racket[x] and @racket[X] are equivalent and @racket['x] and
       @racket['X] represent the same value. The teaching languages
       are case-sensitive by default, and other languages are usually
       case-insensitive. Case-sensitivity can be adjusted through the
       detail section of the language-selection dialog.}
        
 @item{@defterm{All numbers are exact unless @racketmetafont{#i} is
       specified} --- In the @drlang{Beginning Student} through
       @drlang{Intermediate Student with Lambda languages}, numbers
       containing a decimal point are interpreted as exact
       numbers. This interpretation allows students to use familiar
       decimal notation without inadvertently triggering inexact
       arithmetic. Exact numbers with decimal representations are also
       printed in decimal. Inexact inputs and results are explicitly
       marked with @racketmetafont{#i}.}

 @item{@defterm{Procedures must take at least one argument} --- In the
       @drlang{Beginning Student} through @drlang{Intermediate
       Student} languages, defined procedures must consume at least
       one argument. Since the languages have no side-effects,
       zero-argument functions are not useful, and rejecting such
       function definitions helps detect confusing syntactic
       mistakes.}

 @item{@defterm{Identifier required at function call position} --- In
       the @drlang{Beginning Student} through @drlang{Intermediate
       Student} languages, procedure calls must be of the form
       @racket[(_identifier ...)]. This restriction helps detect
       confusing misuses of parentheses, such as @racket[(1)] or
       @racket[((+ 3 4))], which is a common mistake among beginners
       who are used to the optional parentheses of algebra.}

 @item{@defterm{Top-level required at function call position} --- In
       the @drlang{Beginning Student} languages, procedure calls must
       be of the form @racket[(_top-level-identifier ...)], and the
       number of actual arguments must match the number of formal
       arguments if @racket[_top-level-identifier] is
       @racket[define]d. This restriction helps detect confusing
       misuses of parentheses, such as @racket[(x)] where @racket[x]
       is a function argument. DrRacket can detect such mistakes
       syntactically because Beginning Student does not support
       higher-order procedures.}

 @item{@defterm{Primitive and defined functions allowed only in
       function call position} --- In @drlang{Beginning Student}
       languages, the name of a primitive operator or of a defined
       function can be used only after the open-parenthesis of a
       function call (except where teachpack extensions allow
       otherwise, as in the @racket[convert-gui] teachpack). Incorrect
       uses of primitives trigger a syntax error. Incorrect uses of
       defined names trigger a run-time error.  DrRacket can detect
       such mistakes because Beginning Student does not support
       higher-order procedures.}

 @item{@defterm{@racket[lambda] allowed only in definitions} --- In
       the Beginning Student through Intermediate Student languages,
       @racket[lambda] (or @racket[case-lambda]) may appear only in a
       definition, and only as the value of the defined variable.}

 @item{@defterm{Free variables are not allowed} --- In the
       @drlang{Beginning Student} through @drlang{Advanced Student}
       languages, every variable referenced in the definitions window
       must be defined, pre-defined, or the name of a local function
       argument.}

 @item{@defterm{@racket[quote] works only on symbols,
       @racket[quasiquote] disallowed} --- In the @drlang{Beginning
       Student} language, @racket[quote] and @racketvalfont{'} can
       specify only symbols. This restriction avoids the need to
       explain to beginners why @racket[1] and @racket['1] are
       equivalent in standard Racket. In addition,
       @racket[quasiquote], @litchar{`}, @racket[unquote],
       @litchar{,}, @racket[unquote-splicing], and @litchar[",@"] are
       disallowed.}

 @item{@defterm{Unmatched @racket[cond]/@racket[case] is an error} ---
       In the @drlang{Beginning Student} through @drlang{Advanced
       Student} languages, falling through a @racket[cond] or
       @racket[case] expression without matching a clause signals a
       run-time error. This convention helps detect syntactic and
       logical errors in programs.}

 @item{@defterm{Conditional values must be @racket[true] or
       @racket[false]} --- In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, an expression whose value
       is treated as a boolean must return an actual boolean,
       @racket[true] or @racket[false]. This restriction, which
       applies to @racket[if], @racket[cond], @racket[and],
       @racket[or], @racket[nand], and @racket[nor] expressions, helps
       detect errors where a boolean function application is omitted.}

 @item{@defterm{@racket[+], @racket[*], and @racket[/] take at least
       two arguments} --- In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, mathematical operators
       that are infix in algebra notation require at least two
       arguments in DrRacket. This restriction helps detect missing
       arguments to an operator.}

 @item{@defterm{@racket[and], @racket[or], @racket[nand], and
       @racket[nor] require at least 2 expressions} --- In the
       @drlang{Beginning Student} through @drlang{Advanced Student}
       languages, the boolean combination forms require at least two
       sub-expressions.  This restriction helps detect missing or
       ill-formed sub-expressions in a boolean expression.}

 @item{@defterm{@racket[set!] disallowed on arguments} --- In the
       @drlang{Advanced Student} language, @racket[set!] cannot be
       used to mutate variables bound by @racket[lambda]. This
       restriction ensures that the substitution model of function
       application is consistent with DrRacket's evaluation.}

 @item{@defterm{Improper lists disallowed} --- A @defterm{proper list}
       is either an empty list or a list created by @racket[cons]ing
       onto a proper list. In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, @racket[cons] constructs
       only @defterm{proper lists}, signaling an error if the second
       argument is not a proper list. Since beginning students do not
       need improper lists, this restriction help detect logical
       errors in recursive functions.}

 @item{@defterm{Dot is disallowed} --- In the @drlang{Beginning
       Student} through @drlang{Advanced Student} languages, a
       delimited period @litchar{.} is disallowed, (e.g., as an
       improper-list constructor in a quoted form, or for defining
       multi-arity procedures).}

 @item{@defterm{Syntactic form names disallowed as variable names} ---
       In the @drlang{Beginning Student} through @drlang{Advanced
       Student} languages, all syntactic form names are keywords that
       cannot be used as variable names.}

 @item{@defterm{Re-definitions are disallowed} --- In the
       @drlang{Beginning Student} through @drlang{Advanced Student}
       languages, top-level names can never be re-defined.}

 @item{@defterm{Function definitions are allowed only in the
       definitions window} --- In the @drlang{Beginning Student}
       languages, function definitions are not allowed in the
       interactions window.}

]

The teaching languages also deviate from traditional Racket in
printing values. Different printing formats can be selected for any
language through the detail section of language-selection dialog.

@itemize[

 @item{@defterm{Constructor-style output} --- See
       @secref["output-syntax"].}

 @item{@defterm{Quasiquote-style output} --- See
       @secref["output-syntax"].}
     
   @item{@defterm{Rational number printing} -- In the teaching
     languages, all numbers that have a finite decimal expansion are
     printed in decimal form. For those numbers that do not have a
     finite decimal expansion (such as @racket[4/3]) DrRacket offers a
     choice. It either prints them as mixed fractions or as repeating
     decimals, where the repeating portion of the decimal expansion is
     shown with an overbar. In addition, DrRacket only shows the first
     25 digits of the number's decimal expansion. If there are more
     digits, the number appears with an ellipses at the end. Click the
     ellipses to see the next 25 digits of the expansion.

     This setting controls only the initial display of a number.
     Right-clicking or Control-clicking (Mac OS X) on the number lets
     you change from the fraction representation to the decimal
     representation.}

 @item{@defterm{@racket[write] output} --- Prints values with
        @racket[write].}

 @item{@defterm{Show sharing in values} --- Prints interaction results
       using the @racket[shared] syntax, which exposes shared
       structure within a value. For example, the list created by
       @racket[(let ([lt (list 0)]) (list lt lt))] prints as

@racketblock[
(shared ((-1- (list 0))) (list -1- -1-))
]

       instead of

@racketblock[
(list (list 0) (list 0))
]
}

]

A program in the teaching languages should be tested using the check forms ---
 @racket[(check-expect value value)], @racket[(check-within value value value)], or
 @racket[(check-error value string)]. Tests are evaluated when running the program:
 when there are no tests, a warning appears in the interactions window;
 when all tests succeed, an acknowledgement appears in the interactions window;
 otherwise, a testing window appears to report the results. See @secref["menu:view"] 
 for details on configuring the report behavior. 
 
 Tests can be disabled if necessary, see @secref["menu:racket"] for details.

One final difference between these teaching languages and other languages is the way
they save files. That is, when DrRacket saves a file and the current language
is one of these five teaching languages, it inserts three lines of metadata that 
record the precise language (including any options set) and the teachpacks.
This has two benefits: opening the file later restores the settings and the metadata
is formulated in such a way as to be executable code so running @exec{racket} or 
@exec{gracket} on the file in a shell will run the program in the appropriate language.

This meta data always consists of exactly three lines, and so can be stripped out
by three calls to @racket[read-line].
 
@; ----------------------------------------

@section[#:tag "experimental-langs"]{Other Experimental Languages}

For information on @onscreen{Lazy Racket}, see @other-manual['(lib "lazy/lazy.scrbl")].

For information on @onscreen{FrTime}, see @other-manual['(lib "frtime/scribblings/frtime.scrbl")].

For information on @onscreen{Algol 60}, see @other-manual['(lib "algol60/algol60.scrbl")].

@; ----------------------------------------

@include-section["printing.scrbl"]
