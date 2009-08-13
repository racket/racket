#lang scribble/doc
@(require "common.ss"
          (for-label errortrace/errortrace-lib))

@title[#:tag "languages" #:style 'toc]{Languages}

This chapter describes some of the languages that are available for
use within DrScheme. The list here is potentially incomplete, because
new languages can be added through DrScheme plug-ins.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "module"]{Modules}

The @as-index{@drlang{Module} language} is really a kind of
meta-language, where the program itself specifies its language,
usually through a @hash-lang[] line.

More generally, when using @drlang{Module}, the @tech{definitions
window} must contain a module in some form. Besides @hash-lang[], a
Scheme module can be written as @scheme[(module ...)]. In any case,
aside from comments, the @tech{definitions window} must contain
exactly one module.

In the details pane of the module langauge, some of the
configuration options for the Module language that correspond
to using various libraries and thus can be used without DrScheme. 
Here's how, for the ones that are straightforward (the ones
not mentioned here require more sophisticated configuration
of various libraries).

@itemize[
 @item{@bold{Dynamic Properties}: 
        The radio buttons corresond to various uses of the @schememodname[errortrace/errortrace-lib] library.
        
        The @italic{No Debugging or profiling} option means not to use the library at all.
        
        The @italic{Debugging} option means @scheme[(current-compile (make-errortrace-compile-handler))] as well as
        adding @scheme[(build-path "compiled" "errortrace")] to @scheme[use-compiled-file-paths].
        
        The @italic{Debugging and profiling} option means to use @schememodname[errortrace/errortrace-lib] library's 
        @scheme[profiling-enabled] in conjunction with @scheme[current-eval].
        
        The @italic{Syntactic test suite coverage} option means to use @scheme[test-coverage-enabled]
        in conjunction with @scheme[current-eval].

        The other two checkboxes save compiled @tt{.zo} files and adjust the JIT compiler. 
        
        The @italic{populate compiled/ directories} option corresponds to 
        @schemeblock[(current-load/use-compiled 
                      (make-compilation-manager-load/use-compiled-handler))
                     (manager-skip-file-handler file-date-in-collection)]
        plus adding either @scheme[(build-path "compiled" "drscheme")] or 
        @scheme[(build-path "compiled" "drscheme" "errortrace")]
        to the front of @scheme[use-compiled-file-paths], depending if the
        @italic{Debugging} option is set or not.
        
        The @italic{Preserve stacktrace} option corresponds to 
        @schemeblock[(compile-context-preservation-enabled #t)]
        }
 @item{@bold{Output Syntax}: The output syntax options correspond to settings in the @scheme[scheme/pretty] library and the @scheme[scheme/pconvert] library.}
 @item{@bold{Collection Paths}: This corresponds to setting the @scheme[current-library-collection-paths] parameter.}
 @item{@bold{Command-line arguments}: This corresponds to setting the @scheme[current-command-line-arguments] parameter.}
]

@; ----------------------------------------

@section[#:tag "legacy"]{Legacy Languages}

DrScheme supports several historically useful variants of Scheme
without a @hash-lang[] prefix:

@itemize[

 @item{The @as-index{@drlang{R5RS} language} contains those
  primitives and syntax defined in the R@superscript{5}RS Scheme
  standard. See the @schememodname[r5rs] library for details.}

 @item{The @as-index{@defterm{PLT Pretty Big} language} provides a
  language roughly compatible with a language in earlier versions of
  DrScheme. It evaluates a program in the same way as @scheme[load],
  and it starts by importing the following modules:
  @schememodname[mzscheme], @schememodname[scheme/gui/base],
  @schememodname[mzlib/class], @schememodname[mzlib/etc],
  @schememodname[mzlib/file], @schememodname[mzlib/list],
  @schememodname[mzlib/unit], @schememodname[mzlib/include],
  @schememodname[mzlib/defmacro], @schememodname[mzlib/pretty],
  @schememodname[mzlib/string], @schememodname[mzlib/thread],
  @schememodname[mzlib/math], @schememodname[mzlib/match], and
  @schememodname[mzlib/shared].}

 @item{The @as-index{@drlang{Swindle} language} starts with the same
  bindings as @schememodname[swindle], and evaluates the program like
  @scheme[load].}

]

@; ----------------------------------------

@section[#:tag "htdp-langs"]{@|HtDP| Teaching Languages}

Five of DrScheme's languages are specifically designed for teaching:

@itemize[

 @item{The @as-index{@drlang{Beginning Student} language} is a small
       version of Scheme that is tailored for beginning computer
       science students.}
       
 @item{The @as-index{@drlang{Beginning Student with List
       Abbreviations} languages} is an extension to Beginning Student
       that prints lists with @scheme[list] instead of @scheme[cons],
       and accepts @scheme[quasiquote]d input.}

 @item{The @as-index{@drlang{Intermediate Student} language} adds
       local bindings and higher-order functions.}

 @item{The @as-index{@drlang{Intermediate Student with Lambda}
       language} adds anonymous functions.}

 @item{The @as-index{@drlang{Advanced Student} language} adds mutable
       state.}

]

The teaching languages are different from conventional Scheme in a number
of ways:

@itemize[

 @item{@defterm{Case-sensitive identifiers and symbols} --- In a
       case-sensitive language, the variable names @scheme[x] and
       @scheme[X] are distinct, and the symbols @scheme['x] and
       @scheme['X] are also distinct. In a case-insensitive language,
       @scheme[x] and @scheme[X] are equivalent and @scheme['x] and
       @scheme['X] represent the same value. The teaching languages
       are case-sensitive by default, and other languages are usually
       case-insensitive. Case-sensitivity can be adjusted through the
       detail section of the language-selection dialog.}
        
 @item{@defterm{All numbers are exact unless @schememetafont{#i} is
       specified} --- In the @drlang{Beginning Student} through
       @drlang{Intermediate Student with Lambda languages}, numbers
       containing a decimal point are interpreted as exact
       numbers. This interpretation allows students to use familiar
       decimal notation without inadvertently triggering inexact
       arithmetic. Exact numbers with decimal representations are also
       printed in decimal. Inexact inputs and results are explicitly
       marked with @schememetafont{#i}.}

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
       @scheme[(_identifier ...)]. This restriction helps detect
       confusing misuses of parentheses, such as @scheme[(1)] or
       @scheme[((+ 3 4))], which is a common mistake among beginners
       who are used to the optional parentheses of algebra.}

 @item{@defterm{Top-level required at function call position} --- In
       the @drlang{Beginning Student} languages, procedure calls must
       be of the form @scheme[(_top-level-identifier ...)], and the
       number of actual arguments must match the number of formal
       arguments if @scheme[_top-level-identifier] is
       @scheme[define]d. This restriction helps detect confusing
       misuses of parentheses, such as @scheme[(x)] where @scheme[x]
       is a function argument. DrScheme can detect such mistakes
       syntactically because Beginning Student does not support
       higher-order procedures.}

 @item{@defterm{Primitive and defined functions allowed only in
       function call position} --- In @drlang{Beginning Student}
       languages, the name of a primitive operator or of a defined
       function can be used only after the open-parenthesis of a
       function call (except where teachpack extensions allow
       otherwise, as in the @scheme[convert-gui] teachpack). Incorrect
       uses of primitives trigger a syntax error. Incorrect uses of
       defined names trigger a run-time error.  DrScheme can detect
       such mistakes because Beginning Student does not support
       higher-order procedures.}

 @item{@defterm{@scheme[lambda] allowed only in definitions} --- In
       the Beginning Student through Intermediate Student languages,
       @scheme[lambda] (or @scheme[case-lambda]) may appear only in a
       definition, and only as the value of the defined variable.}

 @item{@defterm{Free variables are not allowed} --- In the
       @drlang{Beginning Student} through @drlang{Advanced Student}
       languages, every variable referenced in the definitions window
       must be defined, pre-defined, or the name of a local function
       argument.}

 @item{@defterm{@scheme[quote] works only on symbols,
       @scheme[quasiquote] disallowed} --- In the @drlang{Beginning
       Student} language, @scheme[quote] and @schemevalfont{'} can
       specify only symbols. This restriction avoids the need to
       explain to beginners why @scheme[1] and @scheme['1] are
       equivalent in standard Scheme. In addition,
       @scheme[quasiquote], @litchar{`}, @scheme[unquote],
       @litchar{,}, @scheme[unquote-splicing], and @litchar[",@"] are
       disallowed.}

 @item{@defterm{Unmatched @scheme[cond]/@scheme[case] is an error} ---
       In the @drlang{Beginning Student} through @drlang{Advanced
       Student} languages, falling through a @scheme[cond] or
       @scheme[case] expression without matching a clause signals a
       run-time error. This convention helps detect syntactic and
       logical errors in programs.}

 @item{@defterm{Conditional values must be @scheme[true] or
       @scheme[false]} --- In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, an expression whose value
       is treated as a boolean must return an actual boolean,
       @scheme[true] or @scheme[false]. This restriction, which
       applies to @scheme[if], @scheme[cond], @scheme[and],
       @scheme[or], @scheme[nand], and @scheme[nor] expressions, helps
       detect errors where a boolean function application is omitted.}

 @item{@defterm{@scheme[+], @scheme[*], and @scheme[/] take at least
       two arguments} --- In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, mathematical operators
       that are infix in algebra notation require at least two
       arguments in DrScheme. This restriction helps detect missing
       arguments to an operator.}

 @item{@defterm{@scheme[and], @scheme[or], @scheme[nand], and
       @scheme[nor] require at least 2 expressions} --- In the
       @drlang{Beginning Student} through @drlang{Advanced Student}
       languages, the boolean combination forms require at least two
       sub-expressions.  This restriction helps detect missing or
       ill-formed sub-expressions in a boolean expression.}

 @item{@defterm{@scheme[set!] disallowed on arguments} --- In the
       @drlang{Advanced Student} language, @scheme[set!] cannot be
       used to mutate variables bound by @scheme[lambda]. This
       restriction ensures that the substitution model of function
       application is consistent with DrScheme's evaluation.}

 @item{@defterm{Improper lists disallowed} --- A @defterm{proper list}
       is either an empty list or a list created by @scheme[cons]ing
       onto a proper list. In the @drlang{Beginning Student} through
       @drlang{Advanced Student} languages, @scheme[cons] constructs
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

The teaching languages also deviate from traditional Scheme in
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
     finite decimal expansion (such as @scheme[4/3]) DrScheme offers a
     choice. It either prints them as mixed fractions or as repeating
     decimals, where the repeating portion of the decimal expansion is
     shown with an overbar. In addition, DrScheme only shows the first
     25 digits of the number's decimal expansion. If there are more
     digits, the number appears with an ellipses at the end. Click the
     ellipses to see the next 25 digits of the expansion.

     This setting controls only the initial display of a number.
     Right-clicking or Control-clicking (Mac OS X) on the number lets
     you change from the fraction representation to the decimal
     representation.}

 @item{@defterm{@scheme[write] output} --- Prints values with
        @scheme[write].}

 @item{@defterm{Show sharing in values} --- Prints interaction results
       using the @scheme[shared] syntax, which exposes shared
       structure within a value. For example, the list created by
       @scheme[(let ([lt (list 0)]) (list lt lt))] prints as

@schemeblock[
(shared ((-1- (list 0))) (list -1- -1-))
]

       instead of

@schemeblock[
(list (list 0) (list 0))
]
}

]

A program in the teaching languages should be tested using the check forms ---
 @scheme[(check-expect value value)], @scheme[(check-within value value value)], or
 @scheme[(check-error value string)]. Tests are evaluated when running the program:
 when there are no tests, a warning appears in the interactions window;
 when all tests succeed, an acknowledgement appears in the interactions window;
 otherwise, a testing window appears to report the results. See @secref["menu:view"] 
 for details on configuring the report behavior. 
 
 Tests can be disabled if necessary, see @secref["menu:scheme"] for details.

@; ----------------------------------------

@section[#:tag "profj"]{ProfessorJ}

The ProfessorJ languages are Java based languages designed for teaching and
 experimentation. There are four teaching based languages:

@itemize[

 @item{The @as-index{@drlang{ProfessorJ Beginner} language} is a small
       subset of Java, designed for novice computer science students. 
       Each class must contain a constructor that initializes the class's
       fields; each method must return a value.}
       
 @item{The @as-index{@drlang{ProfessorJ Intermediate} language} is an extension to 
       ProfessorJ Beginner that adds full class-based inheritance and mutation.
       Classes do not require constructors and null values may arise.}

 @item{The @as-index{@drlang{ProfessorJ Intermediate + access} language} adds
       access controls, such as public or private, to member definitions and 
       supports overloading constructor definitions.}

 @item{The @as-index{@drlang{ProfessorJ Advanced} language} adds arrays, loop
       constructs, and package specifications.}

]

The remaining two languages support language extensions and experimentations:

@itemize[
 @item{The @as-index{@drlang{ProfessorJ Full} language} supports most of Java 1.1
       features, as well as a set of constructs designed for the development of 
       unit tests}
 @item{The @as-index{@drlang{ProfessorJ Java + dynamic} language} extends the
       ProfessorJ Full language with a dynamic type and the ability to import Scheme
       libraries directly, for developing programs that use both languages.}
 ]

Value printing can be either @defterm{Class} or @defterm{Class + Field}, selectable
 in the show details section of the language selection window. The Class printing style
 only displays the class name for any object value. The Class + Field style displays
 the class combined with the names and values for all of the class's fields; when displaying
 a recursively defined object, any repeated object reverts to the Class style display for
 the second appearance. Languages with arrays may opt to always display an entire array or
 truncate the middle section of longer arrays.

All of the ProfessorJ languages support testing extensions, and tests are required
 within the teaching languages. The experimental languages, Full and Java + dynamic, 
 allow the removal of these forms within the language selection window. 
 
Programs in the teaching languages must be tested, using a class containing the word
 'Example' in the name and the 'check ... expect ...' comparison forms. On run,
 all Example classes are instanstiated and all methods prefixed with the word 'test' 
 are run. When there are no tests, a warning appears in the interactions window; 
 when all tests succeed, an acknowledgement appears in the interactions window;
 otherwise, a testing window appears to report the results. See @secref["menu:view"] 
 for details on configuring the report behavior. Tests can be disabled if necessary,
 see @secref["menu:scheme"] for details.
 
Unless disabled in the language configuration window, expression-level coverage 
 information is collected during testing. Selecting the buttons within the report
 modifies the color of the program in the definitions window, to distinguish 
 expressions that were used in the test from those that were not. Typing into the
 definitions window restores the original coloring.
      

@; ----------------------------------------

@section[#:tag "experimental-langs"]{Other Experimental Languages}

For information on @onscreen{Lazy Scheme}, see @other-manual['(lib "lazy/lazy.scrbl")].

For information on @onscreen{FrTime}, see @other-manual['(lib "frtime/scribblings/frtime.scrbl")].

For information on @onscreen{Algol 60}, see @other-manual['(lib "algol60/algol60.scrbl")].

@; ----------------------------------------

@include-section["printing.scrbl"]
