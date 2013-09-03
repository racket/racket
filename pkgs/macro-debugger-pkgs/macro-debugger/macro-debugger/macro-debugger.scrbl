#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          (for-label racket/base
                     racket/contract/base
                     racket/lazy-require
                     racket/runtime-path
                     macro-debugger/expand
                     macro-debugger/emit
                     macro-debugger/stepper
                     macro-debugger/stepper-text
                     macro-debugger/syntax-browser
                     (except-in macro-debugger/analysis/check-requires main)
                     (except-in macro-debugger/analysis/show-dependencies main)))

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/pretty
                         macro-debugger/expand
                         macro-debugger/stepper-text
                         (except-in macro-debugger/analysis/check-requires main)
                         (except-in macro-debugger/analysis/show-dependencies main)))
     (the-eval '(current-print pretty-print-handler))
     the-eval))

@(define (defoutput proto . text)
   (nested #:style "leftindent"
          (tabular #:style 'boxed (list (list proto)))
          "\n" "\n"
          (splice text)))

@title{Macro Debugger: Inspecting Macro Expansion}

@author["Ryan Culpepper"]

The macro-debugger collection contains two tools: a stepper for macro
expansion and a standalone syntax browser. The macro stepper shows the
programmer the expansion of a program as a sequence of rewriting
steps, using the syntax browser to display the individual terms. The
syntax browser uses colors and a properties panel to show the term's
syntax properties, such as lexical binding information and source
location.


@section{Macro Stepper}

@defmodule[macro-debugger/stepper]

@defproc[(expand/step [stx any/c])
         void?]{

Expands the syntax (or S-expression) and opens a macro stepper frame
for stepping through the expansion. 
}

@defproc[(expand-module/step [mod module-path?])
         void?]{

Expands the source file named by @racket[mod], which must contains a
single module declaration, and opens a macro stepper frame for
stepping through the expansion.
}

@section{Macro Expansion Tools}

@defmodule[macro-debugger/expand]

This module provides @racket[expand]-like procedures that allow the
user to specify macros whose expansions should be hidden.

Warning: because of limitations in the way macro expansion is
selectively hidden, the resulting syntax may not evaluate to the same
result as the original syntax.

@defproc[(expand-only [stx any/c] [transparent-macros (listof identifier?)])
         syntax?]{

  Expands the given syntax @racket[stx], but only shows the expansion
  of macros whose names occur in @racket[transparent-macros].

  @(examples #:eval the-eval
             (syntax->datum
              (expand-only #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                           (list #'or))))

}

@defproc[(expand/hide [stx any/c] [hidden-macros (listof identifier?)])
         syntax?]{

  Expands the given syntax @racket[stx], but hides the expansion of macros in the
  given identifier list (conceptually, the complement of expand-only).

  @(examples #:eval the-eval
             (syntax->datum
              (expand/hide #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                           (list #'or))))
}

@defproc[(expand/show-predicate [stx any/c] [show? (-> identifier? boolean?)])
         syntax?]{

  Expands the given syntax @racket[stx], but only shows the expansion of macros
  whose names satisfy the predicate @racket[show?].

  @(examples #:eval the-eval
             (syntax->datum
              (expand/show-predicate
               #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
               (lambda (id) (memq (syntax-e id) '(or #%app))))))
}


@section{Macro Stepper API for Macros}

@defmodule[macro-debugger/emit]

Macros can explicitly send information to a listening macro stepper by
using the procedures in this module.

@defproc[(emit-remark [fragment
                       (letrec ([emit-arg/c
                                 (recursive-contract
                                  (or/c string?
                                        syntax?
                                        (listof emit-arg/c)
                                        (-> emit-arg/c)))])
                         emit-arg/c)] ...
                      [#:unmark? unmark? boolean? (syntax-transforming?)])
         void?]{

Emits an event to the macro stepper (if one is listening) containing
the given strings and syntax objects. The macro stepper displays a
remark by printing the strings and syntax objects above a rendering of
the macro's context. The remark is only displayed if the macro that
emits it is considered transparent by the hiding policy.

By default, syntax objects in remarks have the transformer's mark
applied (using @racket[syntax-local-introduce]) so that their
appearance in the macro stepper matches their appearance after the
transformer returns. Unmarking is suppressed if @racket[unmark?] is
@racket[#f].

@racketblock[
(define-syntax (mymac stx)
  (syntax-case stx ()
    [(_ x y)
     (emit-remark "I got some arguments!"
                  #'x
                  "and"
                  #'y)
     #'(list 'x 'y)]))
(mymac 37 (+ 1 2))
]

(Run the fragment above in the macro stepper.)
}

@defproc[(emit-local-step [before syntax?] [after syntax?]
                          [#:id id identifier?])
         void?]{

Emits an event that simulates a local expansion step from
@racket[before] to @racket[after].

The @racket[id] argument acts as the step's ``macro'' for the purposes
of macro hiding.
}


@section{Macro Stepper Text Interface}

@defmodule[macro-debugger/stepper-text]

@defproc[(expand/step-text [stx any/c]
                           [show? (or/c (-> identifier? boolean?)
                                        (listof identifier?))
                                  (lambda (x) #t)])
         void?]{

  Expands the syntax and prints the macro expansion steps. If the
  identifier predicate is given, it determines which macros are shown
  (if absent, all macros are shown). A list of identifiers is also
  accepted.

  @(examples #:eval the-eval
             (expand/step-text #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                               (list #'or))
             #;(expand/step-text #'(let ([x 1]) (even? x)))
             #;(expand/step-text #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                               (lambda (id) (eq? (syntax-e id) 'or))))
}

@defproc[(stepper-text [stx any/c]
                       [show? (or/c (-> identifier? boolean?)
                                    (listof identifier?))
                              (lambda (x) #t)])
         (symbol? -> void?)]{

  Returns a procedure that can be called on the symbol
  @racket['next] to print the next step or on the symbol
  @racket['all] to print out all remaining steps.
}


@section{Syntax Browser}

@defmodule[macro-debugger/syntax-browser]

@defproc[(browse-syntax [stx syntax?])
         void?]{

  Creates a frame with the given syntax object shown. More information
  on using the GUI is available below.
}

@defproc[(browse-syntaxes [stxs (listof syntax?)])
         void?]{

  Like @racket[browse-syntax], but shows multiple syntax objects in
  the same frame. The coloring partitions are shared between the two,
  showing the relationships between subterms in different syntax
  objects.
}


@section{Using the Macro Stepper}

@subsection{Navigation}

The stepper presents expansion as a linear sequence of rewriting
process, and it gives the user controls to step forward or backwards
as well as to jump to the beginning or end of the expansion process.

If the macro stepper is showing multiple expansions, then it also
provides ``Previous term'' and ``Next term'' buttons to go up and down in
the list of expansions. Horizontal lines delimit the current expansion
from the others.

@subsection{Macro Hiding}

Macro hiding lets one see how expansion would look if certain macros
were actually primitive syntactic forms. The macro stepper skips over
the expansion of the macros you designate as opaque, but it still
shows the expansion of their subterms.

The bottom panel of the macro stepper controls the macro hiding
policy. The user changes the policy by selecting an identifier in the
syntax browser pane and then clicking one of ``Hide module'', ``Hide
macro'', or ``Show macro''. The new rule appears in the policy display,
and the user may later remove it using the "Delete" button.

The stepper also offers coarser-grained options that can hide
collections of modules at once. These options have lower precedence
than the rules above.

Macro hiding, even with no macros marked opaque, also hides certain
other kinds of steps: internal defines are not rewritten to letrecs,
begin forms are not spliced into module or block bodies, etc.

@section{Using the Syntax Browser}

@subsection{Selection}

The selection is indicated by bold text.

The user can click on any part of a subterm to select it. To select a
parenthesized subterm, click on either of the parentheses. The
selected syntax is bolded. Since one syntax object may occur inside of
multiple other syntax objects, clicking on one occurrence will cause
all occurrences to be bolded.

The syntax browser displays information about the selected syntax
object in the properties panel on the right, when that panel is
shown. The selected syntax also determines the highlighting done by
the secondary partitioning (see below).

@subsection{Primary Partition}

The primary partition is indicated by foreground color.

The primary partitioning always assigns two syntax subterms the same
color if they have the same marks. In the absence of unhygienic
macros, this means that subterms with the same foreground color were
either present in the original pre-expansion syntax or generated by
the same macro transformation step.

Syntax colored in black always corresponds to unmarked syntax. Such
syntax may be original, or it may be produced by the expansion of a
nonhygienic macro.

Note: even terms that have the same marks might not be
@racket[bound-identifier=?] to each other, because they might occur in
different environments.

@;@example[(bound-identifier=? (let ([x 1]) #'x) #'x)]

@subsection{Secondary Partitioning}

The user may select a secondary partitioning through the Syntax
menu. This partitioning applies only to identifiers. When the user
selects an identifier, all terms in the same equivalence class as the
selected term are highlighted in yellow.

The available secondary partitionings are:
@itemize[
@item{@racket[bound-identifier=?]}
@item{@racket[free-identifier=?]}
]

@subsection{Properties}

When the properties pane is shown, it displays properties of the
selected syntax object. The properties pane has two tabbed pages:

@itemize[
@item{@bold{Term}:

      If the selection is an identifier, shows the binding information
      associated with the syntax object. For more information, see
      @racket[identifier-binding], etc.
}
@item{@bold{Syntax Object}:

      Displays source location information and other properties (see
      @racket[syntax-property]) carried by the syntax object.
}
]

@subsection{Interpreting Syntax}

The binding information of a syntax object may not be the same as
the binding structure of the program it represents. The binding
structure of a program is only determined after macro expansion is
complete.


@section{Finding Useless @racket[require]s}
@section-index["useless-requires"]

@defmodule[macro-debugger/analysis/check-requires]

The ``Check Requires'' utility can be run as a raco subcommand. For
example (from racket root directory):

@commandline{
raco check-requires racket/collects/syntax/*.rkt
}

@commandline{
raco check-requires -kbu openssl
}

Each argument is interpreted as a file path if it exists; otherwise,
it is interpreted as a module path. See @racket[check-requires] for a
description of the output format, known limitations in the script's
recommendations, etc.

@defproc[(check-requires [module-to-analyze module-path?]
                         [#:show-keep? show-keep? boolean? #t]
                         [#:show-bypass? show-bypass? boolean? #t]
                         [#:show-drop? show-drop? boolean? #t]
                         [#:show-uses? show-uses? boolean? #f])
         void?]{

Analyzes @racket[module-to-analyze], detecting useless requires. Each
module imported by @racket[module-to-analyze] is classified as one of
KEEP, BYPASS, or DROP. For each required module, one or more lines is
printed with the module's classification and supporting
information. Output may be suppressed based on classification via
@racket[show-keep?], @racket[show-bypass?], and @racket[show-drop?];
by default, only DROP recommendations are printed.

Modules required @racket[for-label] are not analyzed.

@defoutput[@tt{KEEP @racket[_req-module] at @racket[_req-phase]}]{

  The require of module @racket[_req-module] at phase
  @racket[_req-phase] must be kept because bindings defined within it
  are used. 

  If @racket[show-uses?] is true, the dependencies of
  @racket[module-to-analyze] on @racket[_req-module] are enumerated,
  one per line, in the following format:

  @defoutput[@tt{@racket[_exp-name] at @racket[_use-phase] (@racket[_mode ...]) [RENAMED TO @racket[_ref-name]]}]{

    Indicates an export named @racket[_exp-name] is used at phase
    @racket[_use-phase] (not necessarily the phase it was provided at,
    if @racket[_req-phase] is non-zero). 

    The @racket[_modes] indicate what kind(s) of dependencies were
    observed: used as a @tt{reference}, appeared in a syntax template
    (@tt{quote-syntax}), etc.

    If the @tt{RENAMED TO} clause is present, it indicates that the
    binding is renamed on import into the module, and
    @racket[_ref-name] gives the local name used (@racket[_exp-name]
    is the name under which @racket[_req-module] provides the
    binding).
  }
}

@defoutput[@tt{BYPASS @racket[_req-module] at @racket[_req-phase]}]{

  The require is used, but only for bindings that could be more
  directly obtained via one or more other modules. For example, a use
  of @racketmodname[racket] might be bypassed in favor of
  @racketmodname[racket/base], @racketmodname[racket/match], and
  @racketmodname[racket/contract], etc.

  A list of replacement requires is given, one per line, in the
  following format:

  @defoutput[@tt{TO @racket[_repl-module] at @racket[_repl-phase] [WITH RENAMING]}]{

    Add a require of @racket[_repl-module] at phase
    @racket[_repl-phase]. If @racket[show-uses?] is true, then
    following each @tt{TO} line is an enumeration of the dependencies
    that would be satisfied by @racket[_repl-module] in the same
    format as described under @tt{KEEP} below.

    If the @tt{WITH RENAMING} clause is present, it indicates that at
    least one of the replacement modules provides a binding under a
    different name from the one used locally in the module. Either the
    references should be changed or @racket[rename-in] should be used
    with the replacement modules as necessary.
  }

  Bypass recommendations are restricted by the following rules:
  @itemlist[

  @item{@racket[_repl-module] must not involve crossing a new
    @tt{private} directory from @racket[_req-module]}

  @item{@racket[_repl-module] is never a built-in (``@litchar{#%}'')
    module}

  @item{@racket[_req-module] must not be in the ``no-bypass''
    whitelist}
  ]
}

@defoutput[@tt{DROP @racket[_req-module] at @racket[_req-phase]}]{

  The require appears to be unused, and it can probably be dropped
  entirely.
}

Due to limitations in its implementation strategy,
@racket[check-requires] occasionally suggests dropping or bypassing a
module that should not be dropped or bypassed. The following are
typical reasons for such bad suggestions:

@itemlist[

@item{The module's invocation has side-effects. For example, the
  module body may update a shared table or perform I/O, or it might
  transitively require a module that does. (Consider adding the module
  to the whitelist.)}

@item{Bindings from the module are used in identifier comparisons by a
  macro, such as appearing in the macro's ``literals list.'' In such
  cases, a macro should annotate its expansion with the
  @racket['disappeared-use] property containing the identifier(s)
  compared with its literals; however, most casually-written macros do
  not do so. On the other hand, macros and their literal identifiers
  are typically provided by the same module, so this problem is
  somewhat uncommon.}
]

@examples[#:eval the-eval
(check-requires 'framework)
(check-requires 'openssl #:show-uses? #t)
]
}

@defproc[(show-requires [module-name module-path?])
         (listof (list/c 'keep   module-path? number?)
	         (list/c 'bypass module-path? number? list?)
		 (list/c 'drop   module-path? number?))]{

Like @racket[check-requires], but returns the analysis as a list
instead of printing it. The procedure
returns one element per (non-label) require in the following format:
@itemlist[
@item{@racket[(list 'keep _req-module _req-phase)]}
@item{@racket[(list 'bypass _req-module _req-phase _replacements)]}
@item{@racket[(list 'drop _req-module _req-phase)]}
]

@examples[#:eval the-eval
(show-requires 'framework)
]
}


@section{Showing Module Dependencies}
@section-index["show-dependencies"]

@defmodule[macro-debugger/analysis/show-dependencies]

The ``Show Dependencies'' utility can be run as a raco subcommand. For
example (from racket root directory):

@commandline{
raco show-dependencies -bc racket/collects/openssl/main.rkt
}

@commandline{
raco show-dependencies -c --exclude racket openssl
}

Each argument is interpreted as a file path if it exists; otherwise it
is interpreted as a module path. See @racket[show-dependencies] for a
description of the output format.

@defproc[(show-dependencies [root module-path?] ...
                            [#:exclude exclude
                             (listof module-path?) null]
                            [#:exclude-deps exclude-deps
                             (listof module-path?) null]
                            [#:show-context? show-context? boolean? #f])
         void?]{

Computes the set of modules transitively required by the @racket[root]
module(s). A @racket[root] module is included in the output
only if it is a dependency of another @racket[root] module. The
computed dependencies do not include modules reached through
@racket[dynamic-require] or @racket[lazy-require] or referenced by
@racket[define-runtime-module-path-index] but do include modules
referenced by @racket[define-runtime-module-path] (since that
implicitly creates a @racket[for-label] dependency).

Dependencies are printed, one per line, in the following format:

@defoutput[@tt{@racket[_dep-module] [<- @racket[(_direct-dependent ...)]]}]{

Indicates that @racket[_dep-module] is transitively required by one or
more @racket[root] modules. If @racket[show-context?] is true, then
the @racket[_direct-dependent]s are shown; they are the modules
reachable from (and including) the @racket[root] modules that directly
require @racket[_dep-module].
}

The dependencies are trimmed by removing any module reachable from (or
equal to) a module in @racket[exclude] as well as any module
reachable from (but not equal to) a module in @racket[exclude-deps].

@examples[#:eval the-eval
(show-dependencies 'openssl
                   #:exclude (list 'racket))
(show-dependencies 'openssl
                   #:show-context? #t
                   #:exclude (list 'racket))
]
}

@defproc[(get-dependencies [root module-path?] ...
                           [#:exclude exclude
                            (listof module-path?) null]
                           [#:exclude-deps exclude-deps
                            (listof module-path?) null])
         (listof (list module-path? (listof module-path?)))]{

Like @racket[show-dependencies], but returns a list instead of
producing output. Each element of the list is a list containing a
module path and the module paths of its immediate dependents.

@examples[#:eval the-eval
(get-dependencies 'openssl #:exclude (list 'racket))
]
}

@close-eval[the-eval]
