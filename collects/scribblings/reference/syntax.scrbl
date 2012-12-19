#lang scribble/doc
@(require "mz.rkt" scribble/bnf scribble/core
          (for-label (only-in racket/require-transform
                              make-require-transformer
                              current-require-module-path)
                     racket/require-syntax
                     racket/require
                     (only-in racket/provide-transform
                              make-provide-transformer)
                     racket/keyword-transform
                     racket/provide-syntax
                     racket/provide
                     racket/package
                     racket/splicing
                     racket/runtime-path
                     racket/lazy-require
                     (only-in compiler/cm-accomplice
                              register-external-module)
                     racket/performance-hint
                     syntax/parse))

@(define require-eval (make-base-eval))
@(define syntax-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require (for-syntax racket/base)))
       the-eval)))
@(define meta-in-eval (syntax-eval))

@(define cvt (racketfont "CVT"))
@(define unquote-id (racket unquote))
@(define unquote-splicing-id (racket unquote-splicing))

@(define-syntax-rule (equiv-to-block b)
    (tabular #:style (make-style #f (list (make-table-columns
                                           (list (make-style #f '(baseline))
                                                 (make-style #f '(baseline))))))
             (list (list (para (hspace 2) " is equivalent to" (hspace 1))
                         (racketblock0 b)))))

@(define-syntax-rule (subeqivs [a0 b0] [a b] ...)
   (tabular (map
             list
             (apply
              append
              (list (list (racketblock a0)
                          (equiv-to-block b0))
                    (list (para 'nbsp)
                          (racketblock a)
                          (equiv-to-block b))
                    ...)))))

@title[#:tag "syntax" #:style 'toc]{Syntactic Forms}

This section describes the core syntax forms that appear in a fully
expanded expression, plus many closely related non-core forms.
See @secref["fully-expanded"] for the core grammar.

@local-table-of-contents[]

@subsubsub*section{Notation}

Each syntactic form is described by a BNF-like notation that describes
a combination of (syntax-wrapped) pairs, symbols, and other data (not
a sequence of characters). These grammatical specifications are shown
as in the following specification of a @racketkeywordfont{something}
form:

@specsubform[(@#,racketkeywordfont{something} id thing-expr ...)
             #:contracts ([thing-expr number?])]

Within such specifications,

@itemize[

 @item{@racket[...] indicates zero or more repetitions of the
       preceding datum; more generally, @math{N} consecutive
       @racket[...]s a row indicate a consecutive repetition of the
       preceding @math{N} datums.}

 @item{@racket[...+] indicates one or more repetitions of the
       preceding datum.}

 @item{Italic meta-identifiers play the role of non-terminals. Some
       meta-identifier names imply syntactic constraints:

      @itemize[

        @item{A meta-identifier that ends in @racket[_id] stands for an
              identifier.}

        @item{A meta-identifier that ends in @racket[_keyword] stands
              for a keyword.}

        @item{A meta-identifier that ends with @racket[_expr] (such as
              @racket[_thing-expr]) stands for a sub-form that is
              expanded as an expression.}

        @item{A meta-identifier that ends with @racket[_body] stands
              for a sub-form that is expanded in an
              internal-definition context (see
              @secref["intdef-body"]).}

              ]} 

 @item{Contracts indicate constraints on sub-expression results. For
       example, @racket[_thing-expr @#,elem{:} number?] indicates that
       the expression @racket[_thing-expr] must produce a number.}]

@;------------------------------------------------------------------------
@section[#:tag "module"]{Modules: @racket[module], @racket[module*], ...}

@guideintro["module-syntax"]{@racket[module]}

@defform[(module id module-path form ...)]{

Declares a top-level module or a @tech{submodule}. For a top-level
module, if the @racket[current-module-declare-name] parameter is set,
the parameter value is used for the module name and @racket[id] is
ignored, otherwise @racket[(#,(racket quote) id)] is the name of the
declared module. For a @tech{submodule}, @racket[id] is the name of
the submodule to be used as an element within a @racket[submod] module
path.

@margin-note/ref{For a @racket[module]-like form that works in
definitions context other than the top level or a module body, see
@racket[define-package].}

The @racket[module-path] form must be as for @racket[require], and it
supplies the initial bindings for the body @racket[form]s. That is, it
is treated like a @racket[(require module-path)] prefix before the
@racket[form]s, except that the bindings introduced by
@racket[module-path] can be shadowed by definitions and
@racket[require]s in the module body @racket[form]s.

If a single @racket[form] is provided, then it is partially expanded
in a @tech{module-begin context}. If the expansion leads to
@racket[#%plain-module-begin], then the body of the
@racket[#%plain-module-begin] is the body of the module. If partial
expansion leads to any other primitive form, then the form is wrapped
with @racketidfont{#%module-begin} using the lexical context of the
module body; this identifier must be bound by the initial
@racket[module-path] import, and its expansion must produce a
@racket[#%plain-module-begin] to supply the module body. Finally, if
multiple @racket[form]s are provided, they are wrapped with
@racketidfont{#%module-begin}, as in the case where a single
@racket[form] does not expand to @racket[#%plain-module-begin].

After such wrapping, if any, and before any expansion, an
@indexed-racket['enclosing-module-name] property is attached to the
@racketidfont{#%module-begin} syntax object (see
@secref["stxprops"]); the property's value is a symbol
corresponding to @racket[id].

Each @racket[form] is partially expanded (see
@secref["partial-expansion"]) in a @tech{module context}. Further
action depends on the shape of the form:

@itemize[

 @item{If it is a @racket[begin] form, the sub-forms are flattened
  out into the module's body and immediately processed in place of the
  @racket[begin].}

 @item{If it is a @racket[define-syntaxes] form, then the right-hand side is
  evaluated (in @tech{phase} 1), and the binding is immediately
  installed for further partial expansion within the
  module. Evaluation of the right-hand side is @racket[parameterize]d
  to set @racket[current-namespace] as in @racket[let-syntax].}

 @item{If it is a @racket[begin-for-syntax] form, then the body is
  expanded (in @tech{phase} 1) and evaluated. Expansion within a
  @racket[begin-for-syntax] form proceeds with the same
  partial-expansion process as for a @racket[module] body, but in a
  higher @tech{phase}, and saving all @racket[#%provide] forms for all
  phases until the end of the @racket[module]'s expansion. Evaluation
  of the body is @racket[parameterize]d to set
  @racket[current-namespace] as in @racket[let-syntax].}

 @item{If the form is a @racket[#%require] form, bindings are introduced
   immediately, and the imported modules are @tech{instantiate}d or
   @tech{visit}ed as appropriate.}

 @item{If the form is a @racket[#%provide] form, then it is recorded for
   processing after the rest of the body.}

 @item{If the form is a @racket[define-values] form, then the binding
   is installed immediately, but the right-hand expression is not
   expanded further.}

 @item{If the form is a @racket[module] form, then it is immediately
   expanded and declared for the extent of the current top-level
   enclosing module's expansion.}

 @item{If the form is a @racket[module*] form, then it is not
   expanded further.}

 @item{Similarly, if the form is an expression, it is
   not expanded further.}

]

After all @racket[form]s have been partially expanded this way, then
the remaining expression forms (including those on the right-hand side
of a definition) are expanded in an expression context. After all
expression forms, @racket[#%provide] forms are processed in the order
in which they appear (independent of @tech{phase}) in the expanded
module. Finally, all @racket[module*] forms are expanded in order, so
that each becomes available for use by subsequent @racket[module*]
forms; the enclosing module itself is also available for use by
@racket[module*] @tech{submodules}.

The scope of all imported identifiers covers the entire module body, 
except for nested @racket[module] and @racket[module*] forms (assuming 
a non-@racket[#f] @racket[module-path] in the latter case).
The scope of any identifier defined within the module body similarly
covers the entire module body except for such nested @racket[module] 
and @racket[module*] forms.
The ordering of syntax definitions does not affect the scope of the
syntax names; a transformer for @racket[A] can produce expressions
containing @racket[B], while the transformer for @racket[B] produces
expressions containing @racket[A], regardless of the order of
declarations for @racket[A] and @racket[B]. However, a syntactic form
that produces syntax definitions must be defined before it is used.

No identifier can be imported or defined more than once at any
@tech{phase level} within a single module. Every exported identifier must be imported or
defined. No expression can refer to a @tech{top-level variable}.
A @racket[module*] form in which the enclosing module's bindings are visible
(i.e., a nested @racket[module*] with @racket[#f] instead of a @racket[module-path])
can define or import bindings that @tech{shadow} the enclosing module's bindings.

The evaluation of a @racket[module] form does not evaluate the
expressions in the body of the module. Evaluation merely declares a
module, whose full name depends both on @racket[id] or
@racket[(current-module-declare-name)].

A module body is executed only when the module is explicitly
@techlink{instantiate}d via @racket[require] or
@racket[dynamic-require]. On invocation, imported modules are
instantiated in the order in which they are @racket[require]d
into the module (although earlier instantiations or transitive
@racket[require]s can trigger the instantiation of a module before
its order within a given module). Then, expressions and definitions
are evaluated in order as they appear within the module. Each
evaluation of an expression or definition is wrapped with a
continuation prompt (see @racket[call-with-continuation-prompt]) for
the default continuation and using a prompt handler that re-aborts
and propagates its argument to the next enclosing prompt.

Accessing a @tech{module-level variable} before it is defined signals
a run-time error, just like accessing an undefined global variable.
If a module (in its fully expanded form) does not contain a
@racket[set!]  for an identifier that defined within the module, then
the identifier is a @defterm{constant} after it is defined; its value
cannot be changed afterward, not even through reflective
mechanisms. The @racket[compile-enforce-module-constants] parameter,
however, can be used to disable enforcement of constants.

When a @tech{syntax object} representing a @racket[module] form has a
@indexed-racket['module-language] @tech{syntax property} attached, and
when the property value is a vector of three elements where the first
is a module path (in the sense of @racket[module-path?]) and the
second is a symbol, then the property value is preserved in the
corresponding compiled and/or declared module. The third component of
the vector should be printable and @racket[read]able, so that it can
be preserved in marshaled bytecode. The @racketmodname[racket/base]
and @racketmodname[racket] languages attach
@racket['#(racket/language-info get-info #f)] to a @racket[module]
form. See also @racket[module-compiled-language-info],
@racket[module->language-info], and
@racketmodname[racket/language-info].

If a @racket[module] form has a single body @racket[form] and if the
form is a @racket[#%plain-module-begin] form, then the body
@racket[form] is traversed to find @racket[module] and
@racket[module*] forms that are either immediate, under
@racket[begin], or under @racket[begin-for-syntax]. (That is, the 
body is searched before adding
any lexical context due to the module's initial @racket[module-path]
import.) Each such module form is given a @indexed-racket['submodule]
@tech{syntax property} that whose value is the initial module form.
Then, when @racket[module] or @racket[module*] is expanded in a
submodule position, if the form has a @indexed-racket['submodule]
@tech{syntax property}, the property value is used as the form to
expand. This protocol avoids the contamination of submodule lexical
scope when re-expanding @racket[module] forms that contain submodules.

See also @secref["module-eval-model"] and @secref["mod-parse"].

@defexamples[#:eval (syntax-eval)
(module duck racket/base
  (provide num-eggs quack)
  (define num-eggs 2)
  (define (quack n)
    (unless (zero? n)
      (printf "quack\n")
      (quack (sub1 n)))))
]}


@defform*[((module* id module-path form ...)
           (module* id #f form ...))]{

@guideintro["submodules"]{@racket[module*]}

Like @racket[module], but only for declaring a @tech{submodule} within
a module, and for submodules that may @racket[require] the enclosing module.

Instead of a @racket[module-path] after @racket[id], @racket[#f]
indicates that all bindings from the enclosing module are visible in
the submodule; @racket[begin-for-syntax] forms that wrap the
@racket[module*] form shift the @tech{phase level} of the enclosing
module's bindings relative to the submodule.  When a
@racket[module*] form has a @racket[module-path], the submodule
starts with an empty lexical context in the same way as a top-level
@racket[module] form, and enclosing @racket[begin-for-syntax] forms
have no effect on the submodule.}


@defform[(module+ id form ...)]{

@guideintro["main-and-test"]{@racket[module+]}

Declares and/or adds to a @tech{submodule} named @racket[id].

Each addition for @racket[id] is combined in order to form the entire
submodule using @racket[(module* id #f ....)] at the end of the
enclosing module. If there is only one @racket[module+] for a given
@racket[id], then @racket[(module+ id form ...)] is equivalent to
@racket[(module* id #f form ...)].

A submodule must not be defined using @racket[module+] @emph{and}
@racket[module] or @racket[module*]. That is, if a submodule is made
of @racket[module+] pieces, then it must be made @emph{only} of
@racket[module+] pieces. }


@defform[(#%module-begin form ...)]{

Legal only in a @tech{module begin context}, and handled by the
@racket[module] and @racket[module*] forms.

The @racket[#%module-begin] form of @racketmodname[racket/base] wraps
every top-level expression to print non-@|void-const| results using
@racket[current-print].}

@defform[(#%plain-module-begin form ...)]{

Legal only in a @tech{module begin context}, and handled by the
@racket[module] and @racket[module*] forms.}


@;------------------------------------------------------------------------
@section[#:tag '("require" "provide")]{Importing and Exporting: @racket[require] and @racket[provide]}

@section-index["modules" "imports"]
@section-index["modules" "exports"]

@guideintro["module-require"]{@racket[require]}

@defform/subs[#:literals (only-in prefix-in except-in rename-in lib file planet submod + - =
                          for-syntax for-template for-label for-meta only-meta-in combine-in 
                          relative-in quote)
              (require require-spec ...)
              ([require-spec module-path
                             (only-in require-spec id-maybe-renamed ...)
                             (except-in require-spec id ...)
                             (prefix-in prefix-id require-spec)
                             (rename-in require-spec [orig-id bind-id] ...)
                             (combine-in require-spec ...)
                             (relative-in module-path require-spec ...)
                             (only-meta-in phase-level require-spec ...)
                             (for-syntax require-spec ...)
                             (for-template require-spec ...)
                             (for-label require-spec ...)
                             (for-meta phase-level require-spec ...)
                             derived-require-spec]
               [module-path root-module-path
                            (submod root-module-path submod-path-element ...)
                            (submod "." submod-path-element ...)
                            (submod ".." submod-path-element ...)]
               [root-module-path (#,(racket quote) id)
                            rel-string
                            (lib rel-string ...+)
                            id
                            (file string)
                            (planet id)
                            (planet string)
                            (planet rel-string
                                    (user-string pkg-string vers)
                                    rel-string ...)]
               [submod-path-element id
                                    ".."]
               [id-maybe-renamed id
                                 [orig-id bind-id]]
               [phase-level exact-integer #f]
               [vers code:blank
                     nat
                     (code:line nat minor-vers)]
               [minor-vers nat
                           (nat nat)
                           ((unsyntax (racketidfont "=")) nat)
                           ((unsyntax (racketidfont "+")) nat)
                           ((unsyntax (racketidfont "-")) nat)])]{

In a @tech{top-level context}, @racket[require] @tech{instantiates}
modules (see @secref["module-eval-model"]). In a @tech{top-level
context} or @tech{module context}, expansion of @racket[require]
@tech{visits} modules (see @secref["mod-parse"]). In both contexts and
both evaluation and expansion, @racket[require] introduces bindings
into a @tech{namespace} or a module (see @secref["intro-binding"]).  A
@racket[require] form in a @tech{expression context} or
@tech{internal-definition context} is a syntax error.

A @racket[require-spec] designates a particular set of identifiers to
be bound in the importing context. Each identifier is mapped to a
particular export of a particular module; the identifier to bind may
be different from the symbolic name of the originally exported
identifier. Each identifier also binds at a particular @tech{phase
level}.

No identifier can be bound multiple times in a given @tech{phase
level} by an import, unless all of the bindings refer to the same
original definition in the same module.  In a @tech{module context},
an identifier can be either imported or defined for a given
@tech{phase level}, but not both.

The syntax of @racket[require-spec] can be extended via
@racket[define-require-syntax], and when multiple
@racket[require-spec]s are specified in a @racket[require], the
bindings of each @racket[require-spec] are visible for expanding later
@racket[require-spec]s. The pre-defined forms (as exported by
@racketmodname[racket/base]) are as follows:

 @specsubform[module-path]{ Imports all exported bindings from the
  named module, using the export identifiers as the local identifiers.
  (See below for information on @racket[module-path].) The lexical
  context of the @racket[module-path] form determines the context of
  the introduced identifiers.}

 @defsubform[(only-in require-spec id-maybe-renamed ...)]{
  Like @racket[require-spec], but constrained to those exports for
  which the identifiers to bind match @racket[id-maybe-renamed]: as
  @racket[_id] or as @racket[_orig-id] in @racket[[_orig-id _bind-id]]. If
  the @racket[_id] or @racket[_orig-id] of any @racket[id-maybe-renamed]
  is not in the set that @racket[require-spec] describes, a syntax
  error is reported.

  @defexamples[#:eval (syntax-eval)
    (require (only-in racket/tcp
	              tcp-listen
                      [tcp-accept my-accept]))
    tcp-listen
    my-accept
    tcp-accept
  ]}

 @defsubform[(except-in require-spec id ...)]{ Like
  @racket[require-spec], but omitting those imports for which
  @racket[id]s are the identifiers to bind; if any @racket[id] is not
  in the set that @racket[require-spec] describes, a syntax error is
  reported.

  @defexamples[#:eval (syntax-eval)
    (require (except-in racket/tcp
	                tcp-listen))
    tcp-accept
    tcp-listen
  ]}

 @defsubform[(prefix-in prefix-id require-spec)]{ Like
  @racket[require-spec], but adjusting each identifier to be bound by
  prefixing it with @racket[prefix-id]. The lexical context of the
  @racket[prefix-id] is ignored, and instead preserved from the
  identifiers before prefixing.

  @defexamples[#:eval (syntax-eval)
    (require (prefix-in tcp: racket/tcp))
    tcp:tcp-accept
    tcp:tcp-listen
  ]}

 @defsubform[(rename-in require-spec [orig-id bind-id] ...)]{
  Like @racket[require-spec], but replacing the identifier to
  bind @racket[orig-id] with @racket[bind-id]; if any
  @racket[orig-id] is not in the set that @racket[require-spec]
  describes, a syntax error is reported.
  
  @defexamples[#:eval (syntax-eval)
    (require (rename-in racket/tcp
                        (tcp-accept accept)
			(tcp-listen listen)))
    accept
    listen
  ]}

 @defsubform[(combine-in require-spec ...)]{
  The union of the @racket[require-spec]s.
  
  @defexamples[#:eval (syntax-eval)
    (require (combine-in (only-in racket/tcp tcp-accept)
                         (only-in racket/tcp tcp-listen)))
    tcp-accept
    tcp-listen
  ]}

 @defsubform[(relative-in module-path require-spec ...)]{
  Like the union of the @racket[require-spec]s, but each
  relative module path in a @racket[require-spec] is treated
  as relative to @racket[module-path] instead of the enclosing
  context.

  The @tech{require transformer} that implements @racket[relative-in]
  sets @racket[current-require-module-path] to adjust module paths
  in the @racket[require-spec]s.}

 @defsubform[(only-meta-in phase-level require-spec ...)]{
  Like the combination of @racket[require-spec]s, but removing any
  binding that is not for @racket[phase-level], where @racket[#f] for
  @racket[phase-level] corresponds to the @tech{label phase level}.
  
  The following example imports bindings only at @tech{phase level} 1,
  the transform phase:

  @interaction[#:eval meta-in-eval
  (module nest racket
    (provide (for-syntax meta-eggs)
             (for-meta 1 meta-chicks)
             num-eggs)
    (define-for-syntax meta-eggs 2)
    (define-for-syntax meta-chicks 3)
    (define num-eggs 2))

  (require (only-meta-in 1 'nest))

  (define-syntax (desc stx)
    (printf "~s ~s\n" meta-eggs meta-chicks)
    #'(void))

   (desc)
   num-eggs
  ]

  The following example imports only bindings at @tech{phase level} 0, the
  normal phase.

  @interaction[#:eval meta-in-eval
   (require (only-meta-in 0 'nest))
   num-eggs
  ]}

 @specsubform[#:literals (for-meta)
              (for-meta phase-level require-spec ...)]{Like the combination of
  @racket[require-spec]s, but the binding specified by
  each @racket[require-spec] is shifted by @racket[phase-level]. The
  @tech{label phase level} corresponds to @racket[#f], and a shifting
  combination that involves @racket[#f] produces @racket[#f].
  
  @defexamples[#:eval (syntax-eval)
  (module nest racket
    (provide num-eggs)
    (define num-eggs 2))
  (require (for-meta 0 'nest))
  num-eggs
  (require (for-meta 1 'nest))
  (define-syntax (roost stx)
    (datum->syntax stx num-eggs))
  (roost)
  ]}

 @specsubform[#:literals (for-syntax)
              (for-syntax require-spec ...)]{Same as 
  @racket[(for-meta 1 require-spec ...)].}

 @specsubform[#:literals (for-template)
              (for-template require-spec ...)]{Same as 
  @racket[(for-meta -1 require-spec ...)].}

 @specsubform[#:literals (for-label)
              (for-label require-spec ...)]{Same as 
  @racket[(for-meta #f require-spec ...)].}

 @specsubform[derived-require-spec]{See @racket[define-require-syntax]
 for information on expanding the set of @racket[require-spec]
 forms.}

@guideintro["module-paths"]{module paths}

A @racket[module-path] identifies a module, either a root module or
a @tech{submodule} that is declared lexically within another module.
A root module is identified either through a concrete
name in the form of an identifier, or through an indirect name that
can trigger automatic loading of the module declaration. Except for
the @racket[(#,(racket quote) id)] case below, the actual resolution 
of a root module path is up to the current
@tech{module name resolver} (see
@racket[current-module-name-resolver]), and the description below
corresponds to the default @tech{module name resolver}.

 @specsubform[#:literals (quote)
              (#,(racket quote) id)]{
 Refers to a submodule previously declared with the name
 @racket[id] or a module previously declared interactively with the name
 @racket[id]. When @racket[id] refers to a submodule, @racket[(#,(racket quote) id)]
 is equivalent to @racket[(submod "." id)].

 @examples[
 (code:comment @#,t{a module declared interactively as @racketidfont{test}:})
 (eval:alts (require '@#,racketidfont{test}) (void))]}

 @specsubform[rel-string]{A path relative to the containing source (as
 determined by @racket[current-load-relative-directory] or
 @racket[current-directory]).  Regardless of the current platform,
 @racket[rel-string] is always parsed as a Unix-format relative path:
 @litchar{/} is the path delimiter (multiple adjacent @litchar{/}s are
 treated as a single delimiter), @litchar{..} accesses the parent
 directory, and @litchar{.} accesses the current directory. The path
 cannot be empty or contain a leading or trailing slash, path elements
 before than the last one cannot include a file suffix (i.e., a
 @litchar{.} in an element other than @litchar{.} or @litchar{..}),
 and the only allowed characters are ASCII letters, ASCII digits,
 @litchar{-}, @litchar{+}, @litchar{_}, @litchar{.}, @litchar{/}, and
 @litchar{%}. Furthermore, a @litchar{%} is allowed only when followed
 by two lowercase hexadecimal digits, and the digits must form a
 number that is not the ASCII value of a letter, digit, @litchar{-},
 @litchar{+}, or @litchar{_}.

 @margin-note{The @litchar{%} provision is intended to support a
 one-to-one encoding of arbitrary strings as path elements (after
 UTF-8 encoding). Such encodings are not decoded to arrive at a
 filename, but instead preserved in the file access.}

 If @racket[rel-string] ends with a @filepath{.ss} suffix, it is
 converted to a @filepath{.rkt} suffix. The @tech{compiled-load
 handler} may reverse that conversion if a @filepath{.rkt} file does
 not exist and a @filepath{.ss} exists.

 @examples[
 (code:comment @#,t{a module named @filepath{x.rkt} in the same})
 (code:comment @#,t{directory as the enclosing module's file:})
 (eval:alts (require "x.rkt") (void))
 (code:comment @#,t{a module named @filepath{x.rkt} in the parent directory})
 (code:comment @#,t{of the enclosing module file's directory:})
 (eval:alts (require "../x.rkt") (void))]}

 @defsubform[(lib rel-string ...+)]{A path to a module installed into
 a @tech{collection} (see @secref["collects"]). The @racket[rel-string]s in
 @racket[lib] are constrained similar to the plain @racket[rel-string]
 case, with the additional constraint that a @racket[rel-string]
 cannot contain @litchar{.} or @litchar{..} directory indicators.

 The specific interpretation of the path depends on the number and
 shape of the @racket[rel-string]s:

 @itemize[

    @item{If a single @racket[rel-string] is provided, and if it
    consists of a single element (i.e., no @litchar{/}) with no file
    suffix (i.e., no @litchar{.}), then @racket[rel-string] names a
    @tech{collection}, and @filepath{main.rkt} is the library file name.

    @examples[
    (code:comment @#,t{the main @racketmodname[swindle] library:})
    (eval:alts (require (lib "swindle")) (void))
    (code:comment @#,t{the same:})
    (eval:alts (require (lib "swindle/main.rkt")) (void))]}

    @item{If a single @racket[rel-string] is provided, and if it
    consists of multiple @litchar{/}-separated elements, then each
    element up to the last names a @tech{collection}, subcollection,
    etc., and the last element names a file. If the last element has
    no file suffix, @filepath{.rkt} is added, while a @filepath{.ss}
    suffix is converted to @filepath{.rkt}.

    @examples[
     (code:comment @#,t{@filepath{turbo.rkt} from the @filepath{swindle} collection:})
     (eval:alts (require (lib "swindle/turbo")) (void))
     (code:comment @#,t{the same:})
     (eval:alts (require (lib "swindle/turbo.rkt")) (void))
     (code:comment @#,t{the same:})
     (eval:alts (require (lib "swindle/turbo.ss")) (void))]}

    @item{If a single @racket[rel-string] is provided, and if it
    consists of a single element @italic{with} a file suffix (i.e,
    with a @litchar{.}), then @racket[rel-string] names a file within
    the @filepath{mzlib} @tech{collection}. A @filepath{.ss}
    suffix is converted to @filepath{.rkt}. (This convention is for
    compatibility with older version of Racket.)

    @examples[
    (code:comment @#,t{@filepath{tar.rkt} module from the @filepath{mzlib} collection:})
    (eval:alts (require (lib "tar.ss")) (void))]}

    @item{Otherwise, when multiple @racket[rel-string]s are provided,
    the first @racket[rel-string] is effectively moved after the
    others, and all @racket[rel-string]s are appended with @litchar{/}
    separators. The resulting path names a @tech{collection}, then
    subcollection, etc., ending with a file name. No suffix is added
    automatically, but a @filepath{.ss} suffix is converted to
    @filepath{.rkt}. (This convention is for compatibility with older
    version of Racket.)

    @examples[
    (code:comment @#,t{@filepath{tar.rkt} module from the @filepath{mzlib} collection:})
    (eval:alts (require (lib "tar.ss" "mzlib")) (void))]}
  ]}

 @specsubform[id]{A shorthand for a @racket[lib] form with a single
 @racket[_rel-string] whose characters are the same as in the symbolic
 form of @racket[id]. In addition to the constraints of a @racket[lib]
 @racket[_rel-string], @racket[id] must not contain @litchar{.}.

 @examples[#:eval require-eval
   (eval:alts (require racket/tcp) (void))]}

 @defsubform[(file string)]{Similar to the plain @racket[rel-string]
 case, but @racket[string] is a path---possibly absolute---using the
 current platform's path conventions and @racket[expand-user-path].
 A @filepath{.ss} suffix is converted to @filepath{.rkt}. 

 @examples[(eval:alts (require (file "~/tmp/x.rkt")) (void))]}

 @defsubform*[((planet id)
               (planet string)
               (planet rel-string (user-string pkg-string vers)
                       rel-string ...))]{

 Specifies a library available via the @PLaneT server.

 The first form is a shorthand for the last one, where the @racket[id]'s
 character sequence must match the following @nonterm{spec} grammar:

 @BNF[
 (list @nonterm{spec}
       (BNF-seq @nonterm{owner} @litchar{/} @nonterm{pkg} @nonterm{lib}))
 (list @nonterm{owner} @nonterm{elem})
 (list @nonterm{pkg}
       (BNF-alt @nonterm{elem} (BNF-seq @nonterm{elem} @litchar{:} @nonterm{version})))
 (list @nonterm{version}
       (BNF-alt @nonterm{int} (BNF-seq @nonterm{int} @litchar{:} @nonterm{minor})))
 (list @nonterm{minor}
       (BNF-alt @nonterm{int}
                (BNF-seq @litchar{<=} @nonterm{int})
                (BNF-seq @litchar{>=} @nonterm{int})
                (BNF-seq @litchar{=} @nonterm{int}))
       (BNF-seq @nonterm{int} @litchar{-} @nonterm{int}))
 (list @nonterm{lib} (BNF-alt @nonterm{empty} (BNF-seq @litchar{/} @nonterm{path})))
 (list @nonterm{path} (BNF-alt @nonterm{elem} (BNF-seq @nonterm{elem} @litchar{/} @nonterm{path})))
 ]

 and where an @nonterm{elem} is a non-empty sequence of characters
 that are ASCII letters, ASCII digits, @litchar{-}, @litchar{+},
 @litchar{_}, or @litchar{%} followed by lowercase hexadecimal digits
 (that do not encode one of the other allowed characters), and an
 @nonterm{int} is a non-empty sequence of ASCII digits. As this
 shorthand is expended, a @filepath{.plt} extension is added to
 @nonterm{pkg}, and a @filepath{.rkt} extension is added to
 @nonterm{path}; if no @nonterm{path} is included, @filepath{main.rkt}
 is used in the expansion.

 A @racket[(planet string)] form is like a @racket[(planet id)] form
 with the identifier converted to a string, except that the
 @racket[string] can optionally end with a file extension (i.e., a
 @litchar{.}) for a @nonterm{path}. A @filepath{.ss} file extension is
 converted to @filepath{.rkt}.

 In the more general last form of a @racket[planet] module path, the
 @racket[rel-string]s are similar to the @racket[lib] form, except
 that the @racket[(user-string pkg-string vers)] names a
 @|PLaneT|-based package instead of a @tech{collection}. A version
 specification can include an optional major and minor version, where
 the minor version can be a specific number or a constraint:
 @racket[(_nat _nat)] specifies an inclusive range, @racket[((unsyntax
 (racketidfont "=")) _nat)] specifies an exact match,
 @racket[((unsyntax (racketidfont "+")) _nat)] specifies a minimum
 version and is equivalent to just @racket[_nat], and
 @racket[((unsyntax (racketidfont "-")) _nat)] specifies a maximum
 version. The @racketidfont{=}, @racketidfont{+}, and @racketidfont{-}
 identifiers in a minor-version constraint are recognized
 symbolically.

 @examples[
 (code:comment @#,t{@filepath{main.rkt} in package @filepath{farm} by @filepath{mcdonald}:})
 (eval:alts (require (planet mcdonald/farm)) (void))
 (code:comment @#,t{@filepath{main.rkt} in version >= 2.0 of @filepath{farm} by @filepath{mcdonald}:})
 (eval:alts (require (planet mcdonald/farm:2)) (void))
 (code:comment @#,t{@filepath{main.rkt} in version >= 2.5 of @filepath{farm} by @filepath{mcdonald}:})
 (eval:alts (require (planet mcdonald/farm:2:5)) (void))
 (code:comment @#,t{@filepath{duck.rkt} in version >= 2.5 of @filepath{farm} by @filepath{mcdonald}:})
 (eval:alts (require (planet mcdonald/farm:2:5/duck)) (void))
 ]}

 @defsubform*[((submod root-module submod-path-element ...)
               (submod "." submod-path-element ...)
               (submod ".." submod-path-element ...))]{
  Identifies a @tech{submodule} within the module specified by @racket[root-module]
  or relative to the current module in the case of @racket[(submod "." ....)],
  where  @racket[(submod ".." submod-path-element ...)] is equivalent to
  @racket[(submod "." ".." submod-path-element ...)].
  Submodules have symbolic names, and a sequence of identifiers as @racket[submod-path-element]s
  determine a path of successively nested submodules with the given names.
  A @racket[".."] as a @racket[submod-path-element] names the enclosing module
  of a submodule, and it's intended for use in @racket[(submod "." ....)] 
  and @racket[(submod ".." ....)] forms.}

}

@defform[(local-require require-spec ...)]{

Like @racket[require], but for use in a @tech{internal-definition context} to
import just into the local context. Only bindings from @tech{phase
level} 0 are imported.}


@guideintro["module-provide"]{@racket[provide]}

@defform/subs[#:literals (protect-out all-defined-out all-from-out rename-out 
                          except-out prefix-out struct-out for-meta combine-out
                          for-syntax for-label for-template)
              (provide provide-spec ...)
              ([provide-spec id
                             (all-defined-out)
                             (all-from-out module-path ...)
                             (rename-out [orig-id export-id] ...)
                             (except-out provide-spec provide-spec ...)
                             (prefix-out prefix-id provide-spec)
                             (struct-out id)
                             (combine-out provide-spec ...)
                             (protect-out provide-spec ...)
                             (for-meta phase-level provide-spec ...)
                             (for-syntax provide-spec ...)
                             (for-template provide-spec ...)
                             (for-label provide-spec ...)
                             derived-provide-spec]
               [phase-level exact-integer #f])]{

Declares exports from a module. A @racket[provide] form must appear in
a @tech{module context} or a @tech{module-begin context}.

A @racket[provide-spec] indicates one or more bindings to provide.
For each exported binding, the external name is a symbol that can be
different from the symbolic form of the identifier that is bound
within the module. Also, each export is drawn from a particular
@tech{phase level} and exported at the same @tech{phase level}; by
default, the relevant phase level is the number of
@racket[begin-for-syntax] forms that enclose the @racket[provide]
form.

The syntax of @racket[provide-spec] can be extended by bindings to
@tech{provide transformers} or @tech{provide pre-transformers}, such
as via @racket[define-provide-syntax], but the pre-defined forms are
as follows.

 @specsubform[id]{ Exports @racket[id], which must be @tech{bound}
 within the module (i.e., either defined or imported) at the relevant
 @tech{phase level}. The symbolic form of @racket[id] is used as the
 external name, and the symbolic form of the defined or imported
 identifier must match (otherwise, the external name could be
 ambiguous).

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide num-eggs)
     (define num-eggs 2))
   (require 'nest)
   num-eggs
 ]

 If @racket[id] has a transformer binding to a @tech{rename
 transformer}, then the transformer affects the exported binding. See
 @racket[make-rename-transformer] for more information.}

 @defsubform[(all-defined-out)]{ Exports all identifiers that are
 defined at the relevant @tech{phase level} within the
 exporting module, and that have the same lexical context as the
 @racket[(all-defined-out)] form, excluding bindings to @tech{rename
 transformers} where the target identifier has the
 @racket['not-provide-all-defined] @tech{syntax property}. The
 external name for each identifier is the symbolic form of the
 identifier. Only identifiers accessible from the lexical context of
 the @racket[(all-defined-out)] form are included; that is,
 macro-introduced imports are not re-exported, unless the
 @racket[(all-defined-out)] form was introduced at the same time.

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (all-defined-out))
     (define num-eggs 2))
   (require 'nest)
   num-eggs
 ]}

 @defsubform[(all-from-out module-path ...)]{ Exports all identifiers
 that are imported into the exporting module using a
 @racket[require-spec] built on each @racket[module-path] (see
 @secref["require"]) with no @tech{phase-level} shift.  The symbolic
 name for export is derived from the name that is bound within the
 module, as opposed to the symbolic name of the export from each
 @racket[module-path]. Only identifiers accessible from the lexical
 context of the @racket[module-path] are included; that is,
 macro-introduced imports are not re-exported, unless the
 @racket[module-path] was introduced at the same time.

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide num-eggs)
     (define num-eggs 2))
   (module hen-house racket
     (require 'nest)
     (provide (all-from-out 'nest)))
   (require 'hen-house)
   num-eggs
 ]}

 @defsubform[(rename-out [orig-id export-id] ...)]{ Exports each
 @racket[orig-id], which must be @tech{bound} within the module at
 the relevant @tech{phase level}.  The symbolic name for each export is
 @racket[export-id] instead @racket[orig-d].

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (rename-out [count num-eggs]))
     (define count 2))
   (require 'nest)
   num-eggs
   count
 ]}

 @defsubform[(except-out provide-spec provide-spec ...)]{ Like the
 first @racket[provide-spec], but omitting the bindings listed in each
 subsequent @racket[provide-spec]. If one of the latter bindings is
 not included in the initial @racket[provide-spec], a syntax error is
 reported. The symbolic export name information in the latter
 @racket[provide-spec]s is ignored; only the bindings are used.

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (except-out (all-defined-out)
			  num-chicks))
     (define num-eggs 2)
     (define num-chicks 3))
   (require 'nest)
   num-eggs
   num-chicks
 ]}

 @defsubform[(prefix-out prefix-id provide-spec)]{
 Like @racket[provide-spec], but with each symbolic export name from
 @racket[provide-spec] prefixed with @racket[prefix-id].

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (prefix-out chicken: num-eggs))
     (define num-eggs 2))
   (require 'nest)
   chicken:num-eggs
 ]}

 @defsubform[(struct-out id)]{Exports the bindings associated with a
 structure type @racket[id]. Typically, @racket[id] is bound with
 @racket[(struct id ....)]; more generally, @racket[id] must have a
 @tech{transformer binding} of structure-type information at the relevant
 @tech{phase level}; see @secref["structinfo"].  Furthermore, for
 each identifier mentioned in the structure-type information, the
 enclosing module must define or import one identifier that is
 @racket[free-identifier=?]. If the structure-type information
 includes a super-type identifier, and if the identifier has a
 @tech{transformer binding} of structure-type information, the
 accessor and mutator bindings of the super-type are @italic{not}
 included by @racket[struct-out] for export.

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (struct-out egg))
     (struct egg (color wt)))
   (require 'nest)
   (egg-color (egg 'blue 10))
 ]}

 @defsubform[(combine-out provide-spec ...)]{ The union of the
 @racket[provide-spec]s.

 @defexamples[#:eval (syntax-eval)
   (module nest racket
     (provide (combine-out num-eggs num-chicks))
     (define num-eggs 2)
     (define num-chicks 1))
   (require 'nest)
   num-eggs
   num-chicks
 ]}

 @defsubform[(protect-out provide-spec ...)]{ Like the union of the
 @racket[provide-spec]s, except that the exports are protected;
 requiring modules may refer to these bindings, but may not extract
 these bindings from macro expansions or access them via @racket[eval] without
 access privileges.
 For more details, see @secref["modprotect"]. The @racket[provide-spec] must specify only
 bindings that are defined within the exporting module.

 @examples[#:eval (syntax-eval)
   (module nest racket
     (provide num-eggs (protect-out num-chicks))
     (define num-eggs 2)
     (define num-chicks 3))
   (define weak-inspector (make-inspector (current-code-inspector)))
   (define (weak-eval x)
     (parameterize ([current-code-inspector weak-inspector])
       (define weak-ns (make-base-namespace))
       (namespace-attach-module (current-namespace)
                                ''nest
                                weak-ns)
       (parameterize ([current-namespace weak-ns])
         (namespace-require ''nest)
         (eval x))))
   (require 'nest)
   (list num-eggs num-chicks)
   (weak-eval 'num-eggs)
   (weak-eval 'num-chicks)
 ]}

 @specsubform[#:literals (for-meta) 
              (for-meta phase-level provide-spec ...)]{ Like the union of the
 @racket[provide-spec]s, but adjusted to apply to the @tech{phase
 level} specified by @racket[phase-level] relative to the current
 phase level (where @racket[#f] corresponds to the @tech{label phase
 level}). In particular, an @racket[_id] or @racket[rename-out] form
 as a @racket[provide-spec] refers to a binding at
 @racket[phase-level] relative to the current level, an
 @racket[all-defined-out] exports only definitions at
 @racket[phase-level] relative to the current phase level, and an
 @racket[all-from-out] exports bindings imported with a shift by
 @racket[phase-level].

 @examples[#:eval (syntax-eval)
   (module nest racket
     (begin-for-syntax
      (define eggs 2))
     (define chickens 3)
     (provide (for-syntax eggs)
              chickens))
   (require 'nest)
   (define-syntax (test-eggs stx)
     (printf "Eggs are ~a\n" eggs)
     #'0)
   (test-eggs)
   chickens

   (module broken-nest racket
     (define eggs 2)
     (define chickens 3)
     (provide (for-syntax eggs)
              chickens))

   (module nest2 racket
     (begin-for-syntax
      (define eggs 2))
     (provide (for-syntax eggs)))
   (require (for-meta 2 racket/base)
            (for-syntax 'nest2))
   (define-syntax (test stx)
     (define-syntax (show-eggs stx)
       (printf "Eggs are ~a\n" eggs)
       #'0)
     (begin
       (show-eggs)
       #'0))
   (test)
 ]}

 @specsubform[#:literals (for-syntax) 
              (for-syntax provide-spec ...)]{Same as
 @racket[(for-meta 1 provide-spec ...)].}

 @specsubform[#:literals (for-template) 
              (for-template provide-spec ...)]{Same as
 @racket[(for-meta -1 provide-spec ...)].}

 @specsubform[#:literals (for-label) 
              (for-label provide-spec ...)]{Same as
 @racket[(for-meta #f provide-spec ...)].}

 @specsubform[derived-provide-spec]{See @racket[define-provide-syntax]
 for information on expanding the set of @racket[provide-spec] forms.}

Each export specified within a module must have a distinct symbolic
export name, though the same binding can be specified with the
multiple symbolic names.}


@defform[(for-meta phase-level require-spec ...)]{See @racket[require] and @racket[provide].}
@defform[(for-syntax require-spec ...)]{See @racket[require] and @racket[provide].} @defform[(for-template require-spec ...)]{See @racket[require] and @racket[provide].}
@defform[(for-label require-spec ...)]{See @racket[require] and @racket[provide].}

@defform/subs[(#%require raw-require-spec ...)
              ([raw-require-spec phaseless-spec
                                 (#,(racketidfont "for-meta") phase-level phaseless-spec ...)
                                 (#,(racketidfont "for-syntax") phaseless-spec ...)
                                 (#,(racketidfont "for-template") phaseless-spec ...)
                                 (#,(racketidfont "for-label") phaseless-spec ...)
                                 (#,(racketidfont "just-meta") phase-level raw-require-spec ...)]
               [phase-level exact-integer
                            #f]
               [phaseless-spec raw-module-path
                               (#,(racketidfont "only") raw-module-path id ...)
                               (#,(racketidfont "prefix") prefix-id raw-module-path)
                               (#,(racketidfont "all-except") raw-module-path id ...)
                               (#,(racketidfont "prefix-all-except") prefix-id 
                                                                     raw-module-path id ...)
                               (#,(racketidfont "rename") raw-module-path local-id exported-id)]
               [raw-module-path raw-root-module-path
                                (#,(racketidfont "submod") raw-root-module-path id ...+)
                                (#,(racketidfont "submod") "." id ...+)]
               [raw-root-module-path (#,(racketidfont "quote") id)
                                    rel-string
                                    (#,(racketidfont "lib") rel-string ...)
                                    id
                                    (#,(racketidfont "file") string)
                                    (#,(racketidfont "planet") rel-string
                                                               (user-string pkg-string vers ...))
                                    literal-path])]{

The primitive import form, to which @racket[require] expands. A
@racket[raw-require-spec] is similar to a @racket[_require-spec] in a
@racket[require] form, except that the syntax is more constrained, not
composable, and not extensible. Also, sub-form names like
@racketidfont{for-syntax} and @racketidfont{lib} are recognized
symbolically, instead of via bindings. Although not formalized in the
grammar above, a @racketidfont{just-meta} form cannot appear within a
@racketidfont{just-meta} form.

Each @racket[raw-require-spec] corresponds to the obvious
@racket[_require-spec], but the @racketidfont{rename} sub-form has the
identifiers in reverse order compared to @racket[rename-in].

For most @racket[raw-require-spec]s, the lexical context of the
@racket[raw-require-spec] determines the context of introduced
identifiers. The exception is the @racketidfont{rename} sub-form,
where the lexical context of the @racket[local-id] is preserved.

A @racket[literal-path] as a @racket[raw-root-module-path] corresponds
to a path in the sense of @racket[path?]. Since path values are never
produced by @racket[read-syntax], they appear only in programmatically
constructed expressions. They also appear naturally as arguments to
functions such as @racket[namespace-require], with otherwise take a
quoted @racket[raw-module-spec].}


@defform/subs[(#%provide raw-provide-spec ...)
              ([raw-provide-spec phaseless-spec
                                 (#,(racketidfont "for-meta") phase-level phaseless-spec)
                                 (#,(racketidfont "for-syntax") phaseless-spec)
                                 (#,(racketidfont "for-label") phaseless-spec)
                                 (#,(racketidfont "protect") raw-provide-spec)]
               [phase-level exact-integer
                            #f]
               [phaseless-spec id 
                               (#,(racketidfont "rename") local-id export-id) 
                               (#,(racketidfont "struct") struct-id (field-id ...))
                               (#,(racketidfont "all-from") raw-module-path)
                               (#,(racketidfont "all-from-except") raw-module-path id ...)
                               (#,(racketidfont "all-defined"))
                               (#,(racketidfont "all-defined-except") id ...)
                               (#,(racketidfont "prefix-all-defined") prefix-id) 
                               (#,(racketidfont "prefix-all-defined-except") prefix-id id ...)
                               (#,(racketidfont "protect") phaseless-spec ...)
                               (#,(racketidfont "expand") (id . datum))])]{

The primitive export form, to which @racket[provide] expands.  A
@racket[_raw-module-path] is as for @racket[#%require]. A
@racketidfont{protect} sub-form cannot appear within a
@racket[protect] sub-form.

Like @racket[#%require], the sub-form keywords for @racket[#%provide]
are recognized symbolically, and nearly every
@racket[raw-provide-spec] has an obvious equivalent
@racket[_provide-spec] via @racket[provide], with the exception of the
@racketidfont{struct} and @racketidfont{expand} sub-forms.

A @racket[(#,(racketidfont "struct") struct-id (field-id ...))]
sub-form expands to @racket[struct-id],
@racketidfont{make-}@racket[struct-id],
@racketidfont{struct:}@racket[struct-id],
@racket[struct-id]@racketidfont{?},
@racket[struct-id]@racketidfont{-}@racket[field-id] for each
@racket[field-id], and
@racketidfont{set-}@racket[struct-id]@racketidfont{-}@racket[field-id]@racketidfont{!}
for each @racket[field-id]. The lexical context of the
@racket[struct-id] is used for all generated identifiers.

Unlike @racket[#%require], the @racket[#%provide] form is
macro-extensible via an explicit @racketidfont{expand} sub-form; the
@racket[(id . datum)] part is locally expanded as an expression (even
though it is not actually an expression), stopping when a
@racket[begin] form is produced; if the expansion result is
@racket[(begin raw-provide-spec ...)], it is spliced in place of the
@racketidfont{expand} form, otherwise a syntax error is reported. The
@racketidfont{expand} sub-form is not normally used directly; it
provides a hook for implementing @racket[provide] and @tech{provide
transformers}.

The @racketidfont{all-from} and @racketidfont{all-from-except} forms
re-export only identifiers that are accessible in lexical context of
the @racketidfont{all-from} or @racketidfont{all-from-except} form
itself. That is, macro-introduced imports are not re-exported, unless
the @racketidfont{all-from} or @racketidfont{all-from-except} form was
introduced at the same time. Similarly, @racketidfont{all-defined} and
its variants export only definitions accessible from the lexical
context of the @racket[phaseless-spec] form.}

@; --------------------

@subsection{Additional @racket[require] Forms}

@note-lib-only[racket/require]

The following forms support more complex selection and manipulation of
sets of imported identifiers.

@defform[(matching-identifiers-in regexp require-spec)]{

Like @racket[require-spec], but including only imports whose names
 match @racket[regexp].  The @racket[regexp] must be a literal regular
 expression (see @secref["regexp"]).

@defexamples[#:eval (syntax-eval)
(module zoo racket/base
  (provide tunafish swordfish blowfish
           monkey lizard ant)
  (define tunafish 1)
  (define swordfish 2)
  (define blowfish 3)
  (define monkey 4)
  (define lizard 5)
  (define ant 6))
(require racket/require)
(require (matching-identifiers-in #rx"\\w*fish" 'zoo))
tunafish
swordfish
blowfish
monkey
]}

@defform[(subtract-in require-spec subtracted-spec ...)]{

Like @racket[require-spec], but omitting those imports that would be
  imported by one of the @racket[subtracted-spec]s.

@defexamples[#:eval (syntax-eval)
(module earth racket
  (provide land sea air)
  (define land 1)
  (define sea 2)
  (define air 3))

(module mars racket
  (provide aliens)
  (define aliens 4))

(module solar-system racket
  (require 'earth 'mars)
  (provide (all-from-out 'earth)
           (all-from-out 'mars)))

(require racket/require)
(require (subtract-in 'solar-system 'earth))
land
aliens
]}

@defform[(filtered-in proc-expr require-spec)]{ 

  Applies an arbitrary transformation on the import names (as strings)
  of @racket[require-spec]. The @racket[proc-expr] must evaluate at
  expansion time to a single-argument procedure, which is applied on
  each of the names from @racket[require-spec].  For each name, the
  procedure must return either a string for the import's new name or
  @racket[#f] to exclude the import.

  For example,
  @racketblock[
    (require (filtered-in
              (lambda (name)
                (and (regexp-match? #rx"^[a-z-]+$" name)
                     (regexp-replace #rx"-" (string-titlecase name) "")))
              racket/base))]
  imports only bindings from @racketmodname[racket/base] that match the
  pattern @racket[#rx"^[a-z-]+$"], and it converts the names to ``camel case.''}

@defform[(path-up rel-string ...)]{

Specifies paths to modules named by the @racket[rel-string]s similar
to using the @racket[rel-string]s directly, except that if a required
module file is not found relative to the enclosing source, it is
searched for in the parent directory, and then in the grand-parent
directory, etc., all the way to the root directory. The discovered
path relative to the enclosing source becomes part of the expanded
form.

This form is useful in setting up a ``project environment.''  For
example, using the following @filepath{config.rkt} file in the root
directory of your project:
@racketmod[
  racket/base
  (require racket/require-syntax 
           (for-syntax "utils/in-here.rkt"))
  ;; require form for my utilities
  (provide utils-in)
  (define-require-syntax utils-in in-here-transformer)
]
and using @filepath{utils/in-here.rkt} under the same root directory:
@racketmod[
  racket/base
  (require racket/runtime-path)
  (provide in-here-transformer)
  (define-runtime-path here ".")
  (define (in-here-transformer stx)
    (syntax-case stx ()
      [(_ sym)
       (identifier? #'sym)
       (let ([path (build-path here (format "~a.rkt" (syntax-e #'sym)))])
         (datum->syntax stx `(file ,(path->string path)) stx))]))
]
then @racket[path-up] works for any other module under the project
 directory to find @filepath{config.rkt}:
@racketblock[
  (require racket/require 
           (path-up "config.rkt")
           (utils-in foo))]
Note that the order of requires in the example is important, as each of
the first two bind the identifier used in the following.

An alternative in this scenario is to use @racket[path-up] directly to
find the utility module:
@racketblock[
  (require racket/require 
           (path-up "utils/foo.rkt"))]
but then sub-directories that are called
@filepath{utils} override the one in the project's root.
In other words, the previous method requires only a single unique name.}

@defform/subs[(multi-in subs ...+)
              ([subs sub-path
                     (sub-path ...)]
               [sub-path rel-string
                         id])]{

Specifies multiple files to be required from a hierarchy of
directories or collections. The set of required module paths is computed
as the Cartesian product of the @racket[subs] groups, where each
@racket[sub-path] is combined with other @racket[sub-path]s in order
using a @litchar{/} separator. A @racket[sub-path] as a @racket[subs]
is equivalent to @racket[(sub-path)]. All @racket[sub-path]s in a given
@racket[multi-in] form must be either strings or identifiers.

Examples:

@subeqivs[
[(require (multi-in racket (dict @#,racketidfont{list})))
 (require racket/dict racket/list)]
[(require (multi-in "math" "matrix" "utils.rkt"))
 (require "math/matrix/utils.rkt")]
[(require (multi-in "utils" ("math.rkt" "matrix.rkt")))
 (require "utils/math.rkt" "utils/matrix.rkt")]
[(require (multi-in ("math" "matrix") "utils.rkt"))
 (require "math/utils.rkt" "matrix/utils.rkt")]
[(require (multi-in ("math" "matrix") ("utils.rkt" "helpers.rkt")))
 (require "math/utils.rkt" "math/helpers.rkt"
          "matrix/utils.rkt" "matrix/helpers.rkt")]
]}

@; --------------------

@subsection{Additional @racket[provide] Forms}

@note-lib-only[racket/provide]

@defform[(matching-identifiers-out regexp provide-spec)]{ Like
  @racket[provide-spec], but including only exports of bindings with
  an external name that matches @racket[regexp]. The @racket[regexp]
  must be a literal regular expression (see @secref["regexp"]).}

@defform[(filtered-out proc-expr provide-spec)]{

 Analogous to @racket[filtered-in], but for filtering and renaming
 exports.

  For example,
  @racketblock[
    (provide (filtered-out
              (lambda (name)
                (and (regexp-match? #rx"^[a-z-]+$" name)
                     (regexp-replace
                      #rx"-" (string-titlecase name) "")))
              (all-defined-out)))]
  exports only bindings that match the
  pattern @racket[#rx"^[a-z-]+$"], and it converts the names to ``camel case.''}

@;------------------------------------------------------------------------
@section[#:tag "quote"]{Literals: @racket[quote] and @racket[#%datum]}

Many forms are implicitly quoted (via @racket[#%datum]) as literals. See
@secref["expand-steps"] for more information.

@guideintro["quote"]{@racket[quote]}

@defform[(quote datum)]{

Produces a constant value corresponding to @racket[datum] (i.e., the
representation of the program fragment) without its @tech{lexical
information}, source location, etc.  Quoted pairs, vectors, and boxes
are immutable.

@mz-examples[
(eval:alts (#,(racketkeywordfont "quote") x) 'x)
(eval:alts (#,(racketkeywordfont "quote") (+ 1 2)) '(+ 1 2))
(+ 1 2)
]

}

@defform[(#%datum . datum)]{

Expands to @racket[(#,(racketkeywordfont "quote") datum)], as long as
@racket[datum] is not a keyword. If @racket[datum] is a keyword, a
syntax error is reported.

See also @secref["expand-steps"] for information on how the expander
introduces @racketidfont{#%datum} identifiers.

@mz-examples[
(#%datum . 10)
(#%datum . x)
(#%datum . #:x)
]
}

@;------------------------------------------------------------------------
@section[#:tag "#%expression"]{Expression Wrapper: @racket[#%expression]}

@defform[(#%expression expr)]{

Produces the same result as @racket[expr]. The only use of
@racket[#%expression] is to force the parsing of a form as an
expression.

@mz-examples[
(#%expression (+ 1 2))
(#%expression (define x 10))
]}

@;------------------------------------------------------------------------
@section[#:tag "#%top"]{Variable References and @racket[#%top]}

@defform/none[id]{

Refers to a module-level or local binding, when @racket[id] is
not bound as a transformer (see @secref["expansion"]). At run-time,
the reference evaluates to the value in the @tech{location} associated with
the binding.

When the expander encounters an @racket[id] that is not bound by a
module-level or local binding, it converts the expression to
@racket[(@#,racketidfont{#%top} . id)] giving @racketidfont{#%top}
the lexical context of the @racket[id]; typically, that context refers
to @racket[#%top]. See also @secref["expand-steps"].

@examples[
(define x 10)
x
(let ([x 5]) x)
((lambda (x) x) 2)
]}

@defform[(#%top . id)]{

Refers to a module-level or top-level definition that could bind
@racket[id], even if @racket[id] has a local binding in its context.

Within a @racket[module] form, @racket[(#%top . id)] expands to just
@racket[id]---with the obligation that @racket[id] is defined within
the module. At @tech{phase level} 0, @racket[(#%top . id)] is an
immediate syntax error if @racket[id] is not bound. At @tech{phase
level} 1 and higher, a syntax error is reported if @racket[id] is not
defined at the corresponding phase by the end of @racket[module]-body
@tech{partial expansion}.

See also @secref["expand-steps"] for information on how the expander
introduces @racketidfont{#%top} identifiers.

@examples[
(define x 12)
(let ([x 5]) (#%top . x))
]}

@;------------------------------------------------------------------------
@section{Locations: @racket[#%variable-reference]}

@defform*[#:literals (#%top)
          [(#%variable-reference id)
           (#%variable-reference (#%top . id))
           (#%variable-reference)]]{

Produces an opaque @deftech{variable reference} value representing the
@tech{location} of @racket[id], which must be bound as a variable. If
no @racket[id] is supplied, the resulting value refers to an
``anonymous'' variable defined within the enclosing context (i.e.,
within the enclosing module, or at the top level if the form is not
inside a module).

A @tech{variable reference} can be used with
@racket[variable-reference->empty-namespace],
@racket[variable-reference->resolved-module-path], and
@racket[variable-reference->namespace], but facilities like
@racket[define-namespace-anchor] and
@racket[namespace-anchor->namespace] wrap those to provide a clearer
interface. A @tech{variable reference} is also useful to low-level
extensions; see @other-manual['(lib
"scribblings/inside/inside.scrbl")].}

@;------------------------------------------------------------------------
@section[#:tag "application"]{Procedure Applications and @racket[#%app]}

@section-index{evaluation order}

@guideintro["application"]{procedure applications}

@defform/none[(proc-expr arg ...)]{

Applies a procedure, when @racket[proc-expr] is not an
identifier that has a transformer binding (see
@secref["expansion"]).

More precisely, the expander converts this form to
@racket[(@#,racketidfont{#%app} proc-expr arg ...)], giving
@racketidfont{#%app} the lexical context that is associated with the
original form (i.e., the pair that combines @racket[proc-expr] and its
arguments). Typically, the lexical context of the pair indicates the
procedure-application @racket[#%app] that is described next. See also
@secref["expand-steps"].

@mz-examples[
(+ 1 2)
((lambda (x #:arg y) (list y x)) #:arg 2 1)
]}

@defform[(#%app proc-expr arg ...)]{

Applies a procedure. Each @racket[arg] is one of the following:

 @specsubform[arg-expr]{The resulting value is a non-keyword
                        argument.}

 @specsubform[(code:line keyword arg-expr)]{The resulting value is a
              keyword argument using @racket[keyword]. Each
              @racket[keyword] in the application must be distinct.}

The @racket[proc-expr] and @racket[_arg-expr]s are evaluated in order,
left to right. If the result of @racket[proc-expr] is a procedure that
accepts as many arguments as non-@racket[_keyword]
@racket[_arg-expr]s, if it accepts arguments for all of the
@racket[_keyword]s in the application, and if all required
keyword-based arguments are represented among the @racket[_keyword]s
in the application, then the procedure is called with the values of
the @racket[arg-expr]s. Otherwise, the @exnraise[exn:fail:contract].

The continuation of the procedure call is the same as the continuation
of the application expression, so the results of the procedure are the
results of the application expression.

The relative order of @racket[_keyword]-based arguments matters only
for the order of @racket[_arg-expr] evaluations; the arguments are
associated with argument variables in the applied procedure based on
the @racket[_keyword]s, and not their positions. The other
@racket[_arg-expr] values, in contrast, are associated with variables
according to their order in the application form.

See also @secref["expand-steps"] for information on how the
expander introduces @racketidfont{#%app} identifiers.

@mz-examples[
(#%app + 1 2)
(#%app (lambda (x #:arg y) (list y x)) #:arg 2 1)
(#%app cons)
]}

@defform*[[(#%plain-app proc-expr arg-expr ...)
           (#%plain-app)]]{

Like @racket[#%app], but without support for keyword arguments.
As a special case, @racket[(#%plain-app)] produces @racket['()].}

@;------------------------------------------------------------------------
@section[#:tag "lambda"]{Procedure Expressions: @racket[lambda] and @racket[case-lambda]}

@guideintro["lambda"]{procedure expressions}

@deftogether[(
@defform[(lambda kw-formals body ...+)]
@defform/subs[(λ kw-formals body ...+)
              ([kw-formals (arg ...)
                           (arg ...+ . rest-id)
                           rest-id]
               [arg id
                    [id default-expr]
                    (code:line keyword id)
                    (code:line keyword [id default-expr])])]
)]{

Produces a procedure. The @racket[kw-formals] determines the number of
arguments and which keyword arguments that the procedure accepts.

Considering only the first @racket[arg] case, a simple
@racket[kw-formals] has one of the following three forms:

@specsubform[(id ...)]{ The procedure accepts as many non-keyword
       argument values as the number of @racket[id]s. Each @racket[id]
       is associated with an argument value by position.}

@specsubform[(id ...+ . rest-id)]{ The procedure accepts any number of
       non-keyword arguments greater or equal to the number of
       @racket[id]s. When the procedure is applied, the @racket[id]s
       are associated with argument values by position, and all
       leftover arguments are placed into a list that is associated to
       @racket[rest-id].}

@specsubform[rest-id]{ The procedure accepts any number of non-keyword
       arguments. All arguments are placed into a list that is
       associated with @racket[rest-id].}

More generally, an @racket[arg] can include a keyword and/or default
value. Thus, the first two cases above are more completely specified
as follows:

@specsubform[(arg ...)]{ Each @racket[arg] has the following
       four forms:

        @specsubform[id]{Adds one to both the minimum and maximum
        number of non-keyword arguments accepted by the procedure. The
        @racket[id] is associated with an actual argument by
        position.}

        @specsubform[[id default-expr]]{Adds one to the maximum number
        of non-keyword arguments accepted by the procedure. The
        @racket[id] is associated with an actual argument by position,
        and if no such argument is provided, the @racket[default-expr]
        is evaluated to produce a value associated with @racket[id].
        No @racket[arg] with a @racket[default-expr] can appear
        before an @racket[id] without a @racket[default-expr] and
        without a @racket[keyword].}

       @specsubform[(code:line keyword id)]{The procedure requires a
       keyword-based argument using @racket[keyword]. The @racket[id]
       is associated with a keyword-based actual argument using
       @racket[keyword].}

       @specsubform[(code:line keyword [id default-expr])]{The
       procedure accepts a keyword-based using @racket[keyword]. The
       @racket[id] is associated with a keyword-based actual argument
       using @racket[keyword], if supplied in an application;
       otherwise, the @racket[default-expr] is evaluated to obtain a
       value to associate with @racket[id].}

      The position of a @racket[_keyword] @racket[arg] in
      @racket[kw-formals] does not matter, but each specified
      @racket[keyword] must be distinct.}

@specsubform[(arg ...+ . rest-id)]{ Like the previous case, but
       the procedure accepts any number of non-keyword arguments
       beyond its minimum number of arguments. When more arguments are
       provided than non-@racket[_keyword] arguments among the
       @racket[arg]s, the extra arguments are placed into a
       list that is associated to @racket[rest-id].}

The @racket[kw-formals] identifiers are bound in the
@racket[body]s. When the procedure is applied, a new @tech{location}
is created for each identifier, and the location is filled with the
associated argument value. The @tech{locations} are created and filled
in order, with @racket[_default-expr]s evaluated as needed to fill
locations. @margin-note{In other words, argument bindings with
default-value expressions are evaluated analogous to @racket[let*].}

If any identifier appears in the @racket[body]s that is not one of the
identifiers in @racket[kw-formals], then it refers to the same
location that it would if it appeared in place of the @racket[lambda]
expression. (In other words, variable reference is lexically scoped.)

When multiple identifiers appear in a @racket[kw-formals], they must
be distinct according to @racket[bound-identifier=?].

If the procedure produced by @racket[lambda] is applied to fewer or
more by-position or by-keyword arguments than it accepts, to by-keyword arguments
that it does not accept, or without required by-keyword arguments, then
the @exnraise[exn:fail:contract].

The last @racket[body] expression is in tail position with respect to
the procedure body.

@mz-examples[
((lambda (x) x) 10)
((lambda (x y) (list y x)) 1 2)
((lambda (x [y 5]) (list y x)) 1 2)
(let ([f (lambda (x #:arg y) (list y x))])
 (list (f 1 #:arg 2)
       (f #:arg 2 1)))
]

When compiling a @racket[lambda] or @racket[case-lambda] expression,
Racket looks for a @indexed-racket['method-arity-error] property
attached to the expression (see @secref["stxprops"]). If it is
present with a true value, and if no case of the procedure accepts
zero arguments, then the procedure is marked so that an
@racket[exn:fail:contract:arity] exception involving the procedure
will hide the first argument, if one was provided. (Hiding the first
argument is useful when the procedure implements a method, where the
first argument is implicit in the original source). The property
affects only the format of @racket[exn:fail:contract:arity]
exceptions, not the result of @racket[procedure-arity].

When a keyword-accepting procedure is bound to an identifier in
certain ways, and when the identifier is used in the function position
of an application form, then the application form may be expanded in
such a way that the original binding is obscured as the target of the
application. To help expose the connection between the function
application and function declaration, an identifier in the expansion
of the function application is tagged with a @tech{syntax property}
accessible via @racket[syntax-procedure-alias-property] if it is effectively an alias
for the original identifier. An identifier in the expansion is tagged with a
@tech{syntax property} accessible via @racket[syntax-procedure-converted-arguments-property] if it
is like the original identifier except that the arguments are converted to a
flattened form: keyword arguments, required by-position arguments,
by-position optional arguments, and rest arguments---all as required,
by-position arguments; the keyword arguments are sorted by keyword
name, each optional keyword argument is followed by a boolean to
indicate whether a value is provided, and @racket[#f] is used for an
optional keyword argument whose value is not provided; optional
by-position arguments include @racket[#f] for each non-provided
argument, and then the sequence of optional-argument values is
followed by a parallel sequence of booleans to indicate whether each
optional-argument value was provided.}


@defform/subs[(case-lambda [formals body ...+] ...)
              ([formals (id ...)
                        (id ...+ . rest-id)
                        rest-id])]{
               
Produces a procedure. Each @racket[[formals body ...+]]
clause is analogous to a single @racket[lambda] procedure; applying
the @racket[case-lambda]-generated procedure is the same as applying a
procedure that corresponds to one of the clauses---the first procedure
that accepts the given number of arguments. If no corresponding
procedure accepts the given number of arguments, the
@exnraise[exn:fail:contract].

Note that a @racket[case-lambda] clause supports only
@racket[formals], not the more general @racket[_kw-formals] of
@racket[lambda]. That is, @racket[case-lambda] does not directly
support keyword and optional arguments.

@mz-examples[
(let ([f (case-lambda
          [() 10]
          [(x) x]
          [(x y) (list y x)]
          [r r])])
  (list (f)
        (f 1)
        (f 1 2)
        (f 1 2 3)))
]}

@defform[(#%plain-lambda formals body ...+)]{
Like @racket[lambda], but without support for keyword or optional arguments.
}

@;------------------------------------------------------------------------
@section[#:tag "let"]{Local Binding: @racket[let], @racket[let*], @racket[letrec], ...}

@guideintro["let"]{local binding}

@defform*[[(let ([id val-expr] ...) body ...+)
           (let proc-id ([id init-expr] ...) body ...+)]]{

The first form evaluates the @racket[val-expr]s left-to-right, creates
a new @tech{location} for each @racket[id], and places the values into the
locations. It then evaluates the @racket[body]s, in which the
@racket[id]s are bound. The last @racket[body] expression is in
tail position with respect to the @racket[let] form. The @racket[id]s
must be distinct according to @racket[bound-identifier=?].

@mz-examples[
(let ([x 5]) x)
(let ([x 5])
  (let ([x 2]
        [y x])
    (list y x)))
]

The second form evaluates the @racket[init-expr]s; the resulting
values become arguments in an application of a procedure
@racket[(lambda (id ...) body ...+)], where @racket[proc-id] is bound
within the @racket[body]s to the procedure itself.}

@mz-examples[
(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
]

@defform[(let* ([id val-expr] ...) body ...+)]{

Like @racket[let], but evaluates the @racket[val-expr]s one by
one, creating a @tech{location} for each @racket[id] as soon as the value is
available. The @racket[id]s are bound in the remaining @racket[val-expr]s
as well as the @racket[body]s, and the @racket[id]s need not be
distinct; later bindings shadow earlier bindings.

@mz-examples[
(let* ([x 1]
       [y (+ x 1)])
  (list y x))
]}

@defform[(letrec ([id val-expr] ...) body ...+)]{

Like @racket[let], including left-to-right evaluation of the @racket[val-expr]s,
but the @tech{locations} for all @racket[id]s are
created first and filled with @|undefined-const|, all
@racket[id]s are bound in all @racket[val-expr]s as well as the
@racket[body]s, and each @racket[id] is set immediately after the
corresponding @racket[val-expr] is evaluated. The @racket[id]s must be distinct according to
@racket[bound-identifier=?].

@mz-examples[
(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))]
         [is-odd? (lambda (n)
                    (and (not (zero? n))
                         (is-even? (sub1 n))))])
  (is-odd? 11))
]}

@defform[(let-values ([(id ...) val-expr] ...) body ...+)]{ Like
@racket[let], except that each @racket[val-expr] must produce as many
values as corresponding @racket[id]s, otherwise the
@exnraise[exn:fail:contract]. A separate @tech{location} is created for each
@racket[id], all of which are bound in the @racket[body]s.

@mz-examples[
(let-values ([(x y) (quotient/remainder 10 3)])
  (list y x))
]}

@defform[(let*-values ([(id ...) val-expr] ...) body ...+)]{ Like
@racket[let*], except that each @racket[val-expr] must produce as many
values as corresponding @racket[id]s. A separate @tech{location} is created
for each @racket[id], all of which are bound in the later
@racket[val-expr]s and in the @racket[body]s.

@mz-examples[
(let*-values ([(x y) (quotient/remainder 10 3)]
              [(z) (list y x)])
  z)
]}

@defform[(letrec-values ([(id ...) val-expr] ...) body ...+)]{ Like
@racket[letrec], except that each @racket[val-expr] must produce as
many values as corresponding @racket[id]s. A separate @tech{location} is
created for each @racket[id], all of which are initialized to
@|undefined-const| and bound in all @racket[val-expr]s
and in the @racket[body]s.

@mz-examples[
(letrec-values ([(is-even? is-odd?)
                 (values
                   (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))
                   (lambda (n)
                     (or (= n 1)
                         (is-even? (sub1 n)))))])
  (is-odd? 11))
]}

@defform[(let-syntax ([id trans-expr] ...) body ...+)]{

@margin-note/ref{See also @racket[splicing-let-syntax].}

Creates a @tech{transformer binding} (see
@secref["transformer-model"]) of each @racket[id] with the value of
@racket[trans-expr], which is an expression at @tech{phase level} 1
relative to the surrounding context. (See @secref["id-model"] for
information on @tech{phase levels}.)

The evaluation of each @racket[trans-expr] is @racket[parameterize]d
to set @racket[current-namespace] to a @tech{namespace} that shares
@tech{bindings} and @tech{variables} with the namespace being used to
expand the @racket[let-syntax] form, except that its @tech{base phase}
is one greater.

Each @racket[id] is bound in the @racket[body]s, and not in other
@racket[trans-expr]s.}

@defform[(letrec-syntax ([id trans-expr] ...) body ...+)]{

@margin-note/ref{See also @racket[splicing-letrec-syntax].}

Like @racket[let-syntax], except that each @racket[id] is also bound
within all @racket[trans-expr]s.}

@defform[(let-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

@margin-note/ref{See also @racket[splicing-let-syntaxes].}

Like @racket[let-syntax], but each @racket[trans-expr] must produce as
many values as corresponding @racket[id]s, each of which is bound to
the corresponding value.}

@defform[(letrec-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

@margin-note/ref{See also @racket[splicing-letrec-syntaxes].}

Like @racket[let-syntax], except that each @racket[id] is also bound
within all @racket[trans-expr]s.}

@defform[(letrec-syntaxes+values ([(trans-id ...) trans-expr] ...)
                                 ([(val-id ...) val-expr] ...)
            body ...+)]{

Combines @racket[letrec-syntaxes] with a variant of
@racket[letrec-values]: each @racket[trans-id] and @racket[val-id] is
bound in all @racket[trans-expr]s and @racket[val-expr]s.

The @racket[letrec-syntaxes+values] form is the core form for local
compile-time bindings, since forms like @racket[letrec-syntax] and
@tech{internal-definition contexts} expand to it. In a fully expanded
expression (see @secref["fully-expanded"]), the @racket[trans-id]
bindings are discarded and the form reduces to a combination of
@racket[letrec-values] or @racket[let-values], but
@racket[letrec-syntaxes+values] can appear in the result of
@racket[local-expand] with an empty stop list.

For variables bound by @racket[letrec-syntaxes+values], the
@tech{location}-creation rules differ slightly from
@racket[letrec-values]. The @racket[[(val-id ...) val-expr]] binding
clauses are partitioned into minimal sets of clauses that satisfy the
following rule: if a clause has a @racket[val-id] binding that is
referenced (in a full expansion) by the @racket[val-expr] of an
earlier clause, the two clauses and all in between are in the same
set. If a set consists of a single clause whose @racket[val-expr] does
not refer to any of the clause's @racket[val-id]s, then
@tech{locations} for the @racket[val-id]s are created @emph{after} the
@racket[val-expr] is evaluated. Otherwise, @tech{locations} for all
@racket[val-id]s in a set are created just before the first
@racket[val-expr] in the set is evaluated.

The end result of the @tech{location}-creation rules is that scoping
and evaluation order are the same as for @racket[letrec-values], but
the compiler has more freedom to optimize away @tech{location}
creation. The rules also correspond to a nesting of
@racket[let-values] and @racket[letrec-values], which is how
@racket[letrec-syntaxes+values] for a fully-expanded expression.

See also @racket[local], which supports local bindings with
@racket[define], @racket[define-syntax], and more.}

@;------------------------------------------------------------------------
@section[#:tag "local"]{Local Definitions: @racket[local]}

@note-lib[racket/local]

@defform[(local [definition ...] body ...+)]{

Like @racket[letrec-syntaxes+values], except that the bindings are
expressed in the same way as in the top-level or in a module body:
using @racket[define], @racket[define-values], @racket[define-syntax],
@racket[struct], etc.  Definitions are distinguished from
non-definitions by partially expanding @racket[definition] forms (see
@secref["partial-expansion"]). As in the top-level or in a module
body, a @racket[begin]-wrapped sequence is spliced into the sequence
of @racket[definition]s.}

@;------------------------------------------------------------------------
@include-section["shared.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "if"]{Conditionals: @racket[if], @racket[cond], @racket[and], and @racket[or]}

@guideintro["conditionals"]{conditionals}

@defform[(if test-expr then-expr else-expr)]{

Evaluates @racket[test-expr]. If it produces any value other than
@racket[#f], then @racket[then-expr] is evaluated, and its results are
the result for the @racket[if] form. Otherwise, @racket[else-expr] is
evaluated, and its results are the result for the @racket[if]
form. The @racket[then-expr] and @racket[else-expr] are in tail
position with respect to the @racket[if] form.

@mz-examples[
(if (positive? -5) (error "doesn't get here") 2)
(if (positive? 5) 1 (error "doesn't get here"))
(if 'we-have-no-bananas "yes" "no")
]}

@defform/subs[#:literals (else =>)
              (cond cond-clause ...)
              ([cond-clause [test-expr then-body ...+]
                            [else then-body ...+]
                            [test-expr => proc-expr]
                            [test-expr]])]{

@guideintro["cond"]{@racket[cond]}

A @racket[cond-clause] that starts with @racket[else] must be the last
@racket[cond-clause].

If no @racket[cond-clause]s are present, the result is @|void-const|.

If only a @racket[[else then-body ...+]] is present, then the
@racket[then-body]s are evaluated. The results from all but the last
@racket[then-body] are ignored. The results of the last
@racket[then-body], which is in tail position with respect to the
@racket[cond] form, are the results for the whole @racket[cond]
form.

Otherwise, the first @racket[test-expr] is evaluated. If it produces
@racket[#f], then the result is the same as a @racket[cond] form with
the remaining @racket[cond-clause]s, in tail position with respect to
the original @racket[cond] form. Otherwise, evaluation depends on the
form of the @racket[cond-clause]:

@specsubform[[test-expr then-body ...+]]{The @racket[then-body]s are
evaluated in order, and the results from all but the last
@racket[then-body] are ignored. The results of the last
@racket[then-body], which is in tail position with respect to the
@racket[cond] form, provides the result for the whole @racket[cond]
form.}

@specsubform[#:literals (=>) [test-expr => proc-expr]]{The @racket[proc-expr] is
evaluated, and it must produce a procedure that accepts one argument,
otherwise the @exnraise[exn:fail:contract]. The procedure is applied
to the result of @racket[test-expr] in tail position with respect to
the @racket[cond] expression.}

@specsubform[[test-expr]]{The result of the @racket[test-expr] is
returned as the result of the @racket[cond] form. The
@racket[test-expr] is not in tail position.}

@mz-examples[
(cond)
(cond
  [else 5])
(cond
 [(positive? -5) (error "doesn't get here")]
 [(zero? -5) (error "doesn't get here, either")]
 [(positive? 5) 'here])
(cond
 [(member 2 '(1 2 3)) => (lambda (l) (map - l))])
(cond
 [(member 2 '(1 2 3))])
]}


@defidform[else]{

Recognized specially within forms like @racket[cond]. An
@racket[else] form as an expression is a syntax error.}


@defidform[=>]{

Recognized specially within forms like @racket[cond]. A
@racket[=>] form as an expression is a syntax error.}


@defform[(and expr ...)]{

@guideintro["and+or"]{@racket[and]}

If no @racket[expr]s are provided, then result is @racket[#t].

If a single @racket[expr] is provided, then it is in tail position, so
the results of the @racket[and] expression are the results of the
@racket[expr].

Otherwise, the first @racket[expr] is evaluated. If it produces
@racket[#f], the result of the @racket[and] expression is
@racket[#f]. Otherwise, the result is the same as an @racket[and]
expression with the remaining @racket[expr]s in tail position with
respect to the original @racket[and] form.

@mz-examples[
(and)
(and 1)
(and (values 1 2))
(and #f (error "doesn't get here"))
(and #t 5)
]}

@defform[(or expr ...)]{

@guideintro["and+or"]{@racket[or]}

If no @racket[expr]s are provided, then result is @racket[#f].

If a single @racket[expr] is provided, then it is in tail position, so
the results of the @racket[or] expression are the results of the
@racket[expr].

Otherwise, the first @racket[expr] is evaluated. If it produces a
value other than @racket[#f], that result is the result of the
@racket[or] expression. Otherwise, the result is the same as an
@racket[or] expression with the remaining @racket[expr]s in tail
position with respect to the original @racket[or] form.

@mz-examples[
(or)
(or 1)
(or (values 1 2))
(or 5 (error "doesn't get here"))
(or #f 5)
]}

@;------------------------------------------------------------------------
@section[#:tag "case"]{Dispatch: @racket[case]}

@defform/subs[#:literals (else)
              (case val-expr case-clause ...)
              ([case-clause [(datum ...) then-body ...+]
                            [else then-body ...+]])]{

Evaluates @racket[val-expr] and uses the result to select a
@racket[case-clause]. The selected clause is the first one with a
@racket[datum] whose @racket[quote]d form is @racket[equal?] to the
result of @racket[val-expr]. If no such @racket[datum] is present, the
@racket[else] @racket[case-clause] is selected; if no @racket[else]
@racket[case-clause] is present, either, then the result of the
@racket[case] form is @|void-const|.@margin-note{The @racket[case]
form of @racketmodname[racket] differs from that of @other-manual['(lib
"r6rs/scribblings/r6rs.scrbl")] or @other-manual['(lib
"r5rs/r5rs.scrbl")] by being based @racket[equal?] instead of
@racket[eqv?] (in addition to allowing internal definitions).}

For the selected @racket[case-clause], the results of the last
@racket[then-body], which is in tail position with respect to the
@racket[case] form, are the results for the whole @racket[case] form.

A @racket[case-clause] that starts with @racket[else] must be the last
@racket[case-clause].

@mz-examples[
(case (+ 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
(case (- 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
(case (string-append "do" "g")
 [("cat" "dog" "mouse") "animal"]
 [else "mineral or vegetable"])
(case (list 'y 'x)
 [((a b) (x y)) 'forwards]
 [((b a) (y x)) 'backwards])
(case 'x
 [(x) "ex"]
 [('x) "quoted ex"])
(case (list 'quote 'x)
 [(x) "ex"]
 [('x) "quoted ex"])
]
@def+int[
(define (classify c)
  (case (char-general-category c)
   [(ll lu lt ln lo) "letter"]
   [(nd nl no) "number"]
   [else "other"]))
(classify #\A)
(classify #\1)
(classify #\!)
]}

@;------------------------------------------------------------------------
@section[#:tag "define"]{Definitions: @racket[define], @racket[define-syntax], ...}

@guideintro["define"]{definitions}

@defform*/subs[[(define id expr)
                (define (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg ...)
                       (code:line arg ... @#,racketparenfont{.} rest-id)]
                 [arg arg-id
                      [arg-id default-expr]
                      (code:line keyword arg-id)
                      (code:line keyword [arg-id default-expr])])]{

The first form @tech{bind}s @racket[id] to the result of
@racket[expr], and the second form @tech{bind}s @racket[id] to a
procedure. In the second case, the generated procedure is
@racket[(#,cvt (head args) body ...+)], using the @|cvt| meta-function
defined as follows:

@racketblock[
(#,cvt (id . _kw-formals) . _datum)   = (lambda _kw-formals . _datum)
(#,cvt (head . _kw-formals) . _datum) = (lambda _kw-formals expr)
                                         @#,elem{if} (#,cvt head . _datum) = expr
]

In an @tech{internal-definition context} (see @secref["intdef-body"]), 
a @racket[define] form introduces a local binding.
At the top level, the top-level binding for @racket[id] is created after
evaluating @racket[expr], if it does not exist already, and the
top-level mapping of @racket[id] (in the @techlink{namespace} linked
with the compiled definition) is set to the binding at the same time.

In a context that allows @tech{liberal expansion} of @racket[define],
@racket[id] is bound as syntax if @racket[expr] is an immediate
@racket[lambda] form with keyword arguments or @racket[args] include
keyword arguments.

@defexamples[
(define x 10)
x
]
@def+int[
(define (f x)
  (+ x 1))
(f 10)
]

@def+int[
(define ((f x) [y 20])
  (+ x y))
((f 10) 30)
((f 10))
]
}

@defform[(define-values (id ...) expr)]{

Evaluates the @racket[expr], and @tech{bind}s the results to the
@racket[id]s, in order, if the number of results matches the number of
@racket[id]s; if @racket[expr] produces a different number of results,
the @exnraise[exn:fail:contract].

In an @tech{internal-definition context} (see @secref["intdef-body"]), 
a @racket[define-values] form introduces local bindings.
At the top level, the top-level binding for each @racket[id] is
created after evaluating @racket[expr], if it does not exist already,
and the top-level mapping of each @racket[id] (in the
@techlink{namespace} linked with the compiled definition) is set to
the binding at the same time.

@defexamples[
(define-values () (values))
(define-values (x y z) (values 1 2 3))
z
]

If a @racket[define-values] form for a function definition in a module
body has a @indexed-racket['compiler-hint:cross-module-inline]
@tech{syntax property} with a true value, then the Racket treats the
property as a performance hint.  See
@guidesecref["func-call-performance"] in @|Guide| for more
information, and see also @racket[begin-encourage-inline].}


@defform*[[(define-syntax id expr)
           (define-syntax (head args) body ...+)]]{

The first form creates a @tech{transformer binding} (see
@secref["transformer-model"]) of @racket[id] with the value of
@racket[expr], which is an expression at @tech{phase level} 1 relative
to the surrounding context. (See @secref["id-model"] for information
on @tech{phase levels}.)  Evaluation of @racket[expr] side is
@racket[parameterize]d to set @racket[current-namespace] as in
@racket[let-syntax].

The second form is a shorthand the same as for @racket[define]; it
expands to a definition of the first form where the @racket[expr] is a
@racket[lambda] form.}

In an @tech{internal-definition context} (see @secref["intdef-body"]), 
a @racket[define-syntax] form introduces a local binding.

@defexamples[#:eval (syntax-eval)
(define-syntax foo
  (syntax-rules ()
    ((_ a ...)
     (printf "~a\n" (list a ...)))))

(foo 1 2 3 4)

(define-syntax (bar syntax-object)
  (syntax-case syntax-object ()
    ((_ a ...)
     #'(printf "~a\n" (list a ...)))))

(bar 1 2 3 4)
]

@defform[(define-syntaxes (id ...) expr)]{

Like @racket[define-syntax], but creates a @tech{transformer binding}
for each @racket[id].  The @racket[expr] should produce as many values
as @racket[id]s, and each value is bound to the corresponding
@racket[id].

When @racket[expr] produces zero values for a top-level
@racket[define-syntaxes] (i.e., not in a module or internal-definition
position), then the @racket[id]s are effectively declared without
binding; see @secref["macro-introduced-bindings"].

In an @tech{internal-definition context} (see @secref["intdef-body"]), 
a @racket[define-syntaxes] form introduces local bindings.

@defexamples[#:eval (syntax-eval)
(define-syntaxes (foo1 foo2 foo3)
  (let ([transformer1 (lambda (syntax-object)
			(syntax-case syntax-object ()
			  [(_) #'1]))]
	[transformer2 (lambda (syntax-object)
			(syntax-case syntax-object ()
			  [(_) #'2]))]
	[transformer3 (lambda (syntax-object)
			(syntax-case syntax-object ()
			  [(_) #'3]))])
    (values transformer1
	    transformer2
	    transformer3)))
(foo1)
(foo2)
(foo3)
]}

@defform*[[(define-for-syntax id expr)
           (define-for-syntax (head args) body ...+)]]{

Like @racket[define], except that the binding is at @tech{phase level}
1 instead of @tech{phase level} 0 relative to its context. The
expression for the binding is also at @tech{phase level} 1. (See
@secref["id-model"] for information on @tech{phase levels}.)  The form
is a shorthand for @racket[(begin-for-syntax (define id expr))] or
@racket[(begin-for-syntax (define (head args) body ...+))].

Within a module, bindings introduced by @racket[define-for-syntax]
must appear before their uses or in the same
@racket[define-for-syntax] form (i.e., the @racket[define-for-syntax]
form must be expanded before the use is expanded). In particular,
mutually recursive functions bound by @racket[define-for-syntax] must
be defined by the same @racket[define-for-syntax] form.

@defexamples[#:eval (syntax-eval)
(define-for-syntax helper 2)
(define-syntax (make-two syntax-object)
  (printf "helper is ~a\n" helper)
  #'2)
(make-two)
(code:comment @#,t{`helper' is not bound in the runtime phase})
helper

(define-for-syntax (filter-ids ids)
  (filter identifier? ids))
(define-syntax (show-variables syntax-object)
  (syntax-case syntax-object ()
    [(_ expr ...)
     (with-syntax ([(only-ids ...)
                    (filter-ids (syntax->list #'(expr ...)))])
       #'(list only-ids ...))]))
(let ([a 1] [b 2] [c 3])
  (show-variables a 5 2 b c))]

@defform[(define-values-for-syntax (id ...) expr)]{

Like @racket[define-for-syntax], but @racket[expr] must produce as
many values as supplied @racket[id]s, and all of the @racket[id]s are
bound (at @tech{phase level} 1).}

@defexamples[#:eval (syntax-eval)
(define-values-for-syntax (foo1 foo2) (values 1 2))
(define-syntax (bar syntax-object)
  (printf "foo1 is ~a foo2 is ~a\n" foo1 foo2)
  #'2)
(bar) 
]}

@; ----------------------------------------------------------------------

@subsection[#:tag "require-syntax"]{@racket[require] Macros}

@note-lib-only[racket/require-syntax]

@defform*[[(define-require-syntax id proc-expr)
           (define-require-syntax (id args ...) body ...+)]]{

The first form is like @racket[define-syntax], but for a
@racket[require] sub-form. The @racket[proc-expr] must produce a
procedure that accepts and returns a syntax object representing a
@racket[require] sub-form.

This form expands to @racket[define-syntax] with a use of
@racket[make-require-transformer]; see @secref["require-trans"] for
more information.

The second form is a shorthand the same as for @racket[define-syntax]; it
expands to a definition of the first form where the @racket[expr] is a
@racket[lambda] form.}

@; ----------------------------------------------------------------------

@subsection[#:tag "provide-syntax"]{@racket[provide] Macros}

@note-lib-only[racket/provide-syntax]

@defform*[[(define-provide-syntax id proc-expr)
           (define-provide-syntax (id args ...) body ...+)]]{

The first form is like @racket[define-syntax], but for a
@racket[provide] sub-form. The @racket[proc-expr] must produce a
procedure that accepts and returns a syntax object representing a
@racket[provide] sub-form.

This form expands to @racket[define-syntax] with a use of
@racket[make-provide-transformer]; see @secref["provide-trans"] for
more information.

The second form is a shorthand the same as for @racket[define-syntax]; it
expands to a definition of the first form where the @racket[expr] is a
@racket[lambda] form.}

@;------------------------------------------------------------------------
@section[#:tag "begin"]{Sequencing: @racket[begin], @racket[begin0], and @racket[begin-for-syntax]}

@guideintro["begin"]{@racket[begin] and @racket[begin0]}

@defform*[[(begin form ...)
           (begin expr ...+)]]{

The first form applies when @racket[begin] appears at the top level,
at module level, or in an internal-definition position (before any
expression in the internal-definition sequence). In that case, the
@racket[begin] form is equivalent to splicing the @racket[form]s into
the enclosing context.

The second form applies for @racket[begin] in an expression position.
In that case, the @racket[expr]s are evaluated in order, and the
results are ignored for all but the last @racket[expr]. The last
@racket[expr] is in tail position with respect to the @racket[begin]
form.

@examples[
(begin
  (define x 10)
  x)
(+ 1 (begin
       (printf "hi\n")
       2))
(let-values ([(x y) (begin
                      (values 1 2 3)
                      (values 1 2))])
 (list x y))
]}

@defform[(begin0 expr body ...+)]{

Evaluates the @racket[expr], then evaluates the @racket[body]s,
ignoring the @racket[body] results. The results of the @racket[expr]
are the results of the @racket[begin0] form, but the @racket[expr] is
in tail position only if no @racket[body]s are present.

@mz-examples[
(begin0
  (values 1 2)
  (printf "hi\n"))
]}

@defform[(begin-for-syntax form ...)]{

Allowed only in a @tech{top-level context} or @tech{module context},
shifts the @tech{phase level} of each @racket[form] by one:

@itemize[

 @item{expressions reference bindings at a @tech{phase level} one
       greater than in the context of the @racket[begin-for-syntax]
       form;}

 @item{@racket[define], @racket[define-values],
       @racket[define-syntax], and @racket[define-syntaxes] forms bind
       at a @tech{phase level} one greater than in the context of the
       @racket[begin-for-syntax] form;}

 @item{in @racket[require] and @racket[provide] forms, the default
       @tech{phase level} is greater, which is roughly like wrapping
       the content of the @racket[require] form with
       @racket[for-syntax];}

 @item{expression form @racket[_expr]: converted to
       @racket[(define-values-for-syntax () (begin _expr (values)))], which
       effectively evaluates the expression at expansion time and, in
       the case of a @tech{module context}, preserves the expression
       for future @tech{visit}s of the module.}

]

See also @racket[module] for information about expansion order and
partial expansion for @racket[begin-for-syntax] within a module
context. Evaluation of an @racket[expr] within
@racket[begin-for-syntax] is @racket[parameterize]d to set
@racket[current-namespace] as in @racket[let-syntax].

}

@;------------------------------------------------------------------------
@section[#:tag "when+unless"]{Guarded Evaluation: @racket[when] and @racket[unless]}

@guideintro["when+unless"]{@racket[when] and @racket[unless]}

@defform[(when test-expr body ...+)]{

Evaluates @racket[test-expr]. If the result is @racket[#f], then
the result of the @racket[when] expression is
@|void-const|. Otherwise, the @racket[body]s are evaluated, and the
last @racket[body] is in tail position with respect to the
@racket[when] form.

@mz-examples[
(when (positive? -5)
  (display "hi"))
(when (positive? 5)
  (display "hi")
  (display " there"))
]}

@defform[(unless test-expr body ...+)]{

Equivalent to @racket[(when (not test-expr) body ...+)].

@mz-examples[
(unless (positive? 5)
  (display "hi"))
(unless (positive? -5)
  (display "hi")
  (display " there"))
]}

@;------------------------------------------------------------------------
@section[#:tag "set!"]{Assignment: @racket[set!] and @racket[set!-values]}

@guideintro["set!"]{@racket[set!]}

@defform[(set! id expr)]{

If @racket[id] has a @tech{transformer binding} to an @tech{assignment
transformer}, as produced by @racket[make-set!-transformer] or as an
instance of a structure type with the @racket[prop:set!-transformer]
property, then this form is expanded by calling the assignment
transformer with the full expressions. If @racket[id] has a
@tech{transformer binding} to a @tech{rename transformer} as produced
by @racket[make-rename-transformer] or as an instance of a structure
type with the @racket[prop:rename-transformer] property, then this
form is expanded by replacing @racket[id] with the target identifier
(e.g., the one provided to @racket[make-rename-transformer]). If a
transformer binding has both @racket[prop:set!-transformer] and
@racket[prop:rename-transformer] properties, the latter takes
precedence.

Otherwise, evaluates @racket[expr] and installs the result into the
location for @racket[id], which must be bound as a local variable or
defined as a @tech{top-level variable} or @tech{module-level
variable}. If @racket[id] refers to an imported binding, a syntax
error is reported.  If @racket[id] refers to a @tech{top-level
variable} that has not been defined, the @exnraise[exn:fail:contract].

See also @racket[compile-allow-set!-undefined].

@defexamples[
(define x 12)
(set! x (add1 x))
x
(let ([x 5])
  (set! x (add1 x))
  x)
(set! i-am-not-defined 10)
]}

@defform[(set!-values (id ...) expr)]{

Assuming that all @racket[id]s refer to variables, this form evaluates
@racket[expr], which must produce as many values as supplied
@racket[id]s.  The location of each @racket[id] is filled with the
corresponding value from @racket[expr] in the same way as for
@racket[set!].

@mz-examples[
(let ([a 1]
      [b 2])
  (set!-values (a b) (values b a))
  (list a b))
]

More generally, the @racket[set!-values] form is expanded to

@racketblock[
(let-values ([(_tmp-id ...) expr])
  (set! id _tmp-id) ...)
]

which triggers further expansion if any @racket[id] has a transformer
binding to an @tech{assignment transformer}.}

@;------------------------------------------------------------------------
@include-section["for.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "wcm"]{Continuation Marks: @racket[with-continuation-mark]}

@defform[(with-continuation-mark key-expr val-expr result-expr)]{

The @racket[key-expr], @racket[mark-expr], and @racket[result-expr]
expressions are evaluated in order. After @racket[key-expr] is
evaluated to obtain a key and @racket[mark-expr] is evaluated to
obtain a mark, the key is mapped to the mark in the current
continuation's initial frame. If the frame already has a mark for the
key, it is replaced. Finally, the @racket[result-expr] is evaluated;
the continuation for evaluating @racket[result-expr] is the
continuation of the @racket[with-continuation-mark] expression (so the
result of the @racket[result-expr] is the result of the
@racket[with-continuation-mark] expression, and @racket[result-expr]
is in tail position for the @racket[with-continuation-mark]
expression).

@moreref["contmarks"]{continuation marks}}

@;------------------------------------------------------------------------
@section[#:tag "quasiquote"]{Quasiquoting: @racket[quasiquote], @racket[unquote], and @racket[unquote-splicing]}

@guideintro["qq"]{@racket[quasiquote]}

@defform[(quasiquote datum)]{

The same as @racket[(quote datum)] if @racket[datum] does not include
@racket[(#,unquote-id _expr)] or @racket[(#,unquote-splicing-id _expr)]. An
@racket[(#,unquote-id _expr)] form escapes from the quote, however,
and the result of the @racket[_expr] takes the place of the
@racket[(#,unquote-id _expr)] form in the @racket[quasiquote] result. An
@racket[(#,unquote-splicing-id _expr)] similarly escapes, but the
@racket[_expr] must produce a list, and its elements are spliced as
multiple values place of the @racket[(#,unquote-splicing-id _expr)], which
must appear as the @racket[car] or a quoted pair, as an element of a
quoted vector, or as an element of a quoted @tech{prefab} structure;
in the case of a pair, if the @racket[cdr] of the relevant quoted pair
is empty, then @racket[_expr] need not produce a list, and its result
is used directly in place of the quoted pair (in the same way that
@racket[append] accepts a non-list final argument).  In a quoted
@tech{hash table}, an @racket[(#,unquote-id _expr)] or
@racket[(#,unquote-splicing-id _expr)] expression escapes only in the
second element of an entry pair (i.e., the value), while entry keys
are always implicitly quoted. If @racket[unquote] or
@racket[unquote-splicing] appears within @racket[quasiquote] in any
other way than as @racket[(#,unquote-id _expr)] or
@racket[(#,unquote-splicing-id _expr)], a syntax error is reported.

@mz-examples[
(eval:alts (#,(racket quasiquote) (0 1 2)) `(0 1 2))
(eval:alts (#,(racket quasiquote) (0 (#,unquote-id (+ 1 2)) 4)) `(0 ,(+ 1 2) 4))
(eval:alts (#,(racket quasiquote) (0 (#,unquote-splicing-id (list 1 2)) 4)) `(0 ,@(list 1 2) 4))
(eval:alts (#,(racket quasiquote) (0 (#,unquote-splicing-id 1) 4)) `(0 ,@1 4))
(eval:alts (#,(racket quasiquote) (0 (#,unquote-splicing-id 1))) `(0 ,@1))
]

A @racket[quasiquote], @racket[unquote], or @racket[unquote-splicing]
form is typically abbreviated with @litchar{`}, @litchar{,}, or
@litchar[",@"], respectively. See also @secref["parse-quote"].

@mz-examples[
`(0 1 2)
`(1 ,(+ 1 2) 4)
`#s(stuff 1 ,(+ 1 2) 4)
(eval:alts #,(racketfont (racketvalfont "`#hash((\"a\" . ") "," (racket (+ 1 2)) (racketvalfont "))")) #hash(("a" . 3)))
`#hash((,(+ 1 2) . "a"))
`(1 ,@(list 1 2) 4)
`#(1 ,@(list 1 2) 4)
]

A @racket[quasiquote] form within the original @racket[datum]
increments the level of quasiquotation: within the @racket[quasiquote]
form, each @racket[unquote] or @racket[unquote-splicing] is preserved,
but a further nested @racket[unquote] or @racket[unquote-splicing]
escapes.  Multiple nestings of @racket[quasiquote] require multiple
nestings of @racket[unquote] or @racket[unquote-splicing] to escape.

@mz-examples[
`(1 `,(+ 1 ,(+ 2 3)) 4)
`(1 ```,,@,,@(list (+ 1 2)) 4)
]

The @racket[quasiquote] form allocates only as many fresh cons cells,
vectors, and boxes as are needed without analyzing @racket[unquote]
and @racket[unquote-splicing] expressions. For example, in

@racketblock[
`(,1 2 3)
]

a single tail @racket['(2 3)] is used for every evaluation of the
@racket[quasiquote] expression.

}

@defidform[unquote]{

See @racket[quasiquote], where @racket[unquote] is recognized as an
escape. An @racket[unquote] form as an expression is a syntax error.}

@defidform[unquote-splicing]{

See @racket[quasiquote], where @racket[unquote-splicing] is recognized as an
escape. An @racket[unquote-splicing] form as an expression is a syntax error.}

@;------------------------------------------------------------------------
@section{Syntax Quoting: @racket[quote-syntax]}

@defform[(quote-syntax datum)]{

Similar to @racket[quote], but produces a @tech{syntax object}
that preserves the @tech{lexical information} and source-location
information attached to @racket[datum] at expansion time.

Unlike @racket[syntax] (@litchar{#'}), @racket[quote-syntax] does
not substitute pattern variables bound by @racket[with-syntax],
@racket[syntax-parse], or @racket[syntax-case].

@mz-examples[
(syntax? (quote-syntax x))
(quote-syntax (1 2 3))
(with-syntax ([a #'5])
  (quote-syntax (a b c)))
]
}

@;------------------------------------------------------------------------
@section[#:tag "#%top-interaction"]{Interaction Wrapper: @racket[#%top-interaction]}

@defform[(#%top-interaction . form)]{

Expands to simply @racket[form]. The @racket[#%top-interaction] form
is similar to @racket[#%app] and @racket[#%module-begin], in that it
provides a hook to control interactive evaluation through
@racket[load] (more precisely, the default @tech{load handler}) or
@racket[read-eval-print-loop].}

@;------------------------------------------------------------------------
@include-section["block.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "stratified-body"]{Internal-Definition Limiting: @racket[#%stratified-body]}

@defform[(#%stratified-body defn-or-expr ...)]{

Like @racket[(let () defn-or-expr ...)] for an
@tech{internal-definition context} sequence, except that an expression
is not allowed to precede a definition, and all definitions are
treated as referring to all other definitions (i.e., @tech{locations}
for variables are all allocated first, like @racket[letrec] and
unlike @racket[letrec-syntaxes+values]).

The @racket[#%stratified-body] form is useful for implementing
syntactic forms or languages that supply a more limited kind of
@tech{internal-definition context}.}

@close-eval[require-eval]
@close-eval[meta-in-eval]

@;------------------------------------------------------------------------
@section[#:tag "performance-hint"]{Performance Hints: @racket[begin-encourage-inline]}

@note-lib-only[racket/performance-hint]

@defform[(begin-encourage-inline form ...)]{

Attaches a @racket['compiler-hint:cross-module-inline]
@tech{syntax property} to each @racket[form], which is useful when a
@racket[form] is a function definition. See @racket[define-values].}

@defform*/subs[[(define-inline id expr)
                (define-inline (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg ...)
                       (code:line arg ... @#,racketparenfont{.} rest-id)]
                 [arg arg-id
                      [arg-id default-expr]
                      (code:line keyword arg-id)
                      (code:line keyword [arg-id default-expr])])]{
Like @racket[define], but ensures that the definition will be inlined at its
call sites. Recursive calls are not inlined, to avoid infinite inlining.
Higher-order uses are supported, but also not inlined.

@racket[define-inline] may interfere with the Racket compiler's own inlining
heuristics, and should only be used when other inlining attempts (such as
@racket[begin-encourage-inline]) fail.
}


@;------------------------------------------------------------------------
@section[#:tag "lazy-require"]{Importing Modules Lazily: @racket[lazy-require]}

@note-lib-only[racket/lazy-require]

@defform[(lazy-require [module-path (imported-fun-id ...)] ...)]{

Defines each @racket[imported-fun-id] as a function that, when called,
dynamically requires the export named @racket[imported-fun-id] from
the module specified by @racket[module-path] and calls it with the
same arguments.

If the enclosing relative phase level is not 0, then
@racket[module-path] is also placed in a submodule (with a use of
@racket[define-runtime-module-path-index] at phase level 0 within the
submodule). Introduced submodules have the names
@racket[lazy-require-]@racket[_n]@racketidfont{-}@racket[_m], where
@racket[_n] is a phase-level number and @racket[_m] is a number.

When the use of a lazily-required function triggers module loading,
@racket[register-external-module] declares a potential compilation
dependency (in case the function is used in the process of compiling a
module).}
