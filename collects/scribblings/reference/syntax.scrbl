#lang scribble/doc
@(require "mz.ss"
          scribble/bnf
          (for-label (only-in scheme/require-transform
                              make-require-transformer)
                     scheme/require-syntax
                     scheme/require
                     (only-in scheme/provide-transform
                              make-provide-transformer)
                     scheme/provide-syntax
                     scheme/provide
                     scheme/nest
                     scheme/package
                     scheme/splicing
                     scheme/runtime-path))

@(define require-eval (make-base-eval))
@(define syntax-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require (for-syntax scheme/base)))
       the-eval)))
@(define meta-in-eval (syntax-eval))

@(define cvt (schemefont "CVT"))
@(define unquote-id (scheme unquote))
@(define unquote-splicing-id (scheme unquote-splicing))

@title[#:tag "syntax" #:style 'toc]{Syntactic Forms}

This section describes the core syntax forms that appear in a fully
expanded expression, plus a many closely-related non-core forms.
See @secref["fully-expanded"] for the core grammar.

@local-table-of-contents[]

@subsubsub*section{Notation}

Each syntactic form is described by a BNF-like notation that describes
a combination of (syntax-wrapped) pairs, symbols, and other data (not
a sequence of characters). These grammatical specifications are shown
as in the following specification of a @schemekeywordfont{something}
form:

@specsubform[(@#,schemekeywordfont{something} id thing-expr ...)
             #:contracts ([thing-expr number?])]

Within such specifications,

@itemize[

 @item{@scheme[...] indicates zero or more
       repetitions of the preceding datum.}

 @item{@scheme[...+] indicates one or
       more repetitions of the preceding datum.}

 @item{Italic meta-identifiers play the role of non-terminals. Some
       meta-identifier names imply syntactic constraints:

      @itemize[

        @item{A meta-identifier that ends in @scheme[_id] stands for an
              identifier.}

        @item{A meta-identifier that ends in @scheme[_keyword] stands
              for a keyword.}

        @item{A meta-identifier that ends with @scheme[_expr] (such as
              @scheme[_thing-expr]) stands for a sub-form that is
              expanded as an expression.}

        @item{A meta-identifier that ends with @scheme[_body] stands
              for a sub-form that is expanded in an
              internal-definition context (see
              @secref["intdef-body"]).}

              ]} 

 @item{Contracts indicate constraints on sub-expression results. For
       example, @scheme[_thing-expr @#,elem{:} number?] indicates that
       the expression @scheme[_thing-expr] must produce a number.}]

@;------------------------------------------------------------------------
@section[#:tag "module"]{Modules: @scheme[module], ...}

@guideintro["module-syntax"]{@scheme[module]}

@defform[(module id module-path form ...)]{

Declares a top-level module. If the
@scheme[current-module-declare-name] parameter is set, the parameter
value is used for the module name, otherwise @scheme[(#,(scheme quote)
id)] is the name of the declared module.

@margin-note/ref{For a @scheme[module]-like form for use @emph{within}
modules and other contexts, see @scheme[define-package].}

The @scheme[module-path] form must be as for @scheme[require], and it
supplies the initial bindings for the body @scheme[form]s. That is, it
is treated like a @scheme[(require module-path)] prefix before the
@scheme[form]s, except that the bindings introduced by
@scheme[module-path] can be shadowed by definitions and
@scheme[require]s in the module body @scheme[form]s.

If a single @scheme[form] is provided, then it is partially expanded
in a @tech{module-begin context}. If the expansion leads to
@scheme[#%plain-module-begin], then the body of the
@scheme[#%plain-module-begin] is the body of the module. If partial
expansion leads to any other primitive form, then the form is wrapped
with @schemeidfont{#%module-begin} using the lexical context of the
module body; this identifier must be bound by the initial
@scheme[module-path] import, and its expansion must produce a
@scheme[#%plain-module-begin] to supply the module body. Finally, if
multiple @scheme[form]s are provided, they are wrapped with
@schemeidfont{#%module-begin}, as in the case where a single
@scheme[form] does not expand to @scheme[#%plain-module-begin].

After such wrapping, if any, and before any expansion, an
@indexed-scheme['enclosing-module-name] property is attached to the
@schemeidfont{#%module-begin} syntax object (see
@secref["stxprops"]); the property's value is a symbol
corresponding to @scheme[id].

Each @scheme[form] is partially expanded (see
@secref["partial-expansion"]) in a @tech{module context}. Further
action depends on the shape of the form:

@itemize[

 @item{If it is a @scheme[begin] form, the sub-forms are flattened
  out into the module's body and immediately processed in place of the
  @scheme[begin].}

 @item{If it is a @scheme[define-syntaxes] or
  @scheme[define-values-for-syntax] form, then the right-hand side is
  evaluated (in @tech{phase} 1), and the binding is immediately
  installed for further partial expansion within the
  module. Evaluation of the right-hand side is @scheme[parameterize]d
  to set @scheme[current-namespace] as in @scheme[let-syntax].}

 @item{If the form is a @scheme[require] form, bindings are introduced
   immediately, and the imported modules are @tech{instantiate}d or
   @tech{visit}ed as appropriate.}

 @item{If the form is a @scheme[provide] form, then it is recorded for
   processing after the rest of the body.}

 @item{If the form is a @scheme[define-values] form, then the binding
   is installed immediately, but the right-hand expression is not
   expanded further.}

 @item{Similarly, if the form is an expression, it is
   not expanded further.}

]

After all @scheme[form]s have been partially expanded this way, then
the remaining expression forms (including those on the right-hand side
of a definition) are expanded in an expression context.

The scope of all imported identifiers covers the entire module body,
as does the scope of any identifier defined within the module body.
The ordering of syntax definitions does not affect the scope of the
syntax names; a transformer for @scheme[A] can produce expressions
containing @scheme[B], while the transformer for @scheme[B] produces
expressions containing @scheme[A], regardless of the order of
declarations for @scheme[A] and @scheme[B]. However, a syntactic form
that produces syntax definitions must be defined before it is used.

No identifier can be imported or defined more than once at any
@tech{phase level}. Every exported identifier must be imported or
defined. No expression can refer to a @tech{top-level variable}.

The evaluation of a @scheme[module] form does not evaluate the
expressions in the body of the module. Evaluation merely declares a
module, whose full name depends both on @scheme[id] and
@scheme[(current-module-declare-name)].

The module body is executed only when the module is explicitly
@techlink{instantiate}d via @scheme[require] or
@scheme[dynamic-require]. On invocation, expressions and definitions
are evaluated in order as they appear within the module. Each
evaluation of an expression or definition is wrapped with a
continuation prompt (see @scheme[call-with-continuation-prompt]) for
the default continuation and using the default prompt handler.

Accessing a @tech{module-level variable} before it is defined signals
a run-time error, just like accessing an undefined global variable.
If a module (in its fully expanded form) does not contain a
@scheme[set!]  for an identifier that defined within the module, then
the identifier is a @defterm{constant} after it is defined; its value
cannot be changed afterward, not even through reflective
mechanisms. The @scheme[compile-enforce-module-constants] parameter,
however, can be used to disable enforcement of constants.

When a @tech{syntax object} representing a @scheme[module] form has a
@indexed-scheme['module-language] @tech{syntax property} attached, and
when the property value is a vector of three elements where the first
is a module path (in the sense of @scheme[module-path?]) and the
second is a symbol, then the property value is preserved in the
corresponding compiled and/or declared module. The third component of
the vector should be printable and @scheme[read]able, so that it can
be preserved in marshaled bytecode. See also
@scheme[module-compiled-language-info] and
@scheme[module->language-info].}

See also @secref["module-eval-model"] and @secref["mod-parse"].

@defexamples[#:eval (syntax-eval)
(module duck scheme/base
  (provide num-eggs quack)
  (define num-eggs 2)
  (define (quack n)
    (unless (zero? n)
      (printf "quack\n")
      (quack (sub1 n)))))
]

@defform[(#%module-begin form ...)]{

Legal only in a @tech{module begin context}, and handled by the
@scheme[module] form.

The @scheme[#%module-begin] form of @schememodname[scheme/base] wraps
every top-level expression to print non-@|void-const| results using
@scheme[current-print].}

@defform[(#%plain-module-begin form ...)]{

Legal only in a @tech{module begin context}, and handled by the
@scheme[module] form.}

@;------------------------------------------------------------------------
@section[#:tag '("require" "provide")]{Importing and Exporting: @scheme[require] and @scheme[provide]}

@section-index["modules" "imports"]
@section-index["modules" "exports"]

@guideintro["module-require"]{@scheme[require]}

@defform/subs[#:literals (only-in prefix-in except-in rename-in lib file planet + - =
                          for-syntax for-template for-label for-meta only-meta-in combine-in quote)
              (require require-spec ...)
              ([require-spec module-path
                             (only-in require-spec id-maybe-renamed ...)
                             (except-in require-spec id ...)
                             (prefix-in prefix-id require-spec)
                             (rename-in require-spec [orig-id bind-id] ...)
                             (combine-in require-spec ...)
                             (only-meta-in phase-level require-spec ...)
                             (for-syntax require-spec ...)
                             (for-template require-spec ...)
                             (for-label require-spec ...)
                             (for-meta phase-level require-spec ...)
                             derived-require-spec]
               [module-path (#,(scheme quote) id)
                            rel-string
                            (lib rel-string ...+)
                            id
                            (file string)
                            (planet id)
                            (planet string)
                            (planet rel-string
                                    (user-string pkg-string vers)
                                    rel-string ...)]
               [id-maybe-renamed id
                                 [orig-id bind-id]]
               [phase-level exact-integer #f]
               [vers code:blank
                     nat
                     (code:line nat minor-vers)]
               [minor-vers nat
                           (nat nat)
                           ((unsyntax (schemeidfont "=")) nat)
                           ((unsyntax (schemeidfont "+")) nat)
                           ((unsyntax (schemeidfont "-")) nat)])]{

In a @tech{top-level context}, @scheme[require] @tech{instantiates}
modules (see @secref["module-eval-model"]). In a @tech{top-level
context} or @tech{module context}, expansion of @scheme[require]
@tech{visits} modules (see @secref["mod-parse"]). In both contexts and
both evaluation and expansion, @scheme[require] introduces bindings
into a @tech{namespace} or a module (see @secref["intro-binding"]).  A
@scheme[require] form in a @tech{expression context} or
@tech{internal-definition context} is a syntax error.

A @scheme[require-spec] designates a particular set of identifiers to
be bound in the importing context. Each identifier is mapped to a
particular export of a particular module; the identifier to bind may
be different from the symbolic name of the originally exported
identifier. Each identifier also binds at a particular @tech{phase
level}.

The syntax of @scheme[require-spec] can be extended via
@scheme[define-require-syntax], and when multiple
@scheme[require-spec]s are specified in a @scheme[require], the
bindings of each @scheme[require-spec] are visible for expanding later
@scheme[require-spec]s. The pre-defined forms (as exported by
@scheme[scheme/base]) are as follows:

 @specsubform[module-path]{ Imports all exported bindings from the
  named module, using the export identifiers as the local identifiers.
  (See below for information on @scheme[module-path].) The lexical
  context of the @scheme[module-path] form determines the context of
  the introduced identifiers.}

 @defsubform[(only-in require-spec id-maybe-renamed ...)]{
  Like @scheme[require-spec], but constrained to those exports for
  which the identifiers to bind match @scheme[id-maybe-renamed]: as
  @scheme[_id] or as @scheme[_orig-id] in @scheme[[_orig-id _bind-id]]. If
  the @scheme[_id] or @scheme[_orig-id] of any @scheme[id-maybe-renamed]
  is not in the set that @scheme[require-spec] describes, a syntax
  error is reported.

  @defexamples[#:eval (syntax-eval)
    (require (only-in scheme/tcp
	              tcp-listen
                      (tcp-accept my-accept)))
    tcp-listen
    my-accept
    tcp-accept
  ]}

 @defsubform[(except-in require-spec id ...)]{ Like
  @scheme[require-spec], but omitting those imports for which
  @scheme[id]s are the identifiers to bind; if any @scheme[id] is not
  in the set that @scheme[require-spec] describes, a syntax error is
  reported.

  @defexamples[#:eval (syntax-eval)
    (require (except-in scheme/tcp
	                tcp-listen))
    tcp-accept
    tcp-listen
  ]}

 @defsubform[(prefix-in prefix-id require-spec)]{ Like
  @scheme[require-spec], but adjusting each identifier to be bound by
  prefixing it with @scheme[prefix-id]. The lexical context of the
  @scheme[prefix-id] is ignored, and instead preserved from the
  identifiers before prefixing.

  @defexamples[#:eval (syntax-eval)
    (require (prefix-in tcp: scheme/tcp))
    tcp:tcp-accept
    tcp:tcp-listen
  ]}

 @defsubform[(rename-in require-spec [orig-id bind-id] ...)]{
  Like @scheme[require-spec], but replacing the identifier to
  bind @scheme[orig-id] with @scheme[bind-id]; if any
  @scheme[orig-id] is not in the set that @scheme[require-spec]
  describes, a syntax error is reported.
  
  @defexamples[#:eval (syntax-eval)
    (require (rename-in scheme/tcp
                        (tcp-accept accept)
			(tcp-listen listen)))
    accept
    listen
  ]}

 @defsubform[(combine-in require-spec ...)]{
  The union of the @scheme[require-spec]s.
  
  @defexamples[#:eval (syntax-eval)
    (require (combine-in (only-in scheme/tcp tcp-accept)
                         (only-in scheme/tcp tcp-listen)))
    tcp-accept
    tcp-listen
  ]}

 @defsubform[(only-meta-in phase-level require-spec ...)]{
  Like the combination of @scheme[require-spec]s, but removing any
  binding that is not for @scheme[phase-level], where @scheme[#f] for
  @scheme[phase-level] corresponds to the @tech{label phase level}.
  
  The following example imports bindings only at @tech{phase level} 1,
  the transform phase:

  @interaction[#:eval meta-in-eval
  (module nest scheme
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
  @scheme[require-spec]s, but constrained each binding specified by
  each @scheme[require-spec] is shifted by @scheme[phase-level]. The
  @tech{label phase level} corresponds to @scheme[#f], and a shifting
  combination that involves @scheme[#f] produces @scheme[#f].
  
  @defexamples[#:eval (syntax-eval)
  (module nest scheme
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
  @scheme[(for-meta 1 require-spec ...)].}

 @specsubform[#:literals (for-template)
              (for-template require-spec ...)]{Same as 
  @scheme[(for-meta -1 require-spec ...)].}

 @specsubform[#:literals (for-label)
              (for-label require-spec ...)]{Same as 
  @scheme[(for-meta #f require-spec ...)].}

 @specsubform[derived-require-spec]{See @scheme[define-require-syntax]
 for information on expanding the set of @scheme[require-spec]
 forms.}

@guideintro["module-paths"]{module paths}

A @scheme[module-path] identifies a module, either through a concrete
name in the form of an identifier, or through an indirect name that
can trigger automatic loading of the module declaration. Except for
the @scheme[id] case below, the actual resolution is up to the current
@tech{module name resolver} (see
@scheme[current-module-name-resolver]), and the description below
corresponds to the default @tech{module name resolver}.

 @specsubform[#:literals (quote)
              (#,(scheme quote) id)]{
 Refers to a module previously declared interactively with the name
 @scheme[id].

 @examples[
 (code:comment @#,t{a module declared interactively as @schemeidfont{test}:})
 (eval:alts (require '@#,schemeidfont{test}) (void))]}

 @specsubform[rel-string]{A path relative to the containing source (as
 determined by @scheme[current-load-relative-directory] or
 @scheme[current-directory]).  Regardless of the current platform,
 @scheme[rel-string] is always parsed as a Unix-format relative path:
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

 If @scheme[rel-string] ends with a @filepath{.ss} suffix, it is
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
 a @tech{collection} (see @secref["collects"]). The @scheme[rel-string]s in
 @scheme[lib] are constrained similar to the plain @scheme[rel-string]
 case, with the additional constraint that a @scheme[rel-string]
 cannot contain @litchar{.} or @litchar{..} directory indicators.

 The specific interpretation of the path depends on the number and
 shape of the @scheme[rel-string]s:

 @itemize[

    @item{If a single @scheme[rel-string] is provided, and if it
    consists of a single element (i.e., no @litchar{/}) with no file
    suffix (i.e., no @litchar{.}), then @scheme[rel-string] names a
    @tech{collection}, and @filepath{main.rkt} is the library file name.

    @examples[
    (code:comment @#,t{the main @schememodname[swindle] library:})
    (eval:alts (require (lib "swindle")) (void))
    (code:comment @#,t{the same:})
    (eval:alts (require (lib "swindle/main.rkt")) (void))]}

    @item{If a single @scheme[rel-string] is provided, and if it
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

    @item{If a single @scheme[rel-string] is provided, and if it
    consists of a single element @italic{with} a file suffix (i.e,
    with a @litchar{.}), then @scheme[rel-string] names a file within
    the @filepath{mzlib} @tech{collection}. A @filepath{.ss}
    suffix is converted to @filepath{.rkt}. (This convention is for
    compatibility with older version of PLT Scheme.)

    @examples[
    (code:comment @#,t{@filepath{tar.rkt} module from the @filepath{mzlib} collection:})
    (eval:alts (require (lib "tar.ss")) (void))]}

    @item{Otherwise, when multiple @scheme[rel-string]s are provided,
    the first @scheme[rel-string] is effectively moved after the
    others, and all @scheme[rel-string]s are appended with @litchar{/}
    separators. The resulting path names a @tech{collection}, then
    subcollection, etc., ending with a file name. No suffix is added
    automatically, but a @filepath{.ss} suffix is converted to
    @filepath{.rkt}. (This convention is for compatibility with older
    version of PLT Scheme.)

    @examples[
    (code:comment @#,t{@filepath{tar.rkt} module from the @filepath{mzlib} collection:})
    (eval:alts (require (lib "tar.ss" "mzlib")) (void))]}
  ]}

 @specsubform[id]{A shorthand for a @scheme[lib] form with a single
 @scheme[_rel-string] whose characters are the same as in the symbolic
 form of @scheme[id]. In addition to the constraints of a @scheme[lib]
 @scheme[_rel-string], @scheme[id] must not contain @litchar{.}.

 @examples[#:eval require-eval
   (eval:alts (require scheme/tcp) (void))]}

 @defsubform[(file string)]{Similar to the plain @scheme[rel-string]
 case, but @scheme[string] is a path---possibly absolute---using the
 current platform's path conventions and @scheme[expand-user-path].
 A @filepath{.ss} suffix is converted to @filepath{.rkt}. 

 @examples[(eval:alts (require (file "~/tmp/x.rkt")) (void))]}

 @defsubform*[((planet id)
               (planet string)
               (planet rel-string (user-string pkg-string vers)
                       rel-string ...))]{

 Specifies a library available via the @PLaneT server.

 The first form is a shorthand for the last one, where the @scheme[id]'s
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

 A @scheme[(planet string)] form is like a @scheme[(planet id)] form
 with the identifier converted to a string, except that the
 @scheme[string] can optionally end with a file extension (i.e., a
 @litchar{.}) for a @nonterm{path}. A @filepath{.ss} file extension is
 converted to @filepath{.rkt}.

 In the more general last form of a @scheme[planet] module path, the
 @scheme[rel-string]s are similar to the @scheme[lib] form, except
 that the @scheme[(user-string pkg-string vers)] names a
 @|PLaneT|-based package instead of a @tech{collection}. A version
 specification can include an optional major and minor version, where
 the minor version can be a specific number or a constraint:
 @scheme[(_nat _nat)] specifies an inclusive range, @scheme[((unsyntax
 (schemeidfont "=")) _nat)] specifies an exact match,
 @scheme[((unsyntax (schemeidfont "+")) _nat)] specifies a minimum
 version and is equivalent to just @scheme[_nat], and
 @scheme[((unsyntax (schemeidfont "-")) _nat)] specifies a maximum
 version. The @schemeidfont{=}, @schemeidfont{+}, and @schemeidfont{-}
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

No identifier can be bound multiple times in a given @tech{phase
level} by an import, unless all of the bindings refer to the same
original definition in the same module.  In a @tech{module context},
an identifier can be either imported or defined for a given
@tech{phase level}, but not both.}


@guideintro["module-provide"]{@scheme[provide]}

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

Declares exports from a module. A @scheme[provide] form must appear in
a @tech{module context} or a @tech{module-begin context}.

A @scheme[provide-spec] indicates one or more bindings to provide.
For each exported binding, the external name is a symbol that can be
different from the symbolic form of the identifier that is bound
within the module. Also, each export is drawn from a particular
@tech{phase level} and exported at the same @tech{phase level}.

The syntax of @scheme[provide-spec] can be extended via
@scheme[define-provide-syntax], but the pre-defined forms are as
follows.

 @specsubform[id]{ Exports @scheme[id], which must be @tech{bound}
 within the module (i.e., either defined or imported) at the relevant
 @tech{phase level}. The symbolic form of @scheme[id] is used as the
 external name, and the symbolic form of the defined or imported
 identifier must match (otherwise, the external name could be
 ambiguous).

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide num-eggs)
     (define num-eggs 2))
   (require 'nest)
   num-eggs
 ]

 If @scheme[id] has a transformer binding to a @tech{rename
 transformer}, then the exported binding is the target identifier of
 the @tech{rename transformer}, instead of @scheme[id], unless the
 target identifier has a true value for the
 @scheme['not-free-identifier=?] @tech{syntax property}.}

 @defsubform[(all-defined-out)]{ Exports all identifiers that are
 defined at @tech{phase level} 0 or @tech{phase level} 1 within the
 exporting module, and that have the same lexical context as the
 @scheme[(all-defined-out)] form, excluding bindings to @tech{rename
 transformers} where the target identifier has the
 @scheme['not-provide-all-defined] @tech{syntax property}. The
 external name for each identifier is the symbolic form of the
 identifier. Only identifiers accessible from the lexical context of
 the @scheme[(all-defined-out)] form are included; that is,
 macro-introduced imports are not re-exported, unless the
 @scheme[(all-defined-out)] form was introduced at the same time.

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (all-defined-out))
     (define num-eggs 2))
   (require 'nest)
   num-eggs
 ]}

 @defsubform[(all-from-out module-path ...)]{ Exports all identifiers
 that are imported into the exporting module using a
 @scheme[require-spec] built on each @scheme[module-path] (see
 @secref["require"]) with no @tech{phase-level} shift.  The symbolic
 name for export is derived from the name that is bound within the
 module, as opposed to the symbolic name of the export from each
 @scheme[module-path]. Only identifiers accessible from the lexical
 context of the @scheme[module-path] are included; that is,
 macro-introduced imports are not re-exported, unless the
 @scheme[module-path] was introduced at the same time.

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide num-eggs)
     (define num-eggs 2))
   (module hen-house scheme
     (require 'nest)
     (provide (all-from-out 'nest)))
   (require 'hen-house)
   num-eggs
 ]}

 @defsubform[(rename-out [orig-id export-id] ...)]{ Exports each
 @scheme[orig-id], which must be @tech{bound} within the module at
 @tech{phase level} 0.  The symbolic name for each export is
 @scheme[export-id] instead @scheme[orig-d].

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (rename-out [count num-eggs]))
     (define count 2))
   (require 'nest)
   num-eggs
   count
 ]}

 @defsubform[(except-out provide-spec provide-spec ...)]{ Like the
 first @scheme[provide-spec], but omitting the bindings listed in each
 subsequent @scheme[provide-spec]. If one of the latter bindings is
 not included in the initial @scheme[provide-spec], a syntax error is
 reported. The symbolic export name information in the latter
 @scheme[provide-spec]s is ignored; only the bindings are used.

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (except-out (all-defined-out)
			  num-chicks))
     (define num-eggs 2)
     (define num-chicks 3))
   (require 'nest)
   num-eggs
   num-chicks
 ]}

 @defsubform[(prefix-out prefix-id provide-spec)]{
 Like @scheme[provide-spec], but with each symbolic export name from
 @scheme[provide-spec] prefixed with @scheme[prefix-id].

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (prefix-out chicken: num-eggs))
     (define num-eggs 2))
   (require 'nest)
   chicken:num-eggs
 ]}

 @defsubform[(struct-out id)]{Exports the bindings associated with a
 structure type @scheme[id]. Typically, @scheme[id] is bound with
 @scheme[(define-struct id ....)] or @scheme[(define-struct (id
 _super-id) ....)]; more generally, @scheme[id] must have a
 @tech{transformer binding} of structure-type information at
 @tech{phase level} 0; see @secref["structinfo"].  Furthermore, for
 each identifier mentioned in the structure-type information, the
 enclosing module must define or import one identifier that is
 @scheme[free-identifier=?]. If the structure-type information
 includes a super-type identifier, and if the identifier has a
 @tech{transformer binding} of structure-type information, the
 accessor and mutator bindings of the super-type are @italic{not}
 included by @scheme[struct-out] for export.

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (struct-out egg))
     (define-struct egg (color wt)))
   (require 'nest)
   (egg-color (make-egg 'blue 10))
 ]}

 @defsubform[(combine-out provide-spec ...)]{ The union of the
 @scheme[provide-spec]s.

 @defexamples[#:eval (syntax-eval)
   (module nest scheme
     (provide (combine-out num-eggs num-chicks))
     (define num-eggs 2)
     (define num-chicks 1))
   (require 'nest)
   num-eggs
   num-chicks
 ]}

 @defsubform[(protect-out provide-spec ...)]{ Like the union of the
 @scheme[provide-spec]s, except that the exports are protected; see
 @secref["modprotect"]. The @scheme[provide-spec] must specify only
 bindings that are defined within the exporting module.

 @examples[#:eval (syntax-eval)
   (module nest scheme
     (provide num-eggs (protect-out num-chicks))
     (define num-eggs 2)
     (define num-chicks 3))
   (define weak-inspector (make-inspector (current-code-inspector)))
   (define (weak-eval x)
     (parameterize ([current-code-inspector weak-inspector])
       (eval x)))
   (require 'nest)
   (list num-eggs num-chicks)
   (weak-eval 'num-eggs)
   (weak-eval 'num-chicks)
 ]}

 @specsubform[#:literals (for-meta) 
              (for-meta phase-level provide-spec ...)]{ Like the union of the
 @scheme[provide-spec]s, but adjusted to apply to @tech{phase level}
 specified by @scheme[phase-level] (where @scheme[#f] corresponds to the
 @tech{label phase level}). In particular, an @scheme[_id] or @scheme[rename-out] form as
 a @scheme[provide-spec] refers to a binding at @scheme[phase-level], an
 @scheme[all-defined-out] exports only @scheme[phase-level]
 definitions, and an @scheme[all-from-out] exports bindings
 imported with a shift by @scheme[phase-level].

 @examples[#:eval (syntax-eval)
   (module nest scheme
     (define-for-syntax eggs 2)
     (define chickens 3)
     (provide (for-syntax eggs)
              chickens))
   (require 'nest)
   (define-syntax (test-eggs stx)
     (printf "Eggs are ~a\n" eggs)
     #'0)
   (test-eggs)
   chickens

   (module broken-nest scheme
     (define eggs 2)
     (define chickens 3)
     (provide (for-syntax eggs)
              chickens))

   (module nest2 scheme
    (define-for-syntax eggs 2)
    (provide (for-syntax eggs)))
   (require (for-meta 2 scheme/base)
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
 @scheme[(for-meta 1 provide-spec ...)].}

 @specsubform[#:literals (for-template) 
              (for-template provide-spec ...)]{Same as
 @scheme[(for-meta -1 provide-spec ...)].}

 @specsubform[#:literals (for-label) 
              (for-label provide-spec ...)]{Same as
 @scheme[(for-meta #f provide-spec ...)].}

 @specsubform[derived-provide-spec]{See @scheme[define-provide-syntax]
 for information on expanding the set of @scheme[provide-spec] forms.}

Each export specified within a module must have a distinct symbolic
export name, though the same binding can be specified with the
multiple symbolic names.}


@defform[(for-meta phase-level require-spec ...)]{See @scheme[require] and @scheme[provide].}
@defform[(for-syntax require-spec ...)]{See @scheme[require] and @scheme[provide].} @defform[(for-template require-spec ...)]{See @scheme[require] and @scheme[provide].}
@defform[(for-label require-spec ...)]{See @scheme[require] and @scheme[provide].}

@defform/subs[(#%require raw-require-spec ...)
              ([raw-require-spec phaseless-spec
                                 (#,(schemeidfont "for-meta") phase-level phaseless-spec ...)
                                 (#,(schemeidfont "for-syntax") phaseless-spec ...)
                                 (#,(schemeidfont "for-template") phaseless-spec ...)
                                 (#,(schemeidfont "for-label") phaseless-spec ...)
                                 (#,(schemeidfont "just-meta") phase-level raw-require-spec ...)]
               [phase-level exact-integer
                            #f]
               [phaseless-spec raw-module-path
                               (#,(schemeidfont "only") raw-module-path id ...)
                               (#,(schemeidfont "prefix") prefix-id raw-module-path)
                               (#,(schemeidfont "all-except") raw-module-path id ...)
                               (#,(schemeidfont "prefix-all-except") prefix-id 
                                                                     raw-module-path id ...)
                               (#,(schemeidfont "rename") raw-module-path local-id exported-id)]
               [raw-module-path (#,(schemeidfont "quote") id)
                                rel-string
                                (#,(schemeidfont "lib") rel-string ...)
                                id
                                (#,(schemeidfont "file") string)
                                (#,(schemeidfont "planet") rel-string
                                                           (user-string pkg-string vers ...))])]{

The primitive import form, to which @scheme[require] expands. A
@scheme[raw-require-spec] is similar to a @scheme[_require-spec] in a
@scheme[require] form, except that the syntax is more constrained, not
composable, and not extensible. Also, sub-form names like
@schemeidfont{for-syntax} and @schemeidfont{lib} are recognized
symbolically, instead of via bindings. Although not formalized in the
grammar above, a @schemeidfont{just-meta} form cannot appear within a
@schemeidfont{just-meta} form.

Each @scheme[raw-require-spec] corresponds to the obvious
@scheme[_require-spec], but the @schemeidfont{rename} sub-form has the
identifiers in reverse order compared to @scheme[rename-in].

For most @scheme[raw-require-spec]s, the lexical context of the
@scheme[raw-require-spec] determines the context of introduced
identifiers. The exception is the @schemeidfont{rename} sub-form,
where the lexical context of the @scheme[local-id] is preserved.}


@defform/subs[(#%provide raw-provide-spec ...)
              ([raw-provide-spec phaseless-spec
                                 (#,(schemeidfont "for-meta") phase-level phaseless-spec)
                                 (#,(schemeidfont "for-syntax") phaseless-spec)
                                 (#,(schemeidfont "for-label") phaseless-spec)
                                 (#,(schemeidfont "protect") raw-provide-spec)]
               [phase-level exact-integer
                            #f]
               [phaseless-spec id 
                               (#,(schemeidfont "rename") local-id export-id) 
                               (#,(schemeidfont "struct") struct-id (field-id ...))
                               (#,(schemeidfont "all-from") raw-module-path)
                               (#,(schemeidfont "all-from-except") raw-module-path id ...)
                               (#,(schemeidfont "all-defined"))
                               (#,(schemeidfont "all-defined-except") id ...)
                               (#,(schemeidfont "prefix-all-defined") prefix-id) 
                               (#,(schemeidfont "prefix-all-defined-except") prefix-id id ...)
                               (#,(schemeidfont "protect") phaseless-spec ...)
                               (#,(schemeidfont "expand") (id . datum))])]{

The primitive export form, to which @scheme[provide] expands.  A
@scheme[_raw-module-path] is as for @scheme[#%require]. A
@schemeidfont{protect} sub-form cannot appear within a
@scheme[protect] sub-form.

Like @scheme[#%require], the sub-form keywords for @scheme[#%provide]
are recognized symbolically, and nearly every
@scheme[raw-provide-spec] has an obvious equivalent
@scheme[_provide-spec] via @scheme[provide], with the exception of the
@schemeidfont{struct} and @schemeidfont{expand} sub-forms.

A @scheme[(#,(schemeidfont "struct") struct-id (field-id ...))]
sub-form expands to @scheme[struct-id],
@schemeidfont{make-}@scheme[struct-id],
@schemeidfont{struct:}@scheme[struct-id],
@scheme[struct-id]@schemeidfont{?},
@scheme[struct-id]@schemeidfont{-}@scheme[field-id] for each
@scheme[field-id], and
@schemeidfont{set-}@scheme[struct-id]@schemeidfont{-}@scheme[field-id]@schemeidfont{!}
for each @scheme[field-id]. The lexical context of the
@scheme[struct-id] is used for all generated identifiers.

Unlike @scheme[#%require], the @scheme[#%provide] form is
macro-extensible via an explicit @schemeidfont{expand} sub-form; the
@scheme[(id . datum)] part is locally expanded as an expression (even
though it is not actually an expression), stopping when a
@scheme[begin] form is produced; if the expansion result is
@scheme[(begin raw-provide-spec ...)], it is spliced in place of the
@schemeidfont{expand} form, otherwise a syntax error is reported. The
@schemeidfont{expand} sub-form is not normally used directly; it
provides a hook for implementing @scheme[provide] and @tech{provide
transformers}.

The @schemeidfont{all-from} and @schemeidfont{all-from-except} forms
re-export only identifiers that are accessible in lexical context of
the @schemeidfont{all-from} or @schemeidfont{all-from-except} form
itself. That is, macro-introduced imports are not re-exported, unless
the @schemeidfont{all-from} or @schemeidfont{all-from-except} form was
introduced at the same time. Similarly, @schemeidfont{all-defined} and
its variants export only definitions accessible from the lexical
context of the @scheme[phaseless-spec] form.}

@; --------------------

@subsection{Additional @scheme[require] Forms}

@note-lib-only[scheme/require]

The following forms support more complex selection and manipulation of
sets of imported identifiers.

@defform[(matching-identifiers-in regexp require-spec)]{ Like
  @scheme[require-spec], but including only imports whose names match
  @scheme[regexp].  The @scheme[regexp] must be a literal regular
  expression (see @secref["regexp"]).

@defexamples[#:eval (syntax-eval)
(module zoo scheme/base
  (provide tunafish swordfish blowfish
           monkey lizard ant)
  (define tunafish 1)
  (define swordfish 2)
  (define blowfish 3)
  (define monkey 4)
  (define lizard 5)
  (define ant 6))
(require scheme/require)
(require (matching-identifiers-in #rx"\\w*fish" 'zoo))
tunafish
swordfish
blowfish
monkey
]}

@defform[(subtract-in require-spec subtracted-spec ...)]{ Like
  @scheme[require-spec], but omitting those imports that would be
  imported by one of the @scheme[subtracted-spec]s.

@defexamples[#:eval (syntax-eval)
(module earth scheme
  (provide land sea air)
  (define land 1)
  (define sea 2)
  (define air 3))

(module mars scheme
  (provide aliens)
  (define aliens 4))

(module solar-system scheme
  (require 'earth 'mars)
  (provide (all-from-out 'earth)
           (all-from-out 'mars)))

(require scheme/require)
(require (subtract-in 'solar-system 'earth))
land
aliens
]}

@defform[(filtered-in proc-expr require-spec)]{ The @scheme[proc-expr]
  should evaluate to a single-argument procedure, which is applied on
  each of the names (as strings) that are to be required according to
  @scheme[require-spec].  For each name, the procedure should return
  either a string (possibly different if you want it renamed), or
  @scheme[#f] to exclude the name.  (Note that @scheme[proc-expr] is a
  syntax-time expression.)

  For example,
  @schemeblock[
    (require (filtered-in
              (lambda (name)
                (and (regexp-match? #rx"^[a-z-]+$" name)
                     (regexp-replace
                      #rx"-" (string-titlecase name) "")))
              scheme/base))]
  will get the @scheme[scheme/base] bindings that match the regexp,
  and renamed to use ``camel case.''}

@defform[(path-up rel-string ...)]{

This specifies paths to module named by the @scheme[rel-string]s in a
similar way to using the @scheme[rel-string]s directly, except that if the
required module files are not found there, they are searched for in the parent
directory (in @filepath{../@scheme[_rel-string]}), and then in
the grand-parent directory, going all the way up to the root.  (Note
that the usual caveats hold for a macro that depends on files that it
looks for to determine its expansion: the resulting path becomes part
of the compiled form.)

This form is useful in setting up a ``project environment''.  For
example, you can write a @filepath{config.ss} file in the root
directory of your project with:
@schememod[
  scheme/base
  (require scheme/require-syntax (for-syntax "utils/in-here.ss"))
  ;; require form for my utilities
  (provide utils-in)
  (define-require-syntax utils-in in-here-transformer)
]
and in @filepath{utils/in-here.ss} in the root:
@schememod[
  scheme/base
  (require scheme/runtime-path)
  (provide in-here-transformer)
  (define-runtime-path here ".")
  (define (in-here-transformer stx)
    (syntax-case stx ()
      [(_ sym)
       (identifier? #'sym)
       (let ([path (build-path here (format "~a.ss" (syntax-e #'sym)))])
         (datum->syntax stx `(file ,(path->string path)) stx))]))
]
Finally, you can use it via @scheme[path-up]:
@schemeblock[
  (require scheme/require (path-up "config.ss") (utils-in foo))]
Note that the order of requires in this form is important, as each of
the first two bind the identifier used in the following.

An alternative in this scenario is to use @scheme[path-up] directly to
get to the utility module:
@schemeblock[
  (require scheme/require (path-up "utils/foo.ss"))]
but then you need to be careful with subdirectories that are called
@filepath{utils}, which will override the one in the project's root.
In other words, the previous method requires a single unique name.}

@; --------------------

@subsection{Additional @scheme[provide] Forms}

@note-lib-only[scheme/provide]

@defform[(matching-identifiers-out regexp provide-spec)]{ Like
  @scheme[provide-spec], but including only exports of bindings with
  an external name that matches @scheme[regexp]. The @scheme[regexp]
  must be a literal regular expression (see @secref["regexp"]).}

@defform[(filtered-out proc-expr provide-spec)]{ The
  @scheme[proc-expr] should evaluate to a single-argument procedure,
  which is applied on each of the names (as strings) that are to be
  provided according to @scheme[provide-spec].  For each name, the
  procedure should return either a string (possibly different if you
  want it renamed), or @scheme[#f] to exclude the name.  (Note that
  @scheme[proc-expr] is a syntax-time expression.)

  For example,
  @schemeblock[
    (provide (filtered-out
              (lambda (name)
                (and (regexp-match? #rx"^[a-z-]+$" name)
                     (regexp-replace
                      #rx"-" (string-titlecase name) "")))
              (all-defined-out)))]
  will provide all defined bindings that match the regexp, and renamed
  to use ``camel case''.}

@;------------------------------------------------------------------------
@section[#:tag "quote"]{Literals: @scheme[quote] and @scheme[#%datum]}

Many forms are implicitly quoted (via @scheme[#%datum]) as literals. See
@secref["expand-steps"] for more information.

@guideintro["quote"]{@scheme[quote]}

@defform[(quote datum)]{

Produces a constant value corresponding to @scheme[datum] (i.e., the
representation of the program fragment) without its @tech{lexical
information}, source location, etc.  Quoted pairs, vectors, and boxes
are immutable.

@mz-examples[
(eval:alts (#,(schemekeywordfont "quote") x) 'x)
(eval:alts (#,(schemekeywordfont "quote") (+ 1 2)) '(+ 1 2))
(+ 1 2)
]

}

@defform[(#%datum . datum)]{

Expands to @scheme[(#,(schemekeywordfont "quote") datum)], as long as
@scheme[datum] is not a keyword. If @scheme[datum] is a keyword, a
syntax error is reported.

See also @secref["expand-steps"] for information on how the expander
introduces @schemeidfont{#%datum} identifiers.

@mz-examples[
(#%datum . 10)
(#%datum . x)
(#%datum . #:x)
]
}

@;------------------------------------------------------------------------
@section[#:tag "#%expression"]{Expression Wrapper: @scheme[#%expression]}

@defform[(#%expression expr)]{

Produces the same result as @scheme[expr]. The only use of
@scheme[#%expression] is to force the parsing of a form as an
expression.

@mz-examples[
(#%expression (+ 1 2))
(#%expression (define x 10))
]}

@;------------------------------------------------------------------------
@section[#:tag "#%top"]{Variable References and @scheme[#%top]}

@defform/none[id]{

Refers to a module-level or local binding, when @scheme[id] is
not bound as a transformer (see @secref["expansion"]). At run-time,
the reference evaluates to the value in the location associated with
the binding.

When the expander encounters an @scheme[id] that is not bound by a
module-level or local binding, it converts the expression to
@scheme[(@#,schemeidfont{#%top} . id)] giving @schemeidfont{#%top}
the lexical context of the @scheme[id]; typically, that context refers
to @scheme[#%top]. See also @secref["expand-steps"].

@examples[
(define x 10)
x
(let ([x 5]) x)
((lambda (x) x) 2)
]}

@defform[(#%top . id)]{

Refers to a top-level definition that could bind @scheme[id], even if
@scheme[id] has a local binding in its context. Such references are
disallowed anywhere within a @scheme[module] form.  See also
@secref["expand-steps"] for information on how the expander
introduces @schemeidfont{#%top} identifiers.

@examples[
(define x 12)
(let ([x 5]) (#%top . x))
]}

@;------------------------------------------------------------------------
@section{Locations: @scheme[#%variable-reference]}

@defform*[#:literals (#%top)
          [(#%variable-reference id)
           (#%variable-reference (#%top . id))
           (#%variable-reference)]]{

Produces an opaque @deftech{variable reference} value representing the
location of @scheme[id], which must be bound as a @tech{top-level
variable} or @tech{module-level variable}. If no @scheme[id] is
supplied, the resulting value refers to an ``anonymous'' variable
defined within the enclosing context (i.e., within the enclosing
module, or at the top level if the form is not inside a module).

A @tech{variable reference} can be used with
@scheme[variable-reference->empty-namespace],
@scheme[variable-reference->resolved-module-path], and
@scheme[variable-reference->top-level-namespace], but facilities like
@scheme[define-namespace-anchor] and
@scheme[namespace-anchor->namespace] wrap those to provide an clearer
interface. A @tech{variable reference} is also useful to low-level
extensions; see @other-manual['(lib
"scribblings/inside/inside.scrbl")].}

@;------------------------------------------------------------------------
@section[#:tag "application"]{Procedure Applications and @scheme[#%app]}

@section-index{evaluation order}

@guideintro["application"]{procedure applications}

@defform/none[(proc-expr arg ...)]{

Applies a procedure, when @scheme[proc-expr] is not an
identifier that has a transformer binding (see
@secref["expansion"]).

More precisely, the expander converts this form to
@scheme[(@#,schemeidfont{#%app} proc-expr arg ...)], giving
@schemeidfont{#%app} the lexical context that is associated with the
original form (i.e., the pair that combines @scheme[proc-expr] and its
arguments). Typically, the lexical context of the pair indicates the
procedure-application @scheme[#%app] that is described next. See also
@secref["expand-steps"].

@mz-examples[
(+ 1 2)
((lambda (x #:arg y) (list y x)) #:arg 2 1)
]}

@defform[(#%app proc-expr arg ...)]{

Applies a procedure. Each @scheme[arg] is one of the following:

 @specsubform[arg-expr]{The resulting value is a non-keyword
                        argument.}

 @specsubform[(code:line keyword arg-expr)]{The resulting value is a
              keyword argument using @scheme[keyword]. Each
              @scheme[keyword] in the application must be distinct.}

The @scheme[proc-expr] and @scheme[_arg-expr]s are evaluated in order,
left to right. If the result of @scheme[proc-expr] is a procedure that
accepts as many arguments as non-@scheme[_keyword]
@scheme[_arg-expr]s, if it accepts arguments for all of the
@scheme[_keyword]s in the application, and if all required
keyword-based arguments are represented among the @scheme[_keyword]s
in the application, then the procedure is called with the values of
the @scheme[arg-expr]s. Otherwise, the @exnraise[exn:fail:contract].

The continuation of the procedure call is the same as the continuation
of the application expression, so the results of the procedure are the
results of the application expression.

The relative order of @scheme[_keyword]-based arguments matters only
for the order of @scheme[_arg-expr] evaluations; the arguments are
associated with argument variables in the applied procedure based on
the @scheme[_keyword]s, and not their positions. The other
@scheme[_arg-expr] values, in contrast, are associated with variables
according to their order in the application form.

See also @secref["expand-steps"] for information on how the
expander introduces @schemeidfont{#%app} identifiers.

@mz-examples[
(#%app + 1 2)
(#%app (lambda (x #:arg y) (list y x)) #:arg 2 1)
(#%app cons)
]}

@defform*[[(#%plain-app proc-expr arg-expr ...)
           (#%plain-app)]]{

Like @scheme[#%app], but without support for keyword arguments.
As a special case, @scheme[(#%plain-app)] produces @scheme['()].}

@;------------------------------------------------------------------------
@section[#:tag "lambda"]{Procedure Expressions: @scheme[lambda] and @scheme[case-lambda]}

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

Produces a procedure. The @scheme[kw-formals] determines the number of
arguments and which keyword arguments that the procedure accepts.

Considering only the first @scheme[arg] case, a simple
@scheme[kw-formals] has one of the following three forms:

@specsubform[(id ...)]{ The procedure accepts as many non-keyword
       argument values as the number of @scheme[id]s. Each @scheme[id]
       is associated with an argument value by position.}

@specsubform[(id ...+ . rest-id)]{ The procedure accepts any number of
       non-keyword arguments greater or equal to the number of
       @scheme[id]s. When the procedure is applied, the @scheme[id]s
       are associated with argument values by position, and all
       leftover arguments are placed into a list that is associated to
       @scheme[rest-id].}

@specsubform[rest-id]{ The procedure accepts any number of non-keyword
       arguments. All arguments are placed into a list that is
       associated with @scheme[rest-id].}

More generally, an @scheme[arg] can include a keyword and/or default
value. Thus, the first two cases above are more completely specified
as follows:

@specsubform[(arg ...)]{ Each @scheme[arg] has the following
       four forms:

        @specsubform[id]{Adds one to both the minimum and maximum
        number of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by
        position.}

        @specsubform[[id default-expr]]{Adds one to the maximum number
        of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by position,
        and if no such argument is provided, the @scheme[default-expr]
        is evaluated to produce a value associated with @scheme[id].
        No @scheme[arg] with a @scheme[default-expr] can appear
        before an @scheme[id] without a @scheme[default-expr] and
        without a @scheme[keyword].}

       @specsubform[(code:line keyword id)]{The procedure requires a
       keyword-based argument using @scheme[keyword]. The @scheme[id]
       is associated with a keyword-based actual argument using
       @scheme[keyword].}

       @specsubform[(code:line keyword [id default-expr])]{The
       procedure accepts a keyword-based using @scheme[keyword]. The
       @scheme[id] is associated with a keyword-based actual argument
       using @scheme[keyword], if supplied in an application;
       otherwise, the @scheme[default-expr] is evaluated to obtain a
       value to associate with @scheme[id].}

      The position of a @scheme[_keyword] @scheme[arg] in
      @scheme[kw-formals] does not matter, but each specified
      @scheme[keyword] must be distinct.}

@specsubform[(arg ...+ . rest-id)]{ Like the previous case, but
       the procedure accepts any number of non-keyword arguments
       beyond its minimum number of arguments. When more arguments are
       provided than non-@scheme[_keyword] arguments among the
       @scheme[arg]s, the extra arguments are placed into a
       list that is associated to @scheme[rest-id].}

The @scheme[kw-formals] identifiers are bound in the
@scheme[body]s. When the procedure is applied, a new @tech{location}
is created for each identifier, and the location is filled with the
associated argument value. The @tech{locations} are created and filled
in order, with @scheme[_default-expr]s evaluated as needed to fill
locations. @margin-note{In other words, argument bindings with
default-value expressions are evaluated analogous to @scheme[let*].}

If any identifier appears in the @scheme[body]s that is not one of the
identifiers in @scheme[kw-formals], then it refers to the same
location that it would if it appeared in place of the @scheme[lambda]
expression. (In other words, variable reference is lexically scoped.)

When multiple identifiers appear in a @scheme[kw-formals], they must
be distinct according to @scheme[bound-identifier=?].

If the procedure produced by @scheme[lambda] is applied to fewer or
more by-position or by-keyword arguments than it accepts, to by-keyword arguments
that it does not accept, or without required by-keyword arguments, then
the @exnraise[exn:fail:contract].

The last @scheme[body] expression is in tail position with respect to
the procedure body.

@mz-examples[
((lambda (x) x) 10)
((lambda (x y) (list y x)) 1 2)
((lambda (x [y 5]) (list y x)) 1 2)
(let ([f (lambda (x #:arg y) (list y x))])
 (list (f 1 #:arg 2)
       (f #:arg 2 1)))
]

When compiling a @scheme[lambda] or @scheme[case-lambda] expression,
Scheme looks for a @indexed-scheme['method-arity-error] property
attached to the expression (see @secref["stxprops"]). If it is
present with a true value, and if no case of the procedure accepts
zero arguments, then the procedure is marked so that an
@scheme[exn:fail:contract:arity] exception involving the procedure
will hide the first argument, if one was provided. (Hiding the first
argument is useful when the procedure implements a method, where the
first argument is implicit in the original source). The property
affects only the format of @scheme[exn:fail:contract:arity]
exceptions, not the result of @scheme[procedure-arity].}


@defform/subs[(case-lambda [formals body ...+] ...)
              ([formals (id ...)
                        (id ...+ . rest-id)
                        rest-id])]{
               
Produces a procedure. Each @scheme[[forms body ...+]]
clause is analogous to a single @scheme[lambda] procedure; applying
the @scheme[case-lambda]-generated procedure is the same as applying a
procedure that corresponds to one of the clauses---the first procedure
that accepts the given number of arguments. If no corresponding
procedure accepts the given number of arguments, the
@exnraise[exn:fail:contract].

Note that a @scheme[case-lambda] clause supports only
@scheme[formals], not the more general @scheme[_kw-formals] of
@scheme[lambda]. That is, @scheme[case-lambda] does not directly
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
Like @scheme[lambda], but without support for keyword or optional arguments.
}

@;------------------------------------------------------------------------
@section[#:tag "let"]{Local Binding: @scheme[let], @scheme[let*], @scheme[letrec], ...}

@guideintro["let"]{local binding}

@defform*[[(let ([id val-expr] ...) body ...+)
           (let proc-id ([id init-expr] ...) body ...+)]]{

The first form evaluates the @scheme[val-expr]s left-to-right, creates
a new location for each @scheme[id], and places the values into the
locations. It then evaluates the @scheme[body]s, in which the
@scheme[id]s are bound. The last @scheme[body] expression is in
tail position with respect to the @scheme[let] form. The @scheme[id]s
must be distinct according to @scheme[bound-identifier=?].

@mz-examples[
(let ([x 5]) x)
(let ([x 5])
  (let ([x 2]
        [y x])
    (list y x)))
]

The second form evaluates the @scheme[init-expr]s; the resulting
values become arguments in an application of a procedure
@scheme[(lambda (id ...) body ...+)], where @scheme[proc-id] is bound
within the @scheme[body]s to the procedure itself.}

@mz-examples[
(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
]

@defform[(let* ([id val-expr] ...) body ...+)]{

Similar to @scheme[let], but evaluates the @scheme[val-expr]s one by
one, creating a location for each @scheme[id] as soon as the value is
available. The @scheme[id]s are bound in the remaining @scheme[val-expr]s
as well as the @scheme[body]s, and the @scheme[id]s need not be
distinct; later bindings shadow earlier bindings.

@mz-examples[
(let* ([x 1]
       [y (+ x 1)])
  (list y x))
]}

@defform[(letrec ([id val-expr] ...) body ...+)]{

Similar to @scheme[let], but the locations for all @scheme[id]s are
created first and filled with @|undefined-const|, and all
@scheme[id]s are bound in all @scheme[val-expr]s as well as the
@scheme[body]s. The @scheme[id]s must be distinct according to
@scheme[bound-identifier=?].

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
@scheme[let], except that each @scheme[val-expr] must produce as many
values as corresponding @scheme[id]s, otherwise the
@exnraise[exn:fail:contract]. A separate location is created for each
@scheme[id], all of which are bound in the @scheme[body]s.

@mz-examples[
(let-values ([(x y) (quotient/remainder 10 3)])
  (list y x))
]}

@defform[(let*-values ([(id ...) val-expr] ...) body ...+)]{ Like
@scheme[let*], except that each @scheme[val-expr] must produce as many
values as corresponding @scheme[id]s. A separate location is created
for each @scheme[id], all of which are bound in the later
@scheme[val-expr]s and in the @scheme[body]s.

@mz-examples[
(let*-values ([(x y) (quotient/remainder 10 3)]
              [(z) (list y x)])
  z)
]}

@defform[(letrec-values ([(id ...) val-expr] ...) body ...+)]{ Like
@scheme[letrec], except that each @scheme[val-expr] must produce as
many values as corresponding @scheme[id]s. A separate location is
created for each @scheme[id], all of which are initialized to
@|undefined-const| and bound in all @scheme[val-expr]s
and in the @scheme[body]s.

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

@margin-note/ref{See also @scheme[splicing-let-syntax].}

Creates a @tech{transformer binding} (see
@secref["transformer-model"]) of each @scheme[id] with the value of
@scheme[trans-expr], which is an expression at @tech{phase level} 1
relative to the surrounding context. (See @secref["id-model"] for
information on @tech{phase levels}.)

The evaluation of each @scheme[trans-expr] is @scheme[parameterize]d
to set @scheme[current-namespace] to a @tech{namespace} that shares
@tech{bindings} and @tech{variables} with the namespace being used to
expand the @scheme[let-syntax] form, except that its @tech{base phase}
is one greater.

Each @scheme[id] is bound in the @scheme[body]s, and not in other
@scheme[trans-expr]s.}

@defform[(letrec-syntax ([id trans-expr] ...) body ...+)]{

@margin-note/ref{See also @scheme[splicing-letrec-syntax].}

Like @scheme[let-syntax], except that each @scheme[id] is also bound
within all @scheme[trans-expr]s.}

@defform[(let-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

@margin-note/ref{See also @scheme[splicing-let-syntaxes].}

Like @scheme[let-syntax], but each @scheme[trans-expr] must produce as
many values as corresponding @scheme[id]s, each of which is bound to
the corresponding value.}

@defform[(letrec-syntaxes ([(id ...) trans-expr] ...) body ...+)]{

@margin-note/ref{See also @scheme[splicing-letrec-syntaxes].}

Like @scheme[let-syntax], except that each @scheme[id] is also bound
within all @scheme[trans-expr]s.}

@defform[(letrec-syntaxes+values ([(trans-id ...) trans-expr] ...)
                                 ([(val-id ...) val-expr] ...)
            body ...+)]{

Combines @scheme[letrec-syntaxes] with @scheme[letrec-values]: each
@scheme[trans-id] and @scheme[val-id] is bound in all
@scheme[trans-expr]s and @scheme[val-expr]s.

The @scheme[letrec-syntaxes+values] form is the core form for local
compile-time bindings, since forms like @scheme[letrec-syntax] and
internal @scheme[define-syntax] expand to it. In a fully expanded
expression (see @secref["fully-expanded"]), the @scheme[trans-id]
bindings are discarded and the form reduces to @scheme[letrec], but
@scheme[letrec-syntaxes+values] can appear in the result of
@scheme[local-expand] with an empty stop list.

See also @scheme[local], which supports local bindings with
@scheme[define], @scheme[define-syntax], and more.}

@;------------------------------------------------------------------------
@section[#:tag "local"]{Local Definitions: @scheme[local]}

@note-lib[scheme/local]

@defform[(local [definition ...] body ...+)]{

Like @scheme[letrec], except that the bindings are expressed in the
same way as in the top-level or in a module body: using
@scheme[define], @scheme[define-values], @scheme[define-syntax],
@scheme[define-struct], etc.  Definitions are distinguished from
non-definitions by partially expanding @scheme[definition] forms (see
@secref["partial-expansion"]). As in the top-level or in a module
body, a @scheme[begin]-wrapped sequence is spliced into the sequence
of @scheme[definition]s.}

@;------------------------------------------------------------------------
@include-section["shared.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "if"]{Conditionals: @scheme[if], @scheme[cond], @scheme[and], and @scheme[or]}

@guideintro["conditionals"]{conditionals}

@defform[(if test-expr then-expr else-expr)]{

Evaluates @scheme[test-expr]. If it produces any value other than
@scheme[#f], then @scheme[then-expr] is evaluated, and its results are
the result for the @scheme[if] form. Otherwise, @scheme[else-expr] is
evaluated, and its results are the result for the @scheme[if]
form. The @scheme[then-expr] and @scheme[else-expr] are in tail
position with respect to the @scheme[if] form.

@mz-examples[
(if (positive? -5) (error "doesn't get here") 2)
(if (positive? 5) 1 (error "doesn't get here"))
(if 'we-have-no-bananas "yes" "no")
]}

@defform/subs[#:literals (else =>)
              (cond cond-clause ...)
              ([cond-clause [test-expr then-expr ...+]
                            [else then-expr ...+]
                            [test-expr => proc-expr]
                            [test-expr]])]{

@guideintro["cond"]{@scheme[cond]}

A @scheme[cond-clause] that starts with @scheme[else] must be the last
@scheme[cond-clause].

If no @scheme[cond-clause]s are present, the result is @|void-const|.

If only a @scheme[[else then-expr ...+]] is present, then the
@scheme[then-expr]s are evaluated. The results from all but the last
@scheme[then-expr] are ignored. The results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, are the results for the whole @scheme[cond]
form.

Otherwise, the first @scheme[test-expr] is evaluated. If it produces
@scheme[#f], then the result is the same as a @scheme[cond] form with
the remaining @scheme[cond-clause]s, in tail position with respect to
the original @scheme[cond] form. Otherwise, evaluation depends on the
form of the @scheme[cond-clause]:

@specsubform[[test-expr then-expr ...+]]{The @scheme[then-expr]s are
evaluated in order, and the results from all but the last
@scheme[then-expr] are ignored. The results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[cond] form, provides the result for the whole @scheme[cond]
form.}

@specsubform[#:literals (=>) [test-expr => proc-expr]]{The @scheme[proc-expr] is
evaluated, and it must produce a procedure that accepts on argument,
otherwise the @exnraise[exn:fail:contract]. The procedure is applied
to the result of @scheme[test-expr] in tail position with respect to
the @scheme[cond] expression.}

@specsubform[[test-expr]]{The result of the @scheme[test-expr] is
returned as the result of the @scheme[cond] form. The
@scheme[test-expr] is not in tail position.}

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

Recognized specially within forms like @scheme[cond]. An
@scheme[else] form as an expression is a syntax error.}


@defidform[=>]{

Recognized specially within forms like @scheme[cond]. A
@scheme[=>] form as an expression is a syntax error.}


@defform[(and expr ...)]{

@guideintro["and+or"]{@scheme[and]}

If no @scheme[expr]s are provided, then result is @scheme[#t].

If a single @scheme[expr] is provided, then it is in tail position, so
the results of the @scheme[and] expression are the results of the
@scheme[expr].

Otherwise, the first @scheme[expr] is evaluated. If it produces
@scheme[#f], the result of the @scheme[and] expression is
@scheme[#f]. Otherwise, the result is the same as an @scheme[and]
expression with the remaining @scheme[expr]s in tail position with
respect to the original @scheme[and] form.

@mz-examples[
(and)
(and 1)
(and (values 1 2))
(and #f (error "doesn't get here"))
(and #t 5)
]}

@defform[(or expr ...)]{

@guideintro["and+or"]{@scheme[or]}

If no @scheme[expr]s are provided, then result is @scheme[#f].

If a single @scheme[expr] is provided, then it is in tail position, so
the results of the @scheme[and] expression are the results of the
@scheme[expr].

Otherwise, the first @scheme[expr] is evaluated. If it produces a
value other than @scheme[#f], that result is the result of the
@scheme[or] expression. Otherwise, the result is the same as an
@scheme[or] expression with the remaining @scheme[expr]s in tail
position with respect to the original @scheme[or] form.

@mz-examples[
(or)
(or 1)
(or (values 1 2))
(or 5 (error "doesn't get here"))
(or #f 5)
]}

@;------------------------------------------------------------------------
@section[#:tag "case"]{Dispatch: @scheme[case]}

@defform/subs[#:literals (else)
              (case val-expr case-clause ...)
              ([case-clause [(datum ...) then-expr ...+]
                            [else then-expr ...+]])]{

Evaluates @scheme[val-expr] and uses the result to select a
@scheme[case-clause]. The selected clause is the first one with a
@scheme[datum] whose @scheme[quote]d form is @scheme[eqv?] to the
result of @scheme[val-expr]. If no such @scheme[datum] is present, the
@scheme[else] @scheme[case-clause] is selected; if no @scheme[else]
@scheme[case-clause] is present, either, then the result of the
@scheme[case] form is @|void-const|.

For the selected @scheme[case-clause], the results of the last
@scheme[then-expr], which is in tail position with respect to the
@scheme[case] form, are the results for the whole @scheme[case] form.

A @scheme[case-clause] that starts with @scheme[else] must be the last
@scheme[case-clause].

@mz-examples[
(case (+ 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
(case (- 7 5)
 [(1 2 3) 'small]
 [(10 11 12) 'big])
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
@section[#:tag "define"]{Definitions: @scheme[define], @scheme[define-syntax], ...}

@guideintro["define"]{definitions}

@defform*/subs[[(define id expr)
                (define (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg ...)
                       (code:line arg ... @#,schemeparenfont{.} rest-id)]
                 [arg arg-id
                      [arg-id default-expr]
                      (code:line keyword arg-id)
                      (code:line keyword [arg-id default-expr])])]{

The first form @tech{bind}s @scheme[id] to the result of
@scheme[expr], and the second form @tech{bind}s @scheme[id] to a
procedure. In the second case, the generation procedure is
@scheme[(#,cvt (head args) body ...+)], using the @|cvt| meta-function
defined as follows:

@schemeblock[
(#,cvt (id . _kw-formals) . _datum)   = (lambda _kw-formals . _datum)
(#,cvt (head . _kw-formals) . _datum) = (lambda _kw-formals expr)
                                         @#,elem{if} (#,cvt head . _datum) = expr
]

At the top level, the top-level binding @scheme[id] is created after
evaluating @scheme[expr], if it does not exist already, and the
top-level mapping of @scheme[id] (in the @techlink{namespace} linked
with the compiled definition) is set to the binding at the same time.

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

Evaluates the @scheme[expr], and @tech{bind}s the results to the
@scheme[id]s, in order, if the number of results matches the number of
@scheme[id]s; if @scheme[expr] produces a different number of results,
the @exnraise[exn:fail:contract].

At the top level, the top-level binding for each @scheme[id] is
created after evaluating @scheme[expr], if it does not exist already,
and the top-level mapping of each @scheme[id] (in the
@techlink{namespace} linked with the compiled definition) is set to
the binding at the same time.

@defexamples[
(define-values () (values))
(define-values (x y z) (values 1 2 3))
z
]
}


@defform*[[(define-syntax id expr)
           (define-syntax (head args) body ...+)]]{

The first form creates a @tech{transformer binding} (see
@secref["transformer-model"]) of @scheme[id] with the value of
@scheme[expr], which is an expression at @tech{phase level} 1 relative
to the surrounding context. (See @secref["id-model"] for information
on @tech{phase levels}.)  Evaluation of @scheme[expr] side is
@scheme[parameterize]d to set @scheme[current-namespace] as in
@scheme[let-syntax].

The second form is a shorthand the same as for @scheme[define]; it
expands to a definition of the first form where the @scheme[expr] is a
@scheme[lambda] form.}

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

Like @scheme[define-syntax], but creates a @tech{transformer binding}
for each @scheme[id].  The @scheme[expr] should produce as many values
as @scheme[id]s, and each value is bound to the corresponding
@scheme[id].

When @scheme[expr] produces zero values for a top-level
@scheme[define-syntaxes] (i.e., not in a module or internal-definition
position), then the @scheme[id]s are effectively declared without
binding; see @secref["macro-introduced-bindings"].

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

Like @scheme[define], except that the binding is at @tech{phase level}
1 instead of @tech{phase level} 0 relative to its context. The
expression for the binding is also at @tech{phase level} 1. (See
@secref["id-model"] for information on @tech{phase levels}.)
Evaluation of @scheme[expr] side is @scheme[parameterize]d to set
@scheme[current-namespace] as in @scheme[let-syntax].}

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

Like @scheme[define-for-syntax], but @scheme[expr] must produce as
many values as supplied @scheme[id]s, and all of the @scheme[id]s are
bound (at @tech{phase level} 1).}

@defexamples[#:eval (syntax-eval)
(define-values-for-syntax (foo1 foo2) (values 1 2))
(define-syntax (bar syntax-object)
  (printf "foo1 is ~a foo2 is ~a\n" foo1 foo2)
  #'2)
(bar) 
]

@; ----------------------------------------------------------------------

@subsection[#:tag "require-syntax"]{@scheme[require] Macros}

@note-lib-only[scheme/require-syntax]

@defform*[[(define-require-syntax id expr)
           (define-require-syntax (id args ...) body ...+)]]{

The first form is like @scheme[define-syntax], but for a
@scheme[require] sub-form. The @scheme[proc-expr] must produce a
procedure that accepts and returns a syntax object representing a
@scheme[require] sub-form.

This form expands to @scheme[define-syntax] with a use of
@scheme[make-require-transformer]; see @secref["require-trans"] for
more information.

The second form is a shorthand the same as for @scheme[define-syntax]; it
expands to a definition of the first form where the @scheme[expr] is a
@scheme[lambda] form.}

@; ----------------------------------------------------------------------

@subsection[#:tag "provide-syntax"]{@scheme[provide] Macros}

@note-lib-only[scheme/provide-syntax]

@defform*[[(define-provide-syntax id expr)
           (define-provide-syntax (id args ...) body ...+)]]{

The first form is like @scheme[define-syntax], but for a
@scheme[provide] sub-form. The @scheme[proc-expr] must produce a
procedure that accepts and returns a syntax object representing a
@scheme[provide] sub-form.

This form expands to @scheme[define-syntax] with a use of
@scheme[make-provide-transformer]; see @secref["provide-trans"] for
more information.

The second form is a shorthand the same as for @scheme[define-syntax]; it
expands to a definition of the first form where the @scheme[expr] is a
@scheme[lambda] form.}

@;------------------------------------------------------------------------
@section[#:tag "begin"]{Sequencing: @scheme[begin], @scheme[begin0], and @scheme[begin-for-syntax]}

@guideintro["begin"]{@scheme[begin] and @scheme[begin0]}

@defform*[[(begin form ...)
           (begin expr ...+)]]{

The first form applies when @scheme[begin] appears at the top level,
at module level, or in an internal-definition position (before any
expression in the internal-definition sequence). In that case, the
@scheme[begin] form is equivalent to splicing the @scheme[form]s into
the enclosing context.

The second form applies for @scheme[begin] in an expression position.
In that case, the @scheme[expr]s are evaluated in order, and the
results are ignored for all but the last @scheme[expr]. The last
@scheme[expr] is in tail position with respect to the @scheme[begin]
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

Evaluates the @scheme[expr], then evaluates the @scheme[body]s,
ignoring the @scheme[body] results. The results of the @scheme[expr]
are the results of the @scheme[begin0] form, but the @scheme[expr] is
in tail position only if no @scheme[body]s are present.

@mz-examples[
(begin0
  (values 1 2)
  (printf "hi\n"))
]}

@defform[(begin-for-syntax form ...)]{

Allowed only in a @tech{top-level context} or @tech{module context}.
Each @scheme[form] is partially expanded (see
@secref["partial-expansion"]) to determine one of the following
classifications:

@itemize[

 @item{@scheme[define] or @scheme[define-values] form: converted to
       a @scheme[define-values-for-syntax] form.}

 @item{@scheme[require] form: content is wrapped with
       @scheme[for-syntax].}

 @item{expression form @scheme[_expr]: converted to
       @scheme[(define-values-for-syntax () (begin _expr (values)))], which
       effectively evaluates the expression at expansion time and, in
       the case of a @tech{module context}, preserves the expression
       for future @tech{visit}s of the module.}

]

}

@;------------------------------------------------------------------------
@section[#:tag "when+unless"]{Guarded Evaluation: @scheme[when] and @scheme[unless]}

@guideintro["when+unless"]{@scheme[when] and @scheme[unless]}

@defform[(when test-expr expr ...)]{

Evaluates the @scheme[text-expr]. If the result is @scheme[#f], then
the result of the @scheme[when] expression is
@|void-const|. Otherwise, the @scheme[expr]s are evaluated, and the
last @scheme[expr] is in tail position with respect to the
@scheme[when] form.

@mz-examples[
(when (positive? -5)
  (display "hi"))
(when (positive? 5)
  (display "hi")
  (display " there"))
]}

@defform[(unless test-expr expr ...)]{

Equivalent to @scheme[(when (not test-expr) expr ...)].

@mz-examples[
(unless (positive? 5)
  (display "hi"))
(unless (positive? -5)
  (display "hi")
  (display " there"))
]}

@;------------------------------------------------------------------------
@section[#:tag "set!"]{Assignment: @scheme[set!] and @scheme[set!-values]}

@guideintro["set!"]{@scheme[set!]}

@defform[(set! id expr)]{

If @scheme[id] has a @tech{transformer binding} to an @tech{assignment
transformer}, as produced by @scheme[make-set!-transformer] or as an
instance of a structure type with the @scheme[prop:set!-transformer]
property, then this form is expanded by calling the assignment
transformer with the full expressions. If @scheme[id] has a
@tech{transformer binding} to a @tech{rename transformer} as produced
by @scheme[make-rename-transformer] or as an instance of a structure
type with the @scheme[prop:rename-transformer] property, then this
form is expanded by replacing @scheme[id] with the target identifier
(e.g., the one provided to @scheme[make-rename-transformer]). If a
transformer binding has both @scheme[prop:set!-transformer] ad
@scheme[prop:rename-transformer] properties, the latter takes
precedence.

Otherwise, evaluates @scheme[expr] and installs the result into the
location for @scheme[id], which must be bound as a local variable or
defined as a @tech{top-level variable} or @tech{module-level
variable}. If @scheme[id] refers to an imported binding, a syntax
error is reported.  If @scheme[id] refers to a @tech{top-level
variable} that has not been defined, the @exnraise[exn:fail:contract].

See also @scheme[compile-allow-set!-undefined].

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

Assuming that all @scheme[id]s refer to variables, this form evaluates
@scheme[expr], which must produce as many values as supplied
@scheme[id]s.  The location of each @scheme[id] is filled wih to the
corresponding value from @scheme[expr] in the same way as for
@scheme[set!].

@mz-examples[
(let ([a 1]
      [b 2])
  (set!-values (a b) (values b a))
  (list a b))
]

More generally, the @scheme[set!-values] form is expanded to

@schemeblock[
(let-values ([(_tmp-id ...) expr])
  (set! id _tmp-id) ...)
]

which triggers further expansion if any @scheme[id] has a transformer
binding to an @tech{assignment transformer}.}

@;------------------------------------------------------------------------
@include-section["for.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "wcm"]{Continuation Marks: @scheme[with-continuation-mark]}

@defform[(with-continuation-mark key-expr val-expr result-expr)]{

The @scheme[key-expr], @scheme[mark-expr], and @scheme[result-expr]
expressions are evaluated in order. After @scheme[key-expr] is
evaluated to obtain a key and @scheme[mark-expr] is evaluated to
obtain a mark, the key is mapped to the mark in the current
continuation's initial frame. If the frame already has a mark for the
key, it is replaced. Finally, the @scheme[result-expr] is evaluated;
the continuation for evaluating @scheme[result-expr] is the
continuation of the @scheme[with-continuation-mark] expression (so the
result of the @scheme[result-expr] is the result of the
@scheme[with-continuation-mark] expression, and @scheme[result-expr]
is in tail position for the @scheme[with-continuation-mark]
expression).

@moreref["contmarks"]{continuation marks}}

@;------------------------------------------------------------------------
@section[#:tag "quasiquote"]{Quasiquoting: @scheme[quasiquote], @scheme[unquote], and @scheme[unquote-splicing]}

@defform[(quasiquote datum)]{

The same as @scheme[(quote datum)] if @scheme[datum] does not include
@scheme[(#,unquote-id _expr)] or @scheme[(#,unquote-splicing-id _expr)]. An
@scheme[(#,unquote-id _expr)] form escapes from the quote, however,
and the result of the @scheme[_expr] takes the place of the
@scheme[(#,unquote-id _expr)] form in the @scheme[quasiquote] result. An
@scheme[(#,unquote-splicing-id _expr)] similarly escapes, but the
@scheme[_expr] must produce a list, and its elements are spliced as
multiple values place of the @scheme[(#,unquote-splicing-id _expr)], which
must appear as the @scheme[car] or a quoted pair, as an element of a
quoted vector, or as an element of a quoted @tech{prefab} structure;
in the case of a pair, if the @scheme[cdr] of the relevant quoted pair
is empty, then @scheme[_expr] need not produce a list, and its result
is used directly in place of the quoted pair (in the same way that
@scheme[append] accepts a non-list final argument).  In a quoted
@tech{hash table}, an @scheme[(#,unquote-id _expr)] or
@scheme[(#,unquote-splicing-id _expr)] expression escapes only in the
second element of an entry pair (i.e., the value), while entry keys
are always implicitly quoted. If @scheme[unquote] or
@scheme[unquote-splicing] appears within @scheme[quasiquote] in any
other way than as @scheme[(#,unquote-id _expr)] or
@scheme[(#,unquote-splicing-id _expr)], a syntax error is reported.

@mz-examples[
(eval:alts (#,(scheme quasiquote) (0 1 2)) `(0 1 2))
(eval:alts (#,(scheme quasiquote) (0 (#,unquote-id (+ 1 2)) 4)) `(0 ,(+ 1 2) 4))
(eval:alts (#,(scheme quasiquote) (0 (#,unquote-splicing-id (list 1 2)) 4)) `(0 ,@(list 1 2) 4))
(eval:alts (#,(scheme quasiquote) (0 (#,unquote-splicing-id 1) 4)) `(0 ,@1 4))
(eval:alts (#,(scheme quasiquote) (0 (#,unquote-splicing-id 1))) `(0 ,@1))
]

A @scheme[quasiquote], @scheme[unquote], or @scheme[unquote-splicing]
form is typically abbreviated with @litchar{`}, @litchar{,}, or
@litchar[",@"], respectively. See also @secref["parse-quote"].

@mz-examples[
`(0 1 2)
`(1 ,(+ 1 2) 4)
`#s(stuff 1 ,(+ 1 2) 4)
(eval:alts #,(schemefont (schemevalfont "`#hash((\"a\" . ") "," (scheme (+ 1 2)) (schemevalfont "))")) #hash(("a" . 3)))
`#hash((,(+ 1 2) . "a"))
`(1 ,@(list 1 2) 4)
`#(1 ,@(list 1 2) 4)
]

A @scheme[quasiquote] form within the original @scheme[datum]
increments the level of quasiquotation: within the @scheme[quasiquote]
form, each @scheme[unquote] or @scheme[unquote-splicing] is preserved,
but a further nested @scheme[unquote] or @scheme[unquote-splicing]
escapes.  Multiple nestings of @scheme[quasiquote] require multiple
nestings of @scheme[unquote] or @scheme[unquote-splicing] to escape.

@mz-examples[
`(1 `,(+ 1 ,(+ 2 3)) 4)
`(1 ```,,@,,@(list (+ 1 2)) 4)
]

The @scheme[quasiquote] form allocates only as many fresh cons cells,
vectors, and boxes as are needed without analyzing @scheme[unquote]
and @scheme[unquote-splicing] expressions. For example, in

@schemeblock[
`(,1 2 3)
]

a single tail @scheme['(2 3)] is used for every evaluation of the
@scheme[quasiquote] expression.

}

@defidform[unquote]{

See @scheme[quasiquote], where @scheme[unquote] is recognized as an
escape. An @scheme[unquote] form as an expression is a syntax error.}

@defidform[unquote-splicing]{

See @scheme[quasiquote], where @scheme[unquote-splicing] is recognized as an
escape. An @scheme[unquote-splicing] form as an expression is a syntax error.}

@;------------------------------------------------------------------------
@section{Syntax Quoting: @scheme[quote-syntax]}

@defform[(quote-syntax datum)]{

Produces a @tech{syntax object} that preserves the @tech{lexical
information} and source-location information attached to
@scheme[datum] at expansion time.

@mz-examples[
(syntax? (quote-syntax x))
]
}

@;------------------------------------------------------------------------
@section[#:tag "#%top-interaction"]{Interaction Wrapper: @scheme[#%top-interaction]}

@defform[(#%top-interaction . form)]{

Expands to simply @scheme[form]. The @scheme[#%top-interaction] form
is similar to @scheme[#%app] and @scheme[#%module-begin], in that it
provides a hook to control interactive evaluation through
@scheme[load] (more precisely, the default @tech{load handler}) or
@scheme[read-eval-print-loop].}

@;------------------------------------------------------------------------
@include-section["package.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "nest"]{Flattening Syntactic Sequences: @scheme[nest]}

@note-lib[scheme/nest]

@defform[(nest ([datum ...+] ...) body ...+)]{

Combines nested expressions that syntactically drift to the right into
a more linear textual format, much in the same way that @scheme[let*]
linearizes a sequence of nested @scheme[let] expressions.

For example,

@schemeblock[
(nest ([let ([x 10]
             [y 6])]
       [with-handlers ([exn:fail? (lambda (x) 15)])]
       [parameterize ([current-output-port (current-error-port)])]
       [let-values ([(d r) (quotient/remainder x y)])])
  (display (+ d r)))
]

is equivalent to

@schemeblock[
(let ([x 10]
      [y 6])
  (with-handlers ([exn:fail? (lambda (x) 15)])
    (parameterize ([current-output-port (current-error-port)])
      (let-values ([(d r) (quotient/remainder x y)])
        (display (+ d r))))))
]

The @scheme[nest] form is unusual in that it has no semantics apart
from its expansion, and its implementation is easier to understand
than a precise prose description:

@schemeblock[
(define-syntax nest
  (syntax-rules ()
    [(nest () body0 body ...)
     (let () body0 body ...)]
    [(nest ([form forms ...]) body0 body ...)
     (form forms ... (let () body0 body ...))]
    [(nest ([form forms ...] . more) body0 body ...)
     (form forms ... (nest more body0 body ...))]))
]}


@close-eval[require-eval]
@close-eval[meta-in-eval]
