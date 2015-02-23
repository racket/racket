#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "modfile.rkt"
          (for-label racket/date))

@title[#:tag "module-languages"]{Module Languages}

When using the longhand @racket[module] form for writing modules, the
module path that is specified after the new module's name provides the
initial imports for the module. Since the initial-import module
determines even the most basic bindings that are available in a
module's body, such as @racket[require], the initial import can be
called a @deftech{module language}.

The most common @tech{module languages} are @racketmodname[racket] or
@racketmodname[racket/base], but you can define your own
@tech{module language} by defining a suitable module. For example,
using @racket[provide] subforms like @racket[all-from-out],
@racket[except-out], and @racket[rename-out], you can add, remove, or
rename bindings from @racketmodname[racket] to produce a @tech{module
language} that is a variant of @racketmodname[racket]:

@guideother{@secref["module-syntax"] introduces the longhand
@racket[module] form.}

@interaction[
(module raquet racket
  (provide (except-out (all-from-out racket) lambda)
           (rename-out [lambda function])))
(module score 'raquet
  (map (function (points) (case points
                           [(0) "love"] [(1) "fifteen"]
                           [(2) "thirty"] [(3) "forty"]))
       (list 0 2)))
(require 'score)
]

@; ----------------------------------------
@section[#:tag "implicit-forms"]{Implicit Form Bindings}

If you try to remove too much from @racketmodname[racket] in defining
your own @tech{module language}, then the resulting module
will no longer work right as a @tech{module language}:

@interaction[
(module just-lambda racket
  (provide lambda))
(module identity 'just-lambda
  (lambda (x) x))
]

The @racket[#%module-begin] form is an implicit form that wraps the
body of a module. It must be provided by a module that is to be used
as @tech{module language}:

@interaction[
(module just-lambda racket
  (provide lambda #%module-begin))
(module identity 'just-lambda
  (lambda (x) x))
(require 'identity)
]

The other implicit forms provided by @racket[racket/base] are
@racket[#%app] for function calls, @racket[#%datum] for literals, and
@racket[#%top] for identifiers that have no binding:

@interaction[
(module just-lambda racket
  (provide lambda #%module-begin
           (code:comment @#,t{@racketidfont{ten} needs these, too:})
           #%app #%datum))
(module ten 'just-lambda
  ((lambda (x) x) 10))
(require 'ten)
]

Implicit forms such as @racket[#%app] can be used explicitly in a module,
but they exist mainly to allow a module language to restrict or change
the meaning of implicit uses. For example, a @racket[lambda-calculus]
@tech{module language} might restrict functions to a single argument,
restrict function calls to supply a single argument, restrict the
module body to a single expression, disallow literals, and treat
unbound identifiers as uninterpreted symbols:

@interaction[
(module lambda-calculus racket
  (provide (rename-out [1-arg-lambda lambda]
                       [1-arg-app #%app]
                       [1-form-module-begin #%module-begin]
                       [no-literals #%datum]
                       [unbound-as-quoted #%top]))
  (define-syntax-rule (1-arg-lambda (x) expr)
    (lambda (x) expr))
  (define-syntax-rule (1-arg-app e1 e2)
    (#%app e1 e2))
  (define-syntax-rule (1-form-module-begin e)
    (#%module-begin e))
  (define-syntax (no-literals stx)
    (raise-syntax-error #f "no" stx))
  (define-syntax-rule (unbound-as-quoted . id)
    'id))
(module ok 'lambda-calculus
  ((lambda (x) (x z))
   (lambda (y) y)))
(require 'ok)
(module not-ok 'lambda-calculus
  (lambda (x y) x))
(module not-ok 'lambda-calculus
  (lambda (x) x)
  (lambda (y) (y y)))
(module not-ok 'lambda-calculus
  (lambda (x) (x x x)))
(module not-ok 'lambda-calculus
  10)
]

Module languages rarely redefine @racket[#%app], @racket[#%datum], and
@racket[#%top], but redefining @racket[#%module-begin] is more
frequently useful. For example, when using modules to construct
descriptions of HTML pages where a description is exported from the
module as @racketidfont{page}, an alternate @racket[#%module-begin]
can help eliminate @racket[provide] and quasiquoting
boilerplate, as in @filepath{html.rkt}:

@racketmodfile["html.rkt"]

Using the @filepath{html.rkt} @tech{module language}, a simple web page
can be described without having to explicitly define or export
@racketidfont{page} and starting in @racket[quasiquote]d mode instead
of expression mode:

@interaction[
(module lady-with-the-spinning-head "html.rkt"
  (title "Queen of Diamonds")
  (p "Updated: " ,(now)))
(require 'lady-with-the-spinning-head)
page
]

@; ----------------------------------------
@section[#:tag "s-exp"]{Using @racket[@#,hash-lang[] @#,racketmodname[s-exp]]}

Implementing a language at the level of @hash-lang[] is more complex
than declaring a single module, because @hash-lang[] lets programmers
control several different facets of a language. The
@racketmodname[s-exp] language, however, acts as a kind of
meta-language for using a @tech{module language} with the
@hash-lang[] shorthand:

@racketmod[
s-exp _module-name
_form ...]

is the same as

@racketblock[
(module _name _module-name
  _form ...)
]

where @racket[_name] is derived from the source file containing the
@hash-lang[] program. The name @racketmodname[s-exp] is short for
``@as-index{S-expression},'' which is a traditional name for
Racket's @tech{reader}-level lexical conventions: parentheses,
identifiers, numbers, double-quoted strings with certain backslash
escapes, and so on.

Using @racket[@#,hash-lang[] @#,racketmodname[s-exp]], the
@racket[lady-with-the-spinning-head] example from before can be
written more compactly as:

@racketmod[
s-exp "html.rkt"

(title "Queen of Diamonds")
(p "Updated: " ,(now))
]

Later in this guide, @secref["hash-languages"] explains how to define
your own @hash-lang[] language, but first we explain how you can write
@tech{reader}-level extensions to Racket.
