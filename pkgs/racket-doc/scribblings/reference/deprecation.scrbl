#lang scribble/doc

@(require (for-label racket/deprecation
                     racket/deprecation/transformer)
          "mz.rkt")


@title{Deprecation}


@note-lib-only[racket/deprecation]


A @deftech{deprecated} function, macro, or other API element is one that's been formally declared
obsolete, typically with an intended replacement that users should migrate to. The
@racketmodname[racket/deprecation] library provides a standardized mechanism for declaring
deprecations in a machine-processable manner. These declarations can allow tools such as
@racketmodname[resyntax #:indirect] to automate migrating code away from deprecated APIs. Note that a
dependency on the @racketmodname[racket/deprecation] library does not imply a dependency on any such
tools.


@section{Deprecated Aliases}


@defform[(define-deprecated-alias alias-id target-id)]{

 Binds @racket[alias-id] as an alias of @racket[target-id] with the intent that users of
 @racket[alias-id] should prefer to use @racket[target-id] instead. The given @racket[alias-id] is
 bound as a @tech{deprecated alias transformer}, which is a kind of @tech{rename transformer}. The
 given @racket[target-id] may be bound to a function, macro, or any other kind of binding.

 Note that although @racket[alias-id] is an alias of @racket[target-id], it is @emph{not} considered
 the same binding as @racket[target-id] and is @emph{not} @racket[free-identifier=?]. This is because
 the alias binding must be inspectable at compile-time with @racket[deprecated-alias?] and
 @racket[deprecated-alias-target], and it must remain inspectable even if the alias is
 @racket[provide]d by a module. This requires a module providing the alias and the target to provide
 them as two distinct bindings: one which is bound to a @tech{deprecated alias transformer} and one
 which isn't.

 @(examples
   (require racket/deprecation)
   (define a 42)
   (define-deprecated-alias legacy-a a)
   legacy-a)}


@section{Deprecated Alias Transformers}
@defmodule[racket/deprecation/transformer]


The @racketmodname[racket/deprecation/transformer] module provides compile-time supporting code for
the @racketmodname[racket/deprecation] library, primarily for use in tools that wish to reflect on
deprecated code.

A @deftech{deprecated alias transformer} is a kind of @tech{rename transformer} which signals that the
transformer binding is a @tech{deprecated} alias of the target identifier. This signal is intended
for consumption in tools such as editors (which may wish to display a warning when deprecated aliases
are used) and automated refactoring systems (which may wish to replace deprecated aliases with their
target identifiers automatically).


@defproc[(deprecated-alias? [v any/c]) boolean?]{

 Returns true if @racket[v] is a @tech{deprecated alias transformer} and returns false otherwise.
 Implies @racket[rename-transformer?]. To determine if an identifier is @emph{bound} to a deprecated
 alias transformer, use @racket[syntax-local-value/immediate] and then use @racket[deprecated-alias?]
 on the transformer value.

@(examples
  #:escape UNSYNTAX
  (require (for-syntax racket/base
                       racket/deprecation/transformer)
           racket/deprecation
           syntax/parse/define)

  (define-syntax-parse-rule (is-deprecated? id:id)
    #:do [(define-values (transformer _)
            (syntax-local-value/immediate #'id (λ () (values #false #false))))]
    #:with result #`'#,(deprecated-alias? transformer)
    result)

  (define-deprecated-alias bad-list list)
  (is-deprecated? list)
  (is-deprecated? bad-list))}


@defproc[(deprecated-alias [target identifier?]) deprecated-alias?]{

 Constructs a @tech{deprecated alias transformer} which expands to @racket[target] when used. The
 returned alias is a @tech{rename transformer} and is thus suitable for use with
 @racket[define-syntax]. When expanding, the usage of @racket[target] is annotated with the
 @racket['not-free-identifier=?] syntax property to ensure that the alias and the target are treated
 as distinct bindings, even when @racket[provide]d by a module.

 This constructor is not intended for direct use by users who just want to declare a deprecated alias.
 Such users should prefer the @racket[define-deprecated-alias] form instead.}


@defproc[(deprecated-alias-target [alias deprecated-alias?]) identifier?]{

 Returns the target identifier that @racket[alias] expands to.}
