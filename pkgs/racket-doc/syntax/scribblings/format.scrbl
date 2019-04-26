#lang scribble/manual

@(require scribble/example
          "common.rkt"
          (for-label racket/function
                     racket/format
                     syntax/format
                     syntax/srcloc))

@(define make-format-eval (make-eval-factory '[syntax/format]))

@title[#:tag "format"]{Creating Formatted Identifiers}
@defmodule[syntax/format]

@defproc[(~id [v (or/c identifier? string? symbol? keyword? char? number?)] ...
              [#:context context (or/c syntax? #f) #f]
              [#:source source source-location? context]
              [#:props props (or/c syntax? #f 'infer) 'infer]
              [#:track track (or/c #t (-> identifier? identifier?) #f) #t]
              [#:binder? binder? any/c #t])
         identifier?]{

Builds an @tech[#:doc refman]{identifier} with a name constructed from the @racket[v]s, using
@racket[context] for the @tech[#:doc refman]{lexical information} and @racket[source] for the source
location. Each @racket[v] is converted to a symbol made up of the same characters that would be
produced by @racket[~a], except for @tech[#:doc refman]{identifiers}, which use their symbolic name,
and @tech[#:doc refman]{keywords}, which are converted using @racket[keyword->string] (i.e. the
preceeding @litchar{#:} characters are not included).

@(examples
  #:eval (make-format-eval) #:once
  (~id #'hyphenated "-" #'id)
  (~id #'made #\- 'from #\- '#:many #\- "pieces"))

If @racket[props] is a @tech[#:doc refman]{syntax object} or @racket[#f], then
@tech[#:doc refman #:key "syntax property"]{syntax properties} from @racket[props] are copied to the
new identifier in the same way as for @racket[datum->syntax]. If @racket[props] is @racket['infer],
then the properties of the new identifier are copied from @racket[context] if @racket[track] is
@racket[#f]; if @racket[track] is not @racket[#f], then no properties are copied.

If @racket[track] is not @racket[#f], then a @racket['sub-range-binders] @tech[#:doc refman]{syntax
property} is added to the new identifier. The property value is a list containing an element for each
@tech[#:doc refman]{identifier} @racket[_v-id] provided for @racket[v], and each element is an
immutable @tech[#:doc refman]{vector} with the shape

@(racketblock
  (vector-immutable _result-id _v-id-position _v-id-length 0.5 0.5
                    _v-id 0 _v-id-length 0.5 0.5))

where @racket[_result-id] is the new identifier, @racket[_v-id-position] is the position that
@racket[_v-id]'s name appears in @racket[_result-id], and @racket[_v-id-length] is the length in
characters of @racket[_v-id]'s symbolic name. This protocol helps Check Syntax understand that uses of
the identifier should be treated as uses of the @racket[_v-id]s; see
@seclink["Syntax_Properties_that_Check_Syntax_Looks_For" #:doc '(lib "scribblings/tools/tools.scrbl")
#:indirect? #t]{Syntax Properties that Check Syntax Looks For} for more details.

@(examples
  #:eval (make-format-eval) #:once
  (syntax-property (~id #'foo "-" #'bar) 'sub-range-binders))

If @racket[track] is a procedure, then it is applied to @racket[_result-id] and to each @racket[_v-id]
inside the @racket['sub-range-binders] property. If @racket[track] is @racket[#t], then
@racket[syntax-local-introduce] is used if @racket[(syntax-transforming?)] is @racket[#t], otherwise
@racket[identity] is used. (This default corresponds to the correct behavior when each @racket[_v-id]
comes from the input to a @tech[#:doc refman]{syntax transformer}.)

If @racket[binder?] is @racket[#f], then the new identifier is treated as a use rather than a binder,
which swaps the order of @racket[_result-id] and @racket[_v-id] in the vectors attached to the
@racket['sub-range-binders] property. If @racket[track] is @racket[#f], then the value of
@racket[binder?] has no effect.

@(examples
  #:eval (make-format-eval) #:once
  (syntax-property (~id #'foo "-" #'bar #:binder? #f) 'sub-range-binders))

@history[#:added "7.3.0.1"]}

@defproc[(~id/1 [v (or/c identifier? string? symbol? keyword? char? number?)] ...
                [#:context context (or/c syntax? #f 'infer) 'infer]
                [#:source source (or/c source-location? 'infer) 'infer]
                [#:props props (or/c syntax? #f 'infer) 'infer]
                [#:track track (or/c #t (-> identifier? identifier?) #f) #t]
                [#:binder? binder? any/c #t])
         identifier?]{

Like @racket[~id], but exactly one @racket[v] must be an @tech[#:doc refman]{identifier}, which is
automatically used in place of @racket[context] and/or @racket[source] if their value is
@racket['infer]. All other arguments are handled the same way as for @racket[~id].

@(examples
  #:eval (make-format-eval) #:once
  (~id/1 "prefixed-" #'and "-suffixed"))

@history[#:added "7.3.0.1"]}

@defproc[(~symbol [v (or/c identifier? string? symbol? keyword? char? number?)] ...) symbol?]{

Like @racket[~id], but produces a @tech[#:doc refman]{symbol} instead of an
@tech[#:doc refman]{identifier}.

@(examples
  #:eval (make-format-eval) #:once
  (eval:check (~symbol #'hyphenated "-" #'symbol) 'hyphenated-symbol)
  (eval:check (~symbol #'made #\- 'from #\- '#:many #\- "pieces") 'made-from-many-pieces))
                                                                                              
@history[#:added "7.3.0.1"]}
