#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          (for-label scheme/base
                     scheme/contract
                     stxclass 
                     stxclass/util))

@(define ellipses @scheme[...])
@(define (TODO . pre-flow)
   (make-splice
    (cons (bold "TODO: ")
          (decode-content pre-flow))))

@title{Utilities}

The @schememodname[stxclass] collection includes several utility
modules. They are documented individually below.

As a shortcut, the @schememodname[stxclass/util] module provides all
of the contents of the separate utility modules:

@defmodule[stxclass/util]

The contents of the utility modules are not provided by the main
@schememodname[stxclass] module.

@section{Error reporting}

@defmodule[stxclass/util/error]

The @schememodname[scheme/base] and @schememodname[scheme] languages
provide the @scheme[raise-syntax-error] procedure for reporting syntax
errors. Using @scheme[raise-syntax-error] effectively requires passing
around either a symbol indicating the special form that signals the
error or else a ``contextual'' syntax object from which the special
form's name can be extracted. This library helps manage the contextual
syntax for reporting errors.

@defparam[current-syntax-context stx (or/c syntax? false/c)]{

The current contextual syntax object, defaulting to @scheme[#f].  It
determines the special form name that prefixes syntax errors created
by @scheme[wrong-syntax], as follows:

If it is a syntax object with a @scheme['report-error-as] syntax
property whose value is a symbol, then that symbol is used as the
special form name. Otherwise, the same rules apply as in
@scheme[raise-syntax-error].

}

@defproc[(wrong-syntax [stx syntax?] [format-string string?] [v any/c] ...)
         any]{

Raises a syntax error using the result of
@scheme[(current-syntax-context)] as the ``major'' syntax object and
the provided @scheme[stx] as the specific syntax object. (The latter,
@scheme[stx], is usually the one highlighted by DrScheme.) The error
message is constructed using the format string and arguments, and it
is prefixed with the special form name as described under
@scheme[current-syntax-context].

}

A macro using this system might set the syntax context at the very
beginning of its transformation as follows:
@SCHEMEBLOCK[
(define-syntax (my-macro stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      ___)))
]
Then any calls to @scheme[wrong-syntax] during the macro's
transformation will refer to @scheme[my-macro] (more precisely, the name that
referred to @scheme[my-macro] where the macro was used, which may be
different due to renaming, prefixing, etc).

A macro that expands into a helper macro can insert its own name into
syntax errors raised by the helper macro by installing a
@scheme['report-error-as] syntax property on the helper macro
expression. For example:
@SCHEMEBLOCK[
(define-syntax (public-macro stx)
  (syntax-case stx ()
    [(public-macro stuff)
     (syntax-property
      (syntax/loc stx (my-macro stuff other-internal-stuff))
      'report-error-as
      (syntax-e #'public-macro))]))
]

@;{
@section[Expand]

@defmodule[stxclass/util/expand]

TODO
}

@section{Miscellaneous utilities}

@defmodule[stxclass/util/misc]

@defform[(define-pattern-variable id expr)]{

Evaluates @scheme[expr] and binds it to @scheme[id] as a pattern
variable, so @scheme[id] can be used in subsequent @scheme[syntax]
patterns.

}

@defform[(with-temporaries (temp-id ...) . body)]{

Evaluates @scheme[body] with each @scheme[temp-id] bound as a pattern
variable to a freshly generated identifier.

For example, the following are equivalent:
@SCHEMEBLOCK[
(with-temporaries (x) #'(lambda (x) x))
(with-syntax ([(x) (generate-temporaries '(x))])
  #'(lambda (x) x))
]

}

@defproc[(generate-temporary) identifier?]{

Generates one fresh identifier. Singular form of
@scheme[generate-temporaries].

}

@defproc[(generate-n-temporaries [n exact-nonnegative-integer?])
         (listof identifier?)]{

Generates a list of @scheme[n] fresh identifiers.

}

@defform[(with-catching-disappeared-uses body-expr)]{

Evaluates the @scheme[body-expr], catching identifiers looked up using
@scheme[syntax-local-value/catch]. Returns two values: the result of
@scheme[body-expr] and the list of caught identifiers.

}

@defform[(with-disappeared-uses stx-expr)]{

Evaluates the @scheme[stx-expr], catching identifiers looked up using
@scheme[syntax-local-value/catch]. Adds the caught identifiers to the
@scheme['disappeared-uses] syntax property of the resulting syntax
object.

}

@defproc[(syntax-local-value/catch [id identifier?] [predicate (-> any/c boolean?)])
         any/c]{

Looks up @scheme[id] in the syntactic environment (as
@scheme[syntax-local-value]). If the lookup succeeds and returns a
value satisfying the predicate, the value is returned and @scheme[id]
is recorded (``caught'') as a disappeared use. If the lookup fails or
if the value does not satisfy the predicate, @scheme[#f] is returned
and the identifier is not recorded as a disappeared use.

}


@defproc[(chunk-kw-seq [stx syntax?] 
                       [table
                        (listof (cons/c keyword? 
                                        (listof (-> syntax? any))))]
                       [context (or/c syntax? false/c) #f])
         (values (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))
                 syntax?)]{

Parses a syntax list into keyword-argument ``chunks'' and a syntax
list tail (the remainder of the syntax list). The syntax of the
keyword arguments is specified by @scheme[table], an association list
mapping keywords to lists of checker procedures. The length of the
checker list is the number of ``arguments'' expected to follow the
keyword, and each checker procedure is applied to the corresponding
argument. The result of the checker procedure is entered into the
chunk for that keyword sequence. The same keyword can appear multiple
times in the result list.

The @scheme[context] is used to report errors.

}

@defproc[(chunk-kw-seq/no-dups
                       [stx syntax?] 
                       [table
                        (listof (cons/c keyword? 
                                        (listof (-> syntax? any))))]
                       [context (or/c syntax? false/c) #f])
         (values (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))
                 syntax?)]{

Like @scheme[chunk-kw-seq] filtered by @scheme[reject-duplicate-chunks].

The @scheme[context] is used to report errors.

}

@defproc[(reject-duplicate-chunks
           [chunks (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))])
         void?]{

Raises a syntax error if it encounters the same keyword more than once
in the @scheme[chunks] list.

The @scheme[context] is used to report errors.

}


@section{Structs}

@defmodule[stxclass/util/struct]

@defform[(make struct-id v ...)]{

Constructs an instance of @scheme[struct-id], which must be defined
as a struct name. If @scheme[struct-id] has a different number of
fields than the number of @scheme[v] values provided, @scheme[make]
raises a compile-time error.

}
