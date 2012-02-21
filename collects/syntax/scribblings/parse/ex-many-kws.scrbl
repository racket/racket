#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title{More Keyword Arguments}

This section shows how to express the syntax of @racket[struct]'s
optional keyword arguments using @racket[syntax-parse] patterns.

The part of @racket[struct]'s syntax that is difficult to specify is
the sequence of struct options. Let's get the easy part out of the way
first.

@myinteraction[
(define-splicing-syntax-class maybe-super
  (pattern (~seq super:id))
  (pattern (~seq)))

(define-syntax-class field-option
  (pattern #:mutable)
  (pattern #:auto))

(define-syntax-class field
  (pattern field:id
           #:with (option ...) '())
  (pattern [field:id option:field-option ...]))
]

Given those auxiliary syntax classes, here is a first approximation of
the main pattern, including the struct options:
@racketblock[
(struct name:id super:maybe-super (field:field ...)
  (~or (~seq #:mutable)
       (~seq #:super super-expr:expr)
       (~seq #:inspector inspector:expr)
       (~seq #:auto-value auto:expr)
       (~seq #:guard guard:expr)
       (~seq #:property prop:expr prop-val:expr)
       (~seq #:transparent)
       (~seq #:prefab)
       (~seq #:constructor-name constructor-name:id)
       (~seq #:extra-constructor-name extra-constructor-name:id)
       (~seq #:omit-define-syntaxes)
       (~seq #:omit-define-values))
  ...)
]
The fact that @racket[expr] does not match keywords helps in the case
where the programmer omits a keyword's argument; instead of accepting
the next keyword as the argument expression, @racket[syntax-parse]
reports that an expression was expected.

There are two main problems with the pattern above:
@itemize[
@item{There's no way to tell whether a zero-argument keyword like
@racket[#:mutable] was seen.}
@item{Some options, like @racket[#:mutable], should appear at most
once.}
]

The first problem can be remedied using @racket[~and] patterns to bind
a pattern variable to the keyword itself, as in this sub-pattern:
@racketblock[
(~seq (~and #:mutable mutable-kw))
]
The second problem can be solved using @emph{repetition constraints}:
@racketblock[
(struct name:id super:maybe-super (field:field ...)
  (~or (~optional (~seq (~and #:mutable) mutable-kw))
       (~optional (~seq #:super super-expr:expr))
       (~optional (~seq #:inspector inspector:expr))
       (~optional (~seq #:auto-value auto:expr))
       (~optional (~seq #:guard guard:expr))
       (~seq #:property prop:expr prop-val:expr)
       (~optional (~seq (~and #:transparent transparent-kw)))
       (~optional (~seq (~and #:prefab prefab-kw)))
       (~optional (~seq #:constructor-name constructor-name:id))
       (~optional
         (~seq #:extra-constructor-name extra-constructor-name:id))
       (~optional
         (~seq (~and #:omit-define-syntaxes omit-def-stxs-kw)))
       (~optional (~seq (~and #:omit-define-values omit-def-vals-kw))))
  ...)
]
The @racket[~optional] repetition constraint indicates that an
alternative can appear at most once. (There is a @racket[~once] form
that means it must appear exactly once.) In @racket[struct]'s keyword
options, only @racket[#:property] may occur any number of times.

There are still some problems, though. Without additional help,
@racket[~optional] does not report particularly good errors. We must
give it the language to use, just as we had to give descriptions to
sub-patterns via syntax classes. Also, some related options are
mutually exclusive, such as @racket[#:inspector],
@racket[#:transparent], and @racket[#:prefab].

@racketblock[
(struct name:id super:maybe-super (field:field ...)
  (~or (~optional
         (~or (~seq #:inspector inspector:expr)
              (~seq (~and #:transparent transparent-kw))
              (~seq (~and #:prefab prefab-kw)))
         #:name "#:inspector, #:transparent, or #:prefab option")
       (~optional (~seq (~and #:mutable) mutable-kw)
                  #:name "#:mutable option")
       (~optional (~seq #:super super-expr:expr)
                  #:name "#:super option")
       (~optional (~seq #:auto-value auto:expr)
                  #:name "#:auto-value option")
       (~optional (~seq #:guard guard:expr)
                  #:name "#:guard option")
       (~seq #:property prop:expr prop-val:expr)
       (~optional (~seq #:constructor-name constructor-name:id)
                  #:name "#:constructor-name option")
       (~optional
         (~seq #:extra-constructor-name extra-constructor-name:id)
         #:name "#:extra-constructor-name option")
       (~optional (~seq (~and #:omit-define-syntaxes omit-def-stxs-kw))
                  #:name "#:omit-define-syntaxes option")
       (~optional (~seq (~and #:omit-define-values omit-def-vals-kw))
                  #:name "#:omit-define-values option"))
  ...)
]
Here we have grouped the three incompatible options together under a
single @racket[~optional] constraint. That means that at most one of
any of those options is allowed. We have given names to the optional
clauses. See @racket[~optional] for other customization options.

Note that there are other constraints that we have not represented in
the pattern. For example, @racket[#:prefab] is also incompatible with
both @racket[#:guard] and @racket[#:property]. Repetition constraints
cannot express arbitrary incompatibility relations. The best way to
handle such constraints is with a side condition using
@racket[#:fail-when].
