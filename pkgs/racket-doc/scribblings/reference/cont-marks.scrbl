#lang scribble/doc
@(require scribble/struct scribble/racket "mz.rkt")

@(define (cont n)
   (make-element variable-color
                 (list "C" (make-element 'subscript (list (format "~a" n))))))

@title[#:tag "contmarks"]{Continuation Marks}

See @secref["mark-model"] and @secref["prompt-model"] for
general information about continuation marks.

The list of continuation marks for a key @racket[_k] and a continuation
@racket[_C] that extends @cont[0] is defined as follows:

@itemize[

 @item{If @racket[_C] is an empty continuation, then the mark list is
 @racket[null].}

 @item{If @racket[_C]'s first frame contains a mark @racket[_m] for @racket[_k],
 then the mark list for @racket[_C] is @racket[(cons _m _lst)],
 where @racket[_lst] is the mark list for @racket[_k] in @cont[0].}

 @item{If @racket[_C]'s first frame does not contain a mark keyed by
 @racket[_k], then the mark list for @racket[_C] is the mark list for
 @cont[0].}

]

The @racket[with-continuation-mark] form installs a mark on the first
frame of the current continuation (see @secref["wcm"]).  Procedures
such as @racket[current-continuation-marks] allow inspection of marks.

Whenever Racket creates an exception record for a primitive exception,
it fills the @racket[continuation-marks] field with the value of
@racket[(current-continuation-marks)], thus providing a snapshot of
the continuation marks at the time of the exception.

When a continuation procedure returned by
@racket[call-with-current-continuation] or
@racket[call-with-composable-continuation] is invoked, it restores the
captured continuation, and also restores the marks in the
continuation's frames to the marks that were present when
@racket[call-with-current-continuation] or
@racket[call-with-composable-continuation] was invoked.

@defproc[(continuation-marks [cont (or/c continuation? thread? #f)]
                             [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)])
         continuation-mark-set?]{

Returns an opaque value containing the set of continuation marks for
all keys in the continuation @racket[cont] (or the current
continuation of @racket[cont] if it is a thread) up to the prompt
tagged by @racket[prompt-tag]. If @racket[cont] is @racket[#f], the
resulting set of continuation marks is empty. If @racket[cont] is an escape
continuation (see @secref["prompt-model"]), then the current
continuation must extend @racket[cont], or the
@exnraise[exn:fail:contract]. If @racket[cont] was not captured with
respect to @racket[prompt-tag] and does not include a prompt for
@racket[prompt-tag], the @exnraise[exn:fail:contract]. If
@racket[cont] is a dead thread, the result is an empty set of
continuation marks.}

@defproc[(current-continuation-marks [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)])
         continuation-mark-set?]{

Returns an opaque value containing the set of continuation marks for
all keys in the current continuation up to @racket[prompt-tag]. In
other words, it produces the same value as

@racketblock[
(call-with-current-continuation
  (lambda (k) 
    (continuation-marks k prompt-tag))
  prompt-tag)
]}

@defproc[(continuation-mark-set->list
          [mark-set continuation-mark-set?]
          [key-v any/c]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)])
         list?]{
Returns a newly-created list containing the marks for @racket[key-v]
in @racket[mark-set], which is a set of marks returned by
@racket[current-continuation-marks]. The result list is truncated at
the first point, if any, where continuation frames were originally
separated by a prompt tagged with @racket[prompt-tag].}

@defproc*[([(make-continuation-mark-key) continuation-mark-key?]
           [(make-continuation-mark-key [sym symbol?]) continuation-mark-key?])]{
Creates a continuation mark key that is not @racket[equal?] to the result
of any other value (including prior and future results from
@racket[make-continuation-mark-key]). The continuation mark key can be used
as the key argument for @racket[with-continuation-mark] or accessor procedures
like @racket[continuation-mark-set-first]. The mark key can be chaperoned
or impersonated, unlike other values that are used as the mark key.

The optional @racket[sym] argument, if provided, is used when printing
the continuation mark.
}

@defproc[(continuation-mark-set->list*
          [mark-set continuation-mark-set?]
          [key-list (listof any/c)]
          [none-v any/c #f]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)])
         (listof vector?)]{
Returns a newly-created list containing vectors of marks in
@racket[mark-set] for the keys in @racket[key-list], up to
@racket[prompt-tag]. The length of each vector in the result list is
the same as the length of @racket[key-list], and a value in a
particular vector position is the value for the corresponding key in
@racket[key-list]. Values for multiple keys appear in a single vector
only when the marks are for the same continuation frame in
@racket[mark-set]. The @racket[none-v] argument is used for vector
elements to indicate the lack of a value.}

@defproc[(continuation-mark-set-first 
          [mark-set (or/c continuation-mark-set? #f)]
          [key-v any/c]
          [none-v any/c #f]
          [prompt-tag continuation-prompt-tag? (default-continuation-prompt-tag)])
         any]{
Returns the first element of the list that would be returned by
@racket[(continuation-mark-set->list (or mark-set
(current-continuation-marks prompt-tag)) key-v prompt-tag)], or
@racket[none-v] if the result would be the empty list. Typically, this
result can be computed more quickly using
@racket[continuation-mark-set-first] than using
@racket[continuation-mark-set->list].}

@defproc[(call-with-immediate-continuation-mark
          [key-v any/c]
          [proc (any/c . -> . any)]
          [default-v any/c #f])
         any]{

Calls @racket[proc] with the value associated with @racket[key-v] in
the first frame of the current continuation (i.e., a value that would
be replaced if the call to
@racket[call-with-immediate-continuation-mark] were replaced with a
@racket[with-continuation-mark] form using @racket[key-v] as the key
expression). If no such value exists in the first frame,
@racket[default-v] is passed to @racket[proc]. The @racket[proc] is
called in tail position with respect to the
@racket[call-with-immediate-continuation-mark] call.

This function could be implemented with a combination of
@racket[with-continuation-mark], @racket[current-continuation-marks],
and @racket[continuation-mark-set->list], but
@racket[call-with-immediate-continuation-mark] is implemented more
efficiently; it inspects only the first frame of the current
continuation.}

@defproc[(continuation-mark-key? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a mark key created by
@racket[make-continuation-mark-key], @racket[#f] otherwise.}

@defproc[(continuation-mark-set? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a mark set created by
@racket[continuation-marks] or @racket[current-continuation-marks],
@racket[#f] otherwise.}

@defproc[(continuation-mark-set->context [mark-set continuation-mark-set?])
          list?]{

Returns a list representing an approximate ``@index["stack
dump"]{@as-index{stack trace}}'' for @racket[mark-set]'s
continuation. The list contains pairs, where the @racket[car] of each
pair contains either @racket[#f] or a symbol for a procedure name, and
the @racket[cdr] of each pair contains either @racket[#f] or a
@racket[srcloc] value for the procedure's source location (see
@secref["linecol"]); the @racket[car] and @racket[cdr] are never both
@racket[#f].

Conceptually, the stack-trace list is the result of
@racket[continuation-mark-set->list] with @racket[mark-set] and
Racket's private key for procedure-call marks. The implementation may
be different, however, and the results may merely approximate the
correct answer. Thus, while the result may contain useful hints to
humans about the context of an expression, it is not reliable enough
for programmatic use.

A stack trace is extracted from an exception and displayed by the
default error display handler (see
@racket[error-display-handler]) for exceptions other than
@racket[exn:fail:user] (see @racket[raise-user-error] in
@secref["errorproc"]).}

@examples[
(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(with-continuation-mark 'key 'mark
  (extract-current-continuation-marks 'key))

(with-continuation-mark 'key1 'mark1
  (with-continuation-mark 'key2 'mark2
    (list
     (extract-current-continuation-marks 'key1)
     (extract-current-continuation-marks 'key2))))

(with-continuation-mark 'key 'mark1 
  (with-continuation-mark 'key 'mark2 (code:comment @#,t{replaces previous mark})
    (extract-current-continuation-marks 'key)))

(with-continuation-mark 'key 'mark1 
  (list (code:comment @#,t{continuation extended to evaluate the argument})
   (with-continuation-mark 'key 'mark2 
      (extract-current-continuation-marks 'key))))

(let loop ([n 1000])
  (if (zero? n) 
      (extract-current-continuation-marks 'key) 
      (with-continuation-mark 'key n
        (loop (sub1 n)))))
]
