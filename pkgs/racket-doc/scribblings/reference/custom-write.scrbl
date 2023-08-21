#lang scribble/doc
@(require "mz.rkt" (for-label racket/struct))

@title{Printer Extension}

@defthing[gen:custom-write any/c]{

A @tech{generic interface} (see @secref["struct-generics"]) that
supplies a method, @racket[write-proc] used by the default printer to
@racket[display], @racket[write], or @racket[print] instances of the
structure type.

A @racket[write-proc] method takes three
arguments: the structure to be printed, the target port, and an
argument that is @racket[#t] for @racket[write] mode, @racket[#f] for
@racket[display] mode, or @racket[0] or @racket[1] indicating
the current @tech{quoting depth} for @racket[print] mode.  The
procedure should print the value to the given port using
@racket[write], @racket[display], @racket[print], @racket[fprintf],
@racket[write-special], etc.

The @tech{port write handler}, @tech{port display handler},
and @tech{print handler} are specially
configured for a port given to a custom-write procedure. Printing to
the port through @racket[display], @racket[write], or @racket[print]
prints a value recursively with sharing annotations. To avoid a
recursive print (i.e., to print without regard to sharing with a value
currently being printed), print instead to a string or pipe and
transfer the result to the target port using @racket[write-string] or
@racket[write-special]. To print recursively to a port other than
the one given to the custom-write procedure, copy the given port's
write handler, display handler, and print handler to the other port.

The port given to @racket[write-proc] is not necessarily the actual
target port. In particular, to detect cycles, sharing, and quoting
modes (in the case of @racket[print]), the printer invokes a
custom-write procedure with a port that records information about
recursive prints, and does not retain any other output. This
information-gathering phase needs the same objects (in the
@racket[eq?] sense) to be printed as later, so that the recorded
information can be correlated with printed values.

Recursive print operations may trigger an escape from a call to
@racket[write-proc]. For example, printing may escape during
pretty-printing where a tentative print attempt overflows the line, or
it may escape while printing error output that is constrained to a
limited width.

The following example definition of a @racket[tuple] type includes
a @racket[write-proc] procedure that prints the tuple's list content using
angle brackets in @racket[write] and @racket[print] mode and no brackets in
@racket[display] mode. Elements of the tuple are printed recursively,
so that graph and cycle structure can be represented.

@examples[
(eval:no-prompt
 (define (tuple-print tuple port mode)
   (when mode (write-string "<" port))
   (let ([l (tuple-ref tuple)]
         [recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))])])
     (unless (zero? (vector-length l))
       (recur (vector-ref l 0) port)
       (for-each (lambda (e)
                   (write-string ", " port)
                   (recur e port))
                 (cdr (vector->list l)))))
   (when mode (write-string ">" port))))

(eval:no-prompt
 (struct tuple (ref)
         #:methods gen:custom-write
         [(define write-proc tuple-print)]))

(display (tuple #(1 2 "a")))

(print (tuple #(1 2 "a")))

(let ([t (tuple (vector 1 2 "a"))])
  (vector-set! (tuple-ref t) 0 t)
  (write t))
]

The @racket[make-constructor-style-printer] function can help in the
implementation of a @racket[write-proc], as in this example:

@examples[
 (eval:no-prompt (require racket/struct))
 (eval:no-prompt
  (struct point (x y)
    #:methods gen:custom-write
    [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'point)
        (lambda (obj) (list (point-x obj) (point-y obj)))))]))

  (print (point 1 2))

  (write (point 1 2))]

@history[#:changed "8.7.0.5"
         @elem{Added a check so that omitting
               @racket[_write-proc] is now a syntax error.}]
}

@defthing[prop:custom-write struct-type-property?]{
A @tech{structure type property} (see @secref["structprops"])
that supplies a procedure that corresponds to @racket[gen:custom-write]'s
@racket[write-proc]. Using the @racket[prop:custom-write] property is
discouraged; use the @racket[gen:custom-write] @tech{generic interface}
instead.
}

@defproc[(custom-write? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the @racket[prop:custom-write]
property, @racket[#f] otherwise.}


@defproc[(custom-write-accessor [v custom-write?])
         (custom-write? output-port? (or/c #t #f 0 1) . -> . any)]{

Returns the custom-write procedure associated with @racket[v].}

@deftogether[(
@defthing[prop:custom-print-quotable struct-type-property?]
@defthing[custom-print-quotable? struct-type-property?]
@defthing[custom-print-quotable-accessor struct-type-property?]
)]{

A property and associated predicate and accessor. The property value
is one of @racket['self], @racket['never], @racket['maybe], or
@racket['always]. When a structure has this property in addition to a
@racket[prop:custom-write] property value, then the property value
affects printing in @racket[print] mode; see @secref["printing"]. When
a value does not have the @racket[prop:custom-print-quotable], it is
equivalent to having the @racket['self] property value, which is
suitable both for self-quoting forms and printed forms that are
unreadable.}
