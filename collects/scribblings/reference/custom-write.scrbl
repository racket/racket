#lang scribble/doc
@(require "mz.ss")

@title{Printer Extension}

@defthing[prop:custom-write struct-type-property?]{

Associates a procedure to a structure type to used by the default
printer to @scheme[display], @scheme[write], or @scheme[print]
instances of the structure type.

@moreref["structprops"]{structure type properties}

The procedure for a @scheme[prop:custom-write] value takes three
arguments: the structure to be printed, the target port, and an
argument that is @scheme[#t] for @scheme[write] mode, @scheme[#f] for
@scheme[display] mode, or an exact non-negative integer representing
the current @scheme[quasiquote] depth for @scheme[print] mode.  The
procedure should print the value to the given port using
@scheme[write], @scheme[display], @scheme[print], @scheme[fprintf],
@scheme[write-special], etc.

The @tech{port write handler}, @tech{port display handler}, 
and @tech{print handler} are specially
configured for a port given to a custom-write procedure. Printing to
the port through @scheme[display], @scheme[write], or @scheme[print]
prints a value recursively with sharing annotations. To avoid a
recursive print (i.e., to print without regard to sharing with a value
currently being printed), print instead to a string or pipe and
transfer the result to the target port using @scheme[write-string] and
@scheme[write-special]. To recursively print but to a port other than
the one given to the custom-write procedure, copy the given port's
write handler, display handler, and print handler to the other port.

The port given to a custom-write handler is not necessarily the actual
target port. In particular, to detect cycles and sharing, the printer
invokes a custom-write procedure with a port that records recursive
prints, and does not retain any other output.

Recursive print operations may trigger an escape from the call to the
custom-write procedure (e.g., for pretty-printing where a tentative
print attempt overflows the line, or for printing error output of a
limited width).

The following example definition of a @scheme[tuple] type includes
custom-write procedures that print the tuple's list content using
angle brackets in @scheme[write] and @scheme[print] mode and no brackets in
@scheme[display] mode. Elements of the tuple are printed recursively,
so that graph and cycle structure can be represented.

@defexamples[
(define (tuple-print tuple port mode)
  (when mode (write-string "<" port))
  (let ([l (tuple-ref tuple 0)]
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
  (when mode (write-string ">" port)))

(define-values (s:tuple make-tuple tuple? tuple-ref tuple-set!)
  (make-struct-type 'tuple #f 1 0 #f
                    (list (cons prop:custom-write tuple-print))))

(display (make-tuple #(1 2 "a")))

(print (make-tuple #(1 2 "a")))

(let ([t (make-tuple (vector 1 2 "a"))])
  (vector-set! (tuple-ref t 0) 0 t)
  (write t))
]
}

@defproc[(custom-write? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] has the @scheme[prop:custom-write]
property, @scheme[#f] otherwise.}


@defproc[(custom-write-accessor [v custom-write?])
         (custom-write? output-port? boolean?. -> . any)]{

Returns the custom-write procedure associated with @scheme[v].}
