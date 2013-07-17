#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label unstable/class-iop
                     racket/class
                     racket/contract
                     racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/class unstable/class-iop (for-syntax racket/base)))

@title[#:tag "class-iop"]{Interface-Oriented Programming for Classes}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/class-iop]

@defform[(define-interface name-id (super-ifc-id ...) (method-id ...))]{

Defines @racket[name-id] as a static interface extending the
interfaces named by the @racket[super-ifc-id]s and containing the
methods specified by the @racket[method-id]s.

A static interface name is used by the checked method call variants
(@racket[send/i], @racket[send*/i], and @racket[send/apply/i]). When
used as an expression, a static interface name evaluates to an
interface value.

@examples[#:eval the-eval
(define-interface stack<%> () (empty? push pop))
stack<%>
(define stack%
  (class* object% (stack<%>)
    (define items null)
    (define/public (empty?) (null? items))
    (define/public (push x) (set! items (cons x items)))
    (define/public (pop) (begin (car items) (set! items (cdr items))))
    (super-new)))
]
}

@defform[(define-interface/dynamic name-id ifc-expr (method-id ...))]{

Defines @racket[name-id] as a static interface with dynamic
counterpart @racket[ifc-expr], which must evaluate to an interface
value. The static interface contains the methods named by the
@racket[method-id]s. A run-time error is raised if any
@racket[method-id] is not a member of the dynamic interface
@racket[ifc-expr].

Use @racket[define-interface/dynamic] to wrap interfaces from other
sources.

@examples[#:eval the-eval
(define-interface/dynamic object<%> (class->interface object%) ())
object<%>
]
}

@defform[(send/i obj-exp static-ifc-id method-id arg-expr ...)]{

Checked variant of @racket[send]. 

The argument @racket[static-ifc-id] must be defined as a static
interface. The method @racket[method-id] must be a member of the
static interface @racket[static-ifc-id]; otherwise a compile-time
error is raised.

The value of @racket[obj-expr] must be an instance of the interface
@racket[static-ifc-id]; otherwise, a run-time error is raised.

@examples[#:eval the-eval
(define s (new stack%))
(send/i s stack<%> push 1)
(send/i s stack<%> popp)
(send/i (new object%) stack<%> push 2)
]
}

@defform[(send*/i obj-expr static-ifc-id (method-id arg-expr ...) ...)]{

Checked variant of @racket[send*].

@examples[#:eval the-eval
(send*/i s stack<%>
  (push 2)
  (pop))
]
}

@defform[(send/apply/i obj-expr static-ifc-id method-id arg-expr ... list-arg-expr)]{

Checked variant of @racket[send/apply].

@examples[#:eval the-eval
(send/apply/i s stack<%> push (list 5))
]
}

@defform[(define/i id static-ifc-id expr)]{

Checks that @racket[expr] evaluates to an instance of
@racket[static-ifc-id] before binding it to @racket[id]. If
@racket[id] is subsequently changed (with @racket[set!]), the check is
performed again.

No dynamic object check is performed when calling a method (using
@racket[send/i], etc) on a name defined via @racket[define/i].

}

@deftogether[[
@defform[(init/i (id static-ifc-id maybe-default-expr) ...)]
@defform[(init-field/i (id static-ifc-id maybe-default-expr) ...)]
@defform/subs[(init-private/i (id static-ifc-id maybe-default-expr) ...)
              ([maybe-default-expr (code:blank)
                                   default-expr])]]]{

Checked versions of @racket[init] and @racket[init-field]. The value
attached to each @racket[id] is checked against the given interface.

No dynamic object check is performed when calling a method (using
@racket[send/i], etc) on a name bound via one of these forms.  Note
that in the case of @racket[init-field/i] this check omission is
unsound in the presence of mutation from outside the class. This
should be fixed.

}

@defform[(define-interface-expander id transformer-expr)]{

Defines @racket[id] as a macro that can be used within
@racket[define-interface] forms.

@examples[#:eval the-eval
(define-interface-expander stack-methods
  (lambda (stx) #'[empty? push pop]))
(define-interface stack<%> ()
  ((stack-methods)))
(interface->method-names stack<%>)
]
}

@close-eval[the-eval]
