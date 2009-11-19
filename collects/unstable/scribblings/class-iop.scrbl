#lang scribble/manual
@(require scribble/eval
          "utils.ss"
          (for-label unstable/class-iop
                     scheme/class
                     scheme/contract
                     scheme/base))

@title[#:tag "class-iop"]{Interface-Oriented Programming for Classes}

@(define the-eval (make-base-eval))
@(the-eval '(require scheme/class unstable/class-iop))

@defmodule[unstable/class-iop]

@unstable[@author+email["Ryan Culpepper" "ryanc@plt-scheme.org"]]

@defform[(define-interface name-id (super-ifc-id ...) (method-id ...))]{

Defines @scheme[name-id] as a static interface extending the
interfaces named by the @scheme[super-ifc-id]s and containing the
methods specified by the @scheme[method-id]s.

A static interface name is used by the checked method call variants
(@scheme[send/i], @scheme[send*/i], and @scheme[send/apply/i]). When
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

Defines @scheme[name-id] as a static interface with dynamic
counterpart @scheme[ifc-expr], which must evaluate to an interface
value. The static interface contains the methods named by the
@scheme[method-id]s. A run-time error is raised if any
@scheme[method-id] is not a member of the dynamic interface
@scheme[ifc-expr].

Use @scheme[define-interface/dynamic] to wrap interfaces from other
sources.

@examples[#:eval the-eval
(define-interface/dynamic object<%> (class->interface object%) ())
object<%>
]
}

@defform[(send/i obj-exp static-ifc-id method-id arg-expr ...)]{

Checked variant of @scheme[send]. 

The argument @scheme[static-ifc-id] must be defined as a static
interface. The method @scheme[method-id] must be a member of the
static interface @scheme[static-ifc-id]; otherwise a compile-time
error is raised.

The value of @scheme[obj-expr] must be an instance of the interface
@scheme[static-ifc-id]; otherwise, a run-time error is raised.

@examples[#:eval the-eval
(define s (new stack%))
(send/i s stack<%> push 1)
(send/i s stack<%> popp)
(send/i (new object%) stack<%> push 2)
]
}

@defform[(send*/i obj-expr static-ifc-id (method-id arg-expr ...) ...)]{

Checked variant of @scheme[send*].

@examples[#:eval the-eval
(send*/i s stack<%>
  (push 2)
  (pop))
]
}

@defform[(send/apply/i obj-expr static-ifc-id method-id arg-expr ... list-arg-expr)]{

Checked variant of @scheme[send/apply].

@examples[#:eval the-eval
(send/apply/i s stack<%> push (list 5))
]
}

@defform[(define/i id static-ifc-id expr)]{

Checks that @scheme[expr] evaluates to an instance of
@scheme[static-ifc-id] before binding it to @scheme[id]. If
@scheme[id] is subsequently changed (with @scheme[set!]), the check is
performed again.

No dynamic object check is performed when calling a method (using
@scheme[send/i], etc) on a name defined via @scheme[define/i].

}

@deftogether[[
@defform[(init/i (id static-ifc-id maybe-default-expr) ...)]
@defform[(init-field/i (id static-ifc-id maybe-default-expr) ...)]
@defform/subs[(init-private/i (id static-ifc-id maybe-default-expr) ...)
              ([maybe-default-expr (code:blank)
                                   default-expr])]]]{

Checked versions of @scheme[init] and @scheme[init-field]. The value
attached to each @scheme[id] is checked against the given interface.

No dynamic object check is performed when calling a method (using
@scheme[send/i], etc) on a name bound via one of these forms.  Note
that in the case of @scheme[init-field/i] this check omission is
unsound in the presence of mutation from outside the class. This
should be fixed.

}
