#lang scribble/doc
@(require "utils.rkt" (for-label ffi/unsafe/vm
                                 racket/linklet))

@title[#:tag "vm"]{Virtual Machine Primitives}

@defmodule[ffi/unsafe/vm]{The
@racketmodname[ffi/unsafe/vm] library provides access to functionality
in the underlying virtual machine that is used to implement Racket.}

@history[#:added "7.6.0.7"]

@defproc[(vm-primitive [name symbol?]) any/c]{

Accesses a primitive values at the level of the running Racket virtual
machine, or returns @racket[#f] if @racket[name] is not the name of a
primitive.

Virtual-machine primitives are the ones that can be referenced in a
@tech[#:doc reference.scrbl]{linklet} body. The specific set of
primitives depends on the virtual machine. Many ``primitives'' at the
@racketmodname[racket/base] level or even the @racket['#%kernel] level
are not primitives at the
virtual-machine level. For example, if @racket['eval] is available as
a primitive, it is not the @racket[eval] from
@racketmodname[racket/base].

In general, primitives are unsafe and can only be used with enough
knowledge about Racket's implementation. Here are some tips for
currently available virtual machines:

@itemlist[

 @item{@racket[(system-type 'vm)] is @racket['racket] --- The
       primitives in this virtual machine are mostly the same as the
       ones available from libraries like @racketmodname[racket/base]
       and @racketmodname[racket/unsafe/ops]. As a result, accessing
       virtual machine primitives with @racket[vm-primitive] is rarely
       useful.}

 @item{@racket[(system-type 'vm)] is @racket['chez-scheme] --- The
       primitives in this virtual machine are Chez Scheme primitives,
       except as replaced by a Racket compatibility layer. The
       @racket['eval] primitive is Chez Scheme's @racketidfont{eval}.

       Beware of directly calling a Chez Scheme primitive that uses
       Chez Scheme parameters or @racketidfont{dynamic-wind}
       internally. Note that @racketidfont{eval}, in particular, is
       such a primitive. The problem is that Chez Scheme's
       @racketidfont{dynamic-wind} does not automatically cooperate
       with Racket's continuations or threads. To call such
       primitives, use the @racketidfont{call-with-system-wind}
       primitive, which takes a procedure of no arguments to run in a
       context that bridges Chez Scheme's @racketidfont{dynamic-wind}
       and Racket continuations and threads. For example,

       @racketblock[
         (define primitive-eval (vm-primitive 'eval))
         (define call-with-system-wind (vm-primitive 'call-with-system-wind))
         (define (vm-eval s)
           (call-with-system-wind
            (lambda ()
             (primitive-eval s))))
       ]

       is how @racket[vm-eval] is implemented on Chez Scheme.

       Symbols, numbers, booleans, pairs, vectors, boxes, strings,
       byte strings (i.e., bytevectors), and structures (i.e.,
       records) are interchangeable between Racket and Chez Scheme. A
       Chez Scheme procedure is a Racket procedure, but not all Racket
       procedures are Chez Scheme procedures. To call a Racket
       procedure from Chez Scheme, use the @racketidfont{#%app} form
       that is defined in the Chez Scheme environment when it hosts
       Racket.

       Note that you can access Chez Scheme primitives, including ones
       that are shadowed by Racket's primitives, through the Chez
       Scheme @racketidfont{$primitive} form. For example,
       @racket[(vm-eval '($primitive call-with-current-continuation))]
       accesses the Chez Scheme
       @racketidfont{call-with-current-continuation} primitive instead
       of Racket's replacement (where the replacement works with
       Racket continuations and threads).}

]}

@defproc[(vm-eval [s-expr any/c]) any/c]{

Evaluates @racket[s-expr] using the most primitive available evaluator:

@itemlist[

 @item{@racket[(system-type 'vm)] is @racket['racket] --- Uses
       @racket[compile-linklet] and @racket[instantiate-linklet].}

 @item{@racket[(system-type 'vm)] is @racket['chez-scheme] --- Uses
       Chez Scheme's @racketidfont{eval}.}

]

See @racket[vm-primitive] for some information about how
virtual-machine primitives interact with Racket.}
