#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "implementations" #:style 'quiet]{Implementations}

The definition of Racket aims for determinism and independence from
its implementation. Nevertheless, some details inevitably vary with
the implementation. Racket currently has two main implementations:

@itemlist[

 @item{The @deftech{CS} implementation is the default implementation
       as of Racket version 8.0. This variant is called ``CS'' because
       it uses Chez Scheme as its core compiler and runtime system.

       The CS implementation typically provides the best performance
       for Racket programs. Compiled Racket CS code in a
       @filepath{.zo} file normally contains machine code that is
       specific to an operating system and architecture.}

 @item{The @deftech{BC} implementation was the default implementation
       up until version 7.9. The ``BC'' label stands for ``before
       Chez'' or ``bytecode.''

       Compiled Racket BC code in a @filepath{.zo} file normally
       contains platform-independent bytecode that is further compiled
       to machine code ``just in time'' as the code is loaded.

       Racket BC has two variants: @deftech{3m} and @deftech{CGC}.
       The difference is the @tech{garbage collection} implementation,
       where 3m uses a garbage collector that moves objects in memory
       (an effect that is visible to foreign libraries, for example)
       and keeps precise track of allocated objects, while CGC uses a
       ``conservative'' collector that requires less cooperation from
       an embedding foreign environment. The 3m subvariant tends to
       perform much better than CGC, and it became the default
       variant in version 370 (which would be v3.7 in the current
       versioning convention).}

]

Most Racket programs run the same in all implementation variants, but some Racket
features are available only on some implementation variants, and the
interaction of Racket and foreign functions is significantly different
across the variants. Use @racket[system-type] to get information about
the current running implementation.
