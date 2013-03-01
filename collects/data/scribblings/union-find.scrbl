#lang scribble/manual
@(require scribble/eval
          (for-label data/union-find
                     racket/contract
                     racket/base))

@title[#:tag "union-find"]{Union-Find: Sets with only Canonical Elements}

@(define the-eval (make-base-eval))
@(the-eval '(require data/union-find))

@defmodule[data/union-find]

The union-find algorithm and data structure provides
an API for representing sets that contain a single,
canonical element. The sets support an (imperative) union 
operation (the library picks one of the canonical elements
of the original sets to be the canonical element of the union),
as well as getting and setting the canonical element.

These operations are not thread-safe.

@defproc[(uf-new [c any/c]) uf-set?]{

Makes a new set with the canonical element @racket[c].

This is a constant time operation.
                                           
@examples[#:eval the-eval
                 (uf-new 'whale)
                 (uf-new 'dwarf-lantern)]
}


@defproc[(uf-set? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] was created with @racket[uf-new],
        and @racket[#f] otherwise.
        
This is a constant time operation.

@examples[#:eval the-eval
  (uf-set? (uf-new 'spiny-dogfish))
  (uf-set? "I am not a uf-set")]
}

@defproc[(uf-find [a uf-set?]) any/c]{
  Returns the canonical element of @racket[a].

  This is an amortized (essentially) constant time operation.
                                   
  @examples[#:eval the-eval
                   (uf-find (uf-new 'tasselled-wobbegong))]
}

@defproc[(uf-union! [a uf-set?] [b uf-set?]) void?]{

Imperatively unifies @racket[a] and @racket[b], making
them both have the same canonical element. Either
of @racket[a] or @racket[b]'s  canonical elements may
become the canonical element for the union.

This is an amortized (essentially) constant time operation.

@examples[#:eval the-eval
  (define a (uf-new 'sand-devil))
  (define b (uf-new 'pigeye))
  (uf-union! a b)
  (uf-find a)
  (uf-find b)
]
}

@defproc[(uf-same-set? [a uf-set?] [b uf-set?]) boolean?]{
  Returns @racket[#t] if the sets @racket[a] and @racket[b]
  have been unioned.

This is an amortized (essentially) constant time operation.

@examples[#:eval the-eval
  (define a (uf-new 'finetooth))
  (define b (uf-new 'speartooth))
  (uf-same-set? a b)
  (uf-union! a b)
  (uf-same-set? a b)
]

  
}

@defproc[(uf-set-canonical! [a uf-set?] [c any/c]) void?]{
  Changes @racket[a] to have a new canonical element.
          
This is an amortized (essentially) constant time operation.

 @examples[#:eval the-eval
  (define a (uf-new 'sand-devil))
  (uf-set-canonical! a 'lemon)
  (uf-find a)
  (define b (uf-new 'pigeye))
  (uf-union! a b)
  (uf-set-canonical! b 'sicklefin-lemon)
  (uf-find a)
]
          
}

@close-eval[the-eval]
