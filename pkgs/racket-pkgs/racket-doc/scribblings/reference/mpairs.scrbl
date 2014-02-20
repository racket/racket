#lang scribble/doc
@(require "mz.rkt" scribble/racket (for-label racket/mpair))

@title[#:tag "mpairs"]{Mutable Pairs and Lists}

A @deftech{mutable pair} is like a pair created by @racket[cons], but
it supports @racket[set-mcar!] and @racket[set-mcdr!] mutation
operations to change the parts of the mutable pair (like traditional Lisp and
Scheme pairs).

A @deftech{mutable list} is analogous to a list created with pairs, but
instead created with @tech{mutable pairs}.

A @tech{mutable pair} is not a @tech{pair}; they are completely
separate datatypes. Similarly, a @tech{mutable list} is not a
@tech{list}, except that the empty list is also the empty mutable
list. Instead of programming with mutable pairs and mutable lists,
data structures such as pairs, lists, and hash tables are practically
always better choices.

A @tech{mutable list} can be used as a single-valued sequence (see
@secref["sequences"]). The elements of the @tech{mutable list} serve as elements
of the sequence. See also @racket[in-mlist].

@; ----------------------------------------
@section{Mutable Pair Constructors and Selectors}

@defproc[(mpair? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is
a @tech{mutable pair}, @racket[#f] otherwise.}

@defproc[(mcons [a any/c] [d any/c]) mpair?]{Returns a newly allocated 
@tech{mutable pair} whose first
element is @racket[a] and second element is @racket[d].}

@defproc[(mcar [p mpair?]) any/c]{Returns the first element of the
@tech{mutable pair} @racket[p].}

@defproc[(mcdr [p mpair?]) any/c]{Returns the second element of the
@tech{mutable pair} @racket[p].}


@defproc[(set-mcar! [p mpair?] [v any/v]) 
         void?]{

Changes the @tech{mutable pair} @racket[p] so that its first element is
@racket[v].}

@defproc[(set-mcdr! [p mpair?] [v any/v]) 
         void?]{

Changes the @tech{mutable pair} @racket[p] so that its second element is
@racket[v].}
