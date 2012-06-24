#lang at-exp scheme/base

(require "teachprims.rkt" "and-or-map.rkt"
         mzlib/etc
         (only-in racket/list argmin argmax)
         syntax/docprovide
         (for-syntax scheme/base))

(require "provide-and-scribble.rkt" scribble/manual)

(provide-and-scribble
 procedures
 (all-from-except beginner: (submod "beginner-funs.rkt" without-wrapper) procedures + * - / append) 
 
 ("Numbers (relaxed conditions)"
  @defproc[(+ [x number] ...) number]{Adds all given numbers.}
  @defproc[(* [x number] ...) number]{Multiplies all given numbers.}
  @defproc[(- [x number] ...) number]{Subtracts from the first all remaining numbers.}
  @defproc[(/ [x number] ...) number]{Divides the first by all remaining numbers.}
  )
 
 ("Lists"
  @defproc[((intermediate-append append) [l (listof any)] ...) (listof any)]{Creates a single list from several, by juxtaposition of the items.})
 
 ("Higher-Order Functions"
  @defproc[(map [f (X ... -> Z)] [l (listof X)] ...) (listof Z)]{Constructs a new list by applying a function to each item on one or more existing lists.}
  @defproc[(for-each [f (any ... -> any)] [l (listof any)] ...) void?]{Applies a function to each item on one or more lists for effect only.}
  @defproc[((intermediate-filter filter) [p? (X -> boolean)] [l (listof X)]) (listof X)]{Constructs a list from all those items on a list for which the predicate holds.}
  @defproc[((intermediate-foldr foldr) [f (X Y -> Y)] [base Y] [l (listof X)]) Y]{(foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))}
  @defproc[((intermediate-foldl foldl) [f (X Y -> Y)] [base Y] [l (listof X)]) Y]{(foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))}
  @defproc[(build-list [n nat] [f (nat -> X)]) (listof X)]{(build-list n f) = (list (f 0) ... (f (- n 1)))}
  @defproc[((intermediate-build-string build-string) [n nat] [f (nat -> char)]) string]{(build-string n f) = (string (f 0) ... (f (- n 1)))}
  @defproc[((intermediate-quicksort quicksort) [l (listof X)] [comp (X X -> boolean)]) (listof X)]{Constructs a list from all items on a list in an order according to a predicate.}
  @defproc[((intermediate-sort sort) [l (listof X)] [comp (X X -> boolean)]) (listof X)]{Constructs a list from all items on a list in an order according to a predicate.}
  @defproc[((intermediate-andmap andmap) [p? (X -> boolean)] [l (listof X)]) boolean]{(andmap p (list x-1 ... x-n)) = (and (p x-1) ... (p x-n))}
  @defproc[((intermediate-ormap ormap)   [p? (X -> boolean)] [l (listof X)]) boolean]{(ormap p (list x-1 ... x-n)) = (or (p x-1) ... (p x-n))}
  @defproc[(argmin [f (X -> real)] [l (listof X)]) X]{Finds the (first) element of the list that minimizes the output of the function.}
  @defproc[(argmax [f (X -> real)] [l (listof X)]) X]{Finds the (first) element of the list that maximizes the output of the function.}
  @defproc[(memf [p? (X -> any)] [l (listof X)]) (union false (listof X))]{Produces false if the predicate given as the first argument produces false for all items on the given list. If the given predicate produces true for any of the items on the list, memf returns the sub-list starting from that element.}
  @defproc[(apply [f (X-1 ... X-N -> Y)] [x-1 X-1] ... [l (list X-i+1 ... X-N)]) Y]{Applies a function using items from a list as the arguments.}
  @defproc[(compose [f (X -> Y)] [g (Y -> Z)]) (X -> Z)]{Composes a sequence of procedures into a single procedure.}
  @defproc[(procedure? [x any]) boolean?]{Produces true if the value is a procedure.}))
