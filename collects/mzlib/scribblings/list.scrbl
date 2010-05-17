#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/list))

@mzlib[#:mode title list]

The @schememodname[mzlib/list] library re-exports several functions
from @schememodname[scheme/base] and @schememodname[scheme/list]:

@schemeblock[
cons?
empty?
empty
foldl
foldr
remv
remq
remove
remv*
remq*
remove*
findf
memf
assf
filter
sort
]

@deftogether[(
@defproc[(first [v pair?]) any/c]
@defproc[(second [v (and/c pair? ....)]) any/c]
@defproc[(third [v (and/c pair? ....)]) any/c]
@defproc[(fourth [v (and/c pair? ....)]) any/c]
@defproc[(fifth [v (and/c pair? ....)]) any/c]
@defproc[(sixth [v (and/c pair? ....)]) any/c]
@defproc[(seventh [v (and/c pair? ....)]) any/c]
@defproc[(eighth [v (and/c pair? ....)]) any/c]
)]{

Accesses the first, second, @|etc| elment of ``list'' @scheme[v]. The
argument need not actually be a list; it is inspected only as far as
necessary to obtain an element (unlike the same-named functions from
@schememodname[scheme/list], which do require the argument to be a
list).}


@defproc[(rest [v pair?]) any/c]{

The same as @scheme[cdr].}


@defproc[(last-pair [v pair?]) pair?]{

Returns the last pair in @scheme[v], raising an error if @scheme[v] is
not a pair (but @scheme[v] does not have to be a proper list).}



@defproc[(merge-sorted-lists [lst1 list?][lst2 lst?]
                             [less-than? (any/c any/c . -> . any/c)])
         list?]{

Merges the two sorted input lists, creating a new sorted list.  The
merged result is stable: equal items in both lists stay in the same
order, and these in @scheme[lst1] precede @scheme[lst2].}

@defproc[(mergesort [lst list?] [less-than? (any/c any/c . -> . any/c)])
         list?]{

The same as @scheme[sort].}

@defproc[(quicksort [lst list?] [less-than? (any/c any/c . -> . any/c)])
         list?]{

The same as @scheme[sort].}
