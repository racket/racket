#lang scribble/doc

@(require scribble/manual
          (for-label scheme 
	  	     teachpack/htdp/docs))

@title[#:tag "docs"]{Manipulating Simple HTML Documents: docs.ss}

@declare-exporting[teachpack/htdp/docs]

The teachpack provides three operations for creating simple ``HTML'' documents: 

@deftech{Annotation} An @tech{Annotation} is a symbol that starts with ``<''
and ends in ``>''. An end annotation is one that starts with ``</''.

@defproc[(atom? [x any/c]) boolean?]{Determines whether or not a Scheme value
is a number, a symbol, or a string.} 

@defproc[(annotation? [x any/c]) boolean?]{Determines whether or not a Scheme
symbol is a document annotation.} 

@defproc[(end-annotation [x (unsyntax @tech{Annotation})]) (unsyntax @tech{Annotation})]{Consumes an annotation
and produces a matching ending annotation.} 

@defproc[(write-file [l (list-of atom)]) true]{
Consumes a list of symbols and annotations and prints them out as a
"file".}

Sample session: set teachpack to ``docs.ss''> and click RUN:
@(begin
#reader scribble/comment-reader
(schemeblock
> (annotation? 0)
false
> (annotation? '<bold>)
true
> (end-annotation 0)
end-annotation: not an annotation: 0
> (write-file (list 'a 'b))
a b 
))

