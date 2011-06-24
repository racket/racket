#lang scribble/doc
@(require "common.rkt")

@definterface/title[subarea<%> (area<%>)]{

A @racket[subarea<%>] is a containee @racket[area<%>].

All @racket[subarea<%>] classes accept the following named
 instantiation arguments:
@itemize[

 @item{@indexed-racket[horiz-margin] --- default is @racket[2] for
 @racket[control<%>] classes and @racket[group-box-panel%], 
 @racket[0] for others; passed to
@method[subarea<%> horiz-margin]} 
 @item{@indexed-racket[vert-margin] --- default is @racket[2] for
 @racket[control<%>] classes and @racket[group-box-panel%], 
 @racket[0] for others; passed to
@method[subarea<%> vert-margin]} 
]


@defmethod*[([(horiz-margin)
              (integer-in 0 1000)]
             [(horiz-margin [margin (integer-in 0 1000)])
              void?])]{

Gets or sets the area's horizontal margin, which is added both to the
 right and left, for geometry management. See @|geomdiscuss| for more
 information.

}

@defmethod*[([(vert-margin)
              (integer-in 0 1000)]
             [(vert-margin [margin (integer-in 0 1000)])
              void?])]{

Gets or sets the area's vertical margin, which is added both to the
 top and bottom, for geometry management. See @|geomdiscuss| for more
 information.

}}

