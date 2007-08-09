#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[subarea<%> (area<%>)]{

A @scheme[subarea<%>] is a containee @scheme[area<%>].

All @scheme[subarea<%>] classes accept the following named
 instantiation arguments:
@itemize{

 @item{@indexed-scheme[horiz-margin] --- default is @scheme[2] for
 @scheme[control<%>] classes and @scheme[group-box-panel%], 
 @scheme[0] for others; passed to
@method[subarea<%> horiz-margin]} 
 @item{@indexed-scheme[vert-margin] --- default is @scheme[2] for
 @scheme[control<%>] classes and @scheme[group-box-panel%], 
 @scheme[0] for others; passed to
@method[subarea<%> vert-margin]} 
}




@defmethod*[([(horiz-margin)
              (integer-in 0 1000)]
             [(horiz-margin [margin (integer-in 0 1000)])
              void?])]{
@spec{

Gets or sets the area's horizontal margin, which is added both to the
 right and left, for geometry management. See @|geomdiscuss| for more
 information.

}
@impl{
First case:


Returns the current horizontal margin.



Second case:


Sets the horizontal margin.



}}

@defmethod*[([(vert-margin)
              (integer-in 0 1000)]
             [(vert-margin [margin (integer-in 0 1000)])
              void?])]{
@spec{

Gets or sets the area's vertical margin, which is added both to the
 right and left, for geometry management. See @|geomdiscuss| for more
 information.

}
@impl{
First case:


Returns the current vertical margin.



Second case:


Sets the vertical margin.



}}}

