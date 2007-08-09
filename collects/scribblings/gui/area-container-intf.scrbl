#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[area-container<%> (area<%>)]{

An @scheme[area-container<%>] is a container @scheme[area<%>].

All @scheme[area-container<%>] classes accept the following named
 instantiation arguments:
@itemize{

 @item{@indexed-scheme[border] --- default is @scheme[0]; passed to
@method[area-container<%> border]} 
 @item{@indexed-scheme[spacing] --- default is @scheme[0]; passed to
@method[area-container<%> spacing]} 
 @item{@indexed-scheme[alignment] --- default is class-specific, such as
 @scheme['(center top)] for @scheme[vertical-panel%]; the list
 elements are passed to
@method[area-container<%> set-alignment]} 
}




@defmethod[(get-children)
           list of @scheme[subarea<%>] objects]{
@spec{

Returns a list of the container's non-deleted children. (The non-deleted
 children are the ones currently managed by the container; deleted
 children are generally hidden.) The order of the children in the list
 is significant. For example, in a vertical panel, the first child in
 the list is placed at the top of the panel.

}}

@defmethod[(change-children [filter procedure of one argument, a list of \scmintf{subarea} objects,  that returns a list of \scmintf{subarea} objects])
           void?]{
@spec{

Takes a filter procedure and changes the container's list of non-deleted
 children. The filter procedure takes a list of children areas and
 returns a new list of children areas. The new list must consist of
 children that were created as subareas of this area (i.e.,
@method[area-container<%> change-children] cannot be used to change the parent of a subarea).

After the set of non-deleted children is changed, the container computes
 the sets of newly deleted and newly non-deleted children. Newly deleted
 windows are hidden. Newly non-deleted windows are shown.

Since non-window areas cannot be hidden, non-window areas cannot be
 deleted. If the filter procedure removes non-window subareas,
 an exception is raised and the set of non-deleted children is not changed.

}}

@defmethod[(place-children [info list of list containing two \IntsIn{0}{10000} and two booleans]
                           [width (integer-in 0 10000)]
                           [height (integer-in 0 10000)])
           list of list containing four \IntsIn{0}{10000}]{
@spec{

Called to place the children of a container. See @|geomdiscuss|
 for more information.

}}

@defmethod[(container-size [info list of list containing two \IntsIn{0}{10000} and two booleans])
           two \IntsIn{0}{10000}]{
@spec{

Called to determine the minimum size of a container. See
 @|geomdiscuss| for more information.

}}

@defmethod[(add-child [child (is-a/c subwindow<%>)])
           void?]{
@spec{

Add the given subwindow to the set of non-deleted children. See also
@method[area-container<%> change-children].

}}

@defmethod[(delete-child [child (is-a/c subwindow<%>)])
           void?]{
@spec{

Removes the given subwindow from the list of non-deleted children.  See also
@method[area-container<%> change-children].

}}

@defmethod[(after-new-child [child (is-a/c subarea<%>)])
           void?]{
@spec{

This method is called after a new containee area is created with this
 area as its container. The new child is provided as an argument to
 the method.

}
@impl{

Does nothing.



}}

@defmethod*[([(border)
              (integer-in 0 1000)]
             [(border [margin (integer-in 0 1000)])
              void?])]{
@spec{

Gets or sets the border margin for the container in pixels. This
 margin is used as an inset into the panel's client area before the
 locations and sizes of the subareas are computed.

}
@impl{
First case:


Returns the current border margin.



Second case:


Sets the border margin.



}}

@defmethod*[([(spacing)
              (integer-in 0 1000)]
             [(spacing [spacing (integer-in 0 1000)])
              void?])]{
@spec{

Gets or sets the spacing, in pixels, used between subareas in the
 container. For example, a vertical panel inserts this spacing between
 each pair of vertically aligned subareas (with no extra space at the
 top or bottom).

}
@impl{
First case:


Returns the current spacing.



Second case:


Sets the spacing.



}}

@defmethod[(set-alignment [horiz-align (symbols/c right center left)]
                          [vert-align (symbols/c bottom center top)])
           void?]{
@spec{

Sets the alignment specification for a container, which determines how
 it positions its children when the container has leftover space (when
 a child was not stretchable in a particular dimension).

When the container's horizontal alignment is @scheme['left], the
 children are left-aligned in the container and whitespace is inserted
 to the right.  When the container's horizontal alignment is
 @scheme['center], each child is horizontally centered in the
 container. When the container's horizontal alignment is
 @scheme['right], leftover whitespace is inserted to the left.

Similarly, a container's vertical alignment can be @scheme['top],
 @scheme['center], or @scheme['bottom].

}}

@defmethod[(get-alignment)
           two symbols]{
@spec{

Returns the container's current alignment specification. See
@method[area-container<%> set-alignment] for more information.

}}

@defmethod[(reflow-container)
           void?]{
@spec{

When a container window is not shown, changes to the container's set
 of children do not necessarily trigger the immediate re-computation
 of the container's size and its children's sizes and positions.
 Instead, the recalculation is delayed until the container is shown,
 which avoids redundant computations between a series of changes. The
@method[area-container<%> reflow-container] method forces the immediate recalculation of the container's and its
 children's sizes and locations.

Immediately after calling the
@method[area-container<%> reflow-container] method,
@method[window<%> get-size],
@method[window<%> get-client-size],
@method[window<%> get-width],
@method[window<%> get-height],
@method[window<%> get-x], and
@method[window<%> get-y] report the manager-applied sizes and locations for the container and
 its children, even when the container is hidden. A
 container implementation can call functions such as
@method[window<%> get-size]  at any time to obtain the current state of a window (because the
 functions do not trigger geometry management).

See also
@method[area-container<%> container-flow-modified].

}}

@defmethod[(container-flow-modified)
           void?]{
@spec{

Call this method when the result changes for an overridden flow-defining method, such as
@method[area-container<%> place-children]. The call notifies the geometry manager that the placement of the
 container's children needs to be recomputed. 

The
@method[area-container<%> reflow-container]method only recomputes child positions when the geometry manager
 thinks that the placement has changed since the last computation.

}}

@defmethod[(begin-container-sequence)
           void?]{
@spec{

Suspends geometry management in the container's top-level window
 until
@method[area-container<%> end-container-sequence] is called. The
@method[area-container<%> begin-container-sequence] and 
@method[area-container<%> end-container-sequence] methods are used to bracket a set of container modifications so that
 the resulting geometry is computed only once.  A container sequence also 
 delays show and hide actions by
@method[area-container<%> change-children], as well as the on-screen part of showing via
@method[window<%> show] until the sequence is complete.  Sequence begin and end commands may
 be nested arbitrarily deep.

}}

@defmethod[(end-container-sequence)
           void?]{
@spec{

See
@method[area-container<%> begin-container-sequence].

}
@impl{




}}}

