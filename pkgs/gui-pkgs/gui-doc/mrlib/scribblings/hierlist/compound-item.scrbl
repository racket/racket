#lang scribble/doc
@(require "../common.rkt" (for-label mrlib/hierlist))

@definterface/title[hierarchical-list-compound-item<%>
                    (hierarchical-list-item<%>)]{

Instantiate this interface via @method[hierarchical-list% new-list].


@defmethod[(new-item [mixin ((implementation?/c hierarchical-list-item<%>)
                             . -> .
                             (implementation?/c hierarchical-list-item<%>))
                             (lambda (%) %)])
           (is-a?/c hierarchical-list-item<%>)]{

Like @xmethod[hierarchical-list% new-item].}


@defmethod[(set-no-sublists [no-sublists? any/c]) void?]{

Like @xmethod[hierarchical-list% set-no-sublists].}


@defmethod[(new-list [mixin ((implementation?/c hierarchical-list-compound-item<%>)
                             . -> .
                             (implementation?/c hierarchical-list-compound-item<%>))
                             (lambda (%) %)])
           (is-a?/c hierarchical-list-compound-item<%>)]{

Like @xmethod[hierarchical-list% new-list].}


@defmethod[(delete-item [i (is-a?/c hierarchical-list-item<%>)]) void?]{

Deletes immediate item or sub-list @racket[i] from the sub-list.}


@defmethod[(get-items) (listof (is-a?/c hierarchical-list-item<%>))]{

Returns a list of all immediate items in the sub-list.}


@defmethod*[([(open) void?]
             [(close) void?]
             [(toggle-open/closed) void?])]{

Shows or hides the items of this sub-list.}


@defmethod[(is-open?) boolean?]{

Reports whether the items of this sub-list are visible.}


@defmethod[(get-arrow-snip) (is-a?/c snip%)]{

Returns a snip that corresponds to the arrow to hide/show items of the
sub-list. The result is intended for use by automatic test suites.}

}
