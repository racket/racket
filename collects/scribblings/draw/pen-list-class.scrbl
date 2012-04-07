#lang scribble/doc
@(require "common.rkt")

@defclass/title[pen-list% object% ()]{

A @racket[pen-list%] object maintains a list of @racket[pen%]
 objects to avoid repeatedly creating pen objects. A @racket[pen%]
 object in a pen list cannot be mutated.

A global pen list @indexed-racket[the-pen-list] is created automatically.


@defconstructor[()]{

Creates an empty pen list.


}

@defmethod*[([(find-or-create-pen [color (is-a?/c color%)]
                                  [width (real-in 0 255)]
                                  [style (or/c 'transparent 'solid 'xor 'hilite 
                                               'dot 'long-dash 'short-dash 'dot-dash 
                                               'xor-dot 'xor-long-dash 'xor-short-dash 
                                               'xor-dot-dash)]
                                  [cap (or/c 'round 'projecting 'butt) 'round]
                                  [join (or/c 'round 'bevel 'miter) 'round])
              (is-a?/c pen%)]
             [(find-or-create-pen [color-name string?]
                                  [width (real-in 0 255)]
                                  [style (or/c 'transparent 'solid 'xor 'hilite 
                                               'dot 'long-dash 'short-dash 'dot-dash 
                                               'xor-dot 'xor-long-dash 'xor-short-dash 
                                               'xor-dot-dash)]
                                  [cap (or/c 'round 'projecting 'butt) 'round]
                                  [join (or/c 'round 'bevel 'miter) 'round])
              (or/c (is-a?/c pen%) #f)])]{

Finds a pen of the given specification, or creates one and adds it to
the list.  The arguments are the same as for creating a @racket[pen%]
instance plus a cap and join style as for @method[pen% set-cap] and
@method[pen% set-join]. When @racket[color-name] is provided, however, the return
value is @racket[#f] when no color matching @racket[color-name] can be
found in @racket[the-color-database].

}}

