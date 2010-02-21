#lang scribble/doc
@(require "common.ss")

@defclass/title[pen-list% object% ()]{

A @scheme[pen-list%] object maintains a list of @scheme[pen%]
 objects to avoid repeatedly creating pen objects. A @scheme[pen%]
 object in a pen list cannot be mutated.

A global pen list @indexed-scheme[the-pen-list] is created automatically.





@defconstructor[()]{

Creates an empty pen list.


}

@defmethod*[([(find-or-create-pen [color (is-a?/c color%)]
                                  [width (real-in 0 255)]
                                  [style (one-of/c 'transparent 'solid 'xor 'hilite 
                                                   'dot 'long-dash 'short-dash 'dot-dash 
                                                   'xor-dot 'xor-long-dash 'xor-short-dash 
                                                   'xor-dot-dash)])
              (is-a?/c pen%)]
             [(find-or-create-pen [color-name string?]
                                  [width (real-in 0 255)]
                                  [style (one-of/c 'transparent 'solid 'xor 'hilite 
                                                   'dot 'long-dash 'short-dash 'dot-dash 
                                                   'xor-dot 'xor-long-dash 'xor-short-dash 
                                                   'xor-dot-dash)])
              (or/c (is-a?/c pen%) false/c)])]{

Finds a pen of the given specification, or creates one and adds it to
the list.  The arguments are the same as for creating a @scheme[pen%]
instance. When @scheme[color-name] is provided, however, the return
value is @scheme[#f] when no color matching @scheme[color-name] can be
found in @scheme[the-color-database].

}}

