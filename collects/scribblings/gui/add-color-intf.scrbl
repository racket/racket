#lang scribble/doc
@(require "common.rkt")

@definterface/title[add-color<%> ()]{

An @scheme[add-color<%>] object is used to additively change the RGB
 values of a @scheme[color%] object. An @scheme[add-color<%>] object
 only exists within a @scheme[style-delta%] object.

See also @method[style-delta% get-foreground-add] and
 @method[style-delta% get-background-add].



@defmethod[(get [r (box/c (integer-in -1000 1000))]
                [g (box/c (integer-in -1000 1000))]
                [b (box/c (integer-in -1000 1000))])
           void?]{

Gets all of the additive values.

@boxisfill[(scheme r) @elem{the additive value for the red component of the color}]
@boxisfill[(scheme g) @elem{the additive value for the green component of the color}]
@boxisfill[(scheme b) @elem{the additive value for the blue component of the color}]

}

@defmethod[(get-b)
           (integer-in -1000 1000)]{

Gets the additive value for the blue component of the color.

}

@defmethod[(get-g)
           (integer-in -1000 1000)]{

Gets the additive value for the green component of the color.

}

@defmethod[(get-r)
           (integer-in -1000 1000)]{

Gets the additive value for the red component of the color.

}

@defmethod[(set [r (integer-in -1000 1000)]
                [g (integer-in -1000 1000)]
                [b (integer-in -1000 1000)])
           void?]{

Sets all of the additive values.

}

@defmethod[(set-b [v (integer-in -1000 1000)])
           void?]{

Sets the additive value for the blue component of the color.

}

@defmethod[(set-g [v (integer-in -1000 1000)])
           void?]{

Sets the additive value for the green component of the color.

}

@defmethod[(set-r [v (integer-in -1000 1000)])
           void?]{

Sets the additive value for the red component of the color.

}}

