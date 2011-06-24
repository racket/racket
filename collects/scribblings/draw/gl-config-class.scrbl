#lang scribble/doc
@(require "common.rkt")

@defclass/title[gl-config% object% ()]{

A @racket[gl-config%] object encapsulates configuration information
 for an OpenGL drawing context. Use a @racket[gl-config%] object as an
 initialization argument for @racket[canvas%] or provide it to
 @racket[make-gl-bitmap].


@defconstructor[()]{

Creates a GL configuration that indicates double buffering, a depth
 buffer of size one, no stencil buffer, no accumulation buffer, no
 multisampling, and not stereo.

}

@defmethod[(get-accum-size)
           (integer-in 0 256)]{

Reports the accumulation-buffer size (for each of red, green, blue,
 and alpha) that the configuration requests, where zero means no
 accumulation buffer is requested.

}

@defmethod[(get-depth-size)
           (integer-in 0 256)]{

Reports the depth-buffer size that the configuration requests, where
 zero means no depth buffer is requested.

}

@defmethod[(get-double-buffered)
           boolean?]{

Reports whether the configuration requests double buffering or not.

}

@defmethod[(get-multisample-size)
           (integer-in 0 256)]{

Reports the multisampling size that the configuration requests, where
 zero means no multisampling is requested.

}

@defmethod[(get-stencil-size)
           (integer-in 0 256)]{

Reports the stencil-buffer size that the configuration requests, where
 zero means no stencil buffer is requested.

}

@defmethod[(get-stereo)
           boolean?]{

Reports whether the configuration requests stereo or not.

}

@defmethod[(set-accum-size [on? (integer-in 0 256)])
           void?]{

Adjusts the configuration to request a particular accumulation-buffer
 size for every channel (red, green, blue, and alpha), where zero
 means no accumulation buffer is requested.

}

@defmethod[(set-depth-size [on? (integer-in 0 256)])
           void?]{

Adjusts the configuration to request a particular depth-buffer size,
 where zero means no depth buffer is requested.

}

@defmethod[(set-double-buffered [on? any/c])
           void?]{

Adjusts the configuration to request double buffering or not.

}

@defmethod[(set-multisample-size [on? (integer-in 0 256)])
           void?]{

Adjusts the configuration to request a particular multisample size,
 where zero means no multisampling is requested. If a multisampling
 context is not available, this request will be ignored.

}

@defmethod[(set-stencil-size [on? (integer-in 0 256)])
           void?]{

Adjusts the configuration to request a particular stencil-buffer size,
 where zero means no stencil buffer is requested.

}

@defmethod[(set-stereo [on? any/c])
           void?]{

Adjusts the configuration to request stereo or not.

}}
