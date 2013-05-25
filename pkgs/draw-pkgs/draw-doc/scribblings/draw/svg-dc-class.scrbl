#lang scribble/doc
@(require "common.rkt")

@defclass/title[svg-dc% object% (dc<%>)]{

Similar to @racket[post-script-dc%], but generates a SVG (scalable
 vector graphics) file instead of a PostScript file.

@|PrintNote|

@defconstructor[([width (and/c real? (not/c negative?))]
                 [height (and/c real? (not/c negative?))]
                 [output (or/c path-string? output-port?)]
                 [exists (or/c 'error 'append 'update 'can-update
                               'replace 'truncate
                               'must-truncate 'truncate/replace)
                         'error])]{

The @racket[width] and @racket[height] arguments determine the width
 and height of the generated image.

The image is written to @racket[output]. If @racket[output] is a path
 and the file exists already, then @racket[exists] determines how
 the existing file is handled in the same way as for the @racket[#:exists]
 argument to @racket[open-output-file].}

}

