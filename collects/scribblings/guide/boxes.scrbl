#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "boxes"]{Boxes}

A @defterm{box} is like a single-element vector. It prints as
@litchar{#&} followed by the printed form of the boxed value.  A
@litchar{#&} form can also be used as an expression, but since the
resulting box is constant, it has practically no use.

@examples[
(define b (box "apple"))
b
(unbox b)
(set-box! b '(banana boat))
b
]

@refdetails["boxes"]{boxes and box procedures}
