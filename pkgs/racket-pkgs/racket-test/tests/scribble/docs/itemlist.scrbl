#lang scribble/base
@(require scribble/decode)

@itemlist[
 (list @item{a}
       (list @item{b}))
 @para{c}
 @item{d}
 (splice (list @item{e} (list @para{f})))
]
