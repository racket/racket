#lang scribble/doc

@(require scribble/manual
          (for-label (prefix-in htdp: htdp/image)
                     (prefix-in 2htdp: 2htdp/image)))

@title{Porting from @schememodname[htdp/image]}

why switch(?): faster image comparison, added rotate, scale, and curves, plus a bunch of new polygon primitives

@schemeblock[(htdp:rectangle 10 10 "outline" "black")
             (2htdp:rectangle 10 10 "outline" "black")]

changes:

no pinholes

overlay arguments reversed (added underlay)

lines drawn probably different somewhere (ellipses?)

star function is different (bring back old star function?)
