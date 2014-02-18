(module common-sig scheme/base
  (require scheme/unit)

  (provide texpict-common^)
  (define-signature texpict-common^
    ((struct pict (draw width height ascent descent children panbox last))
     (struct child (pict dx dy sx sy sxy syx))

     black-and-white

     lt-find
     lc-find
     lb-find
     ltl-find
     lbl-find
     ct-find
     cc-find
     cb-find
     ctl-find
     cbl-find
     rt-find
     rc-find
     rb-find
     rtl-find
     rbl-find

     find-lt  ; (left & top)  ; pict pict-path -> dx dy
     find-lc  ; (left & vertical center)
     find-lb  ; (left & bottom)
     find-ltl ; (left and top baseline)
     find-lbl ; (left and bottom baseline)
     find-ct  ; (horizontal center & top)
     find-cc
     find-cb
     find-ctl
     find-cbl
     find-rt
     find-rc
     find-rb
     find-rtl
     find-rbl

     launder  ; pict -> pict

     blank        ; -> pict
     ; w h -> pict
     ; w h d -> pict

     clip-descent   ; pict -> pict
     clip-ascent    ; pict -> pict
     lift           ; pict -> pict
     drop           ; pict -> pict
     baseless       ; pict -> pict
     inset          ; pict i -> pict
                    ; pict hi vi -> pict
                    ; pict l t r b -> pict
     refocus        ; pict pict -> pict
     panorama       ; pict -> pict

     use-last       ; pict pict -> pict
     use-last*      ; pict pict -> pict

     hline        ; w h -> pict
     dash-hline   ; w h seg-length -> pict ; default seg-length is 5
     vline        ; w h -> pict
     dash-vline   ; w h seg-length -> pict ; default seg-length is 5

     frame        ; pict -> pict
     dash-frame   ; pict seg-length -> pict ; default seg-length is 5
     oval         ; pict -> pict
     oval/radius  ; pict r -> pict ; r is radius of corners

     big-circle   ; diameter -> pict

     thick       ; pict -> pict
     thin        ; pict -> pict

     ghost        ; pict -> pict

     record       ; pict pict ... -> pict

     vl-append    ; d pict ... -> pict ; d units between each picture
     vc-append
     vr-append
     ht-append
     hc-append
     hb-append
     htl-append       ; align bottoms of ascents
     hbl-append       ; align tops of descents (normal text alignment)

     lt-superimpose ; pict ... -> pict
     lb-superimpose
     lc-superimpose
     ltl-superimpose
     lbl-superimpose
     rt-superimpose
     rb-superimpose
     rc-superimpose
     rtl-superimpose
     rbl-superimpose
     ct-superimpose
     cb-superimpose
     cc-superimpose
     ctl-superimpose
     cbl-superimpose

     table ; ncols pict-list col-aligns row-aligns col-seps row-seps -> pict

     colorize ; pict color-string -> pict

     picture       ; w h command-list -> pict
     picture*      ; w h a d command-list -> pict

     cons-picture  ; pict command-list -> pict
     cons-picture* ; pict command-list -> pict

     place-over
     place-under
     pin-over
     pin-under
     ))

  (provide texpict-common-setup^)
  (define-signature texpict-common-setup^
    (connect
     ~connect
     convert-pict))

  (provide texpict-internal^)
  (define-signature texpict-internal^
    (prepare-for-output
     pict->command-list
     line-thickness
     line-style)))
