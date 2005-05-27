
(module texpict-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide texpict-extra^)
  (define-signature texpict-extra^
    (read-in-sizes  ; string -> void

     using-pict2e-package

     draw-bezier-lines
     
     use-old-connect

     output-measure-commands

     tex-series-prefix
     serialize-tex-picts
     
     current-tex-sizer

     tex               ; string -> pict
     text-line         ; string -> pict
     text-line/phantom ; string string -> pict
     tex-paragraph     ; w string ['top|'bottom] -> pict

     left-brace     ; h -> pict
     right-brace    ; h -> pict
     left-delimit   ; str h -> pict
     right-delimit  ; str h -> pict
     middle-delimit ; str h -> pict
     top-brace      ; w -> pict
     bottom-brace   ; w -> pict

     pict->string

     pict->commands)))
