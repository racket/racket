#lang scheme/base
(require scheme/class
         mred/private/syntax)

(provide ps-setup%
         current-ps-setup
         paper-sizes)

(define paper-sizes
  '(("A4 210 x 297\n mm" 595 842)
    ("A3 297 x 420 mm" 842 1191)
    ("Letter 8 1/2 x 11 in" 612 791)
    ("Legal 8 1/2 x 14 in" 612 1009)))

(define (paper-name-string? s)
  (and (string? s)
       (assoc paper-sizes s)))

(define ps-setup%
  (class object%
    (properties
     [[string? command] "lpr"]
     [[(make-or-false path-string?) filename] #f]
     [[bool? level-2] #t]
     [[(symbol-in preview file printer) mode] 'file]
     [[(symbol-in portrait landscape) orientation] 'portrait]
     [[paper-name-string? paper-name] "Letter 8 1/2 x 11 in"]
     [[string? preview-command] "gv"])

    (define editor-margin-x 20.0)
    (define editor-margin-y 20.0)
    (define margin-x 16.0)
    (define margin-y 16.0)
    (define scale-x 0.8)
    (define scale-y 0.8)
    (define trans-x 0.0)
    (define trans-y 0.0)
     
    (def/public (get-editor-margin [(make-box nonnegative-real?) x]
                                   [(make-box nonnegative-real?) y])
      (set-box! x editor-margin-x)
      (set-box! y editor-margin-y))
    (def/public (set-editor-margin [nonnegative-real? x]
                                   [nonnegative-real? y])
      (set! editor-margin-x x)
      (set! editor-margin-y y))
     
    (def/public (get-margin [(make-box nonnegative-real?) x]
                            [(make-box nonnegative-real?) y])
      (set-box! x margin-x)
      (set-box! y margin-y))
    (def/public (set-margin [nonnegative-real? x]
                            [nonnegative-real? y])
      (set! margin-x x)
      (set! margin-y y))

    (def/public (get-scaling [(make-box nonnegative-real?) x]
                             [(make-box nonnegative-real?) y])
      (set-box! x scale-x)
      (set-box! y scale-y))
    (def/public (set-scaling [nonnegative-real? x]
                             [nonnegative-real? y])
      (set! scale-x x)
      (set! scale-y y))

    (def/public (get-translation [(make-box nonnegative-real?) x]
                                 [(make-box nonnegative-real?) y])
      (set-box! x trans-x)
      (set-box! y trans-y))
    (def/public (set-translation [nonnegative-real? x]
                                 [nonnegative-real? y])
      (set! trans-x x)
      (set! trans-y y))

    (super-new)))

(define current-ps-setup (make-parameter (new ps-setup%)))
