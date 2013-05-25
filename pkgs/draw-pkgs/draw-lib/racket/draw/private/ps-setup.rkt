#lang racket/base
(require racket/class
         "syntax.rkt")

(provide ps-setup%
         current-ps-setup
         paper-sizes
         
         get-native
         get-native-copy
         set-native)

(define-local-member-name
  get-native
  get-native-copy
  set-native
  get-all-numerics)

(define paper-sizes
  '(("A4 210 x 297 mm" 595 842)
    ("A3 297 x 420 mm" 842 1191)
    ("Letter 8 1/2 x 11 in" 612 791)
    ("Legal 8 1/2 x 14 in" 612 1009)))

(define (paper-name-string? s)
  (and (string? s)
       (assoc s paper-sizes)))

(define ps-setup%
  (class object%
    (properties
     [[string? command] "lpr"]
     [[(make-or-false path-string?) file] #f]
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

    (define native #f)
    (define native-copier #f)
    (define/public (get-native) native)
    (define/public (get-native-copy) 
      (values (and native (native-copier native))
              native-copier))
    (define/public (set-native n copier)
      (set! native n)
      (set! native-copier copier))

    (def/public (copy-from [ps-setup% source]
                           [any? [filename? #f]])
      (set! command (send source get-command))
      (when filename? (set! file (send source get-file)))
      (set! level-2 (send source get-level-2))
      (set! mode (send source get-mode))
      (set! orientation (send source get-orientation))
      (set! paper-name (send source get-paper-name))
      (set! preview-command (send source get-preview-command))
      (set!-values (native native-copier) (send source get-native-copy))
      (set!-values (editor-margin-x editor-margin-y
                                    margin-x margin-y
                                    scale-x scale-y
                                    trans-x trans-y)
                   (send source get-all-numerics)))

    (define/public (get-all-numerics)
      (values editor-margin-x editor-margin-y
              margin-x margin-y
              scale-x scale-y
              trans-x trans-y))
     
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
