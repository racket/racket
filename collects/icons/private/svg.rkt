#lang racket/base

(require racket/class racket/draw racket/list
         racket/contract
         unstable/latent-contract/defthing
         (for-syntax racket/base
                     racket/syntax)
         "utils.rkt"
         slideshow/pict)

(provide (all-defined-out))

(defthing icon-styles (listof symbol?) #:document-value '(diffuse shiny))
(defcontract icon-style/c (or/c #f 'diffuse 'shiny))

(defparam toolbar-icon-height (>/c 1) 16)
(defparam default-icon-height (>/c 1) 24)
(defparam default-icon-style (or/c 'diffuse 'shiny) 'diffuse)

(define (icon-file-name category name color height style) string?
  (define cs-name (cond [(and color style)  (format "~a/~a-~a" color name style)]
                        [color  (format "~a/~a" color name)]
                        [style  (format "~a-~a" name style)]
                        [else   (format "~a" name)]))
  (format "~a/~a/~a.png" category height cs-name))

(defproc (load-icon-pict [category string?]
                         [name string?]
                         [color icon-color/c]
                         [height (>=/c 0) (default-icon-height)]
                         [style icon-style/c (default-icon-style)]) pict?
  (define hs (icon-category-heights category))
  (define icon-height
    (let ([h  (for/first ([h  (in-list hs)] #:when (height . <= . h)) h)])
      (if h h (last hs))))
  (define icon-path
    (build-path svg-icons-base-path (icon-file-name category name color icon-height style)))
  (scale (bitmap icon-path) (/ height icon-height)))

(defproc (load-icon [category string?]
                    [name string?]
                    [color icon-color/c]
                    [height (>=/c 0) (default-icon-height)]
                    [style icon-style/c (default-icon-style)]) (is-a?/c bitmap%)
  (pict->bitmap (load-icon-pict category name color height style)))

(defproc (icon-categories) (listof string?)
  (remove-duplicates
   (let loop ([dirs empty] [res empty])
     (define dir (apply build-path svg-icons-base-path dirs))
     (define files (directory-list dir))
     (append*
      (for/list ([file  (in-list files)])
        (define file-path (build-path dir file))
        (cond [(exact-integer? (string->number (path->string file)))
               res]
              [(directory-exists? file-path)
               (define subdirs (append dirs (list file)))
               (loop subdirs (cons (path->string (apply build-path subdirs)) res))]
              [else
               empty]))))))

(defproc (icon-names [category string?]) (listof string?)
  (define files
    (filter svg-file? (directory-list (build-path svg-icons-base-path category))))
  (for/list ([file  (in-list files)])
    (define-values (base-dir dir-path _) (split-path file))
    (path->string (path-replace-suffix dir-path ""))))

;; ===================================================================================================
;; Icon pict contructors

(define-syntax-rule (define-make-colorized-icon-pict [make-icon-pict category name] ...)
  (begin
    (defproc (make-icon-pict [color icon-color/c]
                             [height (>=/c 0) (default-icon-height)]
                             [style icon-style/c (default-icon-style)]) pict?
      (load-icon-pict category name color height style))
    ...))

(define-syntax-rule (define-make-icon-pict [make-icon-pict category name] ...)
  (begin
    (defproc (make-icon-pict [height (>=/c 0) (default-icon-height)]
                             [style icon-style/c (default-icon-style)]) pict?
      (load-icon-pict category name #f height style))
    ...))

(define-make-colorized-icon-pict
  [go-icon-pict "control" "go"]
  [bar-icon-pict "control" "bar"]
  [back-icon-pict "control" "back"]
  [stop-icon-pict "control" "stop"]
  [check-icon-pict "check" "check"]
  [x-icon-pict "check" "x"]
  [disk-icon-pict "misc" "disk"]
  [plus-icon-pict "symbol" "plus"]
  [up-arrow-icon-pict "arrow" "up"]
  [down-arrow-icon-pict "arrow" "down"]
  [left-arrow-icon-pict "arrow" "left"]
  [right-arrow-icon-pict "arrow" "right"]
  )

(define-make-icon-pict
  [stop-sign-icon-pict "sign" "stop"]
  [magnifying-glass-icon-pict "misc" "magnifying-glass"]
  [magnifying-glass-left-icon-pict "misc" "magnifying-glass-left"]
  [earth-icon-pict "misc" "earth"]
  [moon-icon-pict "misc" "moon"]
  [hash-quote-icon-pict "symbol" "hash-quote"]
  [plt-logo-pict "logo" "plt"]
  [planet-logo-pict "logo" "planet"]
  )

(defproc (step-icon-pict [color icon-color/c]
                         [height (>=/c 0) (default-icon-height)]
                         [style icon-style/c (default-icon-style)]) pict?
  (hc-append (go-icon-pict color height style) (bar-icon-pict color height style)))

(defproc (step-back-icon-pict [color icon-color/c]
                              [height (>=/c 0) (default-icon-height)]
                              [style icon-style/c (default-icon-style)]) pict?
  (hc-append (bar-icon-pict color height style) (back-icon-pict color height style)))

(defproc (continue-icon-pict [color icon-color/c]
                             [height (>=/c 0) (default-icon-height)]
                             [style icon-style/c (default-icon-style)]) pict?
  (hc-append (bar-icon-pict color height style) (go-icon-pict color height style)))

(defproc (continue-back-icon-pict [color icon-color/c]
                                  [height (>=/c 0) (default-icon-height)]
                                  [style icon-style/c (default-icon-style)]) pict?
  (hc-append (back-icon-pict color height style) (bar-icon-pict color height style)))

(defproc (fast-forward-icon-pict [color icon-color/c]
                                 [height (>=/c 0) (default-icon-height)]
                                 [style icon-style/c (default-icon-style)]) pict?
  (define go (go-icon-pict color height style))
  (scale (hc-append go go) 3/4 1))

(defproc (rewind-icon-pict [color icon-color/c]
                           [height (>=/c 0) (default-icon-height)]
                           [style icon-style/c (default-icon-style)]) pict?
  (define back (back-icon-pict color height style))
  (scale (hc-append back back) 3/4 1))

(defproc (pause-icon-pict [color icon-color/c]
                          [height (>=/c 0) (default-icon-height)]
                          [style icon-style/c (default-icon-style)]) pict?
  (define gap (blank (* 1/16 height)))
  (define bar (bar-icon-pict color height style))
  (hc-append gap bar gap bar gap))

(defproc (stop-signs-icon-pict [height (>=/c 0) (default-icon-height)]
                               [style icon-style/c (default-icon-style)]) pict?
  (define h (* 2/3 height))
  (define s1 (inset (stop-sign-icon-pict h style) (* 1/4 h) (* 1/2 h) 0 0))
  (define s2 (inset (stop-sign-icon-pict h style) (* 1/8 h) (* 1/4 h) 0 0))
  (define s3 (stop-sign-icon-pict h style))
  (inset (lt-superimpose s3 s2 s1) (* 1/8 h) 0 (* 1/8 h) 0))

;; ===================================================================================================
;; Icon contructors

(define-syntax (define-make-colorized-icon stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(f-pict ...)  (map (λ (f) (format-id f "~a-pict" f))
                                       (syntax->list #'(f ...)))])
       (syntax/loc stx
         (begin (defproc (f [color icon-color/c]
                            [height (>=/c 0) (default-icon-height)]
                            [style icon-style/c (default-icon-style)]) (is-a?/c bitmap%)
                  (pict->bitmap (f-pict color height style)))
                ...)))]))

(define-syntax (define-make-icon stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(f-pict ...)  (map (λ (f) (format-id f "~a-pict" f))
                                       (syntax->list #'(f ...)))])
       (syntax/loc stx
         (begin (defproc (f [height (>=/c 0) (default-icon-height)]
                            [style icon-style/c (default-icon-style)]) (is-a?/c bitmap%)
                  (pict->bitmap (f-pict height style)))
                ...)))]))

(define-make-colorized-icon
  go-icon
  bar-icon
  back-icon
  stop-icon
  step-icon
  step-back-icon
  continue-icon
  continue-back-icon
  fast-forward-icon
  rewind-icon
  pause-icon
  check-icon
  x-icon
  disk-icon
  plus-icon
  up-arrow-icon
  down-arrow-icon
  left-arrow-icon
  right-arrow-icon
  )

(define-make-icon
  stop-sign-icon
  stop-signs-icon
  magnifying-glass-icon
  magnifying-glass-left-icon
  earth-icon
  moon-icon
  hash-quote-icon
  plt-logo
  planet-logo
  )
