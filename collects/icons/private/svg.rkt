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
(defcontract icon-style/c (or/c 'diffuse 'shiny))

(defparam toolbar-icon-height (>/c 0) 16)
(defparam default-icon-style (or/c 'diffuse 'shiny) 'diffuse)

(defproc (load-icon-pict [category string?] [name string?] [height (>=/c 0)]) pict?
  (define hs (icon-category-heights category))
  (define icon-height
    (let ([h  (for/first ([h  (in-list hs)] #:when (height . <= . h)) h)])
      (if h h (last hs))))
  (define icon-path
    (build-path svg-icons-base-path (format "~a/~a/~a.png" category icon-height name)))
  (scale (bitmap icon-path) (/ height icon-height)))

(defproc (load-icon [category string?] [name string?] [height (>=/c 0)]) (is-a?/c bitmap%)
  (pict->bitmap (load-icon-pict category name height)))

(defproc (format-icon-name [name string?] [color icon-color/c]
                           [style (or/c icon-style/c #f) (default-icon-style)]) string?
  (cond [(and color style)  (format "~a/~a-~a" color name style)]
        [color  (format "~a/~a" color name)]
        [style  (format "~a-~a" name style)]
        [else   (format "~a" name)]))

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
;; Common icon pict contructors

(defproc (go-icon-pict [color icon-color/c] [height (>=/c 0)]
                       [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "run" (format-icon-name "go" color style) height))

(defproc (bar-icon-pict [color icon-color/c] [height (>=/c 0)]
                        [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "run" (format-icon-name "bar" color style) height))

(defproc (back-icon-pict [color icon-color/c] [height (>=/c 0)]
                         [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "run" (format-icon-name "back" color style) height))

(defproc (stop-icon-pict [color icon-color/c] [height (>=/c 0)]
                         [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "run" (format-icon-name "stop" color style) height))

(defproc (step-icon-pict [color icon-color/c] [height (>=/c 0)]
                         [style icon-style/c (default-icon-style)]) pict?
  (hc-append (go-icon-pict color height style)
             (bar-icon-pict color height style)))

(defproc (step-back-icon-pict [color icon-color/c] [height (>=/c 0)]
                              [style icon-style/c (default-icon-style)]) pict?
  (hc-append (bar-icon-pict color height style)
             (back-icon-pict color height style)))

(defproc (continue-icon-pict [color icon-color/c] [height (>=/c 0)]
                             [style icon-style/c (default-icon-style)]) pict?
  (hc-append (bar-icon-pict color height style)
             (go-icon-pict color height style)))

(defproc (continue-back-icon-pict [color icon-color/c] [height (>=/c 0)]
                                  [style icon-style/c (default-icon-style)]) pict?
  (hc-append (back-icon-pict color height style)
             (bar-icon-pict color height style)))

(defproc (fast-forward-icon-pict [color icon-color/c] [height (>=/c 0)]
                                 [style icon-style/c (default-icon-style)]) pict?
  (define go (go-icon-pict color height style))
  (scale (hc-append go go) 3/4 1))

(defproc (rewind-icon-pict [color icon-color/c] [height (>=/c 0)]
                           [style icon-style/c (default-icon-style)]) pict?
  (define back (back-icon-pict color height style))
  (scale (hc-append back back) 3/4 1))

(defproc (pause-icon-pict [color icon-color/c] [height (>=/c 0)]
                          [style icon-style/c (default-icon-style)]) pict?
  (define gap (blank (* 1/16 height)))
  (define bar (bar-icon-pict color height style))
  (hc-append gap bar gap bar gap))

(defproc (stop-sign-icon-pict [color icon-color/c] [height (>=/c 0)]
                              [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "sign" (format-icon-name "stop-sign" color style) height))

(defproc (check-icon-pict [color icon-color/c] [height (>=/c 0)]
                          [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "check" (format-icon-name "check" color style) height))

(defproc (magnifying-glass-icon-pict [color icon-color/c] [height (>=/c 0)]
                                     [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "misc" (format-icon-name "magnifying-glass" color style) height))

(defproc (magnifying-glass-left-icon-pict [color icon-color/c] [height (>=/c 0)]
                                          [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "misc" (format-icon-name "magnifying-glass-left" color style) height))

;; Icons for tools and other special uses

(defproc (stop-signs-icon-pict [height (>=/c 0)] [style icon-style/c (default-icon-style)]) pict?
  (define h (* 2/3 height))
  (define s1 (inset (stop-sign-icon-pict 'red h style) (* 1/4 h) (* 1/2 h) 0 0))
  (define s2 (inset (stop-sign-icon-pict 'orange h style) (* 1/8 h) (* 1/4 h) 0 0))
  (define s3 (stop-sign-icon-pict 'cyan h style))
  (inset (lt-superimpose s3 s2 s1) (* 1/8 h) 0 (* 1/8 h) 0))

(defproc (macro-stepper-icon-pict [height (>=/c 0)] [style icon-style/c (default-icon-style)]) pict?
  (ht-append (load-icon-pict "misc" (format-icon-name "hash-quote" #f style) height)
             (step-icon-pict 'blue height style)))

(defproc (check-syntax-icon-pict [height (>=/c 0)] [style icon-style/c (default-icon-style)]) pict?
  (hb-append
   (magnifying-glass-left-icon-pict #f (* 7/8 height) style)
   (check-icon-pict 'green height style)))

(defproc (check-syntax-small-icon-pict [height (>=/c 0)] [style icon-style/c (default-icon-style)]
                                       ) pict?
  (rb-superimpose
   (hc-append (check-icon-pict 'green height style)
              (blank (* 1/4 height)))
   (magnifying-glass-icon-pict #f (* 3/4 height) style)))

(defproc (plt-logo-pict [height (>=/c 0)] [style icon-style/c (default-icon-style)]) pict?
  (load-icon-pict "logo" (format-icon-name "plt-logo" #f style) height))

;; ===================================================================================================
;; Common icon contructors

(define-syntax (define-wrapped-icon-fun stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(f-pict ...)  (map (λ (f) (format-id f "~a-pict" f))
                                       (syntax->list #'(f ...)))])
       (syntax/loc stx
         (begin (defproc (f [color icon-color/c] [height (>=/c 0)]
                            [style icon-style/c (default-icon-style)]) (is-a?/c bitmap%)
                  (pict->bitmap (f-pict color height style)))
                ...)))]))

(define-syntax (define-wrapped-icon-fun/no-color stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(f-pict ...)  (map (λ (f) (format-id f "~a-pict" f))
                                       (syntax->list #'(f ...)))])
       (syntax/loc stx
         (begin (defproc (f [height (>=/c 0)]
                            [style icon-style/c (default-icon-style)]) (is-a?/c bitmap%)
                  (pict->bitmap (f-pict height style)))
                ...)))]))

(define-wrapped-icon-fun
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
  stop-sign-icon
  check-icon
  magnifying-glass-icon
  magnifying-glass-left-icon)

(define-wrapped-icon-fun/no-color
  stop-signs-icon
  macro-stepper-icon
  check-syntax-icon
  check-syntax-small-icon
  plt-logo)
