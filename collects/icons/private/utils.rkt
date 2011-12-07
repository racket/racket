#lang racket/base

(require racket/class racket/string racket/match racket/list
         racket/system
         racket/file
         slideshow/pict
         xml
         racket/contract
         unstable/latent-contract/defthing)

(provide (all-defined-out))

(define svg-icons-base-path (collection-path "icons/private/svg"))

;; Cairo is good at shrinking bitmaps by 1..1/2, but bad at shrinking bitmaps smaller
;; We therefore need icons rendered every 2x, from half the size typically needed up to the greatest
;; size typically needed
;; Make sure these are sorted
(define icon-heights '(16 32 64))
(define logo-heights '(32 64 128 256))

(define (icon-category-heights category)
  (cond [(equal? category "logo")  logo-heights]
        [else  icon-heights]))

(defthing icon-colors (listof (or/c symbol? #f)) #:document-value
  '(#f white black red green blue orange cyan purple))
(defcontract icon-color/c (apply or/c icon-colors))

(define diffuse-gradient-stops
  #hash((white . ((1 128 128 128) (1 255 255 255)))
        (black . ((1 0 0 0) (1 32 32 32)))
        (red . ((1 127 64 64) (1 255 0 0)))
        (green . ((1 47 86 42) (1 9 170 0)))
        (blue . ((1 64 66 123) (1 0 4 255)))
        (orange . ((1 169 96 64) (1 255 145 0)))
        (cyan . ((1 48 72 85) (1 0 162 255)))
        (purple . ((1 85 64 169) (1 128 0 255)))))

(define undershine-gradient-stops
  #hash((white . ((1 240 240 255) (0 255 255 255)))
        (black . ((1 160 160 180) (0 32 32 32)))
        (red . ((1 255 64 0) (0 255 0 0)))
        (green . ((1 128 255 64) (0 9 170 0)))
        (blue . ((1 64 128 255) (0 0 4 255)))
        (orange . ((1 255 128 64) (0 255 145 0)))
        (cyan . ((1 64 255 220) (0 0 162 255)))
        (purple . ((1 240 128 255) (0 128 0 255)))))

;; ===================================================================================================
;; Colorizing SVGs

(define (color->hex-string color)
  (string-append*
   "#"
   (for/list ([x  (in-list color)])
     (define s (number->string (max (min (inexact->exact (round x)) 255) 0) 16))
     (if (= 1 (string-length s)) (string-append "0" s) s))))

(define (replace-linear-gradient xexpr name stops)
  (let loop ([xexpr xexpr])
    (match xexpr
      [`(linearGradient ((id ,(regexp name))) ,_ ...)
       `(linearGradient
         ((id ,name))
         ,@(for/list ([stop  (in-list stops)]
                      [i     (in-naturals)])
             `(stop ((id ,(string-append name (number->string i)))
                     (offset ,(number->string i))
                     (style ,(format "stop-color:~a;stop-opacity:~a;"
                                     (color->hex-string (rest stop))
                                     (first stop)))))))]
      [(list xs ...)  (map loop xs)]
      [_  xexpr])))

(define (colorize-svg svg-doc diffuse-stops undershine-stops)
  (match-define (document prolog (app xml->xexpr old-element) misc) svg-doc)
  (let* ([element  old-element]
         [element  (replace-linear-gradient element "diffuseColorGradient" diffuse-stops)]
         [element  (replace-linear-gradient element "undershineGradient" undershine-stops)])
    (if (equal? element old-element) #f (document prolog (xexpr->xml element) misc))))

;; ===================================================================================================
;; Rendering

(define (svg-file? file)
  (and (regexp-match #rx"(?i:.*\\.svg)$" (path->string file)) #t))

(define inkscape.exe (find-executable-path "inkscape"))

(define (render/inkscape src-path dest-dir dest-file height)
  (define dest-path (build-path dest-dir dest-file))
  (printf "inkscape -e ~a -h ~a ~a~n" src-path height dest-path)
  (make-directory* dest-dir)
  (system* inkscape.exe
           "-e" dest-path
           "-h" (number->string height)
           src-path))

;; ===================================================================================================
;; Pict stuff

(define (pict-mirror-x p)
  (dc (λ (dc x y)
        (define-values (sx sy) (send dc get-scale))
        (define-values (ox oy) (send dc get-origin))
        (send dc set-scale (- sx) sy)
        (draw-pict p dc (- ox x (pict-width p)) y)
        (send dc set-scale sx sy))
      (pict-width p) (pict-height p)))
