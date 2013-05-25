#lang racket/base
(require slideshow/base pict
         racket/contract/base racket/list racket/match
         racket/stxparam
         (for-syntax racket/base racket/list racket/set syntax/parse)
         "pict.rkt")
(provide (all-from-out "pict.rkt"))

(define-syntax-rule (define-with-parameter name parameter)
  (define-syntax-rule (name value body (... ...))
    (parameterize ([parameter value]) body (... ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter with-size current-font-size)
(define-syntax-rule (with-scale scale expr)
  (with-size (inexact->exact (ceiling (* scale (current-font-size)))) expr))
(define-syntax-rule (define-scale name scale)
  (define-syntax-rule (name expr) (with-scale scale expr)))
(define-scale big 3/2)
(define-scale small 2/3)

(define-with-parameter with-font current-main-font)
(define-syntax-rule (with-style style expr)
  (with-font (cons style (current-main-font)) expr))
(define-syntax-rule (define-style name style)
  (define-syntax-rule (name expr) (with-style style expr)))
(define-style bold 'bold)
(define-style italic 'italic)
(define-style subscript 'subscript)
(define-style superscript 'superscript)
(define-style caps 'caps)

(provide with-size
         with-scale
         big
         small

         with-font
         with-style
         bold
         italic
         subscript
         superscript
         caps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide / Paragraph Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter column current-para-width)

(define (columns . picts)
  (apply hc-append gap-size (map baseless picts)))

(define (column-size n [r (/ n)])
  (* r (- (current-para-width) (* (sub1 n) gap-size))))

(define-syntax-rule (two-columns a b)
  (columns (column (column-size 2) a)
           (column (column-size 2) b)))

(define (mini-slide . picts)
  (apply vc-append gap-size picts))

(provide column
         columns
         column-size
         two-columns
         mini-slide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define =!
  (case-lambda
    [(n) n]
    [(n . ns)
     (if (apply = n ns)
         n
         (error '=! "not all equal: ~a" (cons n ns)))]))

(define (elem->pict elem)
  (if (string? elem) (t elem) elem))

(define (tabular #:gap [gap gap-size]
                 #:vgap [vgap gap]
                 #:hgap [hgap gap]
                 #:align [align lbl-superimpose]
                 #:halign [halign align]
                 #:valign [valign align]
                 . cells)
  (let* ([rows (length cells)]
         [cols (apply =! (map length cells))]
         [picts (map elem->pict (append* cells))]
         [haligns (for/list ([i (in-range 0 cols)]) halign)]
         [valigns (for/list ([i (in-range 0 rows)]) valign)]
         [hseps (for/list ([i (in-range 1 cols)]) hgap)]
         [vseps (for/list ([i (in-range 1 rows)]) vgap)])
    (table cols picts haligns valigns hseps vseps)))

(define (matrixof c)
  (and/c (listof (listof c))
         (flat-named-contract "matrix"
           (match-lambda
             [(list) #t]
             [(list _) #t]
             [(list xs ...) (apply = (map length xs))]))))

(provide/contract
 [tabular (->* []
               [#:gap natural-number/c
                #:hgap natural-number/c
                #:vgap natural-number/c
                #:align (->* [] [] #:rest (listof pict?) pict?)
                #:halign (->* [] [] #:rest (listof pict?) pict?)
                #:valign (->* [] [] #:rest (listof pict?) pict?)]
               #:rest (matrixof (or/c string? pict?))
               pict?)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide Staging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (slide/staged [name ...] body ...)
  (staged [name ...] (slide body ...)))

(provide slide/staged)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Misc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (blank-line)
  (blank 0 (current-font-size)))

(provide/contract
 [blank-line (-> pict?)])

;; Revealing slides. Similar to staging slides, can probably be implemented
;; in terms of staged slides.

(provide reveal revealing-slide items-slide)

(define-syntax-parameter reveal (lambda (stx) (raise-syntax-error 'reveal "reveal")))

(define-for-syntax (find-reveals stx)
  (syntax-case stx (reveal)
    [(reveal n stuff ...)
     (set-add (find-reveals #'(stuff ...)) (syntax-e #'n))]
    [(x rest ...) (set-union (find-reveals #'x) (find-reveals #'(rest ...)))]
    [else (set)]))

(define-for-syntax (largest stuff)
  (define all (sort (set->list stuff) >))
  (if (null? all) -1 (first all)))
   
;; reveals elements one at a time but uses ghost so that the slide doesn't
;; change its layout during each reveal.
;; for each element you want to reveal wrap the element in a (reveal):
;;   (reveal N element) 
;;
;; where N is the order of the reveal you want. 0 will be shown first, then 1, 2, ...
(define-syntax (revealing-slide stx)
  (syntax-case stx ()
    [(_ stuff ...)
     (let ()
       (define max-reveals (largest (find-reveals stx)))
       (define slides (for/list ([i (add1 max-reveals)])
                        (with-syntax ([i i])
                          (syntax (syntax-parameterize
                                    ([reveal (syntax-rules ()
                                               [(reveal n pict)
                                                (show pict (>= i n))]
                                               [(reveal n) (>= i n)])])
                                    (slide stuff ...))))))
       (with-syntax ([(slides ...) slides])
         #'(begin slides ...)))]))

;; (items-slide ("a" "b" "c") #:title "whatever" (reveal 0 ...) (reveal 1 ...) (reveal 2 ...)
;; this will show a, b, c on the left side with one of them highlighted at a time.
;; the first element, a, will be synchronized with showing the first reveal, then
;; the second element, b, will be synchronized with the second reveal, etc.
(define-syntax (items-slide stx)
  (syntax-parse stx
    [(_ (item ...) (~seq keyword-name:keyword keyword-value:expr) ... stuff:expr ...)
     (let ()
       (define max-reveals (length (syntax->list #'(item ...))))
       (define slides (for/list ([i max-reveals])
                        (with-syntax ([i i]
                                      [(keywords ...)
                                       (apply append (syntax->list
                                                       #'((keyword-name keyword-value) ...)))])
                          (syntax (syntax-parameterize ([reveal (syntax-rules ()
                                                                  [(reveal n pict)
                                                                   (show pict (= i n))])])
                                    (let ([show-items
                                            (for/fold ([start (blank)])
                                                      ([in '(item ...)]
                                                       [current (in-naturals)])
                                                      (vr-append start
                                                                 (if (= current i)
                                                                   (text in null (current-font-size))
                                                                   (colorize/alpha (text in null (- (current-font-size) 3))

                                                                             0 0 0 0.3))
                                                                 (blank 1 10)))]
                                          [data (for/fold ([start (blank)])
                                                          ([thing (list stuff ...)])
                                                          (lt-superimpose start thing))])
                                      (slide keywords ...
                                             (ht-append
                                               show-items
                                               (blank 50 1)
                                               data))))))))
       (with-syntax ([(slides ...) slides])
         #'(begin slides ...)))]))
