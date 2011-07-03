#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/contract)
         racket/list
         racket/contract
         racket/gui/base
         slideshow/base
         slideshow/pict
         "ppict.rkt")

;; ============================================================
;; Progressive Slides

(struct p:elem (value))
(struct p:out ())
(struct p:go (placer))

(define pslide-base-pict
  (make-parameter (lambda () (blank client-w client-h))))

(define pslide-default-placer
  (make-parameter (coord 1/2 1/2 'cc)))

;; pslide* : (U p:elem p:out p:go) ... -> void
(define (pslide* . elems)
  (let ([gochunks
         (get-gochunks (p:go (pslide-default-placer))
                       (append elems (list (p:out))))])
    (do-gochunks ((pslide-base-pict)) gochunks)
    (void)))

;; ----

;; A gochunk is (cons p:go (listof (U p:elem p:next)))

;; get-gochunks : p:go (listof (U p:elem p:out p:go)) -> (listof gochunk)
(define (get-gochunks init-go elems)
  (define (loop init-go elems)
    (cond [(and (pair? elems) (p:go? (car elems)))
           (loop (car elems) (cdr elems))]
          [(pair? elems)
           (let-values ([(chunk tail) (split-until p:go? elems)])
             (cons (cons init-go chunk)
                   (if (pair? tail)
                       (loop (car tail) (cdr tail))
                       null)))]
          [(null? elems) null]))
  (loop init-go elems))

(define (do-gochunks base gochunks)
  (for/fold ([base base]) ([gochunk (in-list gochunks)])
    (let* ([placer (p:go-placer (car gochunk))]
           [chunk (cdr gochunk)]
           [base (ppict-go base placer)])
      (do-chunk base chunk))))

;; do-chunk : ppict (listof (U p:elem p:out)) -> ppict
(define (do-chunk base chunk)
  (let ([elem-chunks
         ;; (listof (listof pict?))
         ;;   length is N+1, where N is number of (p:out) in chunk
         ;;   ghosted before visible
         (let elab ([chunk chunk])
           (cond [(and (pair? chunk) (p:out? (car chunk)))
                  (let ([elab-rest (elab (cdr chunk))])
                    (cons (map ghost* (car elab-rest))
                          elab-rest))]
                 [(and (pair? chunk) (p:elem? (car chunk)))
                  (for/list ([elem-chunk (in-list (elab (cdr chunk)))])
                    (cons (p:elem-value (car chunk))
                          elem-chunk))]
                 [(null? chunk)
                  (list null)]))])
    (let out-loop ([chunks elem-chunks])
      (cond [(null? (cdr chunks))
             (apply ppict-add base (car chunks))]
            [else
             (slide (apply ppict-add base (car chunks)))
             (out-loop (cdr chunks))]))))

;; ----

(define (split-until stop? elems)
  (let loop ([elems elems] [rprefix null])
    (cond [(and (pair? elems) (stop? (car elems)))
           (values (reverse rprefix) elems)]
          [(pair? elems)
           (loop (cdr elems) (cons (car elems) rprefix))]
          [(null? elems)
           (values (reverse rprefix) null)])))

(define (ghost* x)
  (if (pict? x) (ghost x) x))

;; ----

(define-syntax (pslide stx)

  (define-splicing-syntax-class fragment
    #:description "pslide fragment"
    (pattern (~seq #:go pl)
             #:declare pl (expr/c #'placer? #:name "argument to #:go")
             #:with code #'(p:go pl.c))
    (pattern (~seq #:next)
             #:with code #'(p:out))
    (pattern (~seq e)
             #:declare e (expr/c #'(or/c pict? real? #f) #:name "element")
             #:with code #'(p:elem e.c)))

  (syntax-parse stx
    [(_ p:fragment ...)
     #'(pslide* p.code ...)]))

;; ---- Example ----

#|
(pslide* (p:elem (t "this is the time to remember"))
         (p:out)
         (p:elem (t "because it will not last forever"))
         (p:out)
         (p:go (coord 1 0 'rt))
         (p:elem 50)
         (p:elem #f)
         (p:elem (t "this is the time"))
         (p:elem (t "because it will")))
|#


;; ============================================================
;; Exports

(provide/contract
 [pslide-base-pict
  (parameter/c (-> pict?))]
 [pslide-default-placer
  (parameter/c placer?)])

(provide pslide)
