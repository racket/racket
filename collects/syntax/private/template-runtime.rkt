#lang racket/base
(require "../stx.rkt"
         unstable/struct)

(provide template-map-apply)

(define-struct ellipses (elem count rest) #:prefab #:omit-define-syntaxes)
(define-struct ellipses-quote (rest) #:prefab #:omit-define-syntaxes)
(define-struct prefab (key fields) #:prefab #:omit-define-syntaxes)

(define (stx-list->vector l)
  (list->vector
   (if (list? l)
       l
       (let loop ([l l])
         (cond
          [(null? l) null]
          [(pair? l) (cons (car l) (loop (cdr l)))]
          [(syntax? l) (loop (syntax-e l))])))))

(define (template-map-apply tmap d->s leaf->s leaf-datum pvar->s pcons ellipses-end data stx)
  (let loop ([tmap tmap][data data][stx stx][local-pcons pcons])
    (cond
     [(not tmap) (if (box? data)
                     (leaf->s (unbox data) stx)
                     (leaf-datum stx))]
     [(eq? tmap #t) (pvar->s data stx)]
     [(pair? tmap)
      (let ([a (loop (car tmap) 
                     (if (pair? data) (car data) (vector-ref data 1))
                     (stx-car stx)
                     pcons)]
            [b (loop (cdr tmap) 
                     (if (pair? data) (cdr data) (vector-ref data 2))
                     (stx-cdr stx)
                     local-pcons)])
        (if (vector? data)
            (d->s
             (vector-ref data 0)
             stx 
             (pcons a b))
            (local-pcons a b)))]
     [(vector? tmap)
      (d->s (car data)
            stx
            (stx-list->vector
             (loop (vector-ref tmap 0)
                   (cdr data)
                   (vector->list (syntax-e stx))
                   cons)))]
     [(box? tmap)
      (d->s (car data)
            stx
            (box
             (loop (unbox tmap)
                   (cdr data)
                   (unbox (syntax-e stx))
                   pcons)))]
     [(ellipses? tmap)
      (let ([prefix (map (lambda (e)
                           (loop (ellipses-elem tmap) 
                                 (if (pair? data) (car data) (vector-ref data 1))
                                 e
                                 local-pcons))
                         (syntax->list (stx-car stx)))]
            [rest (loop (ellipses-rest tmap)
                        (if (pair? data) (cdr data) (vector-ref data 2))
                        (stx-cdr stx)
                        local-pcons)])
        (let ([appended (let loop ([prefix prefix])
                          (if (null? prefix)
                              (ellipses-end rest)
                              (local-pcons (car prefix) (loop (cdr prefix)))))])
          (if (vector? data)
              (d->s (vector-ref data 0)
                    stx
                    appended)
              appended)))]
     [(ellipses-quote? tmap)
      (loop (ellipses-quote-rest tmap) data stx local-pcons)]
     [(prefab? tmap)
      (d->s (car data)
            stx
            (apply
             make-prefab-struct
             (prefab-struct-key (syntax-e stx))
             (loop (prefab-fields tmap)
                   (cdr data)
                   (struct->list (syntax-e stx))
                   pcons)))]
     [else (error "template-map-apply fallthrough")])))
