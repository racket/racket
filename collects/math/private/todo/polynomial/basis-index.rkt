#lang typed/racket/base

(require racket/fixnum
         racket/list
         "../unsafe.rkt")

(provide Basis-Index
         integer->basis-index
         list->basis-index
         basis-index->list
         basis-index-degree
         basis-index+
         basis-index<)

(struct: basis-indexes ([degree : Natural] [list : (Listof Natural)])
  #:transparent)

(: integer->basis-index (Integer -> Natural))
(define (integer->basis-index t)
  (if (t . >= . 0) t (raise-argument-error 'integer->basis-index "Natural" t)))

(: integer->basis-indexes (Integer -> basis-indexes))
(define (integer->basis-indexes t)
  (cond [(t . >= . 0)  (basis-indexes t (list t))]
        [else  (raise-argument-error 'integer->basis-indexes "Natural" t)]))

(: basis-indexes+ (basis-indexes basis-indexes -> basis-indexes))
(define (basis-indexes+ m0 m1)
  (basis-indexes
   (+ (basis-indexes-degree m0)
      (basis-indexes-degree m1))
   (let loop ([t0s  (basis-indexes-list m0)]
              [t1s  (basis-indexes-list m1)])
     (cond [(empty? t0s)  t1s]
           [(empty? t1s)  t0s]
           [else  (list* (+ (first t0s) (first t1s))
                         (loop (rest t0s) (rest t1s)))]))))

(: basis-indexes< (basis-indexes basis-indexes -> Boolean))
(define (basis-indexes< m0 m1)
  (define d0 (basis-indexes-degree m0))
  (define d1 (basis-indexes-degree m1))
  (or (d0 . < . d1)
      (and (= d0 d1)
           (let loop ([t0s  (basis-indexes-list m0)]
                      [t1s  (basis-indexes-list m1)])
             (cond [(empty? t0s)  #f]
                   [(empty? t1s)  #f]
                   [else
                    (define t0 (first t0s))
                    (define t1 (first t1s))
                    (cond [(t0 . < . t1)  #t]
                          [(t0 . > . t1)  #f]
                          [else  (loop (rest t0s) (rest t1s))])])))))

;; ===================================================================================================

(define-type Basis-Index-In (U Integer basis-indexes))
(define-type Basis-Index (U Natural basis-indexes))

(: list->basis-index ((Listof Integer) -> Basis-Index))
(define (list->basis-index orig-ts)
  (cond [(empty? orig-ts)  0]
        [(empty? (rest orig-ts))  (integer->basis-index (first orig-ts))]
        [else
         (let loop ([ts orig-ts]
                    [#{acc : (Listof Natural)} empty]
                    [#{d : Natural} 0])
           (cond [(empty? ts)
                  ;; Remove trailing zeros (they're in the front because `acc' is reversed)
                  (let loop ([ts acc])
                    (cond [(empty? ts)  0]
                          [(empty? (rest ts))  (first ts)]
                          [else
                           (define t (first ts))
                           (cond [(= t 0)  (loop (rest ts))]
                                 [else  (basis-indexes d (reverse ts))])]))]
                 [else
                  (define t (first ts))
                  (cond [(t . < . 0)  (raise-argument-error 'list->basis-index "Natural" orig-ts)]
                        [else  (loop (rest ts) (list* t acc) (+ d t))])]))]))

(: basis-index-degree (Basis-Index-In -> Natural))
(define (basis-index-degree m)
  (cond [(basis-indexes? m)  (basis-indexes-degree m)]
        [(m . < . 0)  (raise-argument-error 'basis-index-degree "(U Natural basis-indexes)" m)]
        [else  m]))

(: basis-index->list (Basis-Index-In -> (Listof Natural)))
(define (basis-index->list m)
  (cond [(basis-indexes? m)  (basis-indexes-list m)]
        [(m . < . 0)  (raise-argument-error 'basis-index->list "(U Natural basis-indexes)" m)]
        [else  (list m)]))

(: basis-index+ (Basis-Index-In Basis-Index-In -> Basis-Index))
(define (basis-index+ m0 m1)
  (if (basis-indexes? m0)
      (if (basis-indexes? m1)
          (basis-indexes+ m0 m1)
          (basis-indexes+ m0 (integer->basis-indexes m1)))
      (if (basis-indexes? m1)
          (basis-indexes+ (integer->basis-indexes m0) m1)
          (+ (integer->basis-index m0)
             (integer->basis-index m1)))))

(: basis-index< (Basis-Index-In Basis-Index-In -> Boolean))
(define (basis-index< m0 m1)
  (if (basis-indexes? m0)
      (if (basis-indexes? m1)
          (basis-indexes< m0 m1)
          (basis-indexes< m0 (integer->basis-indexes m1)))
      (if (basis-indexes? m1)
          (basis-indexes< (integer->basis-indexes m0) m1)
          (< (integer->basis-index m0)
             (integer->basis-index m1)))))
