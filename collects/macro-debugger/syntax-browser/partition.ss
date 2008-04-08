
#lang scheme/base
(require scheme/class
         syntax/boundmap
         syntax/stx
         "interfaces.ss")
(provide new-bound-partition
         partition%
         identifier=-choices)

(define (new-bound-partition)
  (new bound-partition%))

;; representative-symbol : symbol
;; Must be fresh---otherwise, using it could detect rename wraps 
;; instead of only marks.
;; For example, in (lambda (representative) representative)
(define representative-symbol
  (gensym 'representative))

;; unmarked-syntax : identifier
;; Has no marks---used to initialize bound partition so that 
;; unmarked syntax always gets colored "black"
(define unmarked-syntax
  (datum->syntax #f representative-symbol))

(define partition%
  (class* object% (partition<%>)
    (init relation)

    (define related? (or relation (lambda (a b) #f)))
    (field (rep=>num (make-hasheq)))
    (field (obj=>rep (make-weak-hasheq)))
    (field (reps null))
    (field (next-num 0))
    
    (define/public (get-partition obj)
      (rep->partition (obj->rep obj)))

    (define/public (same-partition? A B)
      (= (get-partition A) (get-partition B)))
    
    (define/private (obj->rep obj)
      (hash-ref obj=>rep obj (lambda () (obj->rep* obj))))
    
    (define/public (count)
      next-num)

    (define/private (obj->rep* obj)
      (let loop ([reps reps])
        (cond [(null? reps)
               (new-rep obj)]
              [(related? obj (car reps))
               (hash-set! obj=>rep obj (car reps))
               (car reps)]
              [else
               (loop (cdr reps))])))

    (define/private (new-rep rep)
      (hash-set! rep=>num rep next-num)
      (set! next-num (add1 next-num))
      (set! reps (cons rep reps))
      rep)
    
    (define/private (rep->partition rep)
      (hash-ref rep=>num rep))

    ;; Nearly useless as it stands
    (define/public (dump)
      (hash-for-each 
       rep=>num
       (lambda (k v)
         (printf "~s => ~s~n" k v))))

    (get-partition unmarked-syntax)
    (super-new)
    ))

;; bound-partition%
(define bound-partition%
  (class* object% (partition<%>)
    ;; numbers : bound-identifier-mapping[identifier => number]
    (define numbers (make-bound-identifier-mapping))
    (define next-number 0)
    
    (define/public (get-partition stx)
      (let* ([r (representative stx)]
             [n (bound-identifier-mapping-get numbers r (lambda _ #f))])
        (or n
            (begin0 next-number
              (bound-identifier-mapping-put! numbers r next-number)
              #;(printf "primary partition new stx:~n~s~n~s~n" stx (syntax->datum stx))
              (set! next-number (add1 next-number))))))
    
    (define/public (same-partition? a b)
      (= (get-partition a) (get-partition b)))
    
    (define/public (count)
      next-number)
    
    (define/private (representative stx)
      (datum->syntax stx representative-symbol))

    (get-partition unmarked-syntax)
    (super-new)))

;; Different identifier relations for highlighting.

(define (lift/rep id=?)
  (lambda (A B)
    (let ([ra (datum->syntax A representative-symbol)]
          [rb (datum->syntax B representative-symbol)])
      (id=? ra rb))))

(define (lift id=?)
  (lambda (A B)
    (and (identifier? A) (identifier? B) (id=? A B))))

;; id:same-marks? : syntax syntax -> boolean
(define id:same-marks?
  (lift/rep bound-identifier=?))

;; id:X-module=? : identifier identifier -> boolean
;; If both module-imported, do they come from the same module?
;; If both top-bound, then same source.
(define (id:source-module=? a b)
  (let ([ba (identifier-binding a)]
        [bb (identifier-binding b)])
    (cond [(or (eq? 'lexical ba) (eq? 'lexical bb))
           (free-identifier=? a b)]
          [(and (not ba) (not bb))
           #t]
          [(or (not ba) (not bb))
           #f]
          [else
           (eq? (car ba) (car bb))])))
(define (id:nominal-module=? A B)
  (let ([ba (identifier-binding A)]
        [bb (identifier-binding B)])
    (cond [(or (eq? 'lexical ba) (eq? 'lexical bb))
           (free-identifier=? A B)]
          [(or (not ba) (not bb))
           (and (not ba) (not bb))]
          [else (eq? (caddr ba) (caddr bb))])))

(define (symbolic-identifier=? A B)
  (eq? (syntax-e A) (syntax-e B)))

(define identifier=-choices
  (make-parameter
   `(("<nothing>" . #f)
     ("bound-identifier=?"  . ,bound-identifier=?)
     ("free-identifier=?" . ,free-identifier=?)
     ("module-or-top-identifier=?" . ,module-or-top-identifier=?)
     ("symbolic-identifier=?" . ,symbolic-identifier=?)
     ("same source module" . ,id:source-module=?)
     ("same nominal module" . ,id:nominal-module=?))))
