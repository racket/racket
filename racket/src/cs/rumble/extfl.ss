
(define-record-type extflonum
  (fields str)
  (nongenerative #{extflonum lb32cq34kbljz9rpowkzge-0}))

(define (set-extflonum-print!)
  (record-writer (record-type-descriptor extflonum)
                 (lambda (e p wr)
                   (#%display (extflonum-str e) p))))

;; used by `string->number`
(define (extflonum-string? s)
  ;; It's an extflonum if there's any #\t
  (let loop ([i (string-length s)])
    (and (fx> i 0)
         (let ([i (sub1 i)])
           (let ([c (string-ref s i)])
             (or (char=? #\t c) (char=? #\T c)
                 (loop i)))))))

(define (extflonum-available?) #f)
(define (extflvector? v) #f)

(define-syntax (define-extfl-ids stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(begin
         (define (id v)
           (raise-unsupported-error 'id))
         ...)]))

(define-extfl-ids
  extfl*
  extfl+
  extfl-
  ->extfl
  extfl->exact
  extfl->exact-integer
  extfl->floating-point-bytes
  extfl->fx
  extfl->inexact
  extfl/
  extfl<
  extfl<=
  extfl=
  extfl>
  extfl>=
  extflabs
  extflacos
  extflasin
  extflatan
  extflceiling
  extflcos
  extflexp
  extflexpt
  floating-point-bytes->extfl
  extflfloor
  fx->extfl
  extfllog
  make-shared-extflvector
  make-extflvector
  extflmax
  extflmin
  real->extfl
  extflround
  shared-extflvector
  extflsin
  extflsqrt
  extfltan
  extfltruncate
  extflvector
  extflvector-length
  extflvector-ref
  extflvector-set!

  unsafe-extfl*
  unsafe-extfl+
  unsafe-extfl-
  unsafe-extfl/
  unsafe-extfl<
  unsafe-extfl<=
  unsafe-extfl=
  unsafe-extfl>
  unsafe-extfl>=
  unsafe-extflabs
  unsafe-extflmax
  unsafe-extflmin
  unsafe-extflsqrt
  unsafe-extfl->fx
  unsafe-fx->extfl
  unsafe-extflvector-length
  unsafe-extflvector-ref
  unsafe-extflvector-set!)
