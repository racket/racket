
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
    [(_ (id arg ...) ...)
     #'(begin
         (define/who (id arg ...)
           (check who extflonum? arg) ...
           (raise-unsupported-error who))
         ...)]))

(define-extfl-ids
  (extfl* a b)
  (extfl+ a b)
  (extfl- a b)
  (extfl->exact a)
  (extfl->exact-integer a)
  (extfl->floating-point-bytes a)
  (extfl->fx a)
  (extfl->inexact a)
  (extfl/ a b)
  (extfl< a b)
  (extfl<= a b)
  (extfl= a b)
  (extfl> a b)
  (extfl>= a b)
  (extflabs a)
  (extflacos a)
  (extflasin a)
  (extflatan a)
  (extflceiling a)
  (extflcos a)
  (extflexp a)
  (extflexpt a b)
  (extflfloor a)
  (extfllog a)
  (extflmax a b)
  (extflmin a b)
  (extflround a)
  (extflsin a)
  (extflsqrt a)
  (extfltan a)
  (extfltruncate a)

  (unsafe-extfl* a b)
  (unsafe-extfl+ a b)
  (unsafe-extfl- a b)
  (unsafe-extfl/ a b)
  (unsafe-extfl< a b)
  (unsafe-extfl<= a b)
  (unsafe-extfl= a b)
  (unsafe-extfl> a b)
  (unsafe-extfl>= a b)
  (unsafe-extflabs a)
  (unsafe-extflmax a b)
  (unsafe-extflmin a b)
  (unsafe-extflsqrt a)
  (unsafe-extfl->fx a)
  (unsafe-fx->extfl a))

(define/who (->extfl a)
  (check who exact-integer? a)
  (raise-unsupported-error who))

(define/who (fx->extfl a)
  (check who fixnum? a)
  (raise-unsupported-error who))

(define/who (real->extfl a)
  (check who real? a)
  (raise-unsupported-error who))

(define/who floating-point-bytes->extfl
  (case-lambda
   [(bstr big-endian? start end)
    (check who bytes? bstr)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (case (- end start)
      [(10) (raise-unsupported-error who)]
      [else
       (raise-arguments-error who
                              "length is not 10 bytes"
                              "length" (- end start))])]
   [(bstr)
    (floating-point-bytes->extfl bstr (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian?)
    (floating-point-bytes->extfl bstr big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian? start)
    (floating-point-bytes->extfl bstr big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

(define/who make-extflvector
  (case-lambda
   [(len)
    (check who exact-nonnegative-integer? len)
    (raise-unsupported-error who)]
   [(len v)
    (check who exact-nonnegative-integer? len)
    (check who extflonum? v)
    (raise-unsupported-error who)]))

(define/who make-shared-extflvector
  (case-lambda
   [(len)
    (check who exact-nonnegative-integer? len)
    (raise-unsupported-error who)]
   [(len v)
    (check who exact-nonnegative-integer? len)
    (check who extflonum? v)
    (raise-unsupported-error who)]))

(define/who (extflvector . args)
  (for-each (lambda (a) (check who extflonum? a))
            args)
  (raise-unsupported-error who))

(define/who (shared-extflvector . args)
  (for-each (lambda (a) (check who extflonum? a))
            args)
  (raise-unsupported-error who))

(define/who (extflvector-length a)
  (check who extflvector? a)
  ;; won't get here
  (raise-unsupported-error who))

(define/who (extflvector-ref a i)
  (check who extflvector? a)
  ;; won't get here
  (raise-unsupported-error who))

(define/who (extflvector-set! a i v)
  (check who extflvector? a)
  ;; won't get here
  (raise-unsupported-error who))

(define/who (unsafe-extflvector-length a) (extflvector-length a))
(define/who (unsafe-extflvector-ref a i) (extflvector-ref a i))
(define/who (unsafe-extflvector-set! a i v) (extflvector-set! a i v))
