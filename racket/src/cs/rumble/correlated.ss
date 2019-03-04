
;; This correlated-like layer is meant to be for just source locations
;; and properties that the compiler might inspect. It's exported as
;; `correlated?`, etc., from `racket/linklet`, but as `syntax?`, etc.
;; from '#%kernel.

;; Unlike the real syntax-object layer, a correlated object is not
;; required to have correlated objects inside.

(define-record correlated (e srcloc props))

(define/who datum->correlated
  (case-lambda
   [(ignored datum src props)
    (check who
           :test (or (not src) (correlated? src) (srcloc? src) (encoded-srcloc? src))
           :contract (string-append "(or #f syntax? srcloc?\n"
                                    "    (list/c any/c\n"
                                    "            (or/c exact-positive-integer? #f)\n"
                                    "            (or/c exact-nonnegative-integer? #f)\n"
                                    "            (or/c exact-positive-integer? #f)\n"
                                    "            (or/c exact-nonnegative-integer? #f))\n"
                                    "    (vector/c any/c\n"
                                    "              (or/c exact-positive-integer? #f)\n"
                                    "              (or/c exact-nonnegative-integer? #f)\n"
                                    "              (or/c exact-positive-integer? #f)\n"
                                    "              (or/c exact-nonnegative-integer? #f)))")
           src)
    (check who correlated? :or-false props)
    (if (correlated? datum)
        datum
        (make-correlated datum
                         (extract-srcloc src)
                         (if props
                             (correlated-props props)
                             empty-hasheq)))]
   [(ignored datum src) (datum->correlated ignored datum src #f)]
   [(ignored datum) (datum->correlated ignored datum #f #f)]))

(define (correlated->datum e)
  (cond
   [(correlated? e) (correlated->datum (correlated-e e))]
   [(pair? e) (let ([a (correlated->datum (car e))]
                    [d (correlated->datum (cdr e))])
                (if (and (eq? a (car e))
                         (eq? d (cdr e)))
                    e
                    (cons a d)))]
   [else e]))

(define/who (correlated-property-symbol-keys v)
  (check who correlated? v)
  (hash-map (correlated-props v) (lambda (k v) k)))

(define/who correlated-property
  (case-lambda
   [(v k)
    (check who correlated? v)
    (hash-ref (correlated-props v) k #f)]
   [(v k val)
    (check who correlated? v)
    (make-correlated (correlated-e v)
                     (correlated-srcloc v)
                     (hash-set (correlated-props v) k val))]))

(define/who (correlated-srcloc-field who v srcloc-x)
  (check who correlated? v)
  (let ([s (correlated-srcloc v)])
    (and s (srcloc-x s))))

(define (correlated-source v)
  (correlated-srcloc-field 'correlated-source v srcloc-source))
(define (correlated-line v)
  (correlated-srcloc-field 'correlated-line v srcloc-line))
(define (correlated-column v)
  (correlated-srcloc-field 'correlated-column v srcloc-column))
(define (correlated-position v)
  (correlated-srcloc-field 'correlated-position v srcloc-position))
(define (correlated-span v)
  (correlated-srcloc-field 'correlated-span v srcloc-span))

(define (encoded-srcloc? v)
  (or (and (list? v)
           (= (length v) 5)
           (srcloc-vector? (list->vector v)))
      (and (vector? v)
           (= (vector-length v) 5)
           (srcloc-vector? v))))

(define (srcloc-vector? v)
  (and (or (not (vector-ref v 1))
           (exact-positive-integer? (vector-ref v 1)))
       (or (not (vector-ref v 2))
           (exact-nonnegative-integer? (vector-ref v 2)))
       (or (not (vector-ref v 3))
           (exact-positive-integer? (vector-ref v 3)))
       (or (not (vector-ref v 4))
           (exact-nonnegative-integer? (vector-ref v 4)))))

(define (extract-srcloc src)
  (cond
   [(not src) #f]
   [(srcloc? src) src]
   [(correlated? src) (correlated-srcloc src)]
   [(vector? src) (|#%app|
                   srcloc
                   (vector-ref src 0)
                   (vector-ref src 1)
                   (vector-ref src 2)
                   (vector-ref src 3)
                   (vector-ref src 4))]
   [else (apply srcloc src)]))
