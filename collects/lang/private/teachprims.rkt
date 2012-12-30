#lang scheme 

#| tests are at plt/collects/tests/mzscheme/
collects/tests/mzscheme/beginner.rkt
                    .../beginner-abbr.rkt
                    .../intermediate.rkt
                    .../intermediate-lambda.rkt
                    .../advanced.rkt

Each one has to run separately, since they mangle the top-level
namespace.
|#


(require mzlib/list 
         mzlib/math
         mzlib/etc
	 deinprogramm/signature/signature
	 deinprogramm/signature/signature-english)

(define-syntax (define-teach stx)
  (syntax-case stx ()
    [(_ level id expr)
     (with-syntax ([level-id (datum->syntax
                              (syntax id)
                              (string->symbol
                               (format "~a-~a"
                                       (syntax->datum (syntax level))
                                       (syntax->datum (syntax id))))
                              (syntax id))])
       (syntax (define level-id
                 (let ([id expr])
                   id))))]))

(provide define-teach)

(define-teach beginner list?
  (lambda (x)
    (or (null? x) (pair? x))))

(define cyclic-list? beginner-list?)
;; don't need a special  anymore, since we just check for pairs:

(define (build-arg-list args)
  (let loop ([args args][n 0])
    (cond
      [(null? args) ""]
      [(= n 5) " ..."]
      [else
       (format " ~e~a" (car args) (loop (cdr args) (add1 n)))])))

(define (mk-check-second ok? type)
  (lambda (prim-name a b)
    (unless (ok? b)
      (raise
       (make-exn:fail:contract
        (format "~a: second argument must be ~a ~a, but received ~e and ~e"
                prim-name (a-or-an type) type
                a b)
        (current-continuation-marks))))))

(define check-second 
  (mk-check-second beginner-list? "list"))

(define check-second/cycle
  (mk-check-second cyclic-list? "list or cyclic list"))

(define (mk-check-last ok? type)
  (lambda (prim-name args)
    (let loop ([l args])
      (cond
        [(null? l) (void)]
        [(null? (cdr l))
         (let ([last (car l)])
           (unless (ok? last)
             (raise
              (make-exn:fail:contract
               (format "~a: last argument must be ~a ~a, but received ~e"
                       prim-name (a-or-an type) type
                       last)
               (current-continuation-marks)))))]
        [else (loop (cdr l))]))))

(define check-last
  (mk-check-last beginner-list? "list"))

(define check-last/cycle
  (mk-check-last cyclic-list? "list or cyclic list"))

(define (check-three a b c prim-name ok1? 1type ok2? 2type ok3? 3type)
  (let ([bad
         (lambda (v which type)
           (raise
            (make-exn:fail:contract
             (format "~a: ~a argument must be of a ~a, given ~e, ~e, and ~e"
                     prim-name which type
                     a b c)
             (current-continuation-marks))))])
    (unless (ok1? a) (bad a "first"  1type))
    (unless (ok2? b) (bad b "second" 2type))
    (unless (ok3? c) (bad c "third"  3type))))

(define (positive-real? v)
  (and (real? v) (>= v 0)))

(define (false? v) (eq? v #f))

(define-teach beginner not
  (lambda (a)
    (unless (boolean? a)
      (raise
       (make-exn:fail:contract
        (format "not: expected either true or false; given ~e" a)
        (current-continuation-marks))))
    (not a)))

(define-teach beginner random 
  (lambda (a)
    (random a)))

(define-teach beginner +
  (lambda (a b . args)
    (apply + a b args)))

(define-teach beginner /
  (lambda (a b . args)
    (apply / a b args)))

(define-teach beginner *
  (lambda (a b . args)
    (apply * a b args)))

(define-teach beginner sqr
  (lambda (a)
    (unless (number? a)
      (raise
       (make-exn:fail:contract
        (format "sqr: expected a number; given ~e" a)
        (current-continuation-marks))))
    (sqr a)))

(define-teach beginner memq
  (lambda (a b)
    (check-second 'memq a b)
    (not (boolean? (memq a b)))))

(define-teach beginner memq?
  (lambda (a b)
    (check-second 'memq? a b)
    (not (boolean? (memq a b)))))

(define-teach beginner member 
  (lambda (a b)
    (check-second 'member a b)
    (not (boolean? (member a b)))))

(define-teach beginner member? 
  (lambda (a b)
    (check-second 'member? a b)
    (not (boolean? (member a b)))))

(define-teach beginner remove
  (lambda (a b)
    (check-second 'remove a b)
    (remove a b)))

(define-teach beginner cons 
  (lambda (a b)
    (check-second 'cons a b)
    (cons a b)))

(define-teach beginner car
  (lambda (p) (checked-car p)))

(define-teach beginner first
  (lambda (p) (checked-first p)))

(define-teach beginner cdr
  (lambda (p) (checked-cdr p)))

(define-teach beginner rest
  (lambda (p) (checked-rest p)))

(define-teach beginner list*
  (lambda x
    (check-last 'list* x)
    (apply list* x)))

(define-teach beginner range
  (lambda (start end step)
    (cerr 'range (real? start) "real" start)
    (cerr 'range (real? end) "real" end)
    (cerr 'range (real? step) "real" step)
    (range start end step)))
    
(define-teach beginner append
  (lambda (a b . x)
    (check-last 'append (cons a (cons b x)))
    (apply append a b x)))

(define-teach intermediate append
  (lambda x
    (if (null? x)
        null
        (begin
          (check-last 'append x)
          (apply append x)))))

(define-teach beginner error
  (lambda stuff0
    (define-values (f stuff1)
      (if (and (cons? stuff0) (symbol? (first stuff0)))
          (values (first stuff0) (rest stuff0))
          (values false stuff0)))
    (error (apply
            string-append
            (if f (format "~a: " f) "")
            (for/list ([ele (in-list stuff1)])
              (if (string? ele)
                  ele
                  (format "~e" ele)))))))

(define-teach beginner struct?
  (lambda (x)
    (not (or (number? x)
             (boolean? x)
             (empty? x)
             (pair? x)
             (symbol? x)
             (string? x)
             (procedure? x)
             (vector? x)
             (char? x)
             (port? x)
             (eof-object? x)
             (void? x)))))

(define-teach beginner exit
  (lambda () (exit)))

(define (make-union-equal!?)
  (let* ([ht (make-hasheq)] ;; make-hash
         [union-find (lambda (a)
                       (let loop ([prev a]
                                  [prev-prev a])
                         (let ([v (hash-ref ht prev #f)])
                           (if v
                               (loop v prev)
                               (begin
                                 (let loop ([a a])
                                   (unless (eq? a prev-prev)
                                     (let ([v (hash-ref ht a)])
                                       (hash-set! ht a prev)
                                       (loop v))))
                                 prev)))))])
    (lambda (a b)
      (let ([a (union-find a)]
	    [b (union-find b)])
	(if (eq? a b)
	    #t
	    (begin
	      (hash-set! ht b a)
	      #f))))))

(define (tequal? x y epsilon)
  (let ([union-equal!? (make-union-equal!?)]
	[fail (lambda (fmt arg)
		(raise (make-exn:fail:contract (if (or (eq? arg x)
						       (eq? arg y))
						   (format fmt arg)
						   (format "~a (originally comparing ~e and ~e)" (format fmt arg) x y))
					       (current-continuation-marks))))])
    (let ? ([a x][b y])
      (cond
        [(number? a)
         (and (number? b)
              (beginner-=~ a b epsilon))]
	[(procedure? a)
	 (fail "first argument of equality cannot be a function, given ~e" a)]
	[(procedure? b)
	 (fail "second argument of equality cannot be a function, given ~e" b)]
        [(union-equal!? a b) #t]
        [else (equal?/recur a b ?)]))))

(define (teach-equal? x y)

  (let ([fail (lambda (fmt arg)
		(raise (make-exn:fail:contract (if (or (eq? arg x)
						       (eq? arg y))
						   (format fmt arg)
						   (format "~a (originally comparing ~e and ~e)" (format fmt arg) x y))
					       (current-continuation-marks))))]
	[union-equal!? (make-union-equal!?)])
	      
    (let recur ([a x] [b y])
      (cond
       [(procedure? a)
	(fail "first argument of equality cannot be a function, given ~e" a)]
       [(procedure? b)
	(fail "second argument of equality cannot be a function, given ~e" b)]
       [(and (number? a)
	     (inexact? a))
	(fail "first argument of equality cannot be an inexact number, given ~e" a)]
       [(and (number? b)
	     (inexact? b))
	(fail "first argument of equality cannot be an inexact number, given ~e" b)]
       [(union-equal!? a b) #t]
       [else
	(equal?/recur a b recur)]))))

(define-teach beginner equal?
  (lambda (a b)
    (equal? a b)))

(define-teach beginner =~
  (lambda (a b c)
    (check-three a b c '=~ number? 'number number? 'number positive-real? 'non-negative-real)
    (<= (magnitude (- a b)) c)))

(define-teach beginner equal~?
  (lambda (a b c)
    (check-three a b c 'equal~? values 'any values 'any positive-real? 'non-negative-real)
    (tequal? a b c)))

(define (hocheck name fmt-str . x)
  (raise
   (make-exn:fail:contract
    (string-append (format "~a : " name) (apply format fmt-str x))
    (current-continuation-marks))))

(provide hocheck)

(define-teach intermediate foldr
  (lambda (f e l)
    (unless (and (procedure? f) (procedure-arity-includes? f 2))
      (hocheck 'foldr "first argument must be a function that expects two arguments, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'foldr "third argument must be a list, given ~e" l))
    (foldr f e l)))

(define-teach intermediate foldl
  (lambda (f e l)
    (unless (and (procedure? f) (procedure-arity-includes? f 2))
      (hocheck 'foldl "first argument must be a function that expects two arguments, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'foldl "third argument must be a list, given ~e" l))
    (foldl f e l)))

(define-teach intermediate build-string
  (lambda (n f)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (hocheck 'build-string "second argument must be a function that accepts one argument, given ~e" f))
    (unless (and (number? n) (integer? n) (>= n 0))
      (hocheck 'build-string "first argument must be a natural number, given ~e" n))
    (build-string n (lambda (i)
                      (define r (f i))
                      (unless (char? r)
                        (hocheck 'build-string
                                 "the second argument must be a function that produces a character, ~
                                  given ~e, which produced ~e when given ~e" f r i))
                      r))))



(define-teach advanced cons 
  (lambda (a b)
    (check-second/cycle 'cons a b)
    (cons a b)))

(define-teach advanced list*
  (lambda x
    (check-last/cycle 'list* x)
    (apply list* x)))

(define-teach advanced append
  (lambda x
    (check-last/cycle 'append x)
    (apply append x)))

(define-teach advanced make-hash
  (lambda ([a empty])
    (make-hash (map (lambda (l) (cons (first l) (second l))) a))))

(define-teach advanced make-hasheq
  (lambda ([a empty])
    (make-hasheq (map (lambda (l) (cons (first l) (second l))) a))))

(define-teach advanced make-hasheqv
  (lambda ([a empty])
    (make-hasheqv (map (lambda (l) (cons (first l) (second l))) a))))

(define-teach advanced make-immutable-hash
  (lambda ([a empty])
    (make-immutable-hash (map (lambda (l) (cons (first l) (second l))) a))))

(define-teach advanced make-immutable-hasheq
  (lambda ([a empty])
    (make-immutable-hasheq (map (lambda (l) (cons (first l) (second l))) a))))

(define-teach advanced make-immutable-hasheqv
  (lambda ([a empty])
    (make-immutable-hasheqv (map (lambda (l) (cons (first l) (second l))) a))))

(provide  
 false?
 beginner-not
 beginner-random
 beginner-+
 beginner-/
 beginner-*
 beginner-sqr
 beginner-list?
 beginner-member
 beginner-member?
 beginner-memq
 beginner-memq?
 beginner-remove
 beginner-cons
 beginner-car
 beginner-cdr
 beginner-first
 beginner-rest
 beginner-list*
 beginner-range
 beginner-append
 intermediate-append
 beginner-error
 beginner-struct?
 beginner-exit
 beginner-equal?
 beginner-equal~?
 beginner-=~
 intermediate-foldr
 intermediate-foldl
 intermediate-build-string
 advanced-cons
 advanced-list*
 advanced-append
 advanced-make-hash
 advanced-make-hasheq
 advanced-make-hasheqv
 advanced-make-immutable-hash
 advanced-make-immutable-hasheq
 advanced-make-immutable-hasheqv
 cyclic-list?
 teach-equal?)

;; -----------------------------------------------------------------------------
;; auxiliary stuff, ignore

(define 1-LET "1-letter string")
(define 1-LETTER (format "~a" 1-LET))
(define 1-LETTER* (format "list of ~as" 1-LET))
(define NAT "natural number")

;; Symbol Any -> Boolean 
;; is this a 1-letter string?
(define (1-letter? tag s)
  (unless (string? s) (err tag "expected a ~a, but received a string: ~e" 1-LETTER s))
  (= (string-length s) 1))

;; Symbol Any -> Boolean 
;; is s a list of 1-letter strings
;; effect: not a list, not a list of strings 
(define (1-letter*? tag s)
  (unless (list? s) (err tag "expected a ~a, but received: ~e" 1-LETTER* s))
  (for-each 
   (lambda (c) 
     (unless (string? c) (err tag "expected a ~a, but received: ~e" 1-LETTER* c)))
   s)
  (andmap (compose (curry = 1) string-length) s))

(define (err tag msg-format . args)
  (raise 
   (make-exn:fail:contract
    (apply format (string-append (symbol->string tag) ": " msg-format) args)
    (current-continuation-marks))))

(define (a-or-an after)
  (if (member (string-ref (format "~a" after) 0) '(#\a #\e #\i #\o #\u))
      "an" "a"))

(define cerr 
  (case-lambda
    [(tag check-result format-msg actual)
     (unless check-result
       (err tag (string-append "expected " (a-or-an format-msg) " " format-msg ", but received ~e") actual))]
    [(tag check-result format-msg actual snd)
     (unless check-result
       (err tag (string-append "expected " (a-or-an format-msg) " " format-msg " for the ~a argument, but received ~e")
            snd actual))]))

;; -----------------------------------------------------------------------------

(define-teach beginner string-ith
  (lambda (s n)
    (cerr 'string-ith (string? s) "string" s "first")
    (cerr 'string-ith (and (number? n) (integer? n) (>= n 0)) NAT n "second")
    (define l (string-length s))
    (define f (format "exact integer in [0, ~a) (i.e., less than the length of the given string)" l))
    (cerr 'string-ith (< n l) f n "second")
    (string (string-ref s n))))

;; -----------------------------------------------------------------------------

(define-teach beginner replicate 
  (lambda (n s1)
    (cerr 'replicate (and (number? n) (exact-integer? n) (>= n 0)) NAT n)
    (cerr 'replicate (string? s1) "string" s1)
    (apply string-append (build-list n (lambda (i) s1)))))

;; -----------------------------------------------------------------------------

(define-teach beginner int->string 
  (lambda (i) 
    (cerr 'int->string 
          (and (exact-integer? i) (or (<= 0 i 55295) (<= 57344 i 1114111)))
          "exact integer in [0,55295] or [57344 1114111]"
          i)
    (string (integer->char i))))

;; -----------------------------------------------------------------------------

(define-teach beginner string->int 
  (lambda (s) 
    (cerr 'string->int (1-letter? 'string->int s) 1-LETTER s)
    (char->integer (string-ref s 0))))

;; -----------------------------------------------------------------------------

(define-teach beginner explode 
  (lambda (s)
    (cerr 'explode (string? s) "string" s)
    (map string (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner implode
  (lambda (los)
    (cerr 'implode (1-letter*? 'implode los) 1-LETTER* los)
    (apply string-append los)))

;; -----------------------------------------------------------------------------

(define-teach beginner string-numeric? 
  ;; is this: (number? (string->number s)) enough?
  (lambda (s1)
    (cerr 'string-numeric? (string? s1) "string" s1)
    (andmap char-numeric? (string->list s1))))

;; -----------------------------------------------------------------------------

;; I used copying here and I feel awful. 

(define-teach beginner string-alphabetic? 
  (lambda (s1)
    (cerr 'string-alphabetic? (string? s1) "string" s1)
    (andmap char-alphabetic? (string->list s1))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-whitespace? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s)  "string" s)
    (andmap char-whitespace? (string->list s))))

;; -----------------------------------------------------------------------------
;; I copied the next two, and I feel awful, too. 

(define-teach beginner string-upper-case? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s) "string" s)
    (andmap char-upper-case? (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-lower-case? 
  (lambda (s)
    (cerr 'string-lower-case? (string? s) "string" s)
    (andmap char-lower-case? (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-contains? 
  (lambda (s t)
    (cerr 'string-contains? (string? s) "string" s)
    (cerr 'string-contains? (string? t) "string" t)
    (cons? (regexp-match s t))))

(provide
 beginner-string-ith
 beginner-replicate
 beginner-int->string 
 beginner-string->int
 beginner-explode
 beginner-implode
 beginner-string-numeric?
 beginner-string-alphabetic?
 beginner-string-whitespace?
 beginner-string-upper-case?
 beginner-string-lower-case?
 beginner-string-contains?
 )
