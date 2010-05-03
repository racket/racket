#| tests are at plt/collects/tests/mzscheme/
collects/tests/mzscheme/beginner.ss
                    .../beginner-abbr.ss
                    .../intermediate.ss
                    .../intermediate-lambda.ss
                    .../advanced.ss

Each one has to run separately, since they mangle the top-level
namespace.
|#

;; MF: switched from 
;; module teachprims mzscheme
;; to 
#lang scheme 

(require mzlib/list 
         mzlib/math
         mzlib/etc)

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

;; Don't need this anymore, since we just check for pairs:
#;
(define cyclic-list?
  (lambda (l)
    (or (list? l)
        (and (pair? l)
             (let loop ([hare (cdr l)][turtle l])
               (cond
                 [(eq? hare turtle) #t]
                 [(not (pair? hare)) #f]
                 [(eq? (cdr hare) turtle) #t]
                 [(not (pair? (cdr hare))) #f]
                 [else (loop (cddr hare) (cdr turtle))]))))))

(define cyclic-list? beginner-list?)

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
        (format "~a: second argument must be of type <~a>, given ~e and ~e"
                prim-name type
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
               (format "~a: last argument must be of type <~a>, given ~e; other args:~a"
                       prim-name type
                       last
                       ;; all-but-last:
                       (build-arg-list
                        (let loop ([args args])
                          (cond
                            [(null? (cdr args)) null]
                            [else (cons (car args) (loop (cdr args)))]))))
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
             (format "~a: ~a argument must be of type <~a>, given ~e, ~e, and ~e"
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
        (format "sqr: expected number; given ~e" a)
        (current-continuation-marks))))
    (sqr a)))

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

(define-teach beginner list*
  (lambda x
    (check-last 'list* x)
    (apply list* x)))

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

(define (tequal? a b epsilon)
  (let* ([ht (make-hash)] ;; make-hash
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
                                 prev)))))]
         [union-equal!? (lambda (a b)
                          (let ([a (union-find a)]
                                [b (union-find b)])
                            (if (eq? a b)
                                #t
                                (begin
                                  (hash-set! ht b a)
                                  #f))))])
    (let ? ([a a][b b])
      (cond
        [(real? a)
         (and (real? b)
              (beginner-=~ a b epsilon))]
        [(union-equal!? a b) #t]
        [else (equal?/recur a b ?)]))))

(define-teach beginner equal?
  (lambda (a b)
    (equal? a b)))

(define-teach beginner =~
  (lambda (a b c)
    (check-three a b c '=~ real? 'real real? 'real positive-real? 'non-negative-real)
    (<= (- a c) b (+ a c))))

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

(define (do-sort l cmp? name)
  (unless (beginner-list? l) 
    (hocheck name "first argument must be of type <list>, given ~e" l))
  (unless (and (procedure? cmp?) (procedure-arity-includes? cmp? 2))
    (hocheck name "second argument must be a <procedure> that accepts two arguments, given ~e" cmp?))
  (sort l (lambda (x y) 
            (define r (cmp? x y))
            (unless (boolean? r)
              (hocheck name "the results of the procedure argument must be of type <boolean>, produced ~e" r))
            r)))

(define-teach intermediate quicksort
  (lambda (l cmp?)
    (do-sort l cmp? 'quicksort)))
(define-teach intermediate sort
  (lambda (l cmp?)
    (do-sort l cmp? 'sort)))

(define-teach intermediate foldr
  (lambda (f e l)
    (unless (and (procedure? f) (procedure-arity-includes? f 2))
      (hocheck 'foldr "first argument must be a <procedure> that accepts two arguments, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'foldr "third argument must be of type <list>, given ~e" l))
    (foldr f e l)))

(define-teach intermediate foldl
  (lambda (f e l)
    (unless (and (procedure? f) (procedure-arity-includes? f 2))
      (hocheck 'foldl "first argument must be a <procedure> that accepts two arguments, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'foldl "third argument must be of type <list>, given ~e" l))
    (foldl f e l)))

(define-teach intermediate build-string
  (lambda (n f)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (hocheck 'build-string "second argument must be a <procedure> that accepts one argument, given ~e" f))
    (unless (and (number? n) (integer? n) (>= n 0))
      (hocheck 'build-string "first argument must be of type <natural number>, given ~e" n))
    (build-string n (lambda (i)
                      (define r (f i))
                      (unless (char? r)
                        (hocheck 'build-string
                                "second argument must be a <procedure> that produces a <char>, given ~e, which produced ~e for ~e" f r i))
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
 beginner-remove
 beginner-cons
 beginner-list*
 beginner-append
 intermediate-append
 beginner-error
 beginner-struct?
 beginner-exit
 beginner-equal?
 beginner-equal~?
 beginner-=~
 intermediate-quicksort
 intermediate-sort
 intermediate-foldr
 intermediate-foldl
 intermediate-build-string
 advanced-cons
 advanced-list*
 advanced-append
 cyclic-list?)

;; -----------------------------------------------------------------------------
;; auxiliary stuff, ignore

(define 1-LET "1-letter string")
(define 1-LETTER (format "<~a>" 1-LET))
(define 1-LETTER* (format "<list of ~as>" 1-LET))
(define NAT "<natural number>")

;; Symbol Any -> Boolean 
;; is this a 1-letter string?
(define (1-letter? tag s)
  (unless (string? s) (err tag "~a expected, not a string: ~e" 1-LETTER s))
  (= (string-length s) 1))

;; Symbol Any -> Boolean 
;; is s a list of 1-letter strings
;; effect: not a list, not a list of strings 
(define (1-letter*? tag s)
  (unless (list? s) (err tag "~a expected, not a <list>: ~e" 1-LETTER* s))
  (for-each 
   (lambda (c) 
     (unless (string? c) (err tag "~a expected, not a <string>: ~e" 1-LETTER* c)))
   s)
  (andmap (compose (curry = 1) string-length) s))

(define (err tag msg-format . args)
  (raise 
   (make-exn:fail:contract
    (apply format (string-append (symbol->string tag) ": " msg-format) args)
    (current-continuation-marks))))

(define cerr 
  (case-lambda
    [(tag check-result format-msg actual)
     (unless check-result
       (err tag (string-append format-msg " expected, given ~e") actual))]
    [(tag check-result format-msg actual snd)
     (unless check-result
       (err tag (string-append format-msg " for ~a argument expected, given ~e")
            snd actual))]))

;; -----------------------------------------------------------------------------

(define-teach beginner string-ith
  (lambda (s n)
    (define f "<exact integer in [0, length of the given string (~s)]>")
    (cerr 'string-ith (string? s) "<string>" s "first")
    (cerr 'string-ith (and (number? n) (integer? n) (>= n 0)) NAT n "second")
    (let ([l (string-length s)]) 
      (cerr 'string-ith (< n l) (format f l) n "second"))
    (string (string-ref s n))))

;; -----------------------------------------------------------------------------

(define-teach beginner replicate 
  (lambda (n s1)
    (cerr 'replicate (and (number? n) (exact-integer? n) (>= n 0)) NAT n)
    (cerr 'replicate (string? s1) "<string>" s1)
    (apply string-append (build-list n (lambda (i) s1)))))

;; -----------------------------------------------------------------------------

(define-teach beginner int->string 
  (lambda (i) 
    (cerr 'int->string 
          (and (exact-integer? i) (or (<= 0 i 55295) (<= 57344 i 1114111)))
          "<exact integer in [0,55295] or [57344 1114111]>"
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
    (cerr 'explode (string? s) "<string>" s)
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
    (cerr 'string-numeric? (string? s1) "<string>" s1)
    (andmap char-numeric? (string->list s1))))

;; -----------------------------------------------------------------------------

;; I used copying here and I feel awful. 

(define-teach beginner string-alphabetic? 
  (lambda (s1)
    (cerr 'string-alphabetic? (string? s1) "<string>" s1)
    (andmap char-alphabetic? (string->list s1))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-whitespace? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s)  "<string>" s)
    (andmap char-whitespace? (string->list s))))

;; -----------------------------------------------------------------------------
;; I copied the next two, and I feel awful, too. 

(define-teach beginner string-upper-case? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s) "<string>" s)
    (andmap char-upper-case? (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-lower-case? 
  (lambda (s)
    (cerr 'string-lower-case? (string? s) "<string>" s)
    (andmap char-lower-case? (string->list s))))

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
 beginner-string-lower-case?)
