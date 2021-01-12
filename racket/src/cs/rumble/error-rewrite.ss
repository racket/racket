
(define (condition->exception-constructor v)
  (cond
   [(or (and (format-condition? v)
             (or (string-prefix? "incorrect number of arguments" (condition-message v))
                 (string-suffix? "values to single value return context" (condition-message v))
                 (string-prefix? "incorrect number of values received in multiple value context" (condition-message v))))
        (and (message-condition? v)
             (or (string-prefix? "incorrect argument count in call" (condition-message v))
                 (string-prefix? "incorrect number of values from rhs" (condition-message v)))))
    exn:fail:contract:arity]
   [(and (format-condition? v)
         (who-condition? v)
         (#%memq (condition-who v) '(/ modulo remainder quotient atan angle log))
         (string=? "undefined for ~s" (condition-message v)))
    exn:fail:contract:divide-by-zero]
   [(and (format-condition? v)
         (who-condition? v)
         (#%memq (condition-who v) '(expt atan2))
         (string=? "undefined for values ~s and ~s" (condition-message v)))
    exn:fail:contract:divide-by-zero]
   [(and (format-condition? v)
         (string-prefix? "fixnum overflow" (condition-message v)))
    exn:fail:contract:non-fixnum-result]
   [(and (format-condition? v)
         (or (string=? "attempt to reference undefined variable ~s" (condition-message v))
             (string=? "attempt to assign undefined variable ~s" (condition-message v))))
    (lambda (msg marks)
      (|#%app| exn:fail:contract:variable msg marks (car (condition-irritants v))))]
   [(and (format-condition? v)
         (string-prefix? "~?.  Some debugging context lost" (condition-message v)))
    exn:fail]
   [(and (who-condition? v)
         (eq? 'time-utc->date (condition-who v)))
    exn:fail]
   [else
    exn:fail:contract]))

(define rewrites-added? #f)

(define rewrite-who
  (lambda (n)
    (unless rewrites-added?
      (letrec-syntax ([rename
                       (syntax-rules ()
                         [(_) (void)]
                         [(_ from to . args)
                          (begin
                            (putprop 'from 'error-rename 'to)
                            (rename . args))])])
        (rename bytevector-u8-ref bytes-ref
                bytevector-u8-set! bytes-set!
                bytevector-length bytes-length
                bytevector-copy bytes-copy
                bitwise-arithmetic-shift arithmetic-shift
                fixnum->flonum fx->fl 
                flonum->fixnum fl->fx
                fxarithmetic-shift-right fxrshift
                fxarithmetic-shift-left fxlshift
                fxsll/wraparound fxlshift/wraparound
                real->flonum ->fl
                time-utc->date seconds->date)
        (set! rewrites-added? #t)))
    (getprop n 'error-rename n)))

(define is-not-a-str "~s is not a")
(define result-arity-msg-head "returned ")
(define result-arity-msg-tail " values to single value return context")

(define (rewrite-format who str irritants)
  (cond
   [(equal? str "attempt to reference undefined variable ~s")
    (values (string-append
             "~a: undefined;\n cannot reference an identifier before its definition"
             "\n  alert: compiler pass failed to add more specific guard!")
            irritants)]
   [(and (equal? str "undefined for ~s")
         (equal? irritants '(0)))
    (values "division by zero" null)]
   [(and (string-prefix? result-arity-msg-head str)
         (string-suffix? result-arity-msg-tail str))
    (values (string-append "result arity mismatch;\n"
                           " expected number of values not received\n"
                           "  expected: 1\n"
                           "  received: " (let ([s (substring str
                                                              (string-length result-arity-msg-head)
                                                              (- (string-length str) (string-length result-arity-msg-tail)))])
                                            (if (equal? s "~a")
                                                (number->string (car irritants))
                                                s)))
            null)]
   [(equal? str "~s is not a pair")
    (format-error-values "contract violation\n  expected: pair?\n  given: ~s"
                         irritants)]
   [(and (equal? str "incorrect list structure ~s")
         (cxr->contract who))
    => (lambda (ctc)
         (format-error-values (string-append "contract violation\n  expected: " ctc "\n  given: ~s")
                              irritants))]
   [(equal? str  "~s is not a valid index for ~s")
    (cond
     [(exact-nonnegative-integer? (car irritants))
      (let-values ([(what len)
                    (let ([v (cadr irritants)])
                      (cond
                       [(vector? v) (values "vector" (vector-length v))]
                       [(bytes? v) (values "byte string" (bytes-length v))]
                       [(string? v) (values "string" (string-length v))]
                       [(fxvector? v) (values "fxvector" (fxvector-length v))]
                       [(flvector? v) (values "flvector" (flvector-length v))]
                       [else (values "value" #f)]))])
        (format-error-values (string-append "index is out of range\n"
                                            "  index: ~s\n"
                                            "  valid range: [0, " (if len (number->string (sub1 len)) "...") "]\n"
                                            "  " what ": ~s")
                             irritants))]
     [else
      (format-error-values (string-append "contract violation\n"
                                          "  expected: exact-nonnegative-integer?\n"
                                          "  given: ~s\n"
                                          "  argument position: 2nd\n"
                                          "  first argument...:\n"
                                          "   ~s")
                           irritants)])]
   [(and (> (string-length str) (string-length is-not-a-str))
         (equal? (substring str 0 (string-length is-not-a-str)) is-not-a-str)
         (= 1 (length irritants)))
    (let ([ctc (desc->contract (substring str (string-length is-not-a-str) (string-length str)))])
      (format-error-values (string-append "contract violation\n  expected: " ctc "\n  given: ~s")
                           irritants))]
   [(eq? who 'time-utc->date)
    (values "integer is out-of-range" null)]
   [else
    (format-error-values str irritants)]))

(define (format-error-values str irritants)
  (let ([str (string-copy str)]
        [len (string-length str)])
    (let loop ([i 0] [accum-irritants '()] [irritants irritants])
      (cond
       [(fx= i len)
        ;; `irritants` should be empty by now
        (values str (append (reverse accum-irritants) irritants))]
       [(and (char=? #\~ (string-ref str i))
             (fx< (fx+ i 1) len))
        (case (string-ref str (fx+ i 1))
          [(#\~ #\%) (loop (fx+ i 2) accum-irritants irritants)]
          [(#\s)
           (string-set! str (fx+ i 1) #\a)
           (loop (fx+ i 2)
                 (cons (error-value->string (car irritants))
                       accum-irritants)
                 (cdr irritants))]
          [else (loop (fx+ i 2)
                      (cons (car irritants)
                            accum-irritants)
                      (cdr irritants))])]
       [else (loop (fx+ i 1) accum-irritants irritants)]))))

(define (string-prefix? p str)
  (and (>= (string-length str) (string-length p))
       (string=? (substring str 0 (string-length p)) p)))

(define (string-suffix? p str)
  (and (>= (string-length str) (string-length p))
       (string=? (substring str (- (string-length str) (string-length p)) (string-length str)) p)))

;; Maps a function name like 'cadr to a contract
;; string like "(cons/c any/c pair?)"
(define (cxr->contract who)
  (let-syntax ([gen (lambda (stx)
                      (letrec ([add-all
                                (lambda (pre p tmpl)
                                  (cond
                                   [(null? p) '()]
                                   [else
                                    (cons
                                     (list (string-append (caar p) pre)
                                           (format tmpl (cadar p)))
                                     (add-all pre (cdr p) tmpl))]))])
                        (let ([combos
                               (reverse
                                (let loop ([alts '(x x x)])
                                  (cond
                                   [(null? alts)
                                    `(["a" "pair?"]
                                      ["d" "pair?"])]
                                   [else
                                    (let ([r (loop (cdr alts))])
                                      (append
                                       (add-all "a" r "(cons/c ~a any/c)")
                                       (add-all "d" r "(cons/c any/c ~a)")
                                       r))])))])
                          (with-syntax ([(combo ...)
                                         (map (lambda (c)
                                                (list (list (datum->syntax
                                                             #'here
                                                             (string->symbol (string-append "c" (car c) "r"))))
                                                      (cadr c)))
                                              combos)])
                            #`(case who
                                combo ...
                                [else #f])))))])
    (gen)))

(define (desc->contract str)
  (cond
   [(equal? str " mutable vector")
    "(and/c vector? (not/c immutable?))"]
   [(equal? str " bytevector")
    "bytes?"]
   [(equal? str " mutable bytevector")
    "(and/c bytes? (not/c immutable?))"]
   [(equal? str " mutable box")
    "(and/c box? (not/c immutable?))"]
   [(equal? str " character")
    "char?"]
   [(equal? str " real number")
    "real?"]
   [(equal? str " proper list")
    "list?"]
   [(equal? str "n flvector")
    "flvector?"]
   [else
    (let* ([l (string->list str)]
           [l (cond
               [(and (pair? l)
                     (eqv? (car l) #\space))
                (cdr l)]
               [(and (pair? l)
                     (eqv? (car l) #\n)
                     (pair? (cdr l))
                     (eqv? (cadr l) #\space))
                (cddr l)]
               [else l])])
      (list->string
       (let loop ([l l])
         (cond
          [(null? l) '(#\?)]
          [(eqv? (car l) #\space) (cons #\- (loop (cdr l)))]
          [else (cons (car l) (loop (cdr l)))]))))]))
