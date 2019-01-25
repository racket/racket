
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
                bitwise-arithmetic-shift arithmetic-shift
                fixnum->flonum fx->fl 
                flonum->fixnum fl->fx
                fxarithmetic-shift-right fxrshift
                fxarithmetic-shift-left fxlshift
                real->flonum ->fl)
        (set! rewrites-added? #t)))
    (getprop n 'error-rename n)))

(define (rewrite-format str irritants)
  (cond
   [(equal? str "attempt to reference undefined variable ~s")
    (values (string-append
             "~a: undefined;\n cannot reference an identifier before its definition"
             "\n  alert: compiler pass failed to add more specific guard!")
            irritants)]
   [else
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
         [else (loop (fx+ i 1) accum-irritants irritants)])))]))

(define (string-prefix? p str)
  (and (>= (string-length str) (string-length p))
       (string=? (substring str 0 (string-length p)) p)))

(define (string-suffix? p str)
  (and (>= (string-length str) (string-length p))
       (string=? (substring str (- (string-length str) (string-length p)) (string-length str)) p)))

