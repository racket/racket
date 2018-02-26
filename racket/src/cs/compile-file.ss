
;; Check to make we're using a build of Chez Scheme
;; that has all the features we need.

(define (check-defined expr)
  (unless (guard (x [else #f]) (eval expr))
    (error 'compile-file
           (format
            "failed trying `~a`; probably you need a newer Chez Scheme"
            expr))))

(check-defined 'box-cas!)
(check-defined 'make-arity-wrapper-procedure)
(check-defined 'generate-procedure-source-information)
(check-defined 'object-backreferences)
(check-defined 'current-generate-id)
(check-defined 'load-compiled-from-port)
(check-defined 'collect-rendezvous)
(check-defined '(define-ftype T (function __thread () void)))

;; ----------------------------------------

(current-make-source-object
 (lambda (sfd bfp efp)
   (call-with-values (lambda () (locate-source sfd bfp #t))
     (case-lambda
      [() (error 'compile-config "cannot get line and column")]
      [(name line col)
       (make-source-object sfd bfp efp line col)]))))

(generate-wpo-files #t)

(define (get-opt args flag arg-count)
  (cond
   [(null? args) #f]
   [(equal? (car args) flag)
    (unless (> (length args) arg-count)
      (error 'compile-file "missing argument for ~a" flag))
    (cdr args)]
   [else #f]))

(define whole-program? #f)
(generate-inspector-information #f)
(generate-procedure-source-information #t)
(define build-dir "")

(define-values (src deps)
  (let loop ([args (command-line-arguments)])
    (cond
     [(get-opt args "--debug" 0)
      => (lambda (args)
           (generate-inspector-information #t)
           (loop args))]
     [(get-opt args "--unsafe" 0)
      => (lambda (args)
           (optimize-level 3)
           (loop args))]
     [(get-opt args "--whole-program" 0)
      => (lambda (args)
           (set! whole-program? #t)
           (loop args))]
     [(get-opt args "--dest" 1)
      => (lambda (args)
           (set! build-dir (car args))
           (loop (cdr args)))]
     [(null? args)
      (error 'compile-file "missing source file")]
     [else
      (values (car args) (cdr args))])))

(define src-so
  (letrec ([find-dot (lambda (pos)
                       (let ([pos (sub1 pos)])
                         (cond
                          [(zero? pos) (error 'compile-file "can't find extension in ~s" src)]
                          [(char=? (string-ref src pos) #\.) pos]
                          [else (find-dot pos)])))])
    (string-append (substring src 0 (find-dot (string-length src))) ".so")))

(define dest
  (if (equal? build-dir "")
      src-so
      (string-append build-dir src-so)))

(cond
 [whole-program?
  (unless (= 1 (length deps))
    (error 'compile-file "expected a single dependency for whole-program compilation"))
  (unless (equal? build-dir "")
    (library-directories (list (cons "." build-dir))))
  (compile-whole-program (car deps) src #t)]
 [else
  (for-each load deps)
  (parameterize ([current-generate-id
                  (let ([counter-ht (make-eq-hashtable)])
                    (lambda (sym)
                      (let* ([n (eq-hashtable-ref counter-ht sym 0)]
                             [s ((if (gensym? sym) gensym->unique-string symbol->string) sym)]
                             [g (gensym (symbol->string sym) (format "rkt-~a-~a-~a" src s n))])
                        (eq-hashtable-set! counter-ht sym (+ n 1))
                        g)))])
    (compile-file src dest))])
