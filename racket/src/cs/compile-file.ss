
;; Check to make we're using a build of Chez Scheme
;; that has all the features we need.

(let-values ([(maj min sub) (scheme-version-number)])
  (unless (or (> maj 9)
              (and (= maj 9)
                   (or (> min 5)
                       (and (= min 5)
                            (>= sub 3)))))
    (error 'compile-file "need a newer Chez Scheme")))

(define (check-ok what thunk)
  (unless (guard (x [else #f]) (thunk))
    (error 'compile-file
           (format
            "failed trying `~a`; probably you need a newer Chez Scheme"
            what))))

(define (check-defined expr)
  (check-ok expr (lambda () (eval expr))))

(check-defined 'box-cas!)
(check-defined 'make-arity-wrapper-procedure)
(check-defined 'generate-procedure-source-information)
(check-defined 'object-backreferences)
(check-defined 'current-generate-id)
(check-defined 'load-compiled-from-port)
(check-defined 'collect-rendezvous)
(check-defined '(define-ftype T (function __collect_safe () void)))
(check-defined 'call-setting-continuation-attachment)
(check-defined 'hashtable-cells)
(check-ok "fxvector-set!"
          (lambda ()
            (parameterize ([optimize-level 3]
                           [run-cp0 (lambda (cp0 x) x)])

              (eval '(define (op x)
                       (if (fx- 0) 0 0)))
              (eval '(define (f x)
                       (fxvector-set! x 0 (op 0))))
              (eval '(f (fxvector 0))))))
(check-defined 'vfasl-convert-file)
(check-defined 'compute-size-increments)
(check-defined 'enable-type-recovery)
(check-defined 'make-wrapper-procedure)
(check-defined 'make-phantom-bytevector)
(check-defined 'enable-arithmetic-left-associative)
(check-ok "eq? on flonums"
          (lambda ()
            (let* ([n (string->number "3.14")]
                   [v (vector n n)])
              (collect 0)
              (unless (eq? (vector-ref v 0) (vector-ref v 1))
                (error 'eq-on-flonum "no")))))
(check-defined 'procedure-known-single-valued?)
(check-defined 'compress-format)
(check-defined '#%$record-cas!)

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
(generate-procedure-source-information #f)
(compile-compressed #f)
(enable-arithmetic-left-associative #t)
(define build-dir "")
(define xpatch-path #f)

(define-values (src deps)
  (let loop ([args (command-line-arguments)])
    (cond
     [(get-opt args "--debug" 0)
      => (lambda (args)
           (generate-inspector-information #t)
           (loop args))]
     [(get-opt args "--srcloc" 0)
      => (lambda (args)
           (generate-procedure-source-information #f)
           (loop args))]
     [(get-opt args "--unsafe" 0)
      => (lambda (args)
           (optimize-level 3)
           (loop args))]
     [(get-opt args "--compress" 0)
      => (lambda (args)
           (compile-compressed #t)
           (putenv "PLT_CS_MAKE_COMPRESSED" "y") ; for "linklet.sls"
           (loop args))]
     [(get-opt args "--whole-program" 0)
      => (lambda (args)
           (set! whole-program? #t)
           (loop args))]
     [(get-opt args "--dest" 1)
      => (lambda (args)
           (set! build-dir (car args))
           (loop (cdr args)))]
     [(get-opt args "--xpatch" 1)
      => (lambda (args)
           (set! xpatch-path (car args))
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

(when xpatch-path
  (load xpatch-path))

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
    (cond
     [xpatch-path
      ;; Cross compile: use `compile-to-file` to get a second, host-format output file
      (let ([sfd (let ([i (open-file-input-port src)])
                   (make-source-file-descriptor src i #t))])
        (let ([exprs (call-with-input-file
                      src
                      (lambda (i)
                        (let loop ([pos 0])
                          (let-values ([(e pos) (get-datum/annotations i sfd pos)])
                            (if (eof-object? e)
                                '()
                                ;; Strip enough of the annotation to expose 'library
                                ;; or 'top-level-program:
                                (let ([e (map annotation-expression
                                              (annotation-expression e))])
                                  (cons e (loop pos))))))))])
          (compile-to-file exprs dest)))]
     [else
      ;; Normal mode
      (compile-file src dest)]))])
