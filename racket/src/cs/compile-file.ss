
;; Check to make we're using a build of Chez Scheme
;; that has all the features we need.
(define-values (need-maj need-min need-sub need-dev)
  (values 9 5 5 1))

(unless (guard (x [else #f]) (eval 'scheme-fork-version-number))
  (error 'compile-file
         (error 'compile-file "need the Racket fork of Chez Scheme to build")))

(let-values ([(maj min sub dev) (scheme-fork-version-number)])
  (unless (or (> maj need-maj)
              (and (= maj need-maj)
                   (or (> min need-min)
                       (and (= min need-min)
                            (or (> sub need-sub)
                                (and (= sub need-sub)
                                     (>= dev need-dev)))))))
    (error 'compile-file "need a newer Chez Scheme")))

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
(fasl-compressed #f)
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
           (generate-procedure-source-information #t)
           (loop args))]
     [(get-opt args "--unsafe" 0)
      => (lambda (args)
           (optimize-level 3)
           (loop args))]
     [(get-opt args "--compress" 0)
      => (lambda (args)
           (fasl-compressed #t)
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
     [(get-opt args "--show-cp0" 0)
      => (lambda (args)
           (run-cp0 (lambda (cp0 x)
                      (let ([r (cp0 (cp0 x))])
                        (pretty-print (#%$uncprep r))
                        r)))
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

(define (compile-it)
 (cond
   [whole-program?
    (unless (= 1 (length deps))
      (error 'compile-file "expected a single dependency for whole-program compilation"))
    (printf "Whole-program optimization for Racket core...\n")
    (printf "[If this step runs out of memory, try configuring with `--disable-wpo`]\n")
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
             ;; Pass #t for `force-host-out?' in case  host and target are the same.
             (compile-to-file exprs dest #f #t)))]
        [else
         ;; Normal mode
         (compile-file src dest)]))]))

(time (compile-it))

(printf "    ~a bytes peak memory use\n" (maximum-memory-bytes))
