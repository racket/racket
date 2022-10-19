
;; Check to make we're using a build of Chez Scheme
;; that has all the features we need.
(define-values (need-maj need-min need-sub need-pre)
  (values 9 9 9 10))

(unless (guard (x [else #f]) (eval 'scheme-pre-release))
  (error 'compile-file
         (error 'compile-file "need a newer version of Chez Scheme")))

(let-values ([(maj min sub) (scheme-version-number)]
             [(pre) (scheme-pre-release)])
  (unless (or (> maj need-maj)
              (and (= maj need-maj)
                   (or (> min need-min)
                       (and (= min need-min)
                            (or (> sub need-sub)
                                (and (= sub need-sub)
                                     ;; #f pre-release is after a non-#f pre-release
                                     (if need-pre
                                         (or (not pre)
                                             (>= pre need-pre))
                                         (not pre))))))))
    (error 'compile-file "need a newer Chez Scheme")))

;; ----------------------------------------

(current-make-source-object
 (lambda (sfd bfp efp)
   (call-with-values (lambda () (locate-source sfd bfp #t))
     (case-lambda
      [() (error 'compile-config "cannot get line and column")]
      [(name line col)
       (make-source-object sfd bfp efp line col)]))))

(define (get-opt args flag arg-count)
  (cond
   [(null? args) #f]
   [(equal? (car args) flag)
    (unless (> (length args) arg-count)
      (error 'compile-file "missing argument for ~a" flag))
    (cdr args)]
   [else #f]))

(define whole-program? #f)
(define inspector-information? #f)
(define source-information? #f)
(define compressed? #f)
(define source-dir "")
(define build-dir "")
(define xpatch-path #f)

(define-values (src dest deps)
  (let loop ([args (command-line-arguments)])
    (cond
     [(get-opt args "--debug" 0)
      => (lambda (args)
           (set! inspector-information? #t)
           (loop args))]
     [(get-opt args "--srcloc" 0)
      => (lambda (args)
           (set! source-information? #t)
           (loop args))]
     [(get-opt args "--unsafe" 0)
      => (lambda (args)
           (optimize-level 3)
           (loop args))]
     [(get-opt args "--compress" 0)
      => (lambda (args)
           (set! compressed? #t)
           (putenv "PLT_CS_MAKE_COMPRESSED" "y") ; for "linklet.sls"
           (loop args))]
     [(get-opt args "--compress-more" 0)
      => (lambda (args)
           (set! compressed? #t)
           (putenv "PLT_CS_MAKE_COMPRESSED" "y") ; for "linklet.sls"
           (putenv "PLT_CS_MAKE_COMPRESSED_DATA" "y") ; ditto
           (loop args))]
     [(get-opt args "--whole-program" 0)
      => (lambda (args)
           (set! whole-program? #t)
           (loop args))]
     [(get-opt args "--src" 1)
      => (lambda (args)
           (set! source-dir (car args))
           (loop (cdr args)))]
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
     [(null? (cdr args))
      (error 'compile-file "missing destination file")]
     [else
      (values (car args) (cadr args) (cddr args))])))

(when xpatch-path
  (load xpatch-path))

;; set these after xpatch is loaded, in case they're reset by the patch:
(generate-wpo-files #t)
(generate-inspector-information inspector-information?)
(generate-procedure-source-information source-information?)
(fasl-compressed compressed?)
(enable-arithmetic-left-associative #t)
(library-timestamp-mode 'exists)

(define (compile-it)
 (cond
   [whole-program?
    (unless (= 1 (length deps))
      (error 'compile-file "expected a single dependency for whole-program compilation"))
    (printf "Whole-program optimization for Racket core...\n")
    (printf "[If this step runs out of memory, try configuring with `--disable-wpo`]\n")
    (unless (equal? build-dir "")
      (library-directories (list (cons source-dir build-dir))))
    (compile-whole-program (car deps) dest #t)]
   [else
    (unless (equal? source-dir "")
      (library-directories (list (cons source-dir build-dir)))
      (source-directories (list "." build-dir source-dir)))
    (for-each load deps)
    (parameterize ([current-generate-id
                    (let ([counter-ht (make-eq-hashtable)])
                      (lambda (sym)
                        (let* ([n (eq-hashtable-ref counter-ht sym 0)]
                               [s ((if (gensym? sym) gensym->unique-string symbol->string) sym)]
                               [g (gensym (symbol->string sym) (format "rkt-~a-~a-~a" (path-last src) s n))])
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
