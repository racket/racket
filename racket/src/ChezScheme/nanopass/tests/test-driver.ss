;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests test-driver)
  (export define-passes pass-names passes tracer test-one test-all tests 
          print-file)
  (import (rnrs) (tests helpers))
  
  (define subst
    (lambda (new old tree)
      (cond 
        [(null? tree) '()]
        [(equal? tree old) new]
        [(pair? tree) `(,(subst new old (car tree)) .
                        ,(subst new old (cdr tree)))]
        [else tree])))  

  (define void (lambda () (if #f #f)))
  
  (define-syntax define-passes
    (syntax-rules ()
      [(_ p1 p2 ...) (list '(p1 p2 ...) (list p1 p2 ...))]))

  (define passes
    (let ([pass-list '()])
      (case-lambda
        [() pass-list]
        [(x) (set! pass-list x)])))

  (define-syntax pass-names
    (identifier-syntax (let ([passes (passes)])
                         (if (null? passes) '() (car passes)))))

  (define tests
    (let ([test-list '()])
      (case-lambda
        [() test-list]
        [(x) (set! test-list x)])))

  (define tracer
    (let ([trace-list '()])
      (case-lambda
        [() trace-list]
        [(x)
         (set! trace-list
           (cond
             [(eq? x #t) pass-names]
             [(eq? x #f) '()]
             [(and (symbol? x) (memq x pass-names)) (list x)]
             [(and (list? x) (for-all (lambda (x) (memq x pass-names)) x)) x]
             [else (error 'tracer (format "invalid argument ~s" x))]))])))
  
  (define test-all
    (case-lambda
      [() (test-all #t #f #f)]
      [(emit?) (test-all emit? #f #f)]
      [(emit? print-expr?) (test-all emit? print-expr? #f)]
      [(emit? print-expr? check-eval?)
       (for-each
         (lambda (x)
           (when print-expr? (pretty-print x))
           (unless (test-one x emit?)
             (error 'test-all "test failed")))
         (tests))]))
  
  (define print-file
    (lambda (path)
      (with-input-from-file path
        (letrec ([f (lambda ()
                      (unless (eof-object? (peek-char))
                        (write-char (read-char))
                        (f)))])
          f)))) 
  
  (define test-one
    (case-lambda
      [(original-input-expr) (test-one original-input-expr #t)]
      [(original-input-expr emit?)
       (let ([answer (interpret original-input-expr)])
         (define-syntax on-error
           (syntax-rules ()
             [(_ e0 e1 e2 ...)
              (guard (e [else e0 (raise e)])
                e1 e2 ...)]))
         #;
         (define check-eval
           (lambda (pass-name input-expr output-expr)
             (on-error 
               (begin
                 (printf "~s input:~%" pass-name)
                 (pretty-print input-expr)
                 (printf "========~%~s output:~%" pass-name)
                 (pretty-print output-expr))
               (let ([t (interpret output-exr)])
                 (unless (equal? t answer)
                   (error pass-name 
                     (format "answer is ~s, should have been ~s" t answer)))
             (let ([t (parameterize ([run-cp0 (lambda (cp0 x) x)])
                        (interpret output-expr))])
               (unless (equal? t answer)
                 (error pass-name "answer is ~s, should have been ~s"
                   t answer)))))))
         (define check-eval
           (lambda (pass-name input-expr output-expr)
             (void)))
         (define run
           (lambda (input-expr pass-names pass-procs)
             (if (null? pass-names)
                 input-expr
                 (let ([pass-name (car pass-names)])
                   (when (memq pass-name (tracer)) (printf "~%~s:~%" pass-name))
                   (let ([pass (car pass-procs)])
                     (let ([output-expr
                             (on-error
                               (begin
                                 (printf "~s input:~%" pass-name)
                                 (pretty-print input-expr))
                               (pass input-expr))])
                       (check-eval pass-name input-expr output-expr)
                       (when (memq pass-name (tracer)) 
                         (pretty-print output-expr))
                       (run output-expr (cdr pass-names) (cdr pass-procs))))))))
         ;; AWK - TODO - need to come up with more elegant handling of this
         ;;              since looking up generate-code for each test is
         ;;              pretty hackish.  Maybe passes could handle this as
         ;;              well?
         (define generate-code
           (lambda (expr)
             (let ([passes (passes)])
               (if (null? passes)
                   (error 'generate-code "No passes defined")
                   (let ([proc (let l ([names (car passes)] 
                                       [procs (cadr passes)])
                                 (cond
                                   [(null? names) 
                                    (error 'generate-code
                                           "No generate-code pass defined")]
                                   [(eq? 'generate-code (car names)) (car procs)]
                                   [else (l (cdr names) (cdr procs))]))])
                     (proc expr))))))
         (define run-code
           (lambda (input-expr)
             (define asm-file "t1.s")
             (define err-file "t1.err")
             (define out-file "t1.out")
             (when (memq 'generate-code (tracer)) (printf "~%generate-code:~%"))
             (on-error
               (begin
                 (printf "generate-code input:~%")
                 (pretty-print input-expr))
               (when (file-exists? asm-file) (delete-file asm-file))
               (with-output-to-file asm-file
                 (lambda ()
                   (printf "/* ~%")
                   (pretty-print original-input-expr)
                   (printf "*/~%~%")
                   (print-file "canned.s")
                   (newline)
                   (generate-code input-expr))))
             (on-error
               (begin
                 (printf "generate-code input:~%")
                 (pretty-print input-expr)
                 (printf "========~%generate-code output:~%")
                 (print-file asm-file)
                 (printf "========~%")
                 (print-file err-file))
               (let ([t (assemble-and-run asm-file err-file out-file)])
                 (unless (equal? t answer)
                   (error 'generate-code 
                          (format "answer is ~s, should have been ~s" 
                                  t answer)))))
             (when (memq 'generate-code (tracer)) (print-file asm-file))))
         (reset-seed)
         (let ([expr (run original-input-expr (car (passes)) (cadr (passes)))])
           (when (and emit? (memq 'generate-code pass-names))
             (run-code expr))))]))
  
  (define assemble-and-run
    (lambda (asm-file err-file out-file)
      (define shell
        (lambda (s . args)
          (system (apply format s args))))
      (unless 
        (= 0 (shell "cc -o run startup.c ~a > ~a 2>&1" asm-file err-file))
        (error 'generate-program "build error(s)"))
      (let ([status (shell "exec ./run > ~a 2>&1" out-file)])
        (shell "cat ~a >> ~a" out-file err-file)
        (unless (= status 0)
          (error 'generate-program "run error(s)")))
      ; replace #<void> with "#<void>" to make it something the reader can
      ; handle, then substitute void for "#<void>"
      (shell "sed -e 's/#<void>/\"#<void>\"/g' < ~a > ~a.tmp" 
             out-file out-file)
      (let ([ip (open-input-file (format "~a.tmp" out-file))])
        (let ([x (subst (void) "#<void>" (read ip))])
          (close-input-port ip)
          x)))))

