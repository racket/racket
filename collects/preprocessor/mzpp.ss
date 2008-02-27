#lang scheme/base

(require preprocessor/pp-utils scheme/promise)
(provide (all-from-out preprocessor/pp-utils))

(provide beg-mark end-mark skip-to no-spaces? debug?)
(define beg-mark   (make-parameter "<<"))
(define end-mark   (make-parameter ">>"))
(define skip-to    (make-parameter #f))
(define no-spaces? (make-parameter #f))
(define debug?     (make-parameter #f))

(define current-indentation (make-parameter '()))
(define current-spaces      (make-parameter #f))
(define current-newline?    (make-parameter #f))

(provide thunk)
(define-syntax thunk
  (syntax-rules () [(thunk body0 body1 ...) (lambda () body0 body1 ...)]))

(provide push-indentation pop-indentation)
(define (push-indentation i)
  (current-indentation (cons i (current-indentation))))
(define (pop-indentation)
  (current-indentation (cdr (current-indentation)))
  (current-spaces 'x))

(provide newline*)
(define (newline*)
  (cond
   [(no-spaces?) (newline)]
   [(eq? '- (current-spaces)) (current-spaces #f)]
   [(and (current-newline?) (eq? 'x (current-spaces))) (current-spaces #f)]
   [else (newline)
         (let ([i (current-indentation)] [s (current-spaces)])
           (when (string? s) (display s))
           (current-spaces (and (pair? i) (car i)))
           (current-newline? #t))]))

(provide show)
(define (show . args)
  (define (show x)
    (cond [(or (void? x) (not x) (null? x))]
          [(procedure? x)
           (if (procedure-arity-includes? x 0)
             (call-with-values x show)
             (error 'mzpp "got a bad procedure value: ~e" x))]
          [(promise? x) (show (force x))]
          [(pair? x) (show (car x)) (show (cdr x))]
          [else (if (no-spaces?)
                  (display x)
                  (let ([spc1 (current-spaces)]
                        [spc2 (and (string? x) (regexp-match #rx"^ *$" x))])
                    (when (string? spc1) (display spc1))
                    (current-spaces (and spc2 (car spc2)))
                    (unless spc2 (display x))
                    (current-newline? #f)))]))
  (for-each show args))

(define (process)
  (define scheme? #t)
  (define (open)
    (when scheme? (error 'mzpp "unexpected meta-open token"))
    (set! scheme? #t))
  (define (close)
    (unless scheme? (error 'mzpp "unexpected meta-close token"))
    (set! scheme? #f))
  (define (writeln x) (write x) (newline))
  (define (newln)
    (if scheme? (newline) (writeln `newline*)))
  (define (disp x)
    ((if scheme? display writeln) x))
  (define token-re #f)
  (define token-change-string #f)
  (define (re-quote str . args)
    (regexp
     (apply format str
            (map (lambda (x) (regexp-replace* "[][()|+*?.^$\\]" x "\\\\\\0"))
                 args))))
  (define (set-markers beg end)
    (beg-mark beg)
    (end-mark end)
    (set! token-re (re-quote "(\\\\+)?(~a|~a)" beg end))
    (set! token-change-string
          (re-quote "^ *~a(.*)~a~a(.*)~a *$" beg beg end end)))
  (define skip-to-re (and (skip-to) (re-quote "^~a$" (skip-to))))
  (set-markers (beg-mark) (end-mark))
  (close)
  (let loop ([str (read-line)] [k 0])
    (cond
     [(eof-object? str)]
     [skip-to-re
      (when (regexp-match skip-to-re str)
        (set! skip-to-re #f) (skip-to #f))
      (loop (read-line) 0)]
     [(and (zero? k) (regexp-match token-change-string str)) =>
      (lambda (m)
        (set-markers (cadr m) (caddr m))
        ;; the follwing has no effect on the current text, but will on included
        ;; texts
        (writeln `(thunk (beg-mark ,(cadr m)) (end-mark ,(caddr m))))
        (loop (read-line) 0))]
     [(regexp-match-positions token-re str k) =>
      (lambda (m)
        (let ([token (substring str (caar m) (cdar m))])
          (unless (= k (caar m)) (disp (substring str k (caar m))))
          (cond [(cadr m) (disp (substring str (+ 1 (caadr m)) (cdar m)))]
                [(equal? token (beg-mark))
                 (open)
                 (writeln `(thunk (push-indentation
                                   ,(regexp-replace* #rx"[^ \t\n]"
                                                     (substring str 0 (caar m))
                                                     " "))))]
                [(equal? token (end-mark))
                 (close) (writeln `(thunk (pop-indentation)))]
                [else (disp token)])
          (loop str (cdar m))))]
     [else (cond [(= k (string-length str))]
                 [(zero? k) (disp str)]
                 [else (disp (substring str k))])
           (newln)
           (loop (read-line) 0)]))
  (open))

(define (pp-repl)
  (let loop ()
    (let ([x (read-syntax)])
      (unless (eof-object? x)
        (call-with-values (lambda () (eval x)) show)
        (loop)))))

(define (run files)
  (define (do-files)
    (for-each
     (lambda (file)
       (if (input-port? file)
         (parameterize ([stdin file]) (process))
         (let-values ([(dir name _) (split-path file)])
           (printf "~s\n"
                   `(thunk (cd ,(path->string (if (path? dir) dir (cd))))
                           (current-file ,(path->string name))))
           (with-input-from-file file process)
           (printf "~s\n"
                   `(thunk (cd ,(path->string (cd))) ; return to original dir
                           (current-file #f))))))
     files))
  (if (debug?)
    (do-files)
    (let-values ([(in out) (make-pipe)])
      (define (done) (close-input-port in) (close-output-port out))
      (define (abort-handler thunk)
        (with-handlers ([void (lambda (e) (done) (raise e))])
          (thunk)))
      (thread (lambda ()
                (parameterize ([stdout out]) (abort-handler do-files))
                (close-output-port out)))
      (parameterize ([stdin in]) (abort-handler pp-repl))
      (done)))
  (current-spaces '-))

(provide include)
(define (include . files)
  ;; run the files but don't let them change the current environment
  (parameterize ([skip-to #f]
                 [beg-mark (beg-mark)]
                 [end-mark (end-mark)]
                 [cd (cd)])
    (run files)))

(provide preprocess)
(define (preprocess . files)
  (read-case-sensitive #t)
  (namespace-require 'scheme/base)
  (namespace-require 'preprocessor/mzpp)
  (do-evals)
  (run files))
