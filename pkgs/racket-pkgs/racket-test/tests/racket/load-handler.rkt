#lang racket/base
(require racket/file
         compiler/compile-file)

(provide try-load-handler-now)

(module+ main
  (try-load-handler-now))

(define (try-load-handler-now)
  ;; Check a load handler's treatment of the "expected module" argument.

  (define tmp-dir (build-path (find-system-path 'temp-dir) "lhm"))
  (define tmp-file-name "m.rkt")
  (define tmp-file (build-path tmp-dir tmp-file-name))

  (make-directory* tmp-dir)

  (with-output-to-file tmp-file
    #:exists 'truncate/replace
    (lambda ()
      (write '(module m racket/base
                (define m 'm) ; errors on redeclaration
                (provide m)
                (module* alpha racket/base
                  (define a 'a)
                  (provide a))
                (module* beta racket/base
                  (define b 'b)
                  (provide b))))))

  (when (directory-exists? (build-path tmp-dir "compiled"))
    (delete-directory/files (build-path tmp-dir "compiled")))

  (define (do-test a b where)
    (unless (equal? a b)
      (error 'failed
             "~a expected: ~e got: ~e"
             where a b)))

  (define-syntax-rule (test a b)
    (do-test a b 'b))

  (parameterize ([current-namespace (make-base-namespace)])
    (test 'm (dynamic-require tmp-file 'm))
    ;; From source, all modules get declared:
    (test #t (module-declared? tmp-file #f))
    (test #t (module-declared? `(submod ,tmp-file alpha) #f))
    (test #t (module-declared? `(submod ,tmp-file beta) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #t))
    ;; Requires should succeed:
    (test 'a (dynamic-require `(submod ,tmp-file alpha) 'a))
    (test 'b (dynamic-require `(submod ,tmp-file beta) 'b)))

  (parameterize ([current-namespace (make-base-namespace)])
    (void (compile-file tmp-file)))

  (parameterize ([current-namespace (make-base-namespace)])
    (test 'm (dynamic-require tmp-file 'm))
    ;; From bytecode, modules get declared only on demand:
    (test #t (module-declared? tmp-file #f))
    (test #f (module-declared? `(submod ,tmp-file alpha) #f))
    (test #f (module-declared? `(submod ,tmp-file beta) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #t))
    ;; Requires should succeed:
    (test 'a (dynamic-require `(submod ,tmp-file alpha) 'a))
    (test #t (module-declared? `(submod ,tmp-file alpha) #f))
    (test #f (module-declared? `(submod ,tmp-file beta) #f))
    (test 'b (dynamic-require `(submod ,tmp-file beta) 'b))
    (test #t (module-declared? `(submod ,tmp-file beta) #f)))

  (parameterize ([current-namespace (make-base-namespace)])
    ;; eval compiled code directly:
    (parameterize ([current-module-declare-name (make-resolved-module-path (build-path tmp-dir tmp-file-name))]
                   [read-accept-compiled #t])
      (with-input-from-file (build-path tmp-dir "compiled" (path-add-suffix tmp-file-name #".zo"))
        (lambda () (eval (read)))))
    ;; It's as if we read from source:
    (test 'm (dynamic-require tmp-file 'm))
    ;; From source, all modules get declared:
    (test #t (module-declared? tmp-file #f))
    (test #t (module-declared? `(submod ,tmp-file alpha) #f))
    (test #t (module-declared? `(submod ,tmp-file beta) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #f))
    (test #f (module-declared? `(submod ,tmp-file other) #t))
    ;; Requires should succeed:
    (test 'a (dynamic-require `(submod ,tmp-file alpha) 'a))
    (test 'b (dynamic-require `(submod ,tmp-file beta) 'b)))

  (delete-directory/files tmp-dir))
