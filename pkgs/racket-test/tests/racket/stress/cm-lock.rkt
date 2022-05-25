#lang racket/base
(require racket/file
         compiler/cm)

;; Check that the parallel lock manager doesn't get confused if a
;; thread starts trying to compile a file before another thread trying
;; to compile the same file is detected to have terminated.

(define dir (make-temporary-file "compile-cm-~a" 'directory))

(define fns
  (for/list ([i (in-range 5)])
    (build-path dir (format "compile-~a" i))))

(define (write-compile-stuck fn)
  (call-with-output-file*
   fn
   #:exists 'truncate
   (lambda (o)
     (displayln "#lang racket/base\n" o)
     (writeln `(begin
                 (require (for-syntax racket/base))
                 (define-syntax (m stx)
                   ;; spin a while to change up scheduling:
                   (let loop ([n (random 100000)])
                     (if (zero? n)
                         'ok
                         (loop (sub1 n))))
                   (kill-thread (current-thread)))
                 (m))
              o))))

(define (write-compile-ok fn)
  (call-with-output-file*
   fn
   #:exists 'truncate
   (lambda (o)
     (displayln "#lang racket/base\n" o))))

(define p-l-c (compile-lock->parallel-lock-client (make-compile-lock) (current-custodian)))
(parameterize ([parallel-lock-client p-l-c])
  (define ths
    (for/list ([fn (in-list fns)]
               [j (in-naturals)])
      (thread
       (lambda ()
         (parameterize ([current-namespace (make-base-namespace)])
           (parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
             (write-compile-stuck fn)

             (for ([i (in-range 20)])
               (thread-wait
                (thread (lambda ()
                          (dynamic-require fn #f)
                          (log-error "shouldn't get here!")))))

             (printf "Expect succeed ~a...\n" j)
             (write-compile-ok fn)
             (dynamic-require fn #f)))))))
  (for-each sync ths))

(delete-directory/files dir)
