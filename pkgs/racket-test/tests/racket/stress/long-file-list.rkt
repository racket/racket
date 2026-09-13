#lang racket/base
(require racket/file)

(define N 1024)

(define dir (make-temporary-directory))
dir

(define all-files
  (sort
   (for/list ([i (in-range N)])
     (define name (string->path (format "file_~a" i)))
     (close-output-port (open-output-file (build-path dir name)))
     name)
   path<?))

(define (check files)
  (unless (equal? files all-files)
    (fprintf (current-error-port) "bad result with ~a items\n" (length files))))

(check (directory-list dir))

(for-each
 thread-wait
 (for/list ([i (in-range 10)])
   (thread (lambda () (check (directory-list dir))))))

(let ([M 10])
  (for-each
   thread-wait
   (for/list ([i (in-range 10)])
     (define t0 (thread (lambda () (for ([i (in-range M)]) (check (directory-list dir))))))
     (define t1 (thread (lambda () (kill-thread t0))))
     (thread (lambda ()  (for ([i (in-range M)]) (check (directory-list dir))))))))

(let ([M 10])
  (for-each
   thread-wait
   (for/list ([i (in-range 10)])
     (define t0 (thread #:pool 'own (lambda () (for ([i (in-range M)]) (check (directory-list dir))))))
     (define t1 (thread #:pool 'own (lambda () (kill-thread t0))))
     (thread #:pool 'own (lambda () (for ([i (in-range M)]) (check (directory-list dir))))))))

(delete-directory/files dir)
