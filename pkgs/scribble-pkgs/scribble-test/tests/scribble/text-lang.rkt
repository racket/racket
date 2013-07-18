#lang racket/base

(require tests/eli-tester racket/runtime-path racket/port racket/sandbox
         (prefix-in doc: (lib "scribblings/scribble/text.scrbl")))

(provide text-lang-tests)
(module+ main (text-lang-tests))
(define (text-lang-tests)
  ;; (sample-file-tests)
  (test do (in-documentation-tests)))

;; unused now
(define-runtime-path text-dir "text")
(define (sample-file-tests)
  (parameterize ([current-directory text-dir])
    (for ([ifile (map path->string (directory-list))]
          #:when (and (file-exists? ifile)
                      (regexp-match? #rx"^i[0-9]+\\.ss$" ifile)))
      (define ofile (regexp-replace #rx"^i([0-9]+)\\..*$" ifile "o\\1.txt"))
      (define expected (call-with-input-file ofile
                         (lambda (i) (read-bytes (file-size ofile) i))))
      (define o (open-output-bytes))
      (parameterize ([current-output-port o])
        (dynamic-require (path->complete-path ifile) #f))
      (test (get-output-bytes o) => expected))))

(define-runtime-path this-dir ".")
(define (in-documentation-tests)
  (define (text-test line in-text out-text more)
    (define-values (i o) (make-pipe 512))
    (define-values (expected len-to-read)
      (let ([m (regexp-match-positions #rx"\n\\.\\.\\.$" out-text)])
        (if m
          (values (substring out-text 0 (caar m)) (caar m))
          (values out-text #f))))
    ;; test with name indicating the source
    (define-syntax-rule (t . stuff)
      (test ;; #:failure-message
            ;; (format "text-lang test failure at line ~s" line)
            . stuff))
    (parameterize ([current-directory this-dir]
                   [sandbox-output o]
                   [sandbox-error-output current-output-port]
                   [sandbox-eval-limits '(2 10)])
      (define exn #f)
      (define thd #f)
      (define (run)
        ;; only need to evaluate the module, so we have its output; but do that
        ;; in a thread, since we might want to look at just a prefix of an
        ;; infinite output
        (with-handlers ([void (lambda (e) (set! exn e))])
          (make-module-evaluator in-text)
          (close-output-port o)))
      (for ([m more])
        (call-with-output-file (car m) #:exists 'truncate
          (lambda (o) (display (cdr m) o))))
      (set! thd (thread run))
      (t (if len-to-read (read-string len-to-read i) (port->string i))
         => expected)
      (t (begin (kill-thread thd) (cond [exn => raise] [else #t])))
      (for ([m more])
        (when (file-exists? (car m)) (delete-file (car m))))))
  (call-with-trusted-sandbox-configuration
    (lambda ()
      (for ([t (in-list (doc:tests))])
        (begin (apply text-test t))))))
