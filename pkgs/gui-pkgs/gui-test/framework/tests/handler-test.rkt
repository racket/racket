(module handler-test mzscheme
  (require "test-suite-utils.rkt")

(module test racket/base)

(let* ([filename "framework-group-test.rkt"]
       [tmp-filename (build-path (find-system-path 'temp-dir) filename)])

  (test
   'file-opened
   (lambda (x) (equal? (list filename "GRacket REPL") x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(begin0 (map (lambda (x) (send x get-label)) (get-top-level-windows))
               (send (car (get-top-level-windows)) close)))))

  (test
   'files-opened-twice
   (lambda (x) (equal? (list filename "GRacket REPL") x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(begin0 (map (lambda (x) (send x get-label)) (get-top-level-windows))
               (send (car (get-top-level-windows)) close)))))

  (test
   'file-opened-in-editor
   (lambda (x) (equal? filename x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(let ([f (car (get-top-level-windows))])
	 (send (send f get-editor) get-filename))))))

)
