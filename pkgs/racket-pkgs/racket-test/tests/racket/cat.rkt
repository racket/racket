#lang racket/base

;; Implements a "cat.exe" executable under Windows for the 
;; "subprocess.rktl" test.

(require racket/port
	 racket/cmdline)

(define files
  (command-line
   #:args
   file
   file))

(if (null? files)
    (copy-port (current-input-port)
	       (current-output-port))
    (for ([f (in-list files)])
       (if (equal? f "-")
	   (copy-port (current-input-port)
		      (current-output-port))
	   (if (file-exists? f)
	       (call-with-input-file* f (lambda (in) (copy-port in (current-output-port))))
	       (raise-user-error 'cat "bad file ~a" f)))))
(flush-output)
