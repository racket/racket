#lang scheme/base

(require htdp/error)

(define (read-file f)
  (check-arg 'read-file (string? f) "string" "first" f)
  (check-arg 'read-file (file-exists? f) "name of file in program's folder" "first" f)
  (list->string
    (with-input-from-file f 
      (lambda ()
	(let loop ([accu '()])
	  (define nxt (read-char))
	  (if (eof-object? nxt)
	      (reverse (if (char=? (car accu) #\newline) (cdr accu) accu))
	      (loop (cons nxt accu))))))))
 
(define (write-file f str)
  (check-arg 'read-file (string? f) "string" "first" f)
  (let ([result (not (file-exists? f))])
    (with-output-to-file f 
      (lambda () (printf "~a" str))
      #:exists 'truncate)
    result))

;; -----------------------------------------------------------------------------

(provide 
 read-file ;; String -> String
 ;; read the file f (in current-directory) as a string
 
 write-file ;; String String -> Boolean
 ;; write str to file f (in current-directory); 
 ;; false, if f exists
 ;; true, if f doesn't exist
 )
