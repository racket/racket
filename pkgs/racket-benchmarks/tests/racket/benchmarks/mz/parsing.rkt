#lang racket/base
(require syntax-color/scheme-lexer
         racket/class
         racket/gui/base)

(define path (build-path (collection-path "framework" "private") "frame.rkt"))

(define content
  (with-input-from-file path
    (lambda ()
      (read-bytes (file-size path)))))

(define e (make-object text%))
(send e load-file path)

(define (mk-p)
  ;#;
  (open-input-text-editor e)
  #;
  (open-input-bytes content)
  #;
  (open-input-string (send e get-text 0 'eof)))

(let loop ([n 10])
  (unless (zero? n)
    (printf "lexing\n")
    (time
     (let ([p (mk-p)])
       (port-count-lines! p)
       (time
	(let loop () 
	  (let-values ([(a b c d e) (scheme-lexer p)])
	    (unless (eq? 'eof b)
	      (loop)))))))
    (printf "reading\n")
    (time
     (let ([p (mk-p)])
       (port-count-lines! p)
       (time
	(let loop () 
	  (let ([v (parameterize ([read-accept-reader #t])
                     (read p))])
	    (unless (eof-object? v)
	      (loop)))))))
    (printf "done\n")
    (loop (sub1 n))))

