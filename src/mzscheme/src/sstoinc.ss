
#lang scheme/base

(define to-zo? (member "--zo" (vector->list (current-command-line-arguments))))

(define DIGS-PER-LINE 20)

(namespace-require ''#%kernel)

(let loop ()
  (let ([expr (read)])
    (unless (eof-object? expr)
      (let ([c (compile expr)]
	    [p (open-output-bytes)])
	(write c p)
	(let ([s (get-output-bytes p)])
	  (printf "  {~n    SHARED_OK static MZCOMPILED_STRING_FAR unsigned char expr[] = {")
	  (let loop ([chars (bytes->list s)][pos 0])
	    (unless (null? chars)
	      (let ([char (car chars)])
		(printf "~a," char))
	      (loop (cdr chars)
		    (if (= pos DIGS-PER-LINE)
			(begin
			  (newline)
			  0)
			(add1 pos)))))
	  (printf "0};~n    EVAL_ONE_SIZED_STR((char *)expr, ~a);~n" (bytes-length s))
	  (printf "  }~n")))
      (loop))))
