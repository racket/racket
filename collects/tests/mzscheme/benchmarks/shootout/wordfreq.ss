; $Id: wordfreq-mzscheme.code,v 1.10 2006/06/21 15:05:34 bfulgham Exp $
;  http://shootout.alioth.debian.org/
;  wordfreq.mzscheme by Grzegorz Chrupaa
;  Updated and corrected by Brent Fulgham
;  Re-written by Matthew Flatt with some inspriation from the Python example

(module wordfreq mzscheme
  (require mzlib/list)

  (define t (make-hash-table 'equal))

  (define (register-word! s)
    (let ([s (string-downcase (bytes->string/utf-8 s))])
      (hash-table-put! t s (add1 (hash-table-get t s (lambda () 0))))))

  (let ([in (current-input-port)])
    (let loop ()
      (let ([m (regexp-match #rx#"[a-zA-Z]+" in)])
	(when m
	  (register-word! (car m))
	  (loop)))))

  (for-each display
	    (sort (hash-table-map
		   t
		   (lambda (word count)
		     (let ((count (number->string count)))
		       (format"~a~a ~a~%"
			      (make-string (- 7 (string-length count)) #\space)
			      count
			      word))))
		  string>?)))
