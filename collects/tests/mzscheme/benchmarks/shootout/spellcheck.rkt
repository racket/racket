;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;;
;;; spellcheck benchmark

(module spellcheck mzscheme
  (define dict (make-hash-table 'equal))

  (with-input-from-file "Usr.Dict.Words"
    (lambda ()
      (let loop ()
	(let ([r (read-bytes-line)])
	  (unless (eof-object? r)
	    (hash-table-put! dict r #t)
	    (loop))))))

  (let ([in (current-input-port)])
    (let loop ()
      (let ([w (read-bytes-line in)])
	(unless (eof-object? w)
	  (unless (hash-table-get dict w (lambda () #f))
	    (printf "~a\n" w))
	  (loop))))))
