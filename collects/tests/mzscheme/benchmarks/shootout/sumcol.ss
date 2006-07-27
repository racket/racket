;;; http://shootout.alioth.debian.org/
;;;
;;; Contributed by Eli Barzilay

(module sumcol mzscheme
  (let loop ([acc 0])
    (let ([n (read)])
      (if (eof-object? n)
	  (printf "~a\n" acc)
	  (loop (+ acc n))))))
