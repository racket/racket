#lang scheme/base
(provide stamp) 
(define stamp
  (string-append "2mar2009 "
		 (let ([s "$URL$"])
		   (substring 
		    s 0
		    (- (string-length s)
		       (string-length
			"collects/repos-time-stamp/stamp.ss"))))))
