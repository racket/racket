#lang scheme/base
(provide stamp) 
(define stamp
  (string-append "14apr2009 "
		 (let ([s "$URL$"])
		   (substring 
		    s 6
		    (- (string-length s)
		       (string-length
			"collects/repos-time-stamp/stamp.ss $"))))))
