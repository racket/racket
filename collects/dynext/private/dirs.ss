
(module dirs mzscheme
  (require (lib "plthome.ss" "setup"))

  (define (find-dir name)
    (if plthome
	(build-path plthome name)
	;; Try one up from each collection path:
	(let loop ([l (current-library-collection-paths)])
	  (if (null? l) 
	      ;; Make up a wrong answer:
	      (format "plt~a" name)
	      (let ([p (build-path (car l) 'up name)])
		(if (directory-exists? p)
		    p
		    (loop (cdr l))))))))

  (define include-dir (find-dir "include"))
  (define std-library-dir (find-dir "lib"))

  (provide include-dir std-library-dir))


  
      
