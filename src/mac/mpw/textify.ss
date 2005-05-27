(require (lib "etc.ss"))

(define re (regexp "[.](c|cc|cxx|cpp|h|inc)$"))

(define (go p)
  (cond
   [(directory-exists? p)
    (printf "Checking ~a~n" p)
    (map go
	 (map (lambda (f) (build-path p f))
	      (directory-list p)))]
   [(file-exists? p)
    (when (regexp-match re p)
      (let-values ([(creator type) (file-creator-and-type p)])
	(unless (string=? "TEXT" type)
	  (printf "Textifying ~a~n" p)
	  (file-creator-and-type p creator "TEXT"))))]))

(go (build-path (this-expression-source-directory) 'up 'up))
