
(module mkdirs mzscheme
  
  (define dirs (vector->list (current-command-line-arguments)))

  (define (make-directory* dir)
    (unless (directory-exists? dir)
      (let-values ([(base name dir?) (split-path dir)])
	(when (path? base)
	  (make-directory* base))
	(printf "Making ~a\n" dir)
	(make-directory dir))))

  (for-each make-directory* dirs))
