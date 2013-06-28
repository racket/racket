
(define old-dir (current-directory))
(define new-dir "~/Desktop/gc6.7")
(define really-git? #t)

(require racket/file racket/path racket/system)

(define old (make-hash-table 'equal))
(define new (make-hash-table 'equal))
(define plt-mod (make-hash-table 'equal))
(define mod (make-hash-table 'equal))

(define (fill-table dir ht)
  (parameterize ([current-directory dir])
    (fold-files (lambda (file kind v)
		  (when (eq? kind 'file)
		    (hash-table-put! ht file #t)))
		(void)
		#f)))

(fill-table new-dir new)
(fill-table old-dir old)

(hash-table-for-each
 old
 (lambda (k v)
   (let ([b (path->bytes k)])
     (when (or (regexp-match? #rx#"^(?:^|/)(?:[.](?:git.*|svn)|CVS)(?:/|$)$" b)
               (regexp-match? #rx#"upgrade[.]ss$" b)
               (regexp-match? #rx#"gc[.]h$" b))
       (hash-table-remove! old k)))))

(define (content f n)
  (with-input-from-file f (lambda () (read-bytes n))))

(define (diff f)
  (let ([of (build-path old-dir f)]
	[nf (build-path new-dir f)])
    (let ([o (file-size of)]
	  [n (file-size nf)])
      (let ([co (content of o)]
	    [cn (content nf o)])
	(unless (and (= o n) (bytes=? co cn))
	  (if (and (regexp-match #rx"PLTSCHEME" co)
		   (not (regexp-match #rx"PLTSCHEME" cn)))
	      (hash-table-put! plt-mod f #t)
	      (hash-table-put! mod f #t)))))))

(hash-table-for-each new (lambda (k v)
			   (when (hash-table-get old k (lambda () #f))
			     (diff k)
			     (hash-table-remove! new k)
			     (hash-table-remove! old k))))

(hash-table-for-each old (lambda (k v)
			   (printf "Remove ~a\n" k)))
(hash-table-for-each new (lambda (k v)
			   (printf "Add ~a\n" k)))
(hash-table-for-each mod (lambda (k v)
			   (printf "Changed ~a\n" k)))
(hash-table-for-each plt-mod (lambda (k v)
			       (printf "PLTSCHEME ~a\n" k)))

(unless (null? (hash-table-map plt-mod cons))
  (error "!! Cannot continue until PLTSCHEME diffs are managed !!"))

(define (go cmd)
  (printf "CMD: ~a\n" cmd)
  (when really-git?
    (system cmd)))

(hash-table-for-each new 
		     (lambda (k v)
		       (go (format "cp ~a ~a" 
				   (path->string (build-path new-dir k))
				   (path->string (build-path old-dir k))))))

(let ([s (open-output-string)])
  (fprintf s "rm ")
  (hash-table-for-each old (lambda (k v)
			     (fprintf s "~a/~a " (path->string old-dir) (path->string k))))
  (go (get-output-string s)))

(let ([s (open-output-string)])
  (fprintf s "cd ~a; git add " (path->string old-dir))
  (hash-table-for-each new (lambda (k v)
			     (fprintf s "~a " (path->string k))))
  (go (get-output-string s)))

(let ([s (open-output-string)])
  (fprintf s "cd ~a; git rm " (path->string old-dir))
  (hash-table-for-each old (lambda (k v)
			     (fprintf s "~a " (path->string k))))
  (go (get-output-string s)))

(hash-table-for-each mod 
		     (lambda (k v)
		       (go (format "cp ~a ~a" 
				   (path->string (build-path new-dir k))
				   (path->string (build-path old-dir k))))))
