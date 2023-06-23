(module collects-path racket/base

  (provide collects-path->bytes
	   check-collects-path
	   set-collects-path)

  (module set-executable-tag racket/base
    (provide max-dir-len
             set-executable-tag)
    (define max-dir-len 1024) ; also ok for DLL path: 2 * sizeof(wchar_t)
    (define (set-executable-tag who tag desc file ignore-non-executable? given bs)
      (unless ((bytes-length bs) . <= . max-dir-len)
        (raise-arguments-error who (string-append desc " too long")
                               "given" given))
      (define-values (i o)
        (open-input-output-file file #:exists 'update))
      (dynamic-wind
       void
       (lambda ()
         (cond
           [(regexp-match-positions tag i)
            => (lambda (m)
                 (file-position o (cdar m))
                 (write-bytes bs o)
                 (for ([n (in-range (+ max-dir-len 2 (- (bytes-length bs))))])
                   (write-byte 0 o)))]
           [else
            (unless ignore-non-executable?
              (raise-arguments-error
               who (string-append "could not find " desc " path label in executable")
               "file" file))]))
       (lambda ()
         (close-input-port i)
         (close-output-port o)))))
  (require 'set-executable-tag)

  (define (collects-path->bytes collects-path)
    (and collects-path
	 (cond
	  [(path? collects-path) (path->bytes collects-path)]
	  [(string? collects-path) (string->bytes/locale collects-path)]
	  [(and (list? collects-path)
		(pair? collects-path))
	   (let ([l (map (lambda (p)
			   (cond
			    [(path? p) (path->bytes p)]
			    [(string? p) (string->bytes/locale p)]
			    [else #""]))
			 collects-path)])
	     (let loop ([l l])
	       (if (null? (cdr l))
		   (car l)
		   (bytes-append (car l) #"\0" (loop (cdr l))))))]
	  [else #""])))
  
  (define (check-collects-path who collects-path collects-path-bytes)
    (when collects-path
      (unless (or (path-string? collects-path)
		  (and (list? collects-path)
		       (andmap path-string? collects-path)))
	(raise-type-error who "path, string, list of paths and strings, or #f" 
			  collects-path))
      (unless ((bytes-length collects-path-bytes) . <= . max-dir-len)
	(error who "collects path list is too long"))))

  (define (set-collects-path dest-exe collects-path-bytes)
    (when collects-path-bytes
      (set-executable-tag 'create-embedding-executable
                          #rx#"coLLECTs dIRECTORy:"
                          "collection"
                          dest-exe
                          #f
                          collects-path-bytes
                          collects-path-bytes))))
