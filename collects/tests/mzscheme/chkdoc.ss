
(require-library "mzlib.ss")

(define actual-definitions
  (filter (lambda (s)
	    (let ([s (symbol->string s)])
	      (not (char=? (string-ref s 0) #\#))))
	  (map car (make-global-value-list))))

(define doc-path (collection-path "doc"))

(define r5rs-keywords (with-input-from-file (build-path doc-path "r5rs" "keywords") read))
(define mzscheme-keywords (with-input-from-file (build-path doc-path "mzscheme" "keywords") read))

(define documented
  (map string->symbol (map car (append r5rs-keywords mzscheme-keywords))))

(for-each
 (lambda (doc)
   (unless (memq doc actual-definitions)
     (printf "Documented but doesn't exist: ~a~n" doc)))
 documented)

(for-each
 (lambda (act)
   (unless (memq act documented)
     (printf "Undocumented: ~a~n" act)))
 actual-definitions)
