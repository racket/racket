(module yacc-to-scheme mzscheme
  (require parser-tools/lex
           (prefix : parser-tools/lex-sre)
           parser-tools/yacc
           syntax/readerr
           mzlib/list)
  (provide trans)
  
  (define match-double-string
    (lexer
     ((:+ (:~ #\" #\\)) (append (string->list lexeme)
                                (match-double-string input-port)))
     ((:: #\\ any-char) (cons (string-ref lexeme 1) (match-double-string input-port)))
     (#\" null)))

   (define match-single-string
    (lexer
     ((:+ (:~ #\' #\\)) (append (string->list lexeme)
                                (match-single-string input-port)))
     ((:: #\\ any-char) (cons (string-ref lexeme 1) (match-single-string input-port)))
     (#\' null)))
  
  (define-lex-abbrevs
   (letter (:or (:/ "a" "z") (:/ "A" "Z")))
   (digit (:/ "0" "9"))
   (initial (:or letter (char-set "!$%&*/<=>?^_~@")))
   (subsequent (:or initial digit (char-set "+-.@")))
   (comment (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))

  (define-empty-tokens x
    (EOF PIPE |:| SEMI |%%| %prec))
  (define-tokens y
    (SYM STRING))
  
  (define get-token-grammar
    (lexer-src-pos
     ("%%" '|%%|)
     (":" (string->symbol lexeme))
     ("%prec" (string->symbol lexeme))
     (#\| 'PIPE)
     ((:+ (:or #\newline #\tab " " comment (:: "{" (:* (:~ "}")) "}")))
      (return-without-pos (get-token-grammar input-port)))
     (#\; 'SEMI)
     (#\' (token-STRING (string->symbol (list->string (match-single-string input-port)))))
     (#\" (token-STRING (string->symbol (list->string (match-double-string input-port)))))
     ((:: initial (:* subsequent)) (token-SYM (string->symbol lexeme)))))

  (define (parse-grammar enter-term enter-empty-term enter-non-term)
    (parser
     (tokens x y)
     (src-pos)
     (error (lambda (tok-ok tok-name tok-value start-pos end-pos)
	      (raise-read-error
	       (format "Error Parsing YACC grammar at token: ~a with value: ~a" tok-name tok-value)
	       (file-path)
	       (position-line start-pos)
	       (position-col start-pos)
	       (position-offset start-pos)
	       (- (position-offset end-pos) (position-offset start-pos)))))
	       
     (end |%%|)
     (start gram)
     (grammar
      (gram 
       ((production) (list $1))
       ((production gram) (cons $1 $2)))
      (production
       ((SYM |:| prods SEMI) 
        (begin
          (enter-non-term $1)
          (cons $1 $3))))
      (prods
       ((rhs) (list `(,$1 #f)))
       ((rhs prec) (list `(,$1 ,$2 #f)))
       ((rhs PIPE prods) (cons `(,$1 #f) $3))
       ((rhs prec PIPE prods) (cons `(,$1 ,$2 #f) $4)))
      (prec
       ((%prec SYM)
        (begin
          (enter-term $2)
          (list 'prec $2)))
       ((%prec STRING)
        (begin
          (enter-empty-term $2)
          (list 'prec $2))))
      (rhs
       (() null)
       ((SYM rhs) 
	(begin
	  (enter-term $1)
	  (cons $1 $2)))
       ((STRING rhs) 
	(begin
	  (enter-empty-term $1)
	  (cons $1 $2)))))))
  
  (define (symbol<? a b)
    (string<? (symbol->string a) (symbol->string b)))
  
  (define (trans filename)
    (let* ((i (open-input-file filename))
           (terms (make-hash-table))
           (eterms (make-hash-table))
           (nterms (make-hash-table))
           (enter-term
            (lambda (s)
              (if (not (hash-table-get nterms s (lambda () #f)))
                  (hash-table-put! terms s #t))))
           (enter-empty-term
            (lambda (s)
              (if (not (hash-table-get nterms s (lambda () #f)))
                  (hash-table-put! eterms s #t))))
           (enter-non-term
            (lambda (s)
	     (hash-table-remove! terms s)
              (hash-table-remove! eterms s)
              (hash-table-put! nterms s #t))))
      (port-count-lines! i)
      (file-path filename)
      (regexp-match "%%" i)
      (begin0
       (let ((gram ((parse-grammar enter-term enter-empty-term enter-non-term)
		    (lambda () 
		      (let ((t (get-token-grammar i)))
			t)))))
          `(begin
             (define-tokens t ,(sort (hash-table-map terms (lambda (k v) k)) symbol<?))
             (define-empty-tokens et ,(sort (hash-table-map eterms (lambda (k v) k)) symbol<?))
             (parser
              (start ___)
              (end ___)
              (error ___)
              (tokens t et)
              (grammar ,@gram))))
            (close-input-port i)))))
