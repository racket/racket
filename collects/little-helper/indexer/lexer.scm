; TODO: How is @foo to be handled? 
;       - insert both @foo and foo ?

(module lexer mzscheme
  (provide skip-to-next-token
           read-token
           for-each-token
           for-each-token-in-file
           document->tokens
           token-case-sensitive)
  
  (require (lib "match.ss"))
  
  (define token-case-sensitive (make-parameter #t))
  
  (define (utf-regexp str)
    (byte-regexp (string->bytes/utf-8 str)))
  
  (define whitespace     "[ \n\r\t]")
  (define brackets       "[]\\[\\(\\){}")
  (define punctuation    "[,'`;]")
  (define special        "[\\|]")
  (define delimiter      (string-append "(" whitespace "|" punctuation "|" 
                                        brackets "|" special "|" "\"" ")"))
  (define delimiter*     (string-append delimiter "*"))
  
  ; MzScheme's identifier and symbol syntax is considerably more liberal 
  ; than the syntax specified by R5RS. When input is scanned for tokens,
  ; the following characters delimit an identifier in addition to whitespace:
  ;     " , ' ` ; ( ) [ ] { } 
  (define non-symbol-starter-regexp (utf-regexp delimiter*))
  
  ; In addition, an identifier cannot start with a hash mark (``#'') unless
  ; the hash mark is immediately followed by a percent sign (``%''). The only 
  ; other special characters are backslash (``\'') and quoting vertical 
  ; bars (``|''); any other character is used as part of an identifier.
  
  (define (skip-to-next-token)
    ; (display (format "> ~a\n" (peek-bytes 20 0)))
    (regexp-match non-symbol-starter-regexp (current-input-port))
    (if (eqv? (peek-char) #\#)
        (unless (equal? (peek-string 2 0) "#%")
          (read-char)
          (skip-to-next-token))))
  
  (define non-delimiter  "[^]\\[\\(\\){} \n\r\t,'`;\\|\"]")
  (define non-delimiter* (string-append non-delimiter "*"))
  (define non-delimiter*-regexp (utf-regexp non-delimiter*))
  
  (define (bytes-downcase bs)
    (string->bytes/utf-8
     (string-downcase 
      (bytes->string/utf-8 bs #\space))))
  
  (define read-token
    (case-lambda
      [()    
       (read-token #f)]
      [(count-lines?)
       (let* ([pos (if count-lines?
                       (let-values
                           ([(line col pos) (port-next-location (current-input-port))])
                         (list line col pos))
                       (file-position (current-input-port)))]
              [m   (regexp-match non-delimiter*-regexp (current-input-port))])
         (and m
              (match m
                [(token . _)
                 (list (if (token-case-sensitive)
                           token 
                           (bytes-downcase token))
                       pos)])))]))
  
  (define for-each-token
    (case-lambda
      [(f)
       (for-each-token f #f)]
      [(f count-lines?)
       (unless (eof-object? (peek-char))
         (skip-to-next-token)
         (unless (eof-object? (peek-char))
           (let ([token (read-token count-lines?)])
             (if token
                 (begin
                   (f token)
                   (for-each-token f count-lines?))
                 (error "internal error: token expected after skipping")))))]))
  
  (define (for-each-token-in-file file f)
    (with-input-from-file file
      (lambda ()
        (for-each-token f ))))
  
  ; document->tokens : path -> (list (list byte-string position))
  (define (document->tokens file)
    (let ([tokens '()])
      (call-with-input-file file
        (λ (port)
          (port-count-lines! port)
          (parameterize ([current-input-port port])
            (for-each-token (λ (token) (set! tokens (cons token tokens)))
                            #t))))
      (reverse tokens)))
  
  )
