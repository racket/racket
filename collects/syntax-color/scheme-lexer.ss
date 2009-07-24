(module scheme-lexer mzscheme
  
  (require parser-tools/lex
           (prefix : parser-tools/lex-sre))
  
  (provide scheme-lexer)
   
  (define-lex-abbrevs
         
   ;; For case insensitivity
   [a (char-set "aA")]
   [b (char-set "bB")]
   [c (char-set "cC")]
   [d (char-set "dD")]
   [e (char-set "eE")]
   [f (char-set "fF")]
   [g (char-set "gG")]
   [h (char-set "hH")]
   [i (char-set "iI")]
   [j (char-set "jJ")]
   [k (char-set "kK")]
   [l (char-set "lL")]
   [m (char-set "mM")]
   [n (char-set "nN")]
   [o (char-set "oO")]
   [p (char-set "pP")]
   [q (char-set "qQ")]
   [r (char-set "rR")]
   [s (char-set "sS")]
   [t (char-set "tT")]
   [u (char-set "uU")]
   [v (char-set "vV")]
   [w (char-set "wW")]
   [x (char-set "xX")]
   [y (char-set "yY")]
   [z (char-set "zZ")]

   [digit (:/ "0" "9")]
   [digit2 (:/ "0" "1")]
   [digit8 (:/ "0" "7")]
   [digit10 digit]
   [digit16 (:/ "af" "AF" "09")]

   [langchar (:or (:/ "az" "AZ" "09") "+" "-" "_")]

   [scheme-whitespace whitespace]

   [line-comment (:: ";" (:* (:~ #\newline)))]

   
   ;; What about char->integer constraint?
   [unicode  (:or (:: "u" (:** 1 4 digit16))
                  (:: "U" (:** 1 6 digit16)))]
   
   [character (:or (:: "#\\" any-char)
                   (:: "#\\" character-name)
                   (:: "#\\" (:/ "0" "3") digit8 digit8)
                   (:: "#\\" unicode))]
   
   [character-name (:or (:: s p a c e)
                        (:: n e w l i n e)
                        (:: n u l) 
                        (:: n u l l)
                        (:: b a c k s p a c e)
                        (:: t a b)
                        (:: l i n e f e e d)
                        (:: v t a b)
                        (:: p a g e)
                        (:: r e t u r n)
                        (:: r u b o u t))]
   
   [bad-char (:or "#\\"
                  (:: "#\\" (:>= 2 alphabetic))
                  (:: "#\\" (:/ "0" "3") digit8))]
       
   ;; What about byte string regexp strings
   [str (:or (:: (:? (:or "#px" "#rx")) "\"" (:* string-element (:: "\\" unicode)) "\"")
             byte-str)]
   [byte-str (:: (:? (:or "#px" "#rx")) "#\"" (:* string-element) "\"")]
   [string-element (:or (:~ "\"" "\\")
                        "\\\""
                        "\\\\"
                        "\\a"
                        "\\b"
                        "\\t"
                        "\\n"
                        "\\v"
                        "\\f"
                        "\\r"
                        "\\e"
                        "\\'"
                        (:: "\\" (:** 1 3 digit8))
                        (:: "\\x" (:** 1 2 digit16))
                        (:: "\\" #\newline))]

   [bad-str (:: (:? (:or "#px" "#rx")) (:? "#") "\"" 
                (:* (:~ "\"" "\\")
                    (:: "\\" any-char))
                (:? "\\" "\""))]
   

   [special-numbers (:or (:: n a n ".0") (:: i n f ".0"))]   
   [exponent-marker (:or e s f d l)]
   [sign (char-set "+-")]
   [exactness (:or "#i" "#e" "#I" "#E")]
   [radix2 (:or "#b" "#B")]
   [radix8 (:or "#o" "#O")]
   [radix10 (:or "#d" "#D")]
   [radix16 (:or "#x" "#X")]
   
   [script (:: "#!" (:or #\space #\/) (:* (:~ #\newline) (:: #\\ #\newline)))]

   [identifier-delims (:or (char-set "\",'`()[]{};") scheme-whitespace)]
   [identifier-chars (:~ identifier-delims "\\" "|")]
   [identifier-escapes (:or (:: "\\" any-char)
                            (:: "|" (:* (:~ "|")) "|"))]
   [identifier-start (:or identifier-escapes
                          (:~ identifier-delims "\\" "|" "#")
                          "#%")]
   [identifier (:: identifier-start
                   (:* identifier-escapes identifier-chars))]

   [bad-id-start (:or identifier-escapes
                      (:~ identifier-delims "\\" "|"))]
   [bad-id-escapes (:or identifier-escapes
                        (:: "|" (:* (:~ "|"))))]
   [bad-id (:or (:: bad-id-start
                    (:* identifier-escapes identifier-chars)
                    (:? "\\" bad-id-escapes))
                "\\"
                bad-id-escapes)]
    
   [keyword (:: "#:" (:* identifier-escapes identifier-chars))]
    
   [reader-command (:or (:: "#" c s) (:: "#" c i))]
   [sharing (:or (:: "#" (make-uinteger digit10) "=")
                 (:: "#" (make-uinteger digit10) "#"))]

   [list-prefix (:or "" "#hash" "#hasheq" "#hasheqv" "#s" (:: "#" (:* digit10)))])
  
  (define-lex-trans make-num
    (syntax-rules ()
      ((_ digit radix) (:: (make-prefix radix) (make-complex digit)))))
  
  (define-lex-trans make-prefix
    (syntax-rules ()
      ((_ radix) (:or (:: radix (:? exactness))
                      (:: (:? exactness) radix)))))
  
  (define-lex-trans make-complex
    (syntax-rules ()
      ((_ digit)
       (:or (make-real digit)
            (:: (make-real digit) "@" (make-real digit))
            (:: (make-real digit) "+" (:or special-numbers (make-ureal digit)) i)
            (:: (make-real digit) "-" (:or special-numbers (make-ureal digit)) i)
            (:: (make-real digit) "+" i)
            (:: (make-real digit) "-" i)
            (:: "+" (:or special-numbers (make-ureal digit)) i)
            (:: "-" (:or special-numbers (make-ureal digit)) i)
            (:: "+" i)
            (:: "-" i)))))
  
  (define-lex-trans make-ureal
    (syntax-rules ()
      ((_ digit) (:or (make-uinteger digit)
                      (:: (make-uinteger digit) "/" (make-uinteger digit) (:? (make-suffix digit)))
                      (make-decimal digit)))))

  (define-lex-trans make-real
    (syntax-rules ()
      ((_ digit) (:or (:: (:? sign) (make-ureal digit))
                      (:: (char-set "+-") special-numbers)))))
  
  (define-lex-trans make-uinteger
    (syntax-rules ()
      ((_ digit) (:: (:+ digit) (:* "#")))))
  
  (define-lex-trans make-decimal
    (syntax-rules ()
      ((_ digit)
       (:or (:: (make-uinteger digit) (make-suffix digit))
            (:: "." (:+ digit) (:* "#") (make-suffix digit))
            (:: (:+ digit) "." (:* digit) (:* "#") (make-suffix digit))
            (:: (:+ digit) (:+ "#") "." (:* "#") (make-suffix digit))))))

  (define-lex-trans make-suffix
    (syntax-rules ()
      ((_ digit) (:or "" (:: exponent-marker (:? sign) (:+ digit))))))

  
  (define (ret lexeme type paren start-pos end-pos)
    (values lexeme type paren (position-offset start-pos) (position-offset end-pos)))


  (define get-next-comment
    (lexer
     ["#|" (values 1 end-pos)]
     ["|#" (values -1 end-pos)]
     [(:or "#" "|" (:* (:~ "|" "#")))
      (get-next-comment input-port)]
     [(eof) (values 'eof end-pos)]
     [(special)
      (get-next-comment input-port)]
     [(special-comment)
      (get-next-comment input-port)]))
  
  (define (read-nested-comment num-opens start-pos input)
    (let-values (((diff end) (get-next-comment input)))
      (cond
        ((eq? 'eof diff) (ret "" 'error #f start-pos end))
        (else
         (let ((next-num-opens (+ diff num-opens)))
           (cond
             ((= 0 next-num-opens) (ret "" 'comment #f start-pos end))
             (else (read-nested-comment next-num-opens start-pos input))))))))
  
  (define (get-offset i)
    (let-values (((x y offset) (port-next-location i)))
      offset))
  
  (define (escape-regexp s)
    (apply string-append 
           (map (lambda (c)
                  (if (memq c '(#\( #\) #\* #\+ #\? #\[ #\] #\. #\^ #\\ #\|))
                      (string #\\ c)
                      (string c)))
                (string->list s))))
  
  (define (special-read-line i)
    (let ((next (peek-char-or-special i)))
      (cond
        ((or (eq? next #\newline) (not (char? next)))
         null)
        (else
         (read-char i)
         (cons next (special-read-line i))))))
  
  (define (read-line/skip-over-specials i)
    (let loop ()
      (let ((next (peek-char-or-special i)))
        (cond
          ((or (eq? next #\newline) (eof-object? next))
           null)
          (else
           (read-char-or-special i)
           (if (char? next)
               (cons next (loop))
               (loop)))))))
          
  (define (get-here-string start-pos i)
    (let* ((ender (list->string (special-read-line i)))
           (next-char (peek-char-or-special i)))
      (cond
        ((or (equal? ender "") (not (eq? #\newline next-char)))
         (values (string-append "#<<" ender) 'error #f start-pos (get-offset i)))
        (else
         (read-char i)
         (let loop ((acc (list (string-append "#<<" ender "\n"))))
           (let* ((next-line (list->string (special-read-line i)))
                  (next-char (peek-char-or-special i)))
             (cond
               ((not (or (char? next-char) (eof-object? next-char))) ;; a special
                (values (apply string-append (reverse (cons next-line acc)))
                        'error #f start-pos (get-offset i)))
               ((equal? next-line ender)  ;; end of string
                (values (apply string-append (reverse (cons next-line acc)))
                        'string #f start-pos (get-offset i)))
               ((eof-object? next-char)
                (values (apply string-append (reverse (cons next-line acc)))
                        'error #f start-pos (get-offset i)))
               (else
                (read-char i)
                (loop (cons (string-append next-line "\n") acc))))))))))
        
  (define scheme-lexer
    (lexer
     [(:+ scheme-whitespace)
      (ret lexeme 'white-space #f start-pos end-pos)]
     [(:or "#t" "#f" "#T" "#F" character
           (make-num digit2 radix2)
           (make-num digit8 radix8)
           (make-num digit10 (:? radix10))
           (make-num digit16 radix16))
      (ret lexeme 'constant #f start-pos end-pos)]
     [keyword (ret lexeme 'parenthesis #f start-pos end-pos)]
     [str (ret lexeme 'string #f start-pos end-pos)]
     [";"
      (values (apply string (read-line/skip-over-specials input-port)) 'comment #f 
              (position-offset start-pos)
              (get-offset input-port))]
     #;
     [line-comment
      (ret lexeme 'comment #f start-pos end-pos)]
     ["#;"
      (ret lexeme 'sexp-comment #f start-pos end-pos)]
     ["#|" (read-nested-comment 1 start-pos input-port)]
     [script
      (ret lexeme 'comment #f start-pos end-pos)]
     [(:: list-prefix "(")
      (ret lexeme 'parenthesis '|(| start-pos end-pos)]
     [(:: list-prefix "[")
      (ret lexeme 'parenthesis '|[| start-pos end-pos)]
     [(:: list-prefix "{")
      (ret lexeme 'parenthesis '|{| start-pos end-pos)]
     [(:or ")" "]" "}")
      (ret lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]
     [(:or "'" "`" "#'" "#`" "#&")
      (ret lexeme 'constant #f start-pos end-pos)]
     [(:or sharing reader-command "." "," ",@" "#," "#,@")
      (ret lexeme 'other #f start-pos end-pos)]

     [(:: (:or "#lang " "#!")
          (:or langchar
               (:: langchar (:* (:or langchar "/")) langchar)))
      (ret lexeme 'other #f start-pos end-pos)]
     [(:: (:or "#lang " "#!") (:* (:& any-char (complement whitespace))))
      (ret lexeme 'error #f start-pos end-pos)]
     
     [identifier
      (ret lexeme 'symbol #f start-pos end-pos)]
     ["#<<"
      (get-here-string (position-offset start-pos) input-port)]
     [(special)
      (ret "" 'no-color #f start-pos end-pos)]
     [(special-comment)
      (ret "" 'comment #f start-pos end-pos)]
     [(eof) (values lexeme 'eof #f #f #f)]
     [(:or bad-char bad-str 
           (:& bad-id
               (complement (:: (:or (:: "#" (:or f t)) reader-command sharing "#<<" "#\\" "#|" "#;" "#&" script)
                               any-string))))
      (ret lexeme 'error #f start-pos end-pos)]
     [any-char (extend-error lexeme start-pos end-pos input-port)]))
  
  (define (extend-error lexeme start end in)
    (if (memq (peek-char-or-special in)
              `(special #\newline #\return #\tab #\space #\vtab
                 #\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\;
                 ,eof))
        (ret lexeme 'error #f start end)
        (let-values (((rest end-pos) (get-chunk in)))
          (ret (string-append lexeme rest) 'error #f start end-pos))))
  
  (define get-chunk
    (lexer
     ((:+ (:~ identifier-delims)) (values lexeme end-pos))))
  
  
  )
