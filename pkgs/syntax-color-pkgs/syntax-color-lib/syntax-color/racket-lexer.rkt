#lang racket/base

  (require parser-tools/lex
           racket/contract
           "lexer-contract.rkt"
           (prefix-in : parser-tools/lex-sre))
  
  (provide 
   (contract-out
    [racket-lexer lexer/c])
   racket-lexer/status
   racket-nobar-lexer/status)
   
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

   [racket-whitespace whitespace]

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
   

   [special-numbers (:or (:: n a n ".0") (:: i n f ".0")
                         (:: n a n ".f") (:: i n f ".f"))]
   [special-extflonums (:or (:: n a n ".t") (:: i n f ".t"))]
   [exponent-marker (:or e s f d l)]
   [exponent-marker16 (:or s l)]
   [sign (char-set "+-")]
   [exactness (:or "#i" "#e" "#I" "#E")]
   [radix2 (:or "#b" "#B")]
   [radix8 (:or "#o" "#O")]
   [radix10 (:or "#d" "#D")]
   [radix16 (:or "#x" "#X")]
   
   [script (:: "#!" (:or #\space #\/) (:* (:~ #\newline) (:: #\\ #\newline)))]

   [identifier-delims (:or (char-set "\",'`()[]{};") racket-whitespace)]
   [identifier-chars (:~ identifier-delims "\\" "|")]
   [identifier-escapes (:or (:: "\\" any-char)
                            (:: "|" (:* (:~ "|")) "|"))]
   [identifier-start (:or identifier-escapes
                          (:~ identifier-delims "\\" "|" "#")
                          "#%")]
   [identifier (:: identifier-start
                   (:* identifier-escapes identifier-chars))]

   [nobar-identifier-escapes (:: "\\" any-char)]
   [nobar-identifier-start (:or nobar-identifier-escapes
                                (:~ identifier-delims "\\" "|" "#")
                                "#%")]
   [nobar-identifier (:: nobar-identifier-start
                         (:* nobar-identifier-escapes identifier-chars))]

   [bad-id-start (:or identifier-escapes
                      (:~ identifier-delims "\\" "|"))]
   [bad-id-escapes (:or identifier-escapes
                        (:: "|" (:* (:~ "|"))))]
   [bad-id (:or (:: bad-id-start
                    (:* identifier-escapes identifier-chars)
                    (:? "\\" bad-id-escapes))
                "\\"
                bad-id-escapes)]

   
   [nobar-bad-id-escapes nobar-identifier-escapes]
   [nobar-bad-id (:or (:: bad-id-start
                          (:* nobar-identifier-escapes identifier-chars)
                          (:? "\\" nobar-bad-id-escapes))
                      "\\"
                      nobar-bad-id-escapes)]
    
   [keyword (:: "#:" (:* identifier-escapes identifier-chars))]
   [nobar-keyword (:: "#:" (:* nobar-identifier-escapes identifier-chars))]
    
   [reader-command (:or (:: "#" c s) (:: "#" c i))]
   [sharing (:or (:: "#" (make-uinteger digit10) "=")
                 (:: "#" (make-uinteger digit10) "#"))]

   [list-prefix (:or "" "#hash" "#hasheq" "#hasheqv" "#s" (:: "#" (:* digit10)))])
  
  (define-lex-trans make-num
    (syntax-rules ()
      ((_ digit radix exponent-marker)
       (:: (make-prefix radix) (make-complex digit exponent-marker)))))
  
  (define-lex-trans make-prefix
    (syntax-rules ()
      ((_ radix) (:or (:: radix (:? exactness))
                      (:: (:? exactness) radix)))))
  
  (define-lex-trans make-complex
    (syntax-rules ()
      ((_ digit exponent-marker)
       (:or (make-real digit exponent-marker)
            (:: (make-real digit exponent-marker) "@" (make-real digit exponent-marker))
            (:: (make-real digit exponent-marker) "+" (:or special-numbers (make-ureal digit exponent-marker)) i)
            (:: (make-real digit exponent-marker) "-" (:or special-numbers (make-ureal digit exponent-marker)) i)
            (:: (make-real digit exponent-marker) "+" i)
            (:: (make-real digit exponent-marker) "-" i)
            (:: "+" (:or special-numbers (make-ureal digit exponent-marker)) i)
            (:: "-" (:or special-numbers (make-ureal digit exponent-marker)) i)
            (:: "+" i)
            (:: "-" i)))))
  
  (define-lex-trans make-ureal
    (syntax-rules ()
      ((_ digit exponent-marker)
       (make-ureal* digit exponent-marker make-suffix))))

  (define-lex-trans make-ureal*
    (syntax-rules ()
      ((_ digit exponent-marker make-suffix)
       (:or (make-uinteger digit)
            (:: (make-uinteger digit) "/" (make-uinteger digit) (make-suffix digit exponent-marker))
            (make-decimal digit exponent-marker make-suffix)))))

  (define-lex-trans make-real
    (syntax-rules ()
      ((_ digit exponent-marker)
       (make-real* digit exponent-marker make-suffix special-numbers))))

   (define-lex-trans make-real*
    (syntax-rules ()
      ((_ digit exponent-marker make-suffix special-numbers)
       (:or (:: (:? sign) (make-ureal* digit exponent-marker make-suffix))
            (:: (char-set "+-") special-numbers)))))
  
  (define-lex-trans make-uinteger
    (syntax-rules ()
      ((_ digit) (:: (:+ digit) (:* "#")))))
  
  (define-lex-trans make-decimal
    (syntax-rules ()
      ((_ digit exponent-marker make-suffix)
       (:or (:: (make-uinteger digit) (make-suffix digit exponent-marker))
            (:: "." (:+ digit) (:* "#") (make-suffix digit exponent-marker))
            (:: (:+ digit) "." (:* digit) (:* "#") (make-suffix digit exponent-marker))
            (:: (:+ digit) (:+ "#") "." (:* "#") (make-suffix digit exponent-marker))))))

  (define-lex-trans make-suffix
    (syntax-rules ()
      ((_ digit exponent-marker) (:or "" (:: exponent-marker (:? sign) (:+ digit))))))

  (define-lex-trans make-extflonum-suffix
    (syntax-rules ()
      ((_ digit exponent-marker) (:: exponent-marker (:? sign) (:+ digit)))))

  (define-lex-trans make-extflonum
    (syntax-rules ()
      ((_ digit radix)
       (:: radix (make-real* digit (:or "t" "T") make-extflonum-suffix special-extflonums)))))
  
  (define (ret lexeme type paren start-pos end-pos status)
    (values lexeme type paren (position-offset start-pos) (position-offset end-pos) status))


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
        ((eq? 'eof diff) (ret "" 'error #f start-pos end 'continue))
        (else
         (let ((next-num-opens (+ diff num-opens)))
           (cond
             ((= 0 next-num-opens) (ret "" 'comment #f start-pos end 'continue))
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
         (values (string-append "#<<" ender) 'error #f start-pos (get-offset i) 'datum))
        (else
         (read-char i)
         (let loop ((acc (list (string-append "#<<" ender "\n"))))
           (let* ((next-line (list->string (special-read-line i)))
                  (next-char (peek-char-or-special i)))
             (cond
               ((not (or (char? next-char) (eof-object? next-char))) ;; a special
                (values (apply string-append (reverse (cons next-line acc)))
                        'error #f start-pos (get-offset i)
                        'datum))
               ((equal? next-line ender)  ;; end of string
                (values (apply string-append (reverse (cons next-line acc)))
                        'string #f start-pos (get-offset i)
                        'datum))
               ((eof-object? next-char)
                (values (apply string-append (reverse (cons next-line acc)))
                        'error #f start-pos (get-offset i)
                        'datum))
               (else
                (read-char i)
                (loop (cons (string-append next-line "\n") acc))))))))))

  (define (racket-lexer in)
    (let-values ([(lexeme type paren start end adj) (racket-lexer/status in)])
      (values lexeme type paren start end)))
        
  (define-syntax-rule (lexer/status identifier keyword bad-id)
    (lexer
     [(:+ racket-whitespace)
      (ret lexeme 'white-space #f start-pos end-pos 'continue)]
     [(:: (:or "#true" "#false" "#t" "#f" "#T" "#F")
          (:* (:~ identifier-delims)))
      (ret lexeme 
           (if (member lexeme '("#true" "#false" "#t" "#f" "#T" "#F"))
               'constant 
               'error)
           #f start-pos end-pos 'datum)]
     [(:or character
           (make-num digit2 radix2 exponent-marker)
           (make-num digit8 radix8 exponent-marker)
           (make-num digit10 (:? radix10) exponent-marker)
           (make-num digit16 radix16 exponent-marker16)
           (make-extflonum digit2 radix2)
           (make-extflonum digit8 radix8)
           (make-extflonum digit10 (:? radix10))
           (make-extflonum digit16 radix16))
      (ret lexeme 'constant #f start-pos end-pos 'datum)]
     [keyword (ret lexeme 'hash-colon-keyword #f start-pos end-pos 'datum)]
     [str (ret lexeme 'string #f start-pos end-pos 'datum)]
     [";"
      (values (apply string (read-line/skip-over-specials input-port)) 'comment #f 
              (position-offset start-pos)
              (get-offset input-port)
              'continue)]
     #;
     [line-comment
      (ret lexeme 'comment #f start-pos end-pos)]
     ["#;"
      (ret lexeme 'sexp-comment #f start-pos end-pos 'continue)]
     ["#|" (read-nested-comment 1 start-pos input-port)]
     [script
      (ret lexeme 'comment #f start-pos end-pos 'continue)]
     [(:: list-prefix "(")
      (ret lexeme 'parenthesis '|(| start-pos end-pos 'open)]
     [(:: list-prefix "[")
      (ret lexeme 'parenthesis '|[| start-pos end-pos 'open)]
     [(:: list-prefix "{")
      (ret lexeme 'parenthesis '|{| start-pos end-pos 'open)]
     [(:or ")" "]" "}")
      (ret lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos 'close)]
     [(:or "'" "`" "#'" "#`" "#&")
      (ret lexeme 'constant #f start-pos end-pos 'continue)]
     [(:or sharing reader-command "." "," ",@" "#," "#,@")
      (ret lexeme 'other #f start-pos end-pos 'continue)]

     [(:: (:or "#lang " "#!")
          (:or langchar
               (:: langchar (:* (:or langchar "/")) langchar)))
      (ret lexeme 'other #f start-pos end-pos 'continue)]
     [(:: (:or "#lang " "#!") (:* (:& any-char (complement whitespace))))
      (ret lexeme 'error #f start-pos end-pos 'continue)]
     
     [identifier
      (ret lexeme 'symbol #f start-pos end-pos 'datum)]
     ["#<<"
      (get-here-string (position-offset start-pos) input-port)]
     [(special)
      (ret "" 'no-color #f start-pos end-pos 'datum)]
     [(special-comment)
      (ret "" 'comment #f start-pos end-pos 'continue)]
     [(eof) (values lexeme 'eof #f #f #f #f)]
     [(:or bad-char bad-str 
           (:& bad-id
               (complement (:: (:or (:: "#" (:or f t)) reader-command sharing "#<<" "#\\" "#|" "#;" "#&" script)
                               any-string))))
      (ret lexeme 'error #f start-pos end-pos 'bad)]
     [any-char (extend-error lexeme start-pos end-pos input-port)]))

  (define racket-lexer/status (lexer/status identifier keyword bad-id))
  (define racket-nobar-lexer/status (lexer/status nobar-identifier nobar-keyword nobar-bad-id))
  
  (define (extend-error lexeme start end in)
    (if (memq (peek-char-or-special in)
              `(special #\newline #\return #\tab #\space #\vtab
                 #\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\;
                 ,eof))
        (ret lexeme 'error #f start end 'bad)
        (let-values (((rest end-pos) (get-chunk in)))
          (ret (string-append lexeme rest) 'error #f start end-pos 'bad))))
  
  (define get-chunk
    (lexer
     ((:+ (:~ identifier-delims)) (values lexeme end-pos))))
