;; This implements the equivalent of racket's read-syntax for R5RS scheme.
;; It has not been thoroughly tested.  Also it will read an entire file into a 
;; list of syntax objects, instead of returning one syntax object at a time

(module read mzscheme
  
  (require parser-tools/lex
           (prefix : parser-tools/lex-sre)
           parser-tools/yacc
           syntax/readerr)
  
  (define-tokens data (DATUM))
  (define-empty-tokens delim (OP CP HASHOP QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING DOT EOF))
  
  (define scheme-lexer
    (lexer-src-pos
     
     ;; Skip comments, without accumulating extra position information
     [(:or scheme-whitespace comment) (return-without-pos (scheme-lexer input-port))]
     
     ["#t" (token-DATUM #t)]
     ["#f" (token-DATUM #f)]
     [(:: "#\\" any-char) (token-DATUM (caddr (string->list lexeme)))]
     ["#\\space" (token-DATUM #\space)]
     ["#\\newline" (token-DATUM #\newline)]
     [(:or (:: initial (:* subsequent)) "+" "-" "...") (token-DATUM (string->symbol lexeme))]
     [#\" (token-DATUM (list->string (get-string-token input-port)))]
     [#\( 'OP]
     [#\) 'CP]
     [#\[ 'OP]
     [#\] 'CP]
     ["#(" 'HASHOP]
     [num2 (token-DATUM (string->number lexeme 2))]
     [num8 (token-DATUM (string->number lexeme 8))]
     [num10 (token-DATUM (string->number lexeme 10))]
     [num16 (token-DATUM (string->number lexeme 16))]
     ["'" 'QUOTE]
     ["`" 'QUASIQUOTE]
     ["," 'UNQUOTE]
     [",@" 'UNQUOTE-SPLICING]
     ["." 'DOT]
     [(eof) 'EOF]))
  
  (define get-string-token
    (lexer
     [(:~ #\" #\\) (cons (car (string->list lexeme))
                         (get-string-token input-port))]
     [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
     [(:: #\\ #\") (cons #\" (get-string-token input-port))]
     [#\" null]))
  
  
  (define-lex-abbrevs
   [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
   [digit (:/ #\0 #\9)]
   [scheme-whitespace (:or #\newline #\return #\tab #\space #\vtab)]
   [initial (:or letter (char-set "!$%&*/:<=>?^_~@"))]
   [subsequent (:or initial digit (char-set "+-.@"))]
   [comment (:: #\; (:* (:~ #\newline)) #\newline)]
   
   
   ;; See ${PLTHOME}/collects/syntax-color/racket-lexer.rkt for an example of
   ;; using regexp macros to avoid the cut and paste.
   ;   [numR (:: prefixR complexR)]
   ;   [complexR (:or realR
   ;                (:: realR "@" realR)
   ;                (:: realR "+" urealR "i")
   ;                (:: realR "-" urealR "i")
   ;                (:: realR "+i")
   ;                (:: realR "-i")
   ;                (:: "+" urealR "i")
   ;                (:: "-" urealR "i")
   ;                (:: "+i")
   ;                (:: "-i"))]
   ;   [realR (:: sign urealR)]
   ;   [urealR (:or uintegerR (:: uintegerR "/" uintegerR) decimalR)]
   ;   [uintegerR (:: (:+ digitR) (:* #\#))]
   ;   [prefixR (:or (:: radixR exactness)
   ;               (:: exactness radixR))]
   
   [num2 (:: prefix2 complex2)]
   [complex2 (:or real2
                  (:: real2 "@" real2)
                  (:: real2 "+" ureal2 "i")
                  (:: real2 "-" ureal2 "i")
                  (:: real2 "+i")
                  (:: real2 "-i")
                  (:: "+" ureal2 "i")
                  (:: "-" ureal2 "i")
                  (:: "+i")
                  (:: "-i"))]
   [real2 (:: sign ureal2)]
   [ureal2 (:or uinteger2 (:: uinteger2 "/" uinteger2))]
   [uinteger2 (:: (:+ digit2) (:* #\#))]
   [prefix2 (:or (:: radix2 exactness)
                 (:: exactness radix2))]
   [radix2 "#b"]
   [digit2 (:or "0" "1")]
   [num8 (:: prefix8 complex8)]
   [complex8 (:or real8
                  (:: real8 "@" real8)
                  (:: real8 "+" ureal8 "i")
                  (:: real8 "-" ureal8 "i")
                  (:: real8 "+i")
                  (:: real8 "-i")
                  (:: "+" ureal8 "i")
                  (:: "-" ureal8 "i")
                  (:: "+i")
                  (:: "-i"))]
   [real8 (:: sign ureal8)]
   [ureal8 (:or uinteger8 (:: uinteger8 "/" uinteger8))]
   [uinteger8 (:: (:+ digit8) (:* #\#))]
   [prefix8 (:or (:: radix8 exactness)
                 (:: exactness radix8))]
   [radix8 "#o"]
   [digit8 (:/ "0" "7")]
   
   [num10 (:: prefix10 complex10)]
   [complex10 (:or real10
                   (:: real10 "@" real10)
                   (:: real10 "+" ureal10 "i")
                   (:: real10 "-" ureal10 "i")
                   (:: real10 "+i")
                   (:: real10 "-i")
                   (:: "+" ureal10 "i")
                   (:: "-" ureal10 "i")
                   (:: "+i")
                   (:: "-i"))]
   [real10 (:: sign ureal10)]
   [ureal10 (:or uinteger10 (:: uinteger10 "/" uinteger10) decimal10)]
   [uinteger10 (:: (:+ digit10) (:* #\#))]
   [prefix10 (:or (:: radix10 exactness)
                  (:: exactness radix10))]
   [radix10 (:? "#d")]
   [digit10 digit]
   [decimal10 (:or (:: uinteger10 suffix)
                   (:: #\. (:+ digit10) (:* #\#) suffix)
                   (:: (:+ digit10) #\. (:* digit10) (:* #\#) suffix)
                   (:: (:+ digit10) (:+ #\#) #\. (:* #\#) suffix))]
   
   [num16 (:: prefix16 complex16)]
   [complex16 (:or real16
                   (:: real16 "@" real16)
                   (:: real16 "+" ureal16 "i")
                   (:: real16 "-" ureal16 "i")
                   (:: real16 "+i")
                   (:: real16 "-i")
                   (:: "+" ureal16 "i")
                   (:: "-" ureal16 "i")
                   "+i"
                   "-i")]
   [real16 (:: sign ureal16)]
   [ureal16 (:or uinteger16 (:: uinteger16 "/" uinteger16))]
   [uinteger16 (:: (:+ digit16) (:* #\#))]
   [prefix16 (:or (:: radix16 exactness)
                  (:: exactness radix16))]
   [radix16 "#x"]
   [digit16 (:or digit (:/ #\a #\f) (:/ #\A #\F))]
   
   
   [suffix (:or "" (:: exponent-marker sign (:+ digit10)))]
   [exponent-marker (:or "e" "s" "f" "d" "l")]
   [sign (:or "" "+" "-")]
   [exactness (:or "" "#i" "#e")])
  
  
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))
  
  ;; A macro to build the syntax object
  (define-syntax (build-so stx)
    (syntax-case stx ()
      ((_ value start end)
       (with-syntax ((start-pos (datum->syntax-object 
                                 (syntax end)
                                 (string->symbol 
                                  (format "$~a-start-pos"
                                          (syntax-object->datum (syntax start))))))
                     (end-pos (datum->syntax-object 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax-object->datum (syntax end))))))
                     (source (datum->syntax-object
                              (syntax end)
                              'source-name)))
         (syntax
          (datum->syntax-object 
           #f
           value
           (list source 
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))
           stx-for-original-property))))))
  
  (define (scheme-parser source-name)
    (parser
     (src-pos)
     
     (start s)
     (end EOF)
     (error (lambda (a name val start end)
              (raise-read-error 
               "read-error"
               source-name
               (position-line start)
               (position-col start)
               (position-offset start)
               (- (position-offset end)
                  (position-offset start)))))
     (tokens data delim)
     
     
     (grammar
      
      (s [(sexp-list) (reverse $1)])
      
      (sexp [(DATUM) (build-so $1 1 1)]
            [(OP sexp-list CP) (build-so (reverse $2) 1 3)]
            [(HASHOP sexp-list CP) (build-so (list->vector (reverse $2)) 1 3)]
            [(QUOTE sexp) (build-so (list 'quote $2) 1 2)]
            [(QUASIQUOTE sexp) (build-so (list 'quasiquote $2) 1 2)]
            [(UNQUOTE sexp) (build-so (list 'unquote $2) 1 2)]
            [(UNQUOTE-SPLICING sexp) (build-so (list 'unquote-splicing $2) 1 2)]
            [(OP sexp-list DOT sexp CP) (build-so (append (reverse $2) $4) 1 5)])
      
      (sexp-list [() null]
                 [(sexp-list sexp) (cons $2 $1)]))))
  
  (define (rs sn ip)
    (port-count-lines! ip)
    ((scheme-parser sn) (lambda () (scheme-lexer ip))))
  
  (define readsyntax
    (case-lambda ((sn) (rs sn (current-input-port)))
                 ((sn ip) (rs sn ip))))
  
  (provide (rename readsyntax read-syntax))
  
  )
