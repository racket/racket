#lang racket/gui
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;;; Parser
(define-tokens regular (VARIABLE STRING))
(define-empty-tokens keywords (STATIC CHAR STAR BRACKET EQUALS LBRACE RBRACE SEMICOLON COMMA EOF))

(define lex-xpm
  (lexer
   [(eof) (token-EOF)]
   ["static" (token-STATIC)]
   ["char" (token-CHAR)]
   ["*" (token-STAR)]
   ["[]" (token-BRACKET)]
   ["=" (token-EQUALS)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   [";" (token-SEMICOLON)]
   ["," (token-COMMA)]
   [whitespace (lex-xpm input-port)]
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (lex-xpm input-port)]
   [(:: #\" (:* (:or (:~ #\") "\\\"")) #\")
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ (:or (char-range #\a #\z)
             (char-range #\A #\Z)
             (char-range #\0 #\9)
             #\_
             #\- ; Not really allowed but mini-plt uses it
             ))
    (token-VARIABLE lexeme)]))

(define parse-raw-xpm
  (parser (start xpm)
          (tokens regular keywords)
          (grammar (xpm [(STATIC CHAR STAR VARIABLE BRACKET EQUALS LBRACE
                                 strings
                                 RBRACE)
                         (cons $4 $8)])
                   (strings [(STRING) (list $1)]
                            [(STRING COMMA strings) (list* $1 $3)]))
          (end SEMICOLON)
          (error (lambda (tok-ok? tok-name tok-value)
                   (error
                    'parse-raw-xpm
                    (format 
                     (if tok-ok? 
                         "Did not expect token ~a"
                         "Invalid token ~a")
                     tok-name))))))

;;; Struct

(struct xpm (var width height color-ht x-hotspot y-hotspot pixels extensions) #:transparent)

;;; Reading

(define (xpm-read)
  (match-define (cons var strings)
                (parse-raw-xpm (λ () (lex-xpm (current-input-port)))))
  (define-values (width height ncolors cpp x-hotspot y-hotspot extensions?)
    (parse-values (first strings)))
  (define-values (colors-strs pixels*ext-strs)
    (split-at (rest strings) ncolors))
  (define-values (pixels-strs ext-strs)
    (split-at pixels*ext-strs height))
  (xpm var width height
       (parse-colors-ht cpp colors-strs)
       x-hotspot y-hotspot
       (parse-pixels cpp pixels-strs)
       ext-strs))

(define parse-values
  (match-lambda
    [(regexp #px"^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)$"
             (list _ 
                   (app string->number w)
                   (app string->number h)
                   (app string->number ncolors)
                   (app string->number cpp)))
     (values w h ncolors cpp #f #f #f)]))

(define (in-list* n l)
  (make-do-sequence
   (λ ()
     (values (λ (l)
               (define-values (ret rest) (split-at l n))
               (apply values ret))
              (λ (l)
               (define-values (ret rest) (split-at l n))
               rest)
              l
              (λ (l)
                (not (empty? l)))
              (λ _ #t)
              (λ _ #t)))))

(define (split-string-at s n)
  (values (substring s 0 n)
          (substring s n)))

(define (parse-colors-ht cpp ss)
  (for/hasheq ([s (in-list ss)])
    (define-values (chars rest*) (split-string-at s cpp))
    (define rest (regexp-replace #px"^\\s+" rest* ""))
    (define ps (regexp-split #px"\\s+" rest))
    (values (string->symbol chars)
            (for/hasheq ([(context color) (in-list* 2 ps)])
              (values (string->symbol context)
                      color)))))

(define (split-string s n)
  (for/list ([i (in-range 0 (/ (string-length s) n))])
    (substring s (* i n) (* (add1 i) n))))

(define (parse-pixels cpp ss)
  (for/list ([row (in-list ss)])
    (for/list ([color (in-list (split-string row cpp))])
      (string->symbol color))))

;;; Displaying

(define (hex->number n)
  (string->number n 16))

(define (color->pen% ht c context)
  (define c-ht (hash-ref ht c (λ () (error 'color->pen% "Unknown color ~e" c))))
  (define code (hash-ref c-ht context (λ () (error 'color->pen% "Unknown context ~e for color ~e" context c))))
  (define style
    (match code
      ["None" 'transparent]
      [_ 'solid]))
  (define color
    (match code
      ["None" "black"]
      [(regexp #px"^#([\\da-fA-F]{6})" (list _ hex))
       (match-define (list r g b) (split-string hex 2))
       (make-object color% (hex->number r) (hex->number g) (hex->number b))]
      [_
       (error 'color->pen% "Cannot parse ~e" code)]))
  (make-object pen% color 1 style))

(define xpm->bitmap%
  (match-lambda
    [(xpm var width height color-ht x-hotspot y-hotspot pixels extensions)
     (define the-bitmap (make-object bitmap% width height))
     (define the-dc (new bitmap-dc% [bitmap the-bitmap]))
     (send the-dc set-background (make-object color% "white"))
     (for ([y (in-naturals)]
           [row (in-list pixels)])
       (for ([x (in-naturals)]
             [color (in-list row)])
         (send the-dc set-pen (color->pen% color-ht color 'c))
         (send the-dc draw-point x y)))
     the-bitmap]))

(provide/contract
 [struct xpm ([var string?]
              [width exact-integer?]
              [height exact-integer?]
              [color-ht (hash/c symbol? (hash/c symbol? string?))]
              [x-hotspot (or/c false/c exact-integer?)]
              [y-hotspot (or/c false/c exact-integer?)]
              [pixels (listof (listof symbol?))]
              [extensions (listof string?)])]
 [xpm-read (-> xpm?)]
 [xpm->bitmap% (xpm? . -> . (is-a?/c bitmap%))])