(module lex mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           "../readerr.ss")

  (define-lex-abbrevs
    [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
    [lex:digit (:/ #\0 #\9)]
    [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
    
    [lex:keyword (:or "type" "interface" "class" "mixin" "struct" "extends" "final"
                      "impl" "implements" "init" "export" "as" "at" "with" "fun"   
                      "this" "my" "isa" "int" "bool" "string" "float" "char" "Any"
                      "while" "if" "cond" "else" "new" "super" "cast" "return")]
    [lex:grouping  (:or "{" "}" "[" "]" "(" ")")]
    [lex:separator (:or "," ":" ";" "=" "." "<:" "->" "=>" "@" "_")]
    [lex:operator  (:or "!" "||" "&&" "!=" "==" "<" "<=" ">" ">=" "+" "-" "*" "/" "%" "====")]
    [lex:selector  (:: "#" (:+ lex:digit))]

    [lex:string       (:: #\"                           ;; A quoted string starts with a "
                          (:* (:or (:~ #\\ #\")         ;; and has things in it which are
                                   (:: #\\ any-char)))  ;; not "s (but \" is okay)
                          #\")]                         ;; and ends with a ".
    [lex:character    (:: #\' any-char #\')]
    [lex:ident        (:: (:or lex:letter) (:* (:or #\_ lex:letter lex:digit)))]
    [lex:integer      (:: (:? #\-) (:+ lex:digit))]
    [lex:float        (:: (:? #\-) (:: (:+ lex:digit) #\. (:+ lex:digit)))]
    [lex:line-comment (:: "//" (:* (:~ #\newline)))])
  
  (provide EOF for-prec lex-errors keywords separators operators val-tokens)
  
  (define-tokens EOF
    (EOF))
  
  (define-empty-tokens for-prec
    (UMINUS))
  
  (define-tokens lex-errors
    (UNPARSEABLE))
  
  (define-tokens keywords
    (type interface class mixin struct
          extends final impl implements
          init export as at with
          this my null isa
          int bool string float char Any void
          if cond else true false while fun
          new super cast return))

  (define-tokens separators
    (O_CURLY C_CURLY O_BRACE C_BRACE O_PAREN C_PAREN COMMA COLON SEMI_COLON BINDS DOT SUBTYPE ARROW THICK_ARROW AT USCORE))
  
  (define-tokens operators
    (NOT OR AND NEQ EQUALS LT LE GT GE PLUS MINUS TIMES DIV MOD CLS_EQ))
  
  (define-tokens val-tokens
    (character floatnum string-lit integer id selector))
  
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))
  
  (provide create-src-stx)
  (define (create-src-stx val source-name start-pos end-pos)
    (datum->syntax-object #f val
                          (list
                           source-name
                           (position-line start-pos)
                           (position-col start-pos)
                           (position-offset start-pos)
                           (- (position-offset end-pos)
                              (position-offset start-pos)))
                          stx-for-original-property))
  
  (define-syntax (token stx)
    (syntax-case stx ()
      [(_ name val)
       (identifier? (syntax name))
       (let ([name (syntax name)])
         (with-syntax ([token-name (datum->syntax-object
                                    name
                                    (string->symbol
                                     (format "token-~a" (syntax-e name))))]
                       [source-name (datum->syntax-object name 'source-name)]
                       [start-pos (datum->syntax-object name 'start-pos)]
                       [end-pos (datum->syntax-object name 'end-pos)])
           (syntax 
            (token-name 
             (create-src-stx val source-name start-pos end-pos)))))]))
  
  (define-syntax (ttoken stx)
    (syntax-case stx ()
      [(_ name)
       (identifier? (syntax name))
       (syntax (token name 'name))]))

  (provide generate-honu-lexer)
  (define (generate-honu-lexer source-name)
    (define honu-lexer 
      (lexer-src-pos
       ;; can we just use lex:keyword somehow?
       ["type"       (ttoken type)]
       ["interface"  (ttoken interface)]
       ["class"      (ttoken class)]
       ["mixin"      (ttoken mixin)]
       ["struct"     (ttoken struct)]
       ["extends"    (ttoken extends)]
       ["final"      (ttoken final)]
       ["impl"       (ttoken impl)]
       ["implements" (ttoken implements)]
       ["init"       (ttoken init)]
       ["export"     (ttoken export)]
       ["as"         (ttoken as)]
       ["at"         (ttoken at)]
       ["with"       (ttoken with)]
       ["fun"        (ttoken fun)]
       ["this"       (ttoken this)]
       ["my"         (ttoken my)]
       ["null"       (ttoken null)]
       ["isa"        (ttoken isa)]
       ["int"        (ttoken int)]
       ["bool"       (ttoken bool)]
       ["string"     (ttoken string)]
       ["float"      (ttoken float)]
       ["char"       (ttoken char)]
       ["Any"        (ttoken Any)]
       ["void"       (ttoken void)]
       ["while"      (ttoken while)]
       ["if"         (ttoken if)]
       ["cond"       (ttoken cond)]
       ["else"       (ttoken else)]
       ["new"        (ttoken new)]
       ["super"      (ttoken super)]
       ["cast"       (ttoken cast)]
       ["return"     (ttoken return)]
       ["true"       (token true  #t)]
       ["false"      (token false #f)]
       ["{"          (ttoken O_CURLY)]
       ["}"          (ttoken C_CURLY)]
       ["["          (ttoken O_BRACE)]
       ["]"          (ttoken C_BRACE)]
       ["("          (ttoken O_PAREN)]
       [")"          (ttoken C_PAREN)]
       [","          (ttoken COMMA)]
       [":"          (ttoken COLON)]
       [";"          (ttoken SEMI_COLON)]
       ["->"         (ttoken ARROW)]
       ["=>"         (ttoken THICK_ARROW)]
       ["@"          (ttoken AT)]
       ["_"          (token USCORE  '_)]
       ["="          (token BINDS   'binds)]
       ["!="         (token NEQ     'neq)]
       ["=="         (token EQUALS  'equal)]
       ["===="       (token CLS_EQ  'cls_eq)]
       ["!"          (token NOT     'not)]
       ["&&"         (token AND     'and)]
       ["||"         (token OR      'or)]
       ["<"          (token LT      'lt)]
       ["<="         (token LE      'le)]
       [">"          (token GT      'gt)]
       [">="         (token GE      'ge)]
       ["+"          (token PLUS    'plus)]
       ["-"          (token MINUS   'minus)]
       ["*"          (token TIMES   'times)]
       ["/"          (token DIV     'div)]
       ["%"          (token MOD     'mod)]
       ["."          (token DOT     'dot)]
       ["<:"         (token SUBTYPE 'subtype)]
       [lex:selector
        (token selector (string->number (substring lexeme 1 (string-length lexeme))))]
       [lex:ident
        (token id (string->symbol lexeme))]
       [lex:integer
        (token integer (string->number lexeme))]
       [lex:float
        (token floatnum (string->number lexeme))]
       [lex:character
        (token character (string-ref lexeme 1))]
       [lex:string
        (token string-lit (substring lexeme 1 (- (string-length lexeme) 1)))]
       [lex:line-comment
        (return-without-pos (honu-lexer input-port))]
       [(:: #\/ #\*)
        (begin (comment-lexer source-name start-pos input-port) ;; Get the rest of the comment...
               (return-without-pos (honu-lexer input-port)))]   ;; then get the next token.
       [(:+ lex:whitespace)
        (return-without-pos (honu-lexer input-port))]
       [(eof)
        (ttoken EOF)]
       [any-char (token UNPARSEABLE (string->symbol lexeme))]))
    honu-lexer)
  
  (define comment-lexer
    (lambda (source-name first-pos port)
      (letrec ([lxr (lexer-src-pos
                     [(:: #\/ #\*)
                      (begin (lxr input-port)                        ;; once for the nested comment
                             (return-without-pos (lxr input-port)))] ;; now finish out the current one
                     [(:: #\* #\/)
                      #f] ;; This will get ignored by the call to comment-lexer (whether nested or no)
                     [(eof)
                      (raise-read-error-with-stx
                       "Unexpected end of file while inside block comment."
                       (create-src-stx eof source-name first-pos end-pos))]
                     [(:~)
                      (return-without-pos (lxr input-port))])])
        (lxr port))))
  
  (define (syn-val lex a b c d)
    (values lex a b (position-offset c) (position-offset d)))

  (define get-block-comment
    (lexer
     [(:: #\/ #\*)
      (begin (get-block-comment input-port)    ;; once for the nested comment
             (get-block-comment input-port))]  ;; now finish out the current one
     [(:: #\* #\/)
      end-pos] ;; This will get ignored by the call to comment-lexer (whether nested or no)
     [(eof)
      end-pos]
     [(:~)
      (get-block-comment input-port)]))

  (define (colorize-string my-start-pos)
    (define lxr
      (lexer
       [#\" (syn-val "" 'string #f my-start-pos end-pos)]
       [(eof) (syn-val "" 'error #f my-start-pos end-pos)]
       [(:: #\\ #\") (lxr input-port)]
       [any-char     (lxr input-port)]))
    lxr)
    
  (provide get-syntax-token)
  (define get-syntax-token
    (lexer
     [lex:keyword
      (syn-val lexeme 'keyword #f start-pos end-pos)]
     [lex:operator
      (syn-val lexeme 'keyword #f start-pos end-pos)]
     [lex:selector
      (syn-val lexeme 'keyword #f start-pos end-pos)]

     [lex:separator
      (syn-val lexeme 'default #f start-pos end-pos)]

     [lex:grouping
      (syn-val lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]

     [lex:ident
      (syn-val lexeme 'identifier #f start-pos end-pos)]
     
     [(:or "true" "false" "null")
      (syn-val lexeme 'literal #f start-pos end-pos)]     
     [lex:integer
      (syn-val lexeme 'literal #f start-pos end-pos)]
     [lex:float
      (syn-val lexeme 'literal #f start-pos end-pos)]
     [lex:character
      (syn-val lexeme 'literal #f start-pos end-pos)]

     [#\"
      ((colorize-string start-pos) input-port)]
     
     [(:+ lex:whitespace)
      (syn-val lexeme 'whitespace #f start-pos end-pos)]
     
     [lex:line-comment
      (syn-val lexeme 'comment #f start-pos end-pos)]
     [(:: #\/ #\*)
      (syn-val lexeme 'comment #f start-pos (get-block-comment input-port))]
     
     [(eof)
      (syn-val lexeme 'eof #f start-pos end-pos)]
     
     [any-char
      (syn-val lexeme 'error #f start-pos end-pos)]))
     
  )
