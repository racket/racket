(module parser mzscheme
  
  (require (lib "string.ss"))
  (require (lib "yacc.ss" "parser-tools"))
  (require (lib "lex.ss" "parser-tools"))
  (require (lib "list.ss"))
  
  (require "xl-util.ss")
  (require "formula.ss")
  
  (provide call-parser
           make-parser
           parse-unit)
  
  (define (string-downcase s)
    (let ([tmp (string-copy s)])
      (string-lowercase! tmp)
      tmp))
  
  (define-tokens data-tokens 
                 (STRING 
                  QUOTED_STRING 
                  NUMBER
                  CELL-REF
                  UNIT
                  IDENTIFIER))
  
  (define-tokens op-tokens
                 (ADD-OP
                  MULT-OP
                  BOOL-OP
                  TBL-BEGIN
                  EXP-OP))
  
  (define-empty-tokens cmp-tokens
                       (LT LTE NE GT GTE)) 
  
  (define-empty-tokens syntax-tokens
                       (INVALID-TOKEN
                        EOF
                        EQ 
                        NEG ; for precedence
                        LPAREN RPAREN
                        LBRACE RBRACE
                        COMMA
                        RANGE-SEP SHEET-SEP))
  
  (define-lex-abbrevs 
   [digit (- "0" "9")]
   [pos-digit (- "1" "9")]
   [number-sequence (+ digit)]
   [number-with-point (@ number-sequence "." number-sequence)]
   [unsigned-number (: number-sequence number-with-point)]
   [number unsigned-number]
   [pos-int (@ pos-digit number-sequence)]
   [letter (: (- "a" "z") (- "A" "Z"))]
   [letter-to-V (: (- "a" "v") (- "A" "V"))]
   [letter-to-I (: (- "a" "h") (- "A" "H"))]
   [alphanum_ (: digit letter "_")]
   [alphanum (: letter digit)]
   [cell-letter-sequence (: letter (@ letter-to-I letter) (@ (: "i" "I") letter-to-V))]
   [cell-number-sequence (: pos-digit (@ pos-digit digit)
                            (@ pos-digit digit digit)
                            (@ pos-digit digit digit digit))]
   [cell-reference (@ (? "$") cell-letter-sequence (? "$") cell-number-sequence)]
   [whitespace (: #\space #\tab #\newline #\return)]
   [add-op (: "+" "-")]
   [mult-op (: "*" "/")]
   [exp-op "^"]
   [bool-op (: "<" ">" "<=" ">=" "=" "<>")]
   [tbl-begin "=TABLE("]
   [function-name 
    (: "NOT"
       "AND"
       "OR"
       "SUM"
       "AVERAGE"
       "MIN"
       "MAX"
       )]
   [special-unit-chars (: "(" ")" "^" "*" "-" "/")]
   [unit-chars (^ (: whitespace special-unit-chars))]
   [non-num (^ (: digit whitespace special-unit-chars))]
   [unit (@ (* unit-chars) non-num (* unit-chars))]
   [identifier (: (@ (* letter) (* alphanum_) "_" (* alphanum_))
                  (@ (* alphanum) letter)
                  (@ letter letter letter (* alphanum))
                  (@ (: (- "j" "z") (- "J" "Z")) letter digit)
                  (@ (: "i" "I") (: (- "w" "z") (- "W" "Z")) digit)
                  (@ letter "0"))])
  
  (define unit-lex
    (lexer
     [whitespace (unit-lex input-port)]
     [(eof) 'EOF]
     [number (token-NUMBER (string->number lexeme))]
     [mult-op (token-MULT-OP (string->symbol lexeme))]
     ["-" (token-MULT-OP (string->symbol lexeme))]
     [exp-op (token-EXP-OP (string->symbol lexeme))]
     [unit (token-UNIT lexeme)]
     ["(" (token-LPAREN)]
     [")" (token-RPAREN)]))
  
  (define xl-lex
    (lexer
     [whitespace 
      (xl-lex input-port)]
     [(eof) 'EOF]
     [number
      (token-NUMBER (string->number lexeme))]
     [add-op
      (token-ADD-OP (string->symbol lexeme))]
     [mult-op
      (token-MULT-OP (string->symbol lexeme))]
     [exp-op
      (token-EXP-OP (string->symbol lexeme))]
     [bool-op
      (token-BOOL-OP (string->symbol lexeme))]
     [cell-reference
      (token-CELL-REF 
       (string->symbol 
        (list->string
         (filter (lambda (c) (not (equal? c #\$)))
                 (string->list (string-downcase lexeme))))))]
     [identifier (token-IDENTIFIER lexeme)]
     [tbl-begin (token-TBL-BEGIN lexeme)]
     [":" (token-RANGE-SEP)]
     ["," (token-COMMA)]
     ["(" (token-LPAREN)]
     [")" (token-RPAREN)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     ["=" (token-EQ)]))
  
  (define make-parser 
    (lambda (symbol-table)
      (parser
       
       (start start)
       (end EOF)
       (error (lambda (a b c) (void)))
       (tokens data-tokens 
               op-tokens
               cmp-tokens
               syntax-tokens)
       
       (precs (left ADD-OP)
              (left MULT-OP)
              (left EXP-OP)
              (left BOOL-OP)
              (left NEG)
              (right LPAREN)
              (left RPAREN))
       
       (grammar
        
        (start [() #f]
               [(error start) $2]
               [(expr) $1])
        
        (formula [(EQ expr) $2])
        
        (expr
         ; cells have their own names
         [(CELL-REF) (make-cell-ref $1 (list $1))]
         [(IDENTIFIER)
          (let ([cell-loc (cadr (assoc $1 symbol-table))])
            (make-named-cell-ref cell-loc (list cell-loc) (string->symbol $1)))]
         [(NUMBER) (make-xl-number (gensym) null $1)]
         [(TBL-BEGIN expr COMMA RPAREN)
          (make-tbl-top
           (gensym)
           (formula-dependencies $2)
           $2)]
         [(TBL-BEGIN COMMA expr RPAREN)
          (make-tbl-left
           (gensym)
           (formula-dependencies $3)
           $3)]
         [(LPAREN expr RPAREN) $2]
         [(expr ADD-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr MULT-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr EXP-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr BOOL-OP expr)
          (make-boolean-op
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(ADD-OP expr) (prec NEG)
          (cond [(xl-number? $2) 
                 (make-xl-number (formula-name $2) 
                                 null 
                                 (- 0 (xl-number-val $2)))]
                [else
                 (make-unary-op 
                  (gensym)
                  (formula-dependencies $2)
                  $1 $2)])]
         [(IDENTIFIER LPAREN RPAREN)
          (make-application
           (gensym)
           empty (string->symbol (string-downcase $1)) empty)]
         [(IDENTIFIER LPAREN args RPAREN)
          (make-application 
           (gensym) 
           (cadr $3) (string->symbol (string-downcase $1)) (car $3))])
        
        ; returns list of symbols denoting cells
        (cell-range
         [(CELL-REF RANGE-SEP CELL-REF)
          (get-range-cells $1 $3)])
        
        ; returns two-elt list, (expr deps)
        (arg
         [(expr) (list (list $1) (formula-dependencies $1))]
         [(cell-range) (list (map (lambda (c)
                                    (make-cell-ref c (list c)))
                                  $1)
                             $1)])
        (args
         [(arg) (list (car $1) (cadr $1))]
         [(arg COMMA args) 
          (let ([arg1 (car $1)]
                [dep1 (cadr $1)]
                [arg2 (car $3)]
                [dep2 (cadr $3)])
            (list (append arg1 arg2) (append dep1 dep2)))])))))
  
  (define (call-parser parser s)
    (let* ([port (open-input-string s)]
	   [lex-thunk (lambda () (xl-lex port))])
      (parser lex-thunk)))
  
  (define unit-parser 
    (parser
     
     (start start)
     (end EOF)
     (error (lambda (a b c) (void)))
     (tokens data-tokens 
             op-tokens
             cmp-tokens
             syntax-tokens)
     
     (precs (left MULT-OP)
            (left EXP-OP)
            (left NEG)
            (right LPAREN)
            (left RPAREN))
     
     (grammar
      
      (start [() #f]
             [(error start) $2]
             [(expr) $1])
      
      (expr
       [(expr EXP-OP expr)
        (map (lambda (u)
               (cond [(number? $3) (list (first u) (* $3 (second u)))]
                     [else u])) $1)]
       [(expr MULT-OP expr)
        (let ([invert (lambda (l)
                        (map (lambda (u)
                               (list (first u) (- 0 (second u)))) l))])
          (cond [(eq? $2 '/) (append $1 (invert $3))]
                [else (append $1 $3)]))]
       [(LPAREN expr RPAREN) $2]
       [(LPAREN RPAREN) (list (list 'empty_unit 1))]
       [(NUMBER) $1]
       [(MULT-OP expr)(prec NEG) (- 0 $2)]
       [(UNIT) (list (list (string->symbol $1) 1))]))))
  
  (define (parse-unit s)
    (let* ([port (open-input-string s)]
           [lex-thunk (lambda () (unit-lex port))])
      (unit-parser lex-thunk))))
