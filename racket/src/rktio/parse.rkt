#lang racket/base
(require racket/cmdline
         racket/pretty
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define output-file #f)

(define input-file
  (command-line
   #:once-each
   [("-o") file "Write output to <file>"
    (set! output-file file)]
   #:args
   (file)
   file))

(define-tokens content-tokens
  (ID NUM))

(define-empty-tokens delim-tokens
  (EOF WHITESPACE
       OPEN CLOSE COPEN CCLOSE SEMI COMMA STAR LSHIFT EQUAL
       __RKTIO_H__ EXTERN EXTERN/NOERR EXTERN/STEP
       DEFINE TYPEDEF ENUM STRUCT VOID UNSIGNED SHORT INT CONST))

(define lex
  (lexer-src-pos
   [(eof) 'EOF]
   [";" 'SEMI]
   ["(" 'OPEN]
   [")" 'CLOSE]
   ["{" 'COPEN]
   ["}" 'CCLOSE]
   ["*" 'STAR]
   ["," 'COMMA]
   ["<<" 'LSHIFT]
   ["=" 'EQUAL]
   [(:seq "#" (:* whitespace) "define") 'DEFINE]
   ["typedef" 'TYPEDEF]
   ["enum" 'ENUM]
   ["struct" 'STRUCT]
   ["void" 'VOID]
   ["unsigned" 'UNSIGNED]
   ["short" 'SHORT]
   ["int" 'INT]
   ["const" 'CONST]
   ["__RKTIO_H__" '__RKTIO_H__]
   ["RKTIO_EXTERN" 'EXTERN]
   ["RKTIO_EXTERN_NOERR" 'EXTERN/NOERR]
   ["RKTIO_EXTERN_STEP" 'EXTERN/STEP]
   [(:seq (:or #\_ (:/ #\A #\Z #\a #\z))
          (:* (:or #\_ (:/ #\A #\Z #\a #\z #\0 #\9))))
    (token-ID (string->symbol lexeme))]
   [(:seq (:? "-") (:+ (:/ "0" "9")))
    (token-NUM (string->number lexeme))]
   [(:seq "0x" (:+ (:/ "0" "9") (:/ "A" "F") (:/ "a" "f")))
    (token-NUM (string->number (substring lexeme 2) 16))]
   [(:+ (:or whitespace
             (:seq (:or "#include" "#if" "#endif") (:* (:~ "\n")))
             (:seq "/*"
                   (:* (:or (:~ "*")
                            (:seq (:+ "*") (:~ "/"))))
                   "*/")))
    'WHITESPACE]))

(define parse
  (parser
   (start <prog>)
   (end EOF)
   (tokens content-tokens
           delim-tokens)
   (error (lambda (a t v start end)
            (error 'parse "failed at ~s" (list a t v start end))))
   (src-pos)
   (grammar
    (<prog> [() null]
            [(<decl> <prog>) (cons $1 $2)])
    (<decl> [(DEFINE ID <expr>) `(define ,$2 ,$3)]
            [(DEFINE __RKTIO_H__ <expr>) #f]
            [(DEFINE EXTERN ID) #f]
            [(DEFINE EXTERN/NOERR EXTERN) #f]
            [(DEFINE EXTERN/STEP EXTERN) #f]
            [(STRUCT ID SEMI) #f]
            [(TYPEDEF <type> <id> SEMI)
             (if (eq? $2 $3)
                 #f
                 `(define-type ,(unstar $3) ,(shift-stars $3 $2)))]
            [(TYPEDEF <type> COPEN <fields> ID SEMI)
             (if (eq? $2 $5)
                 `(define-struct-type ,$2 ,$4)
                 (error 'parse "typedef struct names don't match at ~s" $5))]
            [(<extern> <return-type> <id> OPEN <params> SEMI)
             (let ([r-type (shift-stars $3 $2)])
               `(,(adjust-errno $1 r-type) ,r-type ,(unstar $3) ,$5))]
            [(ENUM COPEN <enumeration> SEMI) `(begin . ,(enum-definitions $3))])
    (<extern> [(EXTERN) 'define-function/errno]
              [(EXTERN/STEP) 'define-function/errno+step]
              [(EXTERN/NOERR) 'define-function])
    (<params> [(VOID CLOSE) null]
              [(<paramlist>) $1])
    (<paramlist> [(<type> <id> CLOSE) `((,(shift-stars $2 $1) ,(unstar $2)))]
                 [(<type> <id> COMMA <paramlist>) (cons `(,(shift-stars $2 $1) ,(unstar $2)) $4)])
    (<enumeration> [(CCLOSE) null]
                   [(ID CCLOSE) (list $1)]
                   [(ID COMMA <enumeration>) (cons $1 $3)]
                   [(ID EQUAL <expr> COMMA <enumeration>)
                    (cons `(,$1 ,$3) $5)])
    (<fields> [(CCLOSE) null]
              [(<type> <ids> <fields>)
               (append (map (lambda (id) `(,(shift-stars id $1) ,(unstar id))) $2)
                       $3)])
    (<id> [(ID) $1]
          [(STAR <id>) `(* ,$2)])
    (<ids> [(<id> SEMI) (list $1)]
           [(<id> COMMA <ids>) (cons $1 $3)])
    (<expr> [(ID) $1]
            [(NUM) $1]
            [(OPEN <expr> CLOSE) $2]
            [(OPEN <expr> LSHIFT <expr> CLOSE) `(<< ,$2 ,$4)])
    (<type> [(ID) $1]
            [(CONST <type>) $2]
            [(UNSIGNED SHORT) `unsigned-short]
            [(UNSIGNED INT) `unsigned]
            [(UNSIGNED) 'unsigned]
            [(INT) 'int]
            [(SHORT) 'short]
            [(VOID) 'void]
            [(STRUCT ID) $2])
    (<return-type> [(<type>) $1]))))

(define (adjust-errno def-kind r)
  (cond
    [(eq? r 'rktio_bool_t) 'define-function]
    [(eq? r 'void) 'define-function]
    [else def-kind]))

(define (shift-stars from to)
  (if (and (pair? from)
           (eq? '* (car from)))
      `(* ,(shift-stars (cadr from) to))
      to))

(define (unstar from)
  (if (and (pair? from)
           (eq? '* (car from)))
      (unstar (cadr from))
      from))

(define (enum-definitions l)
  (let loop ([l l] [i 0])
    (cond
      [(null? l) null]
      [(pair? (car l))
       (let ([i (cadar l)])
         (cons
          `(define ,(caar l) ,i)
          (loop (cdr l) (add1 i))))]
      [else
       (cons
        `(define ,(car l) ,i)
        (loop (cdr l) (add1 i)))])))

;; ----------------------------------------

(define content
  (call-with-input-file input-file
    (lambda (i)
      (port-count-lines! i)
      (parse (lambda ()
               (let loop ()
                 (let ([v (lex i)])
                   (cond
                     [(eq? (position-token-token v) 'WHITESPACE) (loop)]
                     [else v]))))))))

(define (show-content)
  (for ([e (in-list content)]
        #:when e)
    (if (and (pair? e)
             (eq? 'begin (car e)))
        (for ([e (in-list (cdr e))])
          (pretty-write e))
        (pretty-write e))))

(if output-file
    (with-output-to-file output-file
      #:exists 'truncate
      (lambda () (show-content)))
    (show-content))

