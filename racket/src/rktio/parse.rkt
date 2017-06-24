#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/list
         racket/match
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

;; Parse "rktio.h" to produce a seqeuence
;;  (define-constant <id> <const>) ...
;;  <type-def> ...
;;  <func-def> ...
;;
;; where
;;  <type-def> = (define-type <id> <id>)
;;             | (define-struct-type <id> ([<type> <name>] ...))
;;
;;  <func-def> = (define-function <type> <id> ([<type> <arg-name>] ...))
;;                 => never fails
;;             | (define-function/errno <err-v> <type> <id> ([<type> <arg-name>] ...))
;;                 => fails when result equals <err-v>
;;             | (define-function/errno+step <err-v> <type> <id> ([<type> <arg-name>] ...))
;;                 => fails when result equals <err-v>, keep step
;;
;;  <type> = <prim-type>
;;         | (ref <type>) ; opaque, needs to be deallocated somehow
;;         | (* <type>)   ; transparent argument, can be represented by a byte string

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
       __RKTIO_H__ EXTERN EXTERN/NOERR EXTERN/STEP EXTERN/ERR
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
   ["RKTIO_EXTERN_ERR" 'EXTERN/ERR]
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
    (<decl> [(DEFINE ID <expr>) `(define-constant ,$2 ,$3)]
            [(DEFINE __RKTIO_H__ <expr>) #f]
            [(DEFINE EXTERN ID) #f]
            [(DEFINE EXTERN/NOERR EXTERN) #f]
            [(DEFINE EXTERN/STEP EXTERN) #f]
            [(DEFINE EXTERN/ERR OPEN ID CLOSE EXTERN) #f]
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
             (let ([r-type (shift-stars $3 $2)]
                   [id (unstar $3)])
               `(,@(adjust-errno $1 r-type id) ,r-type ,id ,$5))]
            [(ENUM COPEN <enumeration> SEMI) `(begin . ,(enum-definitions $3))])
    (<extern> [(EXTERN) 'define-function/errno]
              [(EXTERN/STEP) 'define-function/errno+step]
              [(EXTERN/NOERR) 'define-function]
              [(EXTERN/ERR OPEN ID CLOSE) `(define-function/errno ,$3)])
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

(define (adjust-errno def-kind r id)
  (cond
    [(eq? id 'rktio_init) '(define-function)] ; init is special, because we can't get an error
    [(eq? r 'rktio_bool_t) '(define-function)]
    [(eq? r 'void) '(define-function)]
    [(pair? def-kind) def-kind]
    [(pair? r) '(define-function/errno NULL)]
    [(eq? r 'rktio_ok_t) '(define-function/errno #f)]
    [else (list def-kind)]))

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
          `(define-constant ,(caar l) ,i)
          (loop (cdr l) (add1 i))))]
      [else
       (cons
        `(define-constant ,(car l) ,i)
        (loop (cdr l) (add1 i)))])))

;; ----------------------------------------

(define unsorted-unflattened-content
  (call-with-input-file input-file
    (lambda (i)
      (port-count-lines! i)
      (parse (lambda ()
               (let loop ()
                 (let ([v (lex i)])
                   (cond
                     [(eq? (position-token-token v) 'WHITESPACE) (loop)]
                     [else v]))))))))

(define unsorted-content
  (for*/list ([l (in-list unsorted-unflattened-content)]
              [e (in-list (if (and (pair? l)
                                   (eq? 'begin (car l)))
                              (cdr l)
                              (list l)))])
    e)) 

(define (constant-defn? e)
  (and (pair? e)
       (eq? (car e) 'define-constant)))

(define (type-defn? e)
  (and (pair? e)
       (or (eq? (car e) 'define-type)
           (eq? (car e) 'define-struct-type))))

(define constant-content
  (filter constant-defn? unsorted-content))

(define type-content
  (filter type-defn? unsorted-content))

(define defined-types
  (let ([ht (for/hash ([e (in-list type-content)])
              (values (cadr e) #t))])
    (for/fold ([ht ht]) ([t (in-list '(char int unsigned-short
                                            intptr_t rktio_int64_t))])
      (hash-set ht t #t))))

;; A pointer to a defined type in an argument position
;; is transparent, and it make sense to pass a byte
;; string directly (possibly to be filled in).
;; A pointer to an undefined type is opaque, and a pointer
;; to a defined type is "opaque" in a result position in
;; the sense that it should be explicitly dereferenced and
;; explicitly freed.
(define (update-type t #:as-argument? [as-argument? #f])
  (cond
    [(and (pair? t) (eq? (car t) '*))
     (let ([s (update-type (cadr t))])
       (if (and as-argument?
                (or (pair? s)
                    (hash-ref defined-types s #f)))
           `(* ,s)
           `(ref ,s)))]
    [else t]))

(define (update-bind a #:as-argument? [as-argument? #f])
  `(,(update-type (car a) #:as-argument? as-argument?) ,(cadr a)))

(define (update-types e)
  (match e
    [`(,def ,ret ,name ,args)
     `(,def ,(update-type ret) ,name
        ,(map (lambda (a) (update-bind a #:as-argument? #t)) args))]
    [else e]))

(define (update-type-types e)
  (match e
    [`(define-struct-type ,name ,fields)
     `(define-struct-type ,name ,(map update-bind fields))]
    [else e]))

(define content
  (append
   constant-content
   (map update-type-types type-content)
   (map update-types
        (filter (lambda (e) (not (or (constant-defn? e) (type-defn? e))))
                unsorted-content))))

(define (show-content)
  (printf "(begin\n")
  (for ([e (in-list content)]
        #:when e)
    (pretty-write e))
  (printf ")\n"))

(if output-file
    (with-output-to-file output-file
      #:exists 'truncate
      (lambda () (show-content)))
    (show-content))

