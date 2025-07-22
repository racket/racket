#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/list
         racket/match
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

;; Parse "rktio.h" to produce a sequence
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
;;         | (ref <type>)  ; opaque, needs to be deallocated somehow
;;         | (*ref <type>) ; transparent argument, can be represented by a byte string

(define output-file #f)
(define c-mode? #f)
(define .def-mode? #f)

(define input-file
  (command-line
   #:once-each
   [("-o") file "Write output to <file>"
    (set! output-file file)]
   [("-c") "Generate foreign-symbol registration"
    (set! c-mode? #t)]
   [("-d") "Generate .def file"
    (set! .def-mode? #t)]
   #:args
   (file)
   file))

(define-tokens content-tokens
  (ID NUM))

(define-empty-tokens delim-tokens
  (EOF WHITESPACE
       OPEN CLOSE BOPEN BCLOSE COPEN CCLOSE SEMI COMMA STAR LSHIFT EQUAL
       __RKTIO_H__ EXTERN EXTERN/NOERR EXTERN/STEP EXTERN/ERR
       DEFINE TYPEDEF ENUM STRUCT VOID UNSIGNED SHORT INT CHAR
       CONST NULLABLE BLOCKING MSG_QUEUE))

(define lex
  (lexer-src-pos
   [(eof) 'EOF]
   [";" 'SEMI]
   ["(" 'OPEN]
   [")" 'CLOSE]
   ["[" 'BOPEN]
   ["]" 'BCLOSE]
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
   ["char" 'CHAR]
   ["const" 'CONST]
   ["__RKTIO_H__" '__RKTIO_H__]
   ["RKTIO_EXTERN" 'EXTERN]
   ["RKTIO_EXTERN_ATOMIC" 'EXTERN]
   ["RKTIO_EXTERN_NOERR" 'EXTERN/NOERR]
   ["RKTIO_EXTERN_ATOMIC_NOERR" 'EXTERN/NOERR]
   ["RKTIO_EXTERN_STEP" 'EXTERN/STEP]
   ["RKTIO_EXTERN_ERR" 'EXTERN/ERR]
   ["RKTIO_NULLABLE" 'NULLABLE]
   ["RKTIO_BLOCKING" 'BLOCKING]
   ["RKTIO_MSG_QUEUE" 'MSG_QUEUE]
   [(:seq (:or #\_ (:/ #\A #\Z #\a #\z))
          (:* (:or #\_ (:/ #\A #\Z #\a #\z #\0 #\9))))
    (token-ID (string->symbol lexeme))]
   [(:seq (:? "-") "0" (:+ (:/ "0" "7")))
    (token-NUM (string->number lexeme 8))]
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
            [(DEFINE NULLABLE) #f]
            [(DEFINE BLOCKING) #f]
            [(DEFINE MSG_QUEUE) #f]
            [(STRUCT ID SEMI) #f]
            [(TYPEDEF <type> <id> SEMI)
             (if (eq? $2 $3)
                 #f
                 `(define-type ,(unstar $3) ,(shift-stars $3 $2)))]
            [(TYPEDEF <type> COPEN <fields> ID SEMI)
             (if (eq? $2 $5)
                 `(define-struct-type ,$2 ,$4)
                 (error 'parse "typedef struct names don't match at ~s" $5))]
            [(TYPEDEF <type> STAR OPEN STAR <id> CLOSE OPEN <params> SEMI)
             `(define-type ,$6 function-pointer)]
            [(TYPEDEF <type> OPEN STAR <id> CLOSE OPEN <params> SEMI)
             `(define-type ,$5 function-pointer)]
            [(<extern> <flags> <return-type> <id> OPEN <params> SEMI)
             (let ([r-type (shift-stars $4 $3)]
                   [id (unstar $4)])
               `(,@(adjust-errno $1 r-type id) ,$2 ,r-type ,id ,$6))]
            [(ENUM COPEN <enumeration> SEMI) `(begin . ,(enum-definitions $3))])
    (<extern> [(EXTERN) 'define-function/errno]
              [(EXTERN/STEP) 'define-function/errno+step]
              [(EXTERN/NOERR) 'define-function]
              [(EXTERN/ERR OPEN ID CLOSE) `(define-function/errno ,$3)])
    (<flags> [(BLOCKING) '(blocking)]
             [(MSG_QUEUE) '(msg-queue)]
             [() '()])
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
          [(STAR <id>) `(*ref ,$2)]
          [(ID BOPEN NUM BCLOSE) `(array ,$3 ,$1)])
    (<ids> [(<id> SEMI) (list $1)]
           [(<id> COMMA <ids>) (cons $1 $3)])
    (<expr> [(ID) $1]
            [(NUM) $1]
            [(OPEN <expr> CLOSE) $2]
            [(OPEN <expr> LSHIFT <expr> CLOSE) `(<< ,$2 ,$4)])
    (<type> [(ID) $1]
            [(CONST <type>) $2]
            [(NULLABLE <type>) `(nullable ,$2)]
            [(UNSIGNED SHORT) 'unsigned-short]
            [(UNSIGNED INT) 'unsigned]
            [(UNSIGNED CHAR) 'unsigned-8]
            [(UNSIGNED) 'unsigned]
            [(INT) 'int]
            [(SHORT) 'short]
            [(CHAR) 'char]
            [(VOID) 'void]
            [(STRUCT ID) $2])
    (<return-type> [(<type>) $1]))))

(define (adjust-errno def-kind r id)
  (cond
    [(eq? id 'rktio_init) '(define-function)] ; init is special, because we can't get an error
    [(eq? r 'rktio_bool_t) '(define-function)]
    [(eq? r 'void) '(define-function)]
    [(pair? def-kind) def-kind]
    [(eq? def-kind 'define-function) (list def-kind)]
    [(pair? r) `(,def-kind NULL)]
    [(eq? r 'rktio_ok_t) `(,def-kind #f)]
    [else (list def-kind)]))

(define (shift-stars from to)
  (cond
    [(and (pair? from)
          (eq? '*ref (car from)))
     `(*ref ,(shift-stars (cadr from) to))]
    [(and (pair? from)
          (eq? 'array (car from)))
     `(array ,(cadr from) ,(shift-stars (caddr from) to))]
    [else to]))

(define (unstar from)
  (cond
    [(and (pair? from)
          (eq? '*ref (car from)))
     (unstar (cadr from))]
    [(and (pair? from)
          (eq? 'array (car from)))
     (unstar (caddr from))]
    [else from]))

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
    (for/fold ([ht ht]) ([t (in-list '(char int unsigned-short unsigned-8
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
    [(and (pair? t) (eq? (car t) '*ref))
     (let ([s (update-type (cadr t))])
       (if (and as-argument?
                (or (and (pair? s)
                         (not (eq? (car s) 'nullable)))
                    (hash-ref defined-types s #f)))
           `(*ref ,s)
           `(ref ,s)))]
    [else t]))

(define (update-bind a #:as-argument? [as-argument? #f])
  `(,(update-type (car a) #:as-argument? as-argument?) ,(cadr a)))

(define (update-types e)
  (match e
    [`(,def ,flags ,ret ,name ,args)
     `(,def ,flags ,(update-type ret) ,name
        ,(map (lambda (a) (update-bind a #:as-argument? #t)) args))]
    [`(,def ,err-val ,flags ,ret ,name ,args)
     `(,def ,err-val ,flags ,(update-type ret) ,name
        ,(map (lambda (a) (update-bind a #:as-argument? #t)) args))]
    [_ e]))

(define (update-type-types e)
  (match e
    [`(define-struct-type ,name ,fields)
     `(define-struct-type ,name ,(map update-bind fields))]
    [_ e]))

(define content
  (append
   constant-content
   (map update-type-types type-content)
   (map update-types
        (filter (lambda (e) (not (or (constant-defn? e) (type-defn? e))))
                unsorted-content))))

(define (function-definition? e)
  (and (pair? e)
       (or (eq? 'define-function (car e))
           (eq? 'define-function/errno (car e))
           (eq? 'define-function/errno+step (car e)))))

(define (show-content)
  (cond
    [c-mode?
     (for ([e (in-list content)]
           #:when (function-definition? e))
       (define n (list-ref e (- (length e) 2)))
       (printf "Sforeign_symbol(~s, (void *)~a);\n" (symbol->string n) n))]
    [.def-mode?
     (for ([e (in-list content)]
           #:when (function-definition? e))
       (define n (list-ref e (- (length e) 2)))
       (printf "~a\n" n))]
    [else
     (printf "(begin\n")
     (for ([e (in-list content)]
           #:when e)
       (pretty-write e))
     (printf ")\n")]))

(if output-file
    (with-output-to-file output-file
      #:exists 'truncate
      (lambda ()
        (cond
          [c-mode?
           (printf "/* Extracted from rktio.h by rktio/parse.rkt */\n")]
          [.def-mode?
           (printf "EXPORTS\n")]
          [else
           (printf ";; Extracted from rktio.h by rktio/parse.rkt\n")])
        (show-content)))
    (show-content))
