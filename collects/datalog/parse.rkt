#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "private/lex.rkt"
         "ast.rkt")

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define-values
  (program-parser statement-parser clause-parser literal-parser)
  (apply
   values
   (parser
    (start program statement clause literal)
    (end EOF)
    (tokens dtokens dpunct)
    (src-pos)
    (error
     (lambda (tok-ok? tok-name tok-value start-pos end-pos)
       (raise-syntax-error 'datalog
                           (if tok-ok?
                               (format "Unexpected token ~S" tok-name)
                               (format "Invalid token ~S" tok-name))
                           (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
    (grammar
     (program [(statements) $1])
     (statements [() empty]
                 [(statement statements) (list* $1 $2)])
     (statement [(assertion) $1]
                [(query) $1]
                [(retraction) $1])
     (assertion [(clause DOT) (make-assertion (make-srcloc $1-start-pos $2-end-pos) $1)])
     (retraction [(clause TILDE) (make-retraction (make-srcloc $1-start-pos $2-end-pos) $1)])
     (query [(literal QMARK) (make-query (make-srcloc $1-start-pos $2-end-pos) $1)])
     (clause [(literal TSTILE body) (make-clause (make-srcloc $1-start-pos $3-end-pos) $1 $3)]
             [(literal) (make-clause (make-srcloc $1-start-pos $1-end-pos) $1 empty)])
     (body [(literal COMMA body) (list* $1 $3)]
           [(literal) (list $1)])
     (literal [(predicate-sym LPAREN RPAREN) (make-literal (make-srcloc $1-start-pos $3-end-pos) $1 empty)]
              [(predicate-sym LPAREN terms RPAREN) (make-literal (make-srcloc $1-start-pos $4-end-pos) $1 $3)]
              [(predicate-sym) (make-literal (make-srcloc $1-start-pos $1-end-pos) $1 empty)]
              [(term NEQUAL term) (make-literal (make-srcloc $1-start-pos $3-end-pos) '!= (list $1 $3))]
              [(term EQUAL term) (make-literal (make-srcloc $1-start-pos $3-end-pos) '= (list $1 $3))])
     (predicate-sym [(IDENTIFIER) (make-predicate-sym (make-srcloc $1-start-pos $1-end-pos) (string->symbol $1))]
                    [(STRING) $1])
     (terms [(term) (list $1)]
            [(term COMMA terms) (list* $1 $3)])
     (term [(VARIABLE) (make-variable (make-srcloc $1-start-pos $1-end-pos) (string->symbol $1))]
           [(constant) (make-constant (make-srcloc $1-start-pos $1-end-pos) $1)])
     (constant [(IDENTIFIER) (string->symbol $1)]
               [(STRING) $1]))
    
    (suppress))))

(define ((mk-parser which) ip)
  (define (go)
    (port-count-lines! ip)
    (which (lambda () (dlexer ip))))
  (if (current-source-name)
      (go)
      (parameterize ([current-source-name (object-name ip)]
                     [file-path (object-name ip)])
        (go))))  

(define parse-literal (mk-parser literal-parser))
(define parse-clause (mk-parser clause-parser))
(define parse-statement (mk-parser statement-parser))
(define parse-program (mk-parser program-parser))

(provide/contract
 [current-source-name (parameter/c any/c)]
 [parse-literal (input-port? . -> . literal?)]
 [parse-clause (input-port? . -> . clause?)]
 [parse-statement (input-port? . -> . statement/c)]
 [parse-program (input-port? . -> . program/c)])
