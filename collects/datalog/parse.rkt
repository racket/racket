#lang racket
(require parser-tools/lex
         parser-tools/yacc
         "private/lex.rkt"
         "ast.rkt")

#|
5.1 Literals

A literal, is a predicate symbol followed by an optional parenthesized list of comma separated terms. A predicate symbol is either an identifier or a string. A term is either a variable or a constant. As with predicate symbols, a constant is either an identifier or a string. The following are literals:

     parent(john, douglas)
     zero-arity-literal
     aBcD(-0, "\n\FF")
     "="(3,3)
     ""(-0-0-0,&&&,***,"\00")
5.2 Clauses

A clause is a head literal followed by an optional body. A body is a comma separated list of literals. A clause without a body is called a fact, and a rule when it has one. The punctuation `:-' separates the head of a rule from its body. A clause is safe if every variable in its head occurs in some literal in its body. The following are safe clauses:

     parent(john, douglas)
     ancestor(A, B) :-
         parent(A, B)
     ancestor(A, B) :-
         parent(A, C),
         ancestor(C, B)
5.3 Programs

A Datalog reader consumes a Datalog program. A program is a sequence of zero or more statements, followed by an optional query. A statement is an assertion or a retraction. An assertion is a clause followed by a period, and it adds the clause to the database if it is safe. A retraction is a clause followed by a tilde, and it removes the clause from the database. A query is a literal followed by a question mark. The effect of reading a Datalog program is to modify the database as directed by its statements, and then to return the literal designated as the query. If no query is specified, a reader returns a literal know to have no answers. The following is a program:

     edge(a, b). edge(b, c). edge(c, d). edge(d, a).
     path(X, Y) :- edge(X, Y).
     path(X, Y) :- edge(X, Z), path(Z, Y).
     path(X, Y)?
|#

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
     (statements [(statement) (list $1)]
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
              [(term EQUAL term) (make-literal (make-srcloc $1-start-pos $3-end-pos) '= (list $1 $3))])
     (predicate-sym [(IDENTIFIER) (string->symbol $1)]
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
 [parse-literal (input-port? . -> . literal?)]
 [parse-clause (input-port? . -> . clause?)]
 [parse-statement (input-port? . -> . statement/c)]
 [parse-program (input-port? . -> . program/c)])