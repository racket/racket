#cs
(module parser mzscheme
  (require "parsers/full-parser.ss"
           "parsers/advanced-parser.ss"
           "parsers/intermediate-access-parser.ss"
           "parsers/intermediate-parser.ss"
           "parsers/beginner-parser.ss"
           "parsers/general-parsing.ss"
           "parsers/parse-error.ss"
           "parsers/lexer.ss"
           (prefix err: "comb-parsers/parsers.scm")
           "ast.ss"
           "parameters.ss")
  
  (require (all-except (lib "lex.ss" "parser-tools") input-port)
           (lib "readerr.ss" "syntax")
           (lib "force.ss" "lazy"))
  (provide parse parse-interactions parse-expression parse-type parse-name lex-stream)
  
  ;function to lex in the entire port
  ;lex-port: port string -> (list position-token)
  (define (lex-port port filename)
    (port-count-lines! port)
    (file-path filename)
    (letrec ((getter
              (lambda (acc)
                (let ((cur-tok (get-token port)))
                  (if (eq? 'EOF (position-token-token cur-tok))
                      (cons cur-tok acc)
                      (getter (cons cur-tok acc)))))))
      (reverse! (getter null))))
  ;getter: (list position-token) -> (-> position-token)
  (define (getter token-list)
    (lambda ()
      (begin0 (car token-list)
              (unless (null? (cdr token-list))
                (set! token-list (cdr token-list))))))
  
  (define (error-builder parser old-parser lexed loc)
    (if (new-parser?)
        (lambda ()
          (printf "Syntax error detected~n")
          (let ([result (!!! (parser lexed loc))])
            #;(printf "~a~n" result)
            (if (list? result)
                (raise-read-error (cadr result)
                                  (car (car result))
                                  (cadr (car result))
                                  (caddr (car result))
                                  (cadddr (car result))
                                  (car (cddddr (car result))))
                (old-parser))))
        old-parser))

  ;main parsing function
  
  ;parse: port string symbol -> package
  (define (parse is filename level)
    (let* ((lexed (lex-port is filename))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner) 
         (determine-error (error-builder err:parse-beginner find-beginner-error lexed filename))
         (parse-beginner my-get))
        ((intermediate) 
         (determine-error (error-builder err:parse-intermediate find-intermediate-error lexed filename))
         (parse-intermediate my-get))
        ((intermediate+access)
         (determine-error (error-builder err:parse-intermediate+access (lambda () #t) lexed filename))
         (parse-intermediate+access my-get))
        ((advanced) 
         (determine-error (error-builder err:parse-advanced find-advanced-error lexed filename))
         (parse-advanced my-get))
        ((full) (parse-full my-get)))))
  
  ;parse-interactions: port string symbol -> (U Statement Expression)
  (define (parse-interactions is loc level)
    (let* ((lexed (lex-port is loc))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner) 
         (determine-error (error-builder err:parse-beginner-interact find-beginner-error-interactions lexed loc))
         (parse-beginner-interactions my-get))
        ((intermediate intermediate+access) 
         (determine-error (error-builder err:parse-intermediate-interact find-intermediate-error-interactions lexed loc))
         (parse-intermediate-interactions my-get))
        ((advanced) 
         (determine-error (error-builder err:parse-advanced-interact find-advanced-error-interactions lexed loc))
         (parse-advanced-interactions my-get))
        ((full) (parse-full-interactions my-get)))))
  
  ;parse-expression: port string symbol -> Expression
  (define (parse-expression is loc level)
    (let* ((lexed (lex-port is loc))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner)
         (determine-error find-beginner-error-expression)
         (parse-beginner-expression my-get))
        ((intermediate)
         (determine-error find-intermediate-error-expression)
         (parse-intermediate-expression my-get))
        ((advanced)
         (determine-error find-advanced-error-expression)
         (parse-advanced-expression my-get))
        ((full) (parse-full-expression my-get)))))

  ;parse-type: port string symbol -> type-spec
  (define (parse-type is loc level)
    (let* ((lexed (lex-port is loc))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner)
         (determine-error find-beginner-error-type)
         (parse-beginner-type my-get))
        ((intermediate)
         (determine-error find-intermediate-error-type)
         (parse-intermediate-type my-get))
        ((advanced)
         (determine-error find-advanced-error-type)
         (parse-advanced-type my-get))
        ((full) (parse-full-type my-get)))))
  
  ;parse-name: port string -> id
  ;Might not return if a parse-error occurs
  (define (parse-name is loc)
    (let* ((lexed (lex-port is loc))
           (get (getter lexed))
           (parse-error 
            (lambda (message start stop)
              (raise-read-error message
                                loc
                                (position-line start)
                                (position-col start)
                                (position-offset start)
                                (- (position-offset stop) (position-offset start)))))
           (first (get))
           (next (get))
           (first-tok (position-token-token first))
           (first-start (position-token-start-pos first))
           (first-end (position-token-end-pos first))
           (next-tok (position-token-token next)))
        (cond
          ((and (id-token? first-tok) (eof? next-tok))
           (make-id (token-value first-tok)
                    (make-src (position-line first-start)
                              (position-col first-start)
                              (position-offset first-start)
                              (- (position-offset first-end) (position-offset first-start))
                              loc)))
          ((not (eof? next-tok))
           (parse-error "Only one name may appear here, found excess content" 
                        (position-token-start-pos next) (position-token-end-pos next)))
          (else (parse-error "Only identifiars may be names: not a valid identifier" 
                             first-start first-end)))))
  
  )
