(module parse-error mzscheme
  
  (require "lexer.ss" "general-parsing.ss"
           "../parameters.ss"
           mzlib/etc
           syntax/readerr
           (all-except parser-tools/lex input-port))
  
  (provide
   find-beginner-error find-beginner-error-interactions find-beginner-error-expression find-beginner-error-type           
   find-intermediate-error find-intermediate-error-interactions find-intermediate-error-expression find-intermediate-error-type
   find-advanced-error find-advanced-error-interactions find-advanced-error-expression find-advanced-error-type)

  ;(print-struct #t)
  
  (define level (make-parameter 'beginner))
  (define (beginner?) (eq? (level) 'beginner))
  (define (intermediate?) (eq? (level) 'intermediate))
  (define (advanced?) (eq? (level) 'advanced))
  
  ;find-error: symbol -> (-> (U void #t))
  (define (find-error level-set)
    (lambda ()
      (level level-set)
      (let ((getter ((lex-stream))))
        (parse-program null (getter) 'start getter))))

  ;find-expression-error: symbol -> (-> (U void #t))
  (define (find-expression-error level-set)
    (lambda ()
      (level level-set)
      (let ((getter ((lex-stream))))
        (parse-expression null (getter) 'start getter #f #f))))
  
  ;find-type-error: symbol -> (-> (U void #t))
  (define (find-type-error level-set)
    (lambda ()
      (level level-set)
      (let ((getter ((lex-stream))))
        (parse-type null (getter) 'start getter))))
  
  ;find-beginner-error: -> (U void #t)
  (define find-beginner-error (find-error 'beginner))
  
  (define find-beginner-error-expression (find-expression-error 'beginner))

  (define find-beginner-error-type (find-type-error 'beginner))
  
  ;find-beginner-error-interaction: -> (U bool or token)
  ;Should not return
  (define (find-beginner-error-interactions)
    (let* ((getter ((lex-stream)))
           (first-tok (getter)))
      (let ((returned-tok 
             (case (get-token-name (get-tok first-tok))
               ((EOF) #t)
               ((if return) (parse-statement null first-tok 'start getter #f #f #f))
               ;Taken from Intermediate to allow interaction to say int x = 4;
               ((IDENTIFIER)
                (let ((next (getter)))
                  (if (id-token? (get-tok next))
                      (parse-field first-tok next 'start getter)
                      (parse-expression first-tok next 'name getter #f #f))))
               (else
                (if (prim-type? (get-tok first-tok))
                    (parse-field first-tok (getter) 'start getter)
                    (parse-expression null first-tok 'start getter #f #f))))))
        
        (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
            returned-tok
            (if (and (pair? returned-tok) (semi-colon? (get-tok returned-tok)))
                (parse-error "';' is not allowed here" (get-start returned-tok) (get-end returned-tok))
                (parse-error (format "Only 1 statement, expression, or definition is allowed, found extra input ~a"
                                     (format-out (get-tok returned-tok)))
                             (get-start returned-tok) (get-end returned-tok)))))))
  
  ;find-intermediate-error: -> (U void #t)
  (define find-intermediate-error (find-error 'intermediate))

  ;find-intermediate-error-expression: -> void
  (define find-intermediate-error-expression (find-expression-error 'intermediate))

  ;find-intermediate-error-type: -> void
  (define find-intermediate-error-type (find-type-error 'intermediate))
  
  ;find-error-interaction: -> (U bool or token)
  ;Should not return
  (define (find-intermediate-error-interactions)
    (let* ((getter ((lex-stream)))
           (first-tok (getter)))
      (level 'intermediate)
      (let ((returned-tok 
             (case (get-token-name (get-tok first-tok))
               ((EOF) #t)
               ((if return O_BRACE) (parse-statement null first-tok 'start getter #t #f #f))
               ((IDENTIFIER)
                (let ((next (getter)))
                  (if (id-token? (get-tok next))
                      (parse-statement first-tok next 'local getter #t #f #f)
                      (parse-expression first-tok next 'name getter #t #f))))
               (else
                (if (prim-type? (get-tok first-tok))
                    (parse-statement null first-tok 'start getter #t #f #f)
                    (parse-expression null first-tok 'start getter #t #f))))))
        (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
            returned-tok
            (if (and (pair? returned-tok) (semi-colon? (get-tok returned-tok)))
                (parse-error "';' is not allowed here" (get-start returned-tok) (get-end returned-tok))
                (parse-error (format "Only 1 statement or expression is allowed, found extra input ~a" 
                                     (format-out (get-tok returned-tok)))
                             (get-start returned-tok) (get-end returned-tok)))))))

  ;find-advanced-error: -> (U void #t)
  (define (find-advanced-error)
    (let ((getter ((lex-stream))))
      (level 'advanced)
      (parse-package null (getter) 'start getter)))

  ;find-advanced-error-expression: -> void
  (define find-advanced-error-expression (find-expression-error 'advanced))

  ;find-advanced-error-type: -> void
  (define find-advanced-error-type (find-type-error 'advanced))
  
  ;find-error-interaction: -> (U bool or token)
  ;Should not return
  (define (find-advanced-error-interactions)
    (let* ((getter ((lex-stream)))
           (first-tok (getter)))
      (level 'advanced)
      (let ((returned-tok 
             (case (get-token-name (get-tok first-tok))
               ((EOF) #t)
               ((if return O_BRACE for do while break continue) 
                (parse-statement null first-tok 'start getter #t #f #f))
               ((IDENTIFIER)
                (let ((next (getter)))
                  (cond
                    ((id-token? (get-tok next))
                     (parse-statement first-tok next 'local getter #t #f #f))
                    ((o-bracket? (get-tok next))
                     (parse-statement first-tok next 'local getter #t #f #f))
                    (else 
                     (parse-expression first-tok next 'name getter #t #f)))))
               (else 
                (if (prim-type? (get-tok first-tok))
                    (parse-statement null first-tok 'start getter #t #f #f)
                    (parse-expression null first-tok 'start getter #t #f))))))
        (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
            returned-tok
            (if (and (pair? returned-tok) (semi-colon? (get-tok returned-tok)))
                (parse-error "';' is not allowed here" (get-start returned-tok) (get-end returned-tok))
                (parse-error (format "Only 1 statement or expression is allowed, found extra input ~a"
                                     (format-out (get-tok returned-tok)))
                             (get-start returned-tok) (get-end returned-tok)))))))
  
  ;;-----------------------------------------------------------------------------------------------------------
  ;;Functions for parsing and reporting errors
  
  ;parse-error: string position position
  (define (parse-error message start stop)
    (raise-read-error message
                      (file-path)
                      (position-line start)
                      (position-col start)
                      (+ (position-offset start) (interactions-offset))
                      (- (position-offset stop)
                         (position-offset start))))

  ;token = (list lex-token position position)
  (define get-tok position-token-token)
  (define get-start position-token-start-pos)
  (define (get-end token) 
    (if (or (eq? (get-token-name (get-tok token)) 'STRING_LIT)
            (eq? (get-token-name (get-tok token)) 'STRING_ERROR))
        (cadr (token-value (get-tok token)))
        (position-token-end-pos token)))
  
  ;output-format: token bool -> string
  (define format-out
    (opt-lambda (tok [full? #t])
      (cond
        ((separator? tok) 
         (case (get-token-name tok)
           ((O_BRACE) "{")
           ((C_BRACE) "}")
           ((O_PAREN) "(")
           ((C_PAREN) ")")
           ((O_BRACKET) "[")
           ((C_BRACKET) "]")
           ((SEMI_COLON) ";")
           ((COMMA) ",")
           ((PERIOD) ".")))
        ((eq? (get-token-name tok) 'OR) (if full? "operator ||" "||"))
        ((eq? (get-token-name tok) 'PIPE) (if full? "operator |" "||"))
        ((java-keyword? tok) (if full? 
                            (format "reserved word ~a" (get-token-name tok)) (get-token-name tok)))
        ((id-token? tok) (if full? (format "identifier ~a" (token-value tok)) (token-value tok)))
        ((eq? (get-token-name tok) 'STRING_LIT) (if full? (format "string ~a" (car (token-value tok)))
                                                    (car (token-value tok))))
        ((eq? (get-token-name tok) 'NULL_LIT) (if full? "null value" "null"))
        ((eq? (get-token-name tok) 'TRUE_LIT) (if full? "boolean value true" "true"))
        ((eq? (get-token-name tok) 'FALSE_LIT) (if full? "boolean value false" "false"))
        ((literal-token? tok) (if full? (format "value ~a" (token-value tok)) (token-value tok)))
        ((eq? (get-token-name tok) 'STRING_ERROR)
         (format "malformed string ~a" (car (token-value tok))))
        ((eq? (get-token-name tok) 'NUMBER_ERROR)
         (format "malformed number ~a" (token-value tok)))
        ((eq? (get-token-name tok) 'HEX_LIT)
         (format "hexadecimal formatted number ~a" (token-value tok)))
        ((eq? (get-token-name tok) 'OCT_LIT)
         (format "octal formatted number ~a" (token-value tok)))
        ((eq? (get-token-name tok) 'OTHER_SPECIAL)
         (parse-error "Found a special which is not a legal character in ProfessorJ" 
                      (cadr (token-value tok)) (caddr (token-value tok))))
        ((eq? (get-token-name tok) 'TEST_SUITE) (format "Test Suite Test"))
        ((eq? (get-token-name tok) 'INTERACTIONS_BOX) (format "Java Interactions Box"))
        ((eq? (get-token-name tok) 'EXAMPLE) (format "Java Example Box"))
        ((eq? (get-token-name tok) 'CLASS_BOX) (format "Java Class Box"))
        (else (get-token-name tok)))))

  ;parse-package: token token symbol (-> token) -> void
  (define (parse-package pre cur-tok state getter)
;    (printf "parse-package state: ~a pre ~a cur-tok: ~a~n" state pre cur-tok)
    (let* ((tok (get-tok cur-tok))
           (tokN (get-token-name tok))
           (srt (get-start cur-tok))
           (end (get-end cur-tok))
           (out (format-out tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case tokN
           ((EOF) #t)
           ((package)
            (let ((next (getter)))
              (if (id-token? (get-tok next))
                  (let ((after-id (getter)))
                    (cond
                      ((dot? (get-tok after-id))
                       (parse-package cur-tok (parse-name (getter) getter #f) 'semi-colon getter))
                      ((semi-colon? (get-tok after-id))
                       (parse-program after-id (getter) 'start getter))
                      (else
                       (parse-error 
                        (format "'package' must have a name followed by ';'. ~a ~a is not a legal name" 
                                (format-out (get-tok next)) (format-out (get-tok after-id))) 
                        (get-start next) (get-end after-id)))))
                  (parse-error (format "'package' must have a name followed by ';'. ~a is not allowed" 
                                       (format-out (get-tok next)))
                               srt (get-end next)))))
           ((IDENTIFIER)
            (if (close-to-keyword? tok 'package)
                (parse-error 
                 (format "~a is close to 'package' but is either miscapitalized or mispelled" (token-value tok))
                 srt end)
                (parse-program pre cur-tok 'start getter)))
           (else
            (parse-program pre cur-tok 'start getter))))
        ((semi-colon)
         (case tokN
           ((EOF) (parse-error "'package' must have a name followed by a ';'" ps pe))
           ((SEMI_COLON) (parse-program cur-tok (getter) 'start getter))
           (else (parse-error (format "'package' must have a name followed by a ';'. ~a is not allowed" out) ps end)))))))

  
  ;parse-program: token token symbol (-> token) -> (U void bool)
  (define (parse-program pre cur-tok state getter)
    ;(printf "parse-program state: ~a pre: ~a cur-tok:~a~n" state pre cur-tok)
    (let* ((tok (get-tok cur-tok))
           (tokN (get-token-name tok))
           (srt (get-start cur-tok))
           (end (get-end cur-tok))
           (out (format-out tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case tokN
           ((EOF) #t)
           ((import) 
            (let ((next (getter)))
              (if (id-token? (get-tok next))
                  (let ((after-id (getter)))
                    (cond
                      ((dot? (get-tok after-id))
                       (parse-program cur-tok (parse-name (getter) getter #t) 'semi-colon getter))
                      ((semi-colon? (get-tok after-id))
                       (parse-program after-id (getter) 'start getter))
                      (else
                       (parse-error 
                        (format "'import' must have a name followed by ';'. ~a is not allowed"
                                (format-out (get-tok next)))
                        srt (get-end after-id)))))
                  (parse-error (format "'import' must have a name followed by ';'. ~a is not allowed" out)
                               srt (get-end next)))))
           ((IDENTIFIER)
            (if (close-to-keyword? tok 'import)
                (if (miscapitalized? tok "import")
                    (parse-error "keyword 'import' must be all lower-case letters, and here is not" srt end)
                    (parse-error 
                     (format "~a is close to keyword 'import' but is mispelled" (token-value tok))
                     srt end))
                (parse-definition pre cur-tok 'start getter)))
           ((INTERACTIONS_BOX TEST_SUITE) (parse-definition cur-tok (getter) 'start getter))
           (else
            (parse-definition pre cur-tok 'start getter))))
        ((semi-colon)
         (case tokN
           ((EOF) (parse-error "'import' must have a name followed by a ';'" ps pe))
           ((SEMI_COLON) (parse-program cur-tok (getter) 'start getter))
           ((*)
            (let ((after-star (getter)))
              (cond
                ((semi-colon? (get-tok after-star)) (parse-program after-star (getter) 'start getter))
                (else (parse-error (format "'import' must have a name followed by a ';'. ~a is not allowed" 
                                           (format-out (get-tok after-star))) ps (get-end after-star))))))
           (else (parse-error (format "'import' must have a name followed by a ';'. ~a is not allowed" out) ps end)))))))
  
  ;parse-definition: token token symbol (-> token) -> void
  (define (parse-definition pre cur-tok state getter)
    ;(printf "parse-definition state ~a pre: ~a cur-tok ~a~n" state pre cur-tok)
    (let* ((tok (get-tok cur-tok))
           (tokN (get-token-name tok))
           (srt (get-start cur-tok))
           (end (get-end cur-tok))
           (out (format-out tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
               
      (case state
        ((start) 
         (case tokN
           ((EOF) #t)
           ((class) (parse-definition cur-tok (getter) 'class-id getter))
           ((abstract)
            (if (beginner?)
                (parse-error "Expected class or interface definition, 'abstract' not allowed here" srt end)
                (let* ((next (getter))
                       (next-tok (get-tok next)))
                  (cond
                    ((class? next-tok) (parse-definition cur-tok next state getter))
                    ((eof? next-tok) (parse-error "'abstract' should be followed by a class definition." srt end))
                    (else 
                     (if (close-to-keyword? next-tok 'class)
                         (parse-error (format "Expected 'class' after 'abstract', found ~a which is incorrectly spelled or capitalized."
                                              (token-value next-tok))
                                      srt
                                      (get-end next))
                         (parse-error (format "'abstract' must be immediately followed by 'class' not ~a." (format-out next-tok))
                                      srt
                                      (get-end next))))))))
           ((interface) 
            ;(if (or (intermediate?) (advanced?))
            (parse-definition cur-tok (getter) 'interface-id getter)
            ;(parse-error (format "Expected class definition, found ~a which may not be written here" out) srt end)))
            )
           ((public)
            (if (advanced?)
                (parse-definition cur-tok (getter) 'start getter)
                (parse-error "Expected class definition, found 'public' which may not be written here" srt end)))
           ((INTERACTIONS_BOX TEST_SUITE CLASS_BOX) (parse-definition cur-tok (getter) 'start getter))
           ((import)
            (parse-error "Expected class definition, found 'import', which may only appear at the top of a file" srt end))
           ((package)
            (if (advanced?)
                (parse-error "Expected class definition, found 'package' declaration. package must be the first item of a file" srt end)
                (parse-error "Expected class definition, found 'package' which may not appear here" srt end)))
           (else 
            (cond
              ((close-to-keyword? tok 'class)
                (parse-error (format "expected 'class', found ~a which is incorrectly spelled or capitalized"
                                     (token-value tok))
                             srt end))
              ((close-to-keyword? tok 'abstract)
               (if (beginner?)
                   (parse-error (format "Excepted class or interface definition, found ~a" (token-value tok)) srt end)
                   (parse-error (format "Expected 'abstract class' or 'class', found ~a which is incorrectly spelled or capitalized"
                                        (token-value tok))
                                srt end)))
              ((close-to-keyword? tok 'interface)
               (parse-error (format "Expected 'interface' or 'class', found ~a which is incorrectly spelled or capitalized"
                                    (token-value tok)) srt end))
              ((and (advanced?) (close-to-keyword? tok 'public))
               (parse-error (string-append
                             (format 
                              "Expected 'interface' or 'class'. Found ~a, which is close to 'public' which is allowed.~n"
                              (token-value tok))
                             "Check capitalization and spelling")
                            srt end))
              ((or (if-token? tok) (return-token? tok)
                   (and (advanced?) (or (for-token? tok) (while-token? tok) (do-token? tok))))
               (parse-error (format "Expected class definition, found ~a. Statements must be in a method or interactions window" out)
                            srt end))
              ((prim-type? tok) 
               (parse-error (format "Expected class definition, found ~a. Fields and methods must be in a class body" out)
                            srt end))
              ((id-token? tok)
               (parse-error (format "Expected class definition, found ~a. Fields, methods, and expressions may not be written here"
                                    out) srt end))
              (else
               (parse-error (format "Expected class definition, found ~a which may not be written here" out)
                            srt end))))))
        ((class-id)
         (case tokN
           ((EOF) (parse-error "'class' should be followed by a class name and body" ps pe))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error (format "Expected class body after ~a" (token-value tok)) srt end))
                ((and (extends? next-tok) (or (intermediate?) (advanced?)))
                 (parse-definition next (getter) 'extends getter))
                ((implements? next-tok)
                 (parse-definition next (getter) 'implements getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((and (or (intermediate?) (advanced?)) (close-to-keyword? next-tok 'extends) )
                 (parse-error (format "Found ~a, which is similar to 'extends'" (token-value next-tok))
                              (get-start next) (get-end next)))
                ((close-to-keyword? next-tok 'implements)
                 (parse-error (format "Found ~a, which is similar to 'implements'" (token-value next-tok))
                              (get-start next) (get-end next)))
                ((open-separator? next-tok)
                 (parse-error (format "Expected { to begin class body, but found ~a" (format-out next-tok))
                              (get-start next) (get-end next)))
                ((c-brace? tok)
                 (parse-error (format "Class body must be opened with { before being closed, found ~a" out)
                              (get-start next) (get-end next)))
                (else
                 (parse-error 
                  (format "Class name must be followed by ~a 'implements' or a { to start class body, found ~a"
                          (if (not (beginner?)) "'extends' clause or " "") 
                          (format-out next-tok)) srt (get-end next))))))
           (else 
            (if (java-keyword? tok) 
                (parse-error (format "class may not be called ~a as this is a reserved term" tokN) srt end)
                (parse-error (format "expected a name for this class, given ~a" out) srt end)))))
        ((interface-id)
         (case tokN
           ((EOF) (parse-error "'interface' should be followed by an interface name and body" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error (format "Expected interface body after ~a" (token-value tok)) srt end))
                ((extends? next-tok) 
                 (if (beginner?)
                     (parse-error "Expected a '{' to begin interface body, found 'extends' which is not allowed here." 
                                  (get-start next) (get-end next))
                     (parse-definition next (getter) 'iface-extends getter)))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((close-to-keyword? next-tok 'extends)
                 (if (beginner?)
                     (parse-error (format "Expected a '{' to begin interface body, ~a cannot appear here" (token-value next-tok))
                                  (get-start next) (get-end next))
                     (parse-error (format "Found ~a, which is similar to 'extends'." (token-value next-tok)) 
                                  (get-start next) (get-end next))))
                ((open-separator? next-tok)
                 (parse-error (format "Expected { to begin interface body, but found ~a" (format-out next-tok))
                              (get-start next) (get-end next)))
                ((c-brace? next-tok)
                 (parse-error (format "Interface body must be opened with { before being closed, found ~a"
                                      (format-out next-tok)) (get-start next) (get-end next)))
                ((implements? next-tok)
                 (parse-error "Interfaces may not implement other interfaces" ps (get-end next)))
                (else
                 (parse-error (format "Interface name must be follwed by 'extends' or a { to start its body, found ~a"
                                      (format-out next-tok)) srt (get-end next))))))
           (else
            (if (java-keyword? tok)
                (parse-error (format "An interface may not be called ~a, as this is a reserved term." tokN) srt end)
                (parse-error (format "Expected a name for this interface, given ~a" out) srt end)))))
        ((extends) 
         (cond
           ((eof? tok) (parse-error "Expected parent class after extends." ps pe))
           ((id-token? tok)
            ;(if (beginner?)
            ;    (parse-definition cur-tok (getter) 'class-body getter)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((implements? next-tok) (parse-definition next (getter) 'implements getter))
                ((close-to-keyword? next-tok 'implements)
                 (parse-error (format "Expected 'implements', found ~a which is close to 'implements'" (token-value next-tok))
                              (get-start next) (get-end next)))
                (else (parse-definition cur-tok next 'class-body getter)))))
           ((o-brace? tok) (parse-error "Expected a parent name after extends and before the class body starts" srt end))
           ((java-keyword? tok)
            (parse-error (format "Expected a name after extends, found reserved word ~a" tokN) srt end))
           (else (parse-error (format "extends must be followed by parent name, found ~a" out) ps end))))
        ;Intermediate
        ((implements)
         (cond
           ((eof? tok) (parse-error "Expected implemented interface after implements, and class body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more implemented interfaces or class body" srt end))
                ((comma? next-tok) 
                 (if (beginner?)
                     (parse-error "Only one interface may be implemented, found ',' which should not appear here"
                                  (get-start next) (get-end next))
                     (parse-definition next (getter) 'implements-list getter)))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((id-token? next-tok)
                 (parse-error "Implemented interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the class body, found ~a" (format-out next-tok))
                                   (get-start next) (get-end next))))))
           ((java-keyword? tok)
            (parse-error (format "Expected an interface name, which may not be reserved word ~a" tokN) srt end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end))))
        ;Intermediate
        ((implements-list)
         (cond
           ((eof? tok) (parse-error "Expected an interface name and class body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more interfaces or a class body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'implements-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((id-token? next-tok) (parse-error "Implemented interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the class body, found ~a" (format-out next-tok))
                                   (get-start next) (get-end next))))))
           ((java-keyword? tok)
            (parse-error (format "Expected an interface name for implements clause, found reserved term ~a" tokN) srt end))
           ((o-brace? tok)
            (parse-error "Expected an additional interface after comma before { to start class body" ps end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end))))
        ;Intermediate 
        ((iface-extends)
         (cond
           ((eof? tok) (parse-error "Expected interface name to extend after extends, and interface body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more extended interfaces or interface body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'iface-extends-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((implements? next-tok) 
                 (parse-error "An interface may not implement other interfaces" (get-start next) (get-end next)))
                ((id-token? next-tok) (parse-error "Extended interfaces must be separated by a comma" srt (get-end next)))
                (else 
                 (parse-error (format "Expected more interfaces to extend of interface body, found ~a" (format-out next-tok))
                              (get-start next) (get-end next))))))
           ((java-keyword? tok)
            (parse-error (format "Expected a name of an interface to extend, found reserved term ~a, which cannot be a name"
                                 tokN) srt end))
           (else
            (parse-error (format "Expected a name of an interface to extend, found ~a" out) srt end))))
        ;Intermediate
        ((iface-extends-list)
         (cond
           ((eof? tok) (parse-error "Expected interface name to extend after comma, and interface body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more interfaces or an interface body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'iface-extends-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((id-token? next-tok) (parse-error "Extended interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the interface body, found ~a" (format-out next-tok))
                                   (get-start next) (get-end next))))))
           ((java-keyword? tok)
            (parse-error (format "Expected an interface name for extends clause, found reserved term ~a" tokN) srt end))
           ((o-brace? tok)
            (parse-error "Expected an additional interface after comma before { to start interface body" ps end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end)))) 
        ((class-body)
         (case tokN
           ((EOF) (parse-error (format "Expected class body to begin after ~a" (format-out (get-tok pre))) ps pe))
           ((O_BRACE) (parse-definition cur-tok (parse-members cur-tok (getter) 'start getter #f #f) 'class-body-end getter))
           (else 
            (cond
              ((open-separator? tok)
               (parse-error (format "expected { to begin class body, but found ~a" out) srt end))
              ((close-separator? tok)
               (parse-error (format "Class body must be opened with { before being closed, found ~a" out) srt end))
              (else
               (parse-error (format "Expected { to start class body, found ~a" out) srt end))))))
        ((class-body-end)
         (case tokN
           ((EOF) (parse-error "Expected a '}' to close class body" ps end))
           ((C_BRACE) 
            (let ((next (getter)))
              (if (c-brace? (get-tok next))
                  (parse-error "Unnecessary '}', class body already closed" srt (get-end next))
                  (parse-definition cur-tok next 'start getter))))
           (else (parse-error (format "Expected a '}' to close class body, found ~a" out) ps end))))
        ((iface-body)
         (case tokN
           ((EOF) (parse-error (format "Expected interface body to begin after ~a" (format-out (get-tok pre))) ps pe))
           ((O_BRACE) (parse-definition cur-tok (parse-iface-body null (getter) 'start getter) 'iface-body-end getter))
           (else
            (cond
              ((open-separator? tok)
               (parse-error (format "Expected '{' to begne interface body, but found ~a" out) srt end))
              ((close-separator? tok)
               (parse-error (format "Interface body must be opened with '{' before being closed, found ~a" out) srt end))
              (else (parse-error (format "Expected '{' to start interface body, found ~a" out) srt end))))))
        ((iface-body-end)
          (case tokN
            ((EOF) (parse-error "Expected a '}' to close interface body" ps end))
            ((C_BRACE)
             (let ((next (getter)))
               (if (c-brace? (get-tok next))
                   (parse-error "Unnecessary '}', interface body is already closed" srt (get-end next))
                   (parse-definition cur-tok next 'start getter))))
            (else (parse-error (format "Expected a '}' to close interface body, found ~a" out) ps end)))))))
  
  ;parse-type: token token symbol (->token) -> void
  (define (parse-type pre cur state getter)
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (format-out tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (cond
           ((eof? tok) (parse-error "Expected a type, found nothing" srt end))
           ((prim-type? tok) (parse-type cur (getter) 'end-or-array getter))
           ((id-token? tok) (parse-type cur (getter) 'qualified-or-array getter))
           ;I can do better here: expand close-to-keyword to specify close to which type of keyword
           (else
            (parse-error (format "Expected a type, found ~a, which is not the valid beginning of a type" out)
                         srt end))))
         ((end-or-array)
          (cond
            ((dot? tok) (parse-error (format "'.' cannot follow primitive type ~a in a type declaration" 
                                             (format-out (get-tok pre))) ps end))
            ((and (advanced?) (o-bracket? tok))
             (parse-type pre (getter) 'array-close getter))
            (else 
             (parse-error 
              (format "Only one item may appear in a type declaration, ~a is a complete type, ~a maynot appear"
                      (format-out (get-tok pre)) out) ps end))))
         ((qualified-or-array)
          (cond
            ((dot? tok)
             (parse-error "This type declaration maynot contain a '.'" ps end))
            ((and (advanced?) (o-bracket? tok))
             (parse-type pre (getter) 'array-close getter))
            (else
             (parse-error 
              (format "Only one item may appear in a type declaration, ~a appears to be a complete type, ~a maynot appear"
                      (format-out (get-tok pre)) out) ps end))))
         ((array-close)
          (cond
            ((c-bracket? tok) 
             (let ((next (getter)))
               (cond
                 ((o-bracket? (get-tok next))
                  (parse-type next (getter) 'array-close getter))
                 (else 
                  (parse-error 
                   (format "Only one type may appear in a type declaration, ] appears to complete the type, ~a maynot appear"
                           (format-out (get-tok next)))
                   ps (get-end next))))))
            (else
             (parse-error
              (format "Expected a ] to close the array type, found ~a, which maynot appear here" out) ps end)))))))
              
  ;parse-members: token token symbol (->token) boolean -> token
  (define (parse-members pre cur state getter abstract-method? just-method?)
    ;(printf "parse-members: state ~a pre ~a current ~a~n" state (get-tok pre) (get-tok cur))
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (format-out tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      #;(printf "parse-members: pre-out ~a current-out ~a~n" (if (null? pre) null (format-out (get-tok pre))) out)

      (case state
        ((start)
         (cond
           ((eof? tok) cur)
           ((and (c-brace? tok) (not just-method?)) cur)
           ((and (c-brace? tok) just-method?) (parse-error "Encountered extra }" srt end))
           ((and (or (intermediate?) (advanced?)) (abstract? tok))
            (parse-members cur (getter) 'method getter #t just-method?))
           ((prim-type? tok) (parse-members cur (getter) 'method-or-field getter #f just-method?))
           ;Intermediate & Advanced
           ((and (or (intermediate?) (advanced?)) (void-token? tok)) (parse-members cur (getter) 'method-id getter #f just-method?))
           ((id-token? tok) (parse-members cur (getter) 'member getter #f just-method?))
           ;Advanced
           ((and (advanced?)
                 (or (public? tok) (private? tok) (protected? tok) (static? tok) (final? tok)))
            (parse-members cur (getter) 'start getter #f just-method?))
           ;Advanced
           ((and (advanced?) (o-brace? tok))
            (if (modifier-token? (get-tok pre))
                (parse-error (format "Initilization body may not be preceeded with any modifier. Found ~a" 
                                     (format-out (get-tok pre))) ps end)
                (parse-members cur (parse-statement pre cur 'start getter #t #f #f) 'start getter #f just-method?)))
           (else 
            (parse-error 
             (format "Only fields, methods and a constructor may be within the class body, found ~a" out) srt end))))
        ((member)
         (cond
           ((eof? tok) (parse-error "This class may not end here, class body still requires a }" ps pe))
           ((dot? tok) 
            (if (beginner?)
                (parse-error "The name of a type or class may not contain a '.'" ps end)
                (parse-members cur (parse-name (getter) getter #f) 'method-or-field getter abstract-method? just-method?)))
           ((id-token? tok) (parse-members pre cur 'method-or-field getter abstract-method? just-method?))
           ((o-paren? tok) (parse-members cur (getter) 'ctor-parms getter abstract-method? just-method?))
           ((c-paren? tok) (parse-error "( must precede ) in parameter list" srt end))
           ((and (advanced?) (o-bracket? tok))
            (parse-members pre cur 'method-or-field getter abstract-method? just-method?))
           ((open-separator? tok)
            (parse-error (format "'(' must be used to start parameter list, found ~a" out) srt end))
           ((prim-type? tok)
            (parse-error 
             (format "Methods and fields may not be named for primitive type ~a, which appears in the name position." kind)
             srt end))
           ((java-keyword? tok)
            (parse-error 
             (format "Expected a name for this field or method, ~a is a reserved word and cannot be the name." kind)
             srt end))
           (else (parse-error (format "Expected a name for this field or method, found ~a." out) srt end))))
        ((method-or-field)
         (case kind
           ((EOF) (parse-error "Method or field must have a name, and the class body still requires a '}'." ps pe))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (format-out n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) 
                 (parse-error "Method or field has not completed, and the class body still requires a '}'." srt end))
                ;Just ended a field
                ((semi-colon? n-tok) (parse-members next (getter) 'start getter #f just-method?))
                ;Intermediate and Advanced
                ((comma? n-tok) 
                 (if (or (intermediate?) (advanced?))
                     (parse-members next (getter) 'field-list getter abstract-method? just-method?)
                     (parse-error (format "Expected an end to field ~a, fields end in ';', ',' is not allowed." (token-value tok))
                                  srt ne)))
                ((and #;(or (intermediate?) (advanced?)) (teaching-assignment-operator? n-tok))
                 (let ((assign-exp (getter)))
                   (cond
                     ((eof? (get-tok assign-exp))
                      (parse-error (format "Expected an expression to bind to ~a, and the class body still needs a '}'." 
                                           (token-value tok)) srt end))
                     ((and (advanced?) (o-brace? (get-tok assign-exp)))
                      (parse-members next (parse-array-init assign-exp (getter) 'start getter) 'field-init-end getter #f just-method?))
                     (else
                      (parse-members next (parse-expression null assign-exp 'start getter #f #f) 'field-init-end getter #f just-method?)))))
                ((o-paren? n-tok) (parse-members next (getter) 'method-parms getter abstract-method? just-method?))
                ((o-bracket? n-tok)
                 (parse-error
                  "'[' cannot appear here. Array specifications must immediately follow the type. Methods begin with '('."
                  srt ne))
                ((open-separator? n-tok) 
                 (parse-error (format "Method parameter list must begin with '(', found ~a." n-out) srt ne))
                ((id-token? n-tok)
                 (parse-error
                  (string-append 
                   (format "Incorrectly formed field or method declaration. ~a may not appear here.~n" n-out)
                   (if (and (id-token? (get-tok pre))
                            (close-to-keyword? (get-tok pre) 'abstract))
                       (format
                        "~a is close to 'abstract' but may be miscapitalized or misspelled.~n"
                        (format-out (get-tok pre)))
                       "")
                   (if (or (intermediate?) (advanced?))
                       "A field is Type Name followed by '=', ',', or ';'. A method is Type Name followed by '('."
                       "A field is Type Name followed by '=', or ';''. A method is Type Name followed by '('."))
                  ps ne))
                (else 
                 (if (or (intermediate?) (advanced?))
                     (parse-error 
                      (format "Expected ';' to end field or abstract method parameter list, found ~a." n-out) srt ne)
                     (parse-error 
                      (format "Expected ';' to end field. Found ~a." n-out) srt ne))))))
           (else 
            (if (and (advanced?) (o-bracket? tok))
                (let* ((next (getter))
                       (next-tok (get-tok next)))
                  (cond 
                    ((eof? next-tok) (parse-error "Expected ] to end array type, and class still requires a }" srt end))
                    ((c-bracket? next-tok) (parse-members next (getter) 'method-or-field getter abstract-method? just-method?))
                    ((o-bracket? next-tok) 
                     (parse-error "Array type may not have [[. A closing ] is required before beginning a new []" 
                                  srt (get-end next)))
                    (else
                     (parse-error (format "Array type is of the form Type[]. ~a is not allowed." (format-out next-tok)) srt
                                  (get-end next)))))
                (parse-error 
                 (if (java-keyword? tok)
                     (format "Expected a name for this field or method, cannot be named reserved word ~a" kind)
                     (format "Expected a name for this field or method, found ~a" out))
                 srt end)))))
        ;Intermediate
        ((field-list)
         (case kind
           ((EOF) (parse-error "Expected an additional field name after comma, class body still requires a }" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (format-out n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Field declaration has not completed, and class body still requires a }" srt end))
                ((semi-colon? n-tok) (parse-members next (getter) 'start getter #f just-method?))
                ((comma? n-tok) (parse-members next (getter) 'field-list getter #f just-method?))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (cond
                     ((eof? (get-tok assign-exp))
                      (parse-error (format "Expected an expression to bind to ~a, and class body still needs a }" 
                                           (token-value tok)) srt end))
                     ((and (advanced?) (o-brace? (get-tok assign-exp)))
                      (parse-members next (parse-array-init assign-exp (getter) 'start getter) 'field-init-end getter #f just-method?))
                     (else
                      (parse-members next (parse-expression null assign-exp 'start getter #f #f) 'field-init-end getter #f just-method?)))))
                ((id-token? n-tok)
                 (parse-error (format "Fields must be separated by commas, ~a not allowed" n-out) srt ne))
                (else (parse-error (format "Expected ; to end field, or more field names, found ~a" n-out) srt ne)))))
           (else
            (parse-error
             (if (java-keyword? tok)
                 (format "Expected a name for this field, cannot be named reseved word ~a" kind)
                 (format "Expected a name for this field, found ~a" out)) srt end))))
        ;Intermediate
        ((field-init-end)
         (case kind
           ((EOF) 
            (if (beginner?)
                (parse-error "Expected a ';' after field, class body still requires a '};." ps pe)
                (parse-error "Expected a ';' or comma after field, class body still requires a '}'." ps pe)))
           ((COMMA) 
            (if (beginner?)
                (parse-error "Expected a ';' to end field, found ',' which does not end the field declaration"
                             ps end)
                (parse-members cur (getter) 'field-list getter #f just-method?)))
           ((SEMI_COLON) (parse-members cur (getter) 'start getter #f just-method?))
           ((IDENTIFIER) 
            (if (beginner?)
                (parse-error (format "Expected a ';' to end field, found ~a which is not allowed." out)
                             srt end)
                (parse-error (format "Fields must be separated by commas, ~a not allowed" out) srt end)))
           (else 
            (parse-error 
             (if (beginner?) 
                 (format "Expected a ';' to end the field, found ~a" out)
                 (format "Expected a ';' to end field, or more field names, found ~a" out)) srt end))))
        ((method)
         (cond
           ((eof? tok) (parse-error "Expected method, and class body still requires a }" ps pe))
           ((or (prim-type? tok) (and (or (intermediate?) (advanced?)) (void-token? tok)))
            (parse-members cur (getter) 'method-id getter abstract-method? just-method?))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-kind (get-token-name next-tok))
                   (next-end (get-end next))
                   (next-start (get-start next)))
              (cond
                ((eof? next-tok) (parse-error "Expected method name, and class body still requires a }" srt end))
                ((dot? next-tok) 
                 (if (beginner?)
                     (parse-error "The name of a type or class may not contain '.'" srt next-end)
                     (parse-members next (parse-name (getter) getter #f) 'method-id getter abstract-method? just-method?)))
                ((o-paren? next-tok)
                 (parse-error "Declaration is similar to constructor, which cannot be abstract" ps next-end))
                ((semi-colon? next-tok)
                 (parse-error "Declaration is similar to a field, which cannot be abstract" ps next-end))
                ((id-token? next-tok) (parse-members cur next 'method-id getter abstract-method? just-method?))
                ((java-keyword? next-tok) 
                 (parse-error 
                  (format "Expected method name, found ~a which is reserved and cannot be a method's name" next-kind)
                  next-start next-end))
                (else (parse-error (format "Expected a method name, found ~a" (format-out next-tok)) 
                                   next-start next-end)))))
           ((java-keyword? tok)
            (if (and (advanced?) (modifier-token? tok))
                (parse-members cur (getter) 'method getter abstract-method? just-method?)
                (parse-error 
                 (format "Expected return type of the method, reserved word ~a is not a type" kind) srt end)))
           (else (parse-error (format "Expected return type of a method, found ~a" out) srt end))))
        ((method-id)
         (case kind
           ((EOF) (parse-error "Expected method name, and class body still requires a }" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (format-out next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) (parse-error "Expected method body, and class body still requires a }" srt end))
                ((o-paren? next-tok) (parse-members next (getter) 'method-parms getter abstract-method? just-method?))
                ((c-paren? next-tok) 
                 (parse-error "Expected a ( to start parameter list but encountered the closing )" next-start next-end))
                ((semi-colon? next-tok) 
                 (parse-error "Declaration is similar to a field, which cannot be abstract" ps next-end))
                ((open-separator? next-tok)
                 (parse-error (format "Method parameter list is started by (, found ~a" next-out) next-start next-end))
                (else (parse-error (format "Expected ( for parameter list, found ~a" next-out) next-start next-end)))))
           (else
            (if (java-keyword? tok)
                (parse-error 
                 (format "Expected method name, found ~a which is reserved and cannot be a method's name" kind)
                 srt end)
                (parse-error (format "Expected method name, found ~a" out) srt end)))))
        ((ctor-parms)
         (cond
           ((eof? tok) (parse-error "Expected constructor parameters, and class body still requires a }" ps pe))
           ((o-paren? tok) 
            (parse-error "Constructor parameter list already started, an additional ( is not needed" srt end))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (format-out next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))                             
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected constructor body, and class body still requires a }" srt end))
                ((c-paren? next-tok) 
                 (parse-error "Constructor parameter list already closed, unneeded )" next-start next-end))
                ((o-brace? next-tok) 
                 (parse-members next (parse-ctor-body null (getter) getter) 'ctor-end getter #f just-method?))
                ((open-separator? next-tok)
                 (parse-error (format "Constructor body begins with a {, found ~a" next-out) next-start next-end))
                ((semi-colon? next-tok) 
                 (parse-error "Expected a constructor body, ; is only allowed for abstract methods" next-start next-end))
                (else
                 (parse-error (format "Expected a constructor body, starting with {, found ~a" next-out) next-start next-end)))))
           ((or (prim-type? tok) (id-token? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of parameter list, and class body still requires a }"  srt end))
                ((comma? next-tok) (parse-error "Variable name must follow type before ," srt next-end))
                ((c-paren? next-tok) (parse-error "Variable name must follow type before )" srt next-end))
                ((id-token? next-tok)
                 (let* ((afterID (getter))
                        (afterID-tok (get-tok afterID)))
                   (cond
                     ((eof? afterID-tok) 
                      (parse-error "Expected rest of parameter list, and class body requires a }" next-start next-end))
                     ((c-paren? afterID-tok) (parse-members next afterID 'ctor-parms getter #f just-method?))
                     ((close-separator? afterID-tok) 
                      (parse-error (format "Expected a ) to close parameter list, found ~a" (format-out afterID-tok))
                                   (get-start afterID) (get-end afterID)))
                     ((comma? afterID-tok)
                      (let* ((afterC (getter))
                             (afterC-tok (get-tok afterC)))
                        (cond
                          ((eof? afterC-tok) (parse-error "Expected rest of parameter list, and class body requires a }"
                                                          (get-start afterID) (get-end afterID)))
                          ((c-paren? afterC-tok)
                           (parse-error "Comma is unneeded before ) unless another variable is declared." 
                                        (get-start afterID) (get-end afterC)))
                          ((comma? afterC-tok)
                           (parse-error "Parameter list should not have ,, Only one ',' is needed." 
                                        (get-start afterID) (get-end afterC)))
                          (else (parse-members afterID afterC 'ctor-parms getter #f just-method?)))))
                     ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                      (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                           (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                   next-start (get-end afterID)))
                     (else (parse-error (format "Expected , or ) in parameter list found ~a" (format-out afterID-tok))
                                        (get-start afterID) (get-end afterID))))))
                ((java-keyword? next-tok)
                 (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                      (get-token-name next-tok))
                              next-start next-end))
                ((and (advanced?) (o-bracket? next-tok))
                 (parse-members cur next 'array-type getter #f just-method?))
                (else (parse-error (format "Expected new parameter name after type, found ~a" (format-out next-tok))
                                   next-start next-end)))))
           ((java-keyword? tok)
            (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
           (else (parse-error (format "Expected a parameter or ), found ~a" out) srt end))))
        ((array-type)
         (case kind
           ((EOF) (parse-error "Expected remainder of constructor parameters" ps pe))
           ((O_BRACKET) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond 
                ((eof? next-tok) (parse-error "Expected remainder of array type declaration." srt end))
                ((c-bracket? next-tok) (parse-members next (getter) 'array-type getter #f just-method?))
                (else
                 (parse-error (format "Expected ']' to close array type, found ~a which is not allowed." 
                                      (format-out next-tok))
                              srt (get-end next))))))
           ((COMMA) (parse-error "Expected new paramter name after type, found ','." srt end))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected ')' to close parameter list, or more parameters" srt end))
                
                ((comma? next-tok)
                 (let* ((afterC (getter))
                        (afterC-tok (get-tok afterC)))
                   (cond
                     ((eof? afterC-tok) (parse-error "Expected rest of parameter list for constructor"
                                                     (get-start next) (get-end next)))
                     ((c-paren? afterC-tok)
                      (parse-error "Comma is unneeded before ) unless another variable is desired" 
                                   (get-start next) (get-end afterC)))
                     ((comma? afterC-tok)
                      (parse-error "Parameter list should not have ,, Only one is needed" 
                                   (get-start next) (get-end afterC)))
                     (else (parse-members next afterC 'ctor-parms getter #f just-method?)))))
                ((c-paren? next-tok) (parse-members cur next 'ctor-parms getter #f just-method?))
                (else (parse-error (format "Expected ',' or ')' in parameters found ~a which is not allowed" 
                                           (format-out next-tok))
                                   srt (get-end next))))))
           (else
            (parse-error (format "Expected parameter name after type, found ~a" out) srt end))))
        ((method-array-type)
         (case kind
           ((EOF) (parse-error "Expected remainder of method parameters" ps pe))
           ((O_BRACKET) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond 
                ((eof? next-tok) (parse-error "Expected remainder of array type" srt end))
                ((c-bracket? next-tok) (parse-members cur (getter) 'method-array-type getter abstract-method? just-method?))
                (else
                 (parse-error (format "Expected ']' to close array type, found ~a which is not allowed" 
                                      (format-out next-tok))
                              srt (get-end next))))))
           ((COMMA) (parse-error "Expected new paramter name after type, found ','" srt end))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected ')' to close parameter list, or more parameters" srt end))
                
                ((comma? next-tok)
                 (let* ((afterC (getter))
                        (afterC-tok (get-tok afterC)))
                   (cond
                     ((eof? afterC-tok) (parse-error "Expected rest of parameter list for method"
                                                     (get-start next) (get-end next)))
                     ((c-paren? afterC-tok)
                      (parse-error "Comma is unneeded before ) unless another variable is desired" 
                                   (get-start next) (get-end afterC)))
                     ((comma? afterC-tok)
                      (parse-error "Parameter list should not have ,, Only one is needed" 
                                   (get-start next) (get-end afterC)))
                     (else (parse-members next afterC 'method-parms getter abstract-method? just-method?)))))
                ((c-paren? next-tok) (parse-members cur next 'method-parms getter abstract-method? just-method?))
                (else (parse-error (format "Expected ',' or ')' in parameters found ~a which is not allowed" 
                                           (format-out next-tok))
                                   srt (get-end next))))))
           (else
            (parse-error (format "Expected parameter name after type, found ~a" out) srt end))))
        ((method-parms)
         (cond
           ((eof? tok) (parse-error "Expceted method parameters, and class body still requires }" ps pe))
           ((o-paren? tok) 
            (parse-error "Method parameter list already started, an additional ( is not needed" srt end))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (format-out next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok)
                 (parse-error "Expected method body, and class body still requires a '}'" srt end))
                ((c-paren? next-tok) 
                 (parse-error "Method parameter list already closed, unneeded ')'" next-start next-end))
                ((o-brace? next-tok)
                 (if abstract-method?
                     (parse-error "abstract methods may not have a body. Found '{' when ';' was expected" next-start next-end)
                     (parse-members next (if (or (intermediate?) (advanced?))
                                             (parse-method-body null (getter) getter #f #f)
                                             (parse-statement null (getter) 'start getter #f #f #f))
                                    'method-end getter abstract-method? just-method?)))                
                ((open-separator? next-tok)
                 (if abstract-method?
                     (parse-error (format "abstract methods should end with ';', found ~a" next-out) next-start next-end) 
                     (parse-error (format "Method body begins with a '{', found ~a" next-out) next-start next-end)))
                ((semi-colon? next-tok) 
                 (cond
                   ((or (beginner?) (not abstract-method?))
                    (parse-error "Method must have a body, beginning with '{'. ';' not allowed" next-start next-end))
                   (else (parse-members next (getter) 'start getter #f just-method?))))
                (else
                 (if abstract-method?
                     (parse-error (format "Expected a ';' to end abstract method, found ~a" next-out) next-start next-end)
                     (parse-error (format "Expected a method body, starting with '{', found ~a" next-out) next-start next-end))))))
           ((or (prim-type? tok) (id-token? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of parameter list, and class body still requires a }" srt end))
                ((comma? next-tok)  (parse-error "Variable name must follow type before ," srt next-end))
                ((c-paren? next-tok)  (parse-error "Variable name must follow type before )" srt next-end))
                ((id-token? next-tok)
                 (let* ((afterID (getter))
                        (afterID-tok (get-tok afterID)))
                   (cond
                     ((eof? afterID-tok) 
                      (parse-error "Expected rest of parameter list, and class body requires a '}'" next-start next-end))
                     ((c-paren? afterID-tok) (parse-members next afterID 'method-parms getter abstract-method? just-method?))
                     ((comma? afterID-tok)
                      (let* ((afterC (getter))
                             (afterC-tok (get-tok afterC)))
                        (cond
                          ((eof? afterC-tok) (parse-error "Expected rest of parameter list, and class body requires a '}'"
                                                          (get-start afterID) (get-end afterID)))
                          ((c-paren? afterC-tok)
                           (parse-error "Comma is unneeded before ) unless another variable is desired" 
                                        (get-start afterID) (get-end afterC)))
                          ((comma? afterC-tok) 
                           (parse-error "Parameter list should not have ,, Only one is needed" 
                                        (get-start afterID) (get-end afterC)))
                          (else (parse-members afterID afterC 'method-parms getter abstract-method? just-method?)))))
                     ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                      (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                           (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                   next-start (get-end afterID)))
                     (else (parse-error (format "Expected , or ) in parameter list found ~a" (format-out afterID-tok))
                                        (get-start afterID) (get-end afterID))))))
                ((java-keyword? next-tok)
                 (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                      (get-token-name next-tok))
                              next-start next-end))
                ((and (advanced?) (o-bracket? next-tok))
                 (parse-members cur next 'method-array-type getter abstract-method? just-method?))
                (else (parse-error (format "Expected new parameter name after type, found ~a" (format-out next-tok))
                                   next-start next-end)))))
           ((java-keyword? tok)
            (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
           (else (parse-error (format "Expected a parameter or ), found ~a" (format-out tok)) srt end))))
        ((ctor-end)
         (case kind
           ((EOF) (parse-error "Expected } to end constructor body, and class body still requires }" ps pe))
           ((C_BRACE) (parse-members cur (getter) 'start getter #f just-method?))
           ((if return) 
            (parse-error (format "Statements are not permitted in the constructor body, found ~a" kind) srt end))
           (else (parse-error (format "Expected a } to end the constructor, found ~a" out) srt end))))
        ((method-end)
         (case kind
           ((EOF) (parse-error "Expected } to end method body, and class body still requires }" ps pe))
           ((C_BRACE) (parse-members cur (getter) 'start getter #f just-method?))
           (else 
            (parse-error (format "Expected 1 statement, and then } for method body. Found ~a instead of }" out)
                         srt end)))))))
  
  ;parse-array-init token token symbol (-> token) -> token
  (define (parse-array-init pre cur-tok state getter)
    ;(printf "parse-array-init state ~a pre ~a cur-tok ~a~n" state pre cur-tok)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((EOF) (parse-error "Expected expressions to create array or '}' to end it" ps pe))
           ((O_BRACE) (parse-array-init pre (parse-array-init cur-tok (getter) 'start getter) 'comma-or-end getter))
           ((C_BRACE) (getter))
           ((SEMI_COLON) (parse-error "Expected a '}' to close array before ending field" ps end))
           (else (parse-array-init pre (parse-expression null cur-tok 'start getter #f #f) 'comma-or-end getter))))
        ((comma-or-end)
         (case kind
           ((EOF) (parse-error "Expected ',' for more expressions of the array or '}' to end it" ps pe))
           ((C_BRACE) (getter))
           ((COMMA)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected further expressions for array" ps end))
                ((c-brace? next-tok) (parse-error "Expected further expressions for array after ',', not '}' to end it" ps 
                                                  (get-end next)))
                ((o-brace? next-tok)
                 (parse-array-init pre (parse-array-init next (getter) 'start getter) 'comma-or-end getter))
                (else
                 (parse-array-init pre (parse-expression null next 'start getter #f #f) 'comma-or-end getter)))))
           (else
            (parse-error (format "Items of the array must be separated by ',' or end with '}' to close the array. Found ~a" out)
                         ps end)))))))

  ;Beginner
  ;parse-field: token token symbol (->token) -> token
  (define (parse-field pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((EOF) (parse-error "Expected a name for this variable declaration" ps pe))
           ((IDENTIFIER) (parse-field cur-tok (getter) 'equals getter))
           ((=) (parse-error "Expected a name for this variable declaration inbetween the type and =" ps end))
           (else
            (if (java-keyword? tok)
                (parse-error 
                 (format "Expected a name for this declaration, reserved word ~a may not be the name" kind) start end)
                (parse-error
                 (format "Expected a name for this declaration, found ~a" out) start end)))))
        ((equals)
         (case kind
           ((=) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression for this declaration" start end)
                  (parse-field cur-tok (parse-expression cur-tok next 'start getter #f #f) 'end getter))))
           ((COMMA)
            (parse-error "Expected an assignment of the given name to a value, found ',' which is not allowed here."
                         ps end))
           ((SEMI_COLON)
            (parse-error "Expected an assignment of the given name to a value, found ';' which is not allowed here."
                         ps end))
           (else
            (parse-error (format "Expected an assignment of the given name to a value, found ~a" out) ps end))))
        ((end)
         (case kind
           ((EOF) (parse-error "Declaration must end with a ';'" ps pe))
           ((SEMI_COLON) (getter))
           (else
            (parse-error (format "Expected an end to this declartion, found ~a" out) start end)))))))
  ;Intermediate
  ;parse-iface-body: token token symbol (->token) -> token
  (define (parse-iface-body pre cur state getter) 
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (format-out tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
    (case state
      ((start)          
       (cond
         ((or (eof? tok) (c-brace? tok)) cur)
         ((and (not (beginner?)) (abstract? tok))
          (parse-iface-body cur (getter) 'method-type getter))
         ((prim-type? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((and (not (beginner?)) (void-token? tok))
          (parse-iface-body cur (getter) 'method-id getter))
         ((id-token? tok) (parse-iface-body cur (getter) 'method-id getter))
         (else 
          (parse-error 
           (format "Only methods may be within the interface body, found ~a" out) srt end))))
      ((method-type)
       (cond
         ((eof? tok) (parse-error "Expected method, and interface body still requires a }" ps pe))
         ((prim-type? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((and (not (beginner?)) (void-token? tok))
          (parse-iface-body cur (getter) 'method-id getter))
         ((id-token? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((java-keyword? tok)
          (parse-error 
           (format "Expected return type of the method, reserved word ~a is not a type" kind) srt end))
         (else (parse-error (format "Expected return type of a method, found ~a" out) srt end))))
      ((method-id)
       (case kind
         ((EOF) (parse-error "Expected method name, and interface body still requires a }" ps pe))
         ((IDENTIFIER)
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-out (format-out next-tok))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) (parse-error "Expected method parameters, and interface body still requires a }" srt end))
              ((o-paren? next-tok) (parse-iface-body next (getter) 'parms getter))
              ((c-paren? next-tok)
               (parse-error "Expected a ( to start parameter list but encountered the closing )" next-start next-end))
              ((semi-colon? next-tok) 
               (parse-error "Declaration is similar to a field, which maynot be in an interface" ps next-end))
              ((open-separator? next-tok)
               (parse-error (format "Method parameter list is started by (, found ~a" next-out) next-start next-end))
              (else (parse-error (format "Expected ( for parameter list, found ~a" next-out) next-start next-end)))))
         ((PERIOD) 
          (if (id-token? (get-tok pre)) 
              (parse-iface-body cur (parse-name (getter) getter #f) 'method-id getter)
              (parse-error (format "~a cannot be followed by '.'" (format-out pre)) ps end)))
         ((O_PAREN) (parse-error "Expected a method name before parameter list" ps end))
         ((SEMI_COLON) (parse-error "Declaration is similar to a field, which maynot be in an interface" ps end))
         (else
          (if (java-keyword? tok)
              (parse-error 
               (format "Expected method name, found ~a which is reserved and cannot be a method's name" kind) srt end)
              (parse-error (format "Expected method name, found ~a" out) srt end)))))
      ((parms)
       (cond
         ((eof? tok) (parse-error "Expceted method parameters, and interface body still requires }" ps pe))
         ((o-paren? tok) 
          (parse-error "Method parameter list already started, an additional ( is not needed" srt end))
         ((c-paren? tok)
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-out (format-out next-tok))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) (parse-error "Expected a ';', and interface body still requires a }" srt end))
              ((c-paren? next-tok) 
               (parse-error "Method parameter list already closed, unneeded )" next-start next-end))
              ((o-brace? next-tok)
               (parse-error "Method in interface maynot have a body" next-start next-end))
              ((semi-colon? next-tok) (parse-iface-body next (getter) 'start getter))
              (else
               (parse-error (format "Expected a ';', found ~a" next-out) next-start next-end)))))
         ((or (prim-type? tok) (id-token? tok))
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) 
               (parse-error "Expected rest of parameter list, and interface body still requires a }" srt end))
              ((comma? next-tok)  (parse-error "Variable name must follow type before ," srt next-end))
              ((c-paren? next-tok)  (parse-error "Variable name must follow type before )" srt next-end))
              ((id-token? next-tok)
               (let* ((afterID (getter))
                      (afterID-tok (get-tok afterID))
                      (afterID-s (get-start afterID)))
                 (cond
                   ((eof? afterID-tok) 
                    (parse-error "Expected rest of parameter list, and interface body requires a }" next-start next-end))
                   ((c-paren? afterID-tok) (parse-iface-body next afterID 'parms getter))
                   ((comma? afterID-tok)
                    (let* ((afterC (getter))
                           (afterC-tok (get-tok afterC))
                           (afterC-end (get-end afterC)))
                      (cond
                        ((eof? afterC-tok) 
                         (parse-error "Expected rest of parameter list, and class body requires a }" afterID-s afterC-end))
                        ((c-paren? afterC-tok)
                         (parse-error "Comma is unneeded before ) unless another variable is desired" afterID-s afterC-end))
                        ((comma? afterC-tok) 
                         (parse-error "Parameter list should not have ',,' Only one is needed" afterID-s afterC-end))
                        (else (parse-iface-body afterID afterC 'parms getter)))))
                   ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                    (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                         (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                 next-start (get-end afterID)))
                   (else (parse-error (format "Expected , or ) in parameter list found ~a" (format-out afterID-tok))
                                      afterID-s (get-end afterID))))))
              ((java-keyword? next-tok)
               (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                    (get-token-name next-tok))
                            next-start next-end))
              (else (parse-error (format "Expected new parameter name after type, found ~a" (format-out next-tok))
                                 next-start next-end)))))
         ((java-keyword? tok)
          (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
         (else (parse-error (format "Expected a parameter or ), found ~a" (format-out tok)) srt end)))))))
  
  ;parse-type: token (-> token) bool -> token
  (define (parse-name cur-tok getter star-ok?) 
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (start (get-start cur-tok))
           (stop (get-end cur-tok)))
      (case kind
        ((IDENTIFIER) 
         (let ((next-tok (getter)))
           (if (dot? (get-tok next-tok))
               (parse-name (getter) getter star-ok?)
               next-tok)))
        ((PERIOD) (parse-error "It is not allowed to have two .s, only one is necessary" start stop))
        ((*)
         (if star-ok? 
             cur-tok
             (parse-error "A name may not contain a *" start stop)))
        (else 
         (cond
           ((eq? 'this kind)
            (parse-error "'this' cannot occur after a '.', only before" start stop))
           ((java-keyword? tok)
            (parse-error (format "Expected name after '.', found reserved word ~a, which may not appear here" kind)
                         start stop))
           (else
            (parse-error (format "Expected name after '.', found ~a" (format-out tok)) start stop)))))))
  
  ;parse-ctor-body: token token (->token) -> token
  (define (parse-ctor-body pre cur-tok getter)
    (case (get-token-name (get-tok cur-tok))
      ((EOF C_BRACE) cur-tok)
      ((super)
       (if (beginner?)
           (parse-error "Constructor may only initialize the fields of this class. Found super, which is not allowed"
                        (get-start cur-tok) (get-end cur-tok))
           (parse-ctor-call cur-tok (getter) 'start getter)))
      ((this)
       (cond
         ((advanced?) (parse-ctor-call cur-tok (getter) 'start getter))
         ((intermediate?) (parse-method-body pre cur-tok getter #t #f))
         ((beginner?) (parse-beginner-ctor-body null cur-tok 'start getter))))
      (else 
       (if (beginner?)
           (parse-beginner-ctor-body null cur-tok 'start getter) 
           (parse-method-body pre cur-tok getter #t #f)))))

  ;parse-ctor-call: token token symbol -> token
  (define (parse-ctor-call pre cur-tok state getter) 
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((O_PAREN) (parse-ctor-call cur-tok (getter) 'ctor-args getter))
           ((PERIOD) 
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (ne (get-end next)))
              (cond
                ((id-token? next-tok) 
                 (parse-method-body pre (parse-statement next (getter) 'assign-or-call getter #t #t #f) getter #t #f))
                ((java-keyword? next-tok)
                 (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name next-tok))
                              start ne))
                (else
                 (parse-error (format "Expected identifer after '.', found ~a" (format-out next-tok)) start ne)))))
           (else (parse-error (format "~a cannot be used here" (get-token-name pre)) ps end))))
        ((ctor-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((C_PAREN)
            (let ((next (getter)))
              (if (semi-colon? (get-tok next))
;                  (getter)
                  (if (beginner?)
                      (parse-beginner-ctor-body null (getter) 'start getter)
                      (parse-method-body next (getter) getter #t #t))
                  (parse-error (format "Expected a ';' after constructor call, found ~a" (format-out (get-tok next)))
                               start (get-end next)))))
           (else 
            (parse-ctor-call cur-tok (parse-expression pre cur-tok 'start getter #f #f) 'more-args getter))))
        ((more-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((C_PAREN) (parse-ctor-call pre cur-tok 'ctor-args getter))
           ((COMMA) 
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-ctor-call cur-tok (parse-expression cur-tok next 'start getter #f #f) 'more-args getter))))
           (else 
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close constructor arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between constructor arguments, found ~a" out) start end))))))))

  
  ;Beginner
  ;parse-beginner-ctor-body: token token symbol (-> token) -> token
  (define (parse-beginner-ctor-body pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((C_BRACE EOF) cur-tok)
           ((super) 
            (parse-error 
             (format "~a~n~a"
                     "Calling the parent's constructor must be the first action of a constructor,"
                     "and maynot appear here")
             start end))
           ((this) 
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (ns (get-start next))
                   (ne (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of field initialization, constructor and class need }" start end))
                ((dot? next-tok)
                 (let* ((afterD (getter))
                        (afterD-tok (get-tok afterD))
                        (ae (get-end afterD)))
                   (cond
                     ((id-token? afterD-tok) (parse-beginner-ctor-body afterD (getter) 'assign-op getter))
                     ((java-keyword? afterD-tok) 
                      (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name afterD-tok))
                                   ns ae))
                     (else
                      (parse-error (format "Expected identifer after '.', found ~a" (format-out afterD-tok)) ns ae)))))
                (else (parse-error (format "Expected this.Field, found ~a instead of '.'" (format-out next-tok)) ns ne)))))
           ((IDENTIFIER)
            (let ((next (getter)))
              (if (dot? (get-tok next)) 
                  (parse-beginner-ctor-body next (parse-name (getter) getter #f) 'assign-op getter)
                  (parse-beginner-ctor-body cur-tok next 'assign-op getter))))
           (else 
            (if (java-keyword? tok)
                (parse-error (format "Expected name, found reserved word ~a" kind) start end)
                (parse-error (format "Expected name, found ~a" out) start end)))))
        ((assign-op)
         (case kind
           ((EOF) (parse-error "Expected rest of field initialization (=), constructor and class need }" ps pe))
           ((=) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after = for field initialization" start end)
                  (parse-beginner-ctor-body cur-tok (parse-expression null next 'start getter #f #f) 'assign-end getter))))
           (else (parse-error (format "Expected = to be used in initializing the field in this constructor, found ~a" out) start end))))
        ((assign-end)
         (cond
           ((eof? tok) (parse-error "Expected a ; to end field intialization, constructor and class need }" ps pe))
           ((semi-colon? tok) (parse-beginner-ctor-body cur-tok (getter) 'start getter))
           (else (parse-error (format "Expected a ; to end field initialization, found ~a" out) start end)))))))
  
    
  ;Intermediate
  ;parse-method-body: token token (->token) bool bool-> token
  (define (parse-method-body pre cur-tok getter ctor? call-seen?)
;    (printf "parse-method-body pre ~a cur-tok ~a~n" pre cur-tok)
    (case (get-token-name (get-tok cur-tok))
      ((C_BRACE EOF) cur-tok)
      (else (parse-method-body pre 
                               (parse-statement pre cur-tok 'start getter #t ctor? call-seen?) 
                               getter ctor? call-seen?))))
  
  ;Intermediate - addition of parameter id-ok?
  ;parse-statement: token token symbol (->token) bool bool bool-> token
  (define (parse-statement pre cur-tok state getter id-ok? ctor? super-seen?)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      ;(printf "parse-statement: ~a pre ~a cur-tok ~a ~n" state pre cur-tok)
      (case state
        ((start)
         (case kind
           ((if) (parse-statement cur-tok (getter) 'if getter id-ok? ctor? super-seen?))
           ((return)
            (let ((next (getter)))
              (cond
                ((eof? (get-tok next)) 
                 (parse-error (if (or (advanced?) (intermediate?) )
                                  "Expected rest of return" 
                                  "Expected expression for return") 
                              start end))
                ((and (or (advanced?) (intermediate?)) (semi-colon? (get-tok next))) (getter))
                (else (parse-statement cur-tok (parse-expression null next 'start getter #f #f) 
                                       'return getter id-ok? ctor? super-seen?)))))
           ((IDENTIFIER)
            (if (beginner?)
                (parse-error
                 (let ((v (token-value tok)))
                   (cond
                    ((close-to-keyword? tok 'if) 
                     (format "Expected 'if', found ~a which is perhaps miscapitalized or spelled" v))
                    ((close-to-keyword? tok 'return)
                     (format "Expected 'return', found ~a which is perhaps miscapitalized or spelled" v))
                    (else
                     (format "Expected a statement, found ~a. Statements begin with 'if' or 'return'" out)))) 
                 start end)
                (let* ((next (getter))
                       (next-tok (get-tok next)))
                  (cond
                    ((dot? next-tok)
                     (parse-statement next (parse-name (getter) getter #f) 'statement-or-var getter id-ok? ctor? super-seen?))
                    ((literal-token? next-tok)
                     (parse-error 
                      (if (close-to-keyword? tok 'return)
                          (string-append
                           (format "~a ~a is not the correct beginning of a statement. ~a is similar to 'return'~n"
                                   (token-value tok) (format-out next-tok #f) (token-value tok))
                           "Check spelling and capitalization")
                          (format "It is an error to have ~a ~a as a statement" out (format-out next-tok #f)))
                      start (get-end next)))
                    ((this? next-tok)
                     (parse-error
                      (if (close-to-keyword? tok 'return)
                          (string-append 
                           (format "'~a this' is not the correct beginning of a statement. ~a is similar to 'return'~n"
                                   (token-value tok) (token-value tok))
                           "Check spelling and capitalization")
                          (format "It is an error to have '~a this' as a statement" out))
                      start (get-end next)))
                    (else
                     (parse-statement cur-tok next 'statement-or-var getter id-ok? ctor? super-seen?))))))
           (else
            (when (beginner?)
              (parse-error (format "Expected a statement, found ~a. Statements begin with 'if' or 'return'" out) start end))
            ;Intermediate cases
            (case kind
              ;From ctor-beginner-body
              ((this super)
               (let* ((next (getter))
                      (next-tok (get-tok next))
                      (ns (get-start next))
                      (ne (get-end next)))
                 (cond
                   ;Intermediate error change
                   ((eof? next-tok) (parse-error (format "Expected ~a.name, unexpected end" kind) start end))
                   ((dot? next-tok)
                    (let* ((afterD (getter))
                           (afterD-tok (get-tok afterD))
                           (ae (get-end afterD)))
                      (cond
                        ;Intermediate changed next state
                        ((id-token? afterD-tok) 
                         (parse-statement afterD (getter) 'assign-or-call getter id-ok? ctor? super-seen?))
                        ((java-keyword? afterD-tok)
                         (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name afterD-tok))
                                      ns ae))
                        (else
                         (parse-error (format "Expected identifer after '.', found ~a" (format-out afterD-tok)) ns ae)))))
                   ((o-paren? next-tok)
                    (cond
                      ((and ctor? super-seen?)
                       (parse-error (format "Expected ~a.name, found ~a instead of '.'" kind (format-out next-tok)) ns ne))
                      (ctor?
                       (parse-error (string-append (format "~a() calls must be the first item of the constructor body.~n" kind)
                                                   (format "Or a name was expected to complete ~a.name constructrion." kind))
                                    start ne))
                      (else
                       (parse-error (string-append (format "~a() calls may only appear in the constructor" kind)
                                                   (format "Or a name was expected to complete ~a.name construction." kind))
                                    start ne))))
                   (else (parse-error (format "Expected ~a.name, found ~a instead of '.'" kind (format-out next-tok)) ns ne)))))
              ;Intermediate
              ((new) (parse-statement cur-tok (parse-expression cur-tok (getter) 'alloc-start getter #f #f)
                                      'end-exp getter id-ok? ctor? super-seen?))
              ;Intermediate
              ((O_PAREN) 
               (parse-statement cur-tok 
                                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f #f)
                                                  'c-paren getter #f #f)
                                'end-exp getter id-ok? ctor? super-seen?))
              ;Intermediate
              ((O_BRACE) 
               (parse-statement cur-tok (parse-method-body cur-tok (getter) getter ctor? super-seen?) 
                                          'c-brace getter #t ctor? super-seen?))
              ;Intermediate - changed wholly
              (else
               (cond
                 ((prim-type? tok) (parse-statement cur-tok (getter) 'local getter id-ok? ctor? super-seen?))
                 ;Advanced
                 ((and (advanced?) (for-token? tok))
                  (parse-statement cur-tok (getter) 'for getter id-ok? ctor? super-seen?))
                 ((and (advanced?) (do-token? tok))
                  (parse-statement cur-tok (getter) 'do getter id-ok? ctor? super-seen?))
                 ((and (advanced?) (while-token? tok))
                  (parse-statement cur-tok (getter) 'while getter id-ok? ctor? super-seen?))
                 ((and (advanced?) (or (break-token? tok) (continue-token? tok)))
                  (parse-statement cur-tok (getter) 'break-continue getter id-ok? ctor? super-seen?))
                 ((java-keyword? tok)
                  (parse-error (format "Expected name, found reserved word ~a" kind) start end))
                 (else 
                  (parse-error (format "Expected statement, found ~a, which cannot begin a statement" out) start end))))))))
        ((if)
         (case kind
           ((EOF) (parse-error "Expected conditional test for 'if'" ps pe))
           ((O_PAREN) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error (format "Expected conditional expression for 'if'") start end)
                  (parse-statement cur-tok (parse-expression null next 'start getter #f #f) 
                                   'if-then getter id-ok? ctor? super-seen?))))
           (else 
            (parse-error (format "Conditional expression for 'if' must be started with '(', found ~a" out) start end))))
        ((if-then)
         (case kind
           ((EOF) 
            (if (advanced?)
                (parse-error "Expected ')' to close conditional for 'if'" ps pe)
                (parse-error "Expected ')' to close conditional for 'if', and then and else statements for 'if'" ps pe)))
           ((C_PAREN)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected statement for then branch of 'if'" start end))
                ((c-paren? next-tok) 
                 (parse-error "Conditional expression already closed, extra ')' found" start (get-end next)))
                (else (parse-statement cur-tok (parse-statement null next 'start getter #f ctor? super-seen?)
                                       'if-else getter id-ok? ctor? super-seen?)))))
           (else 
            (parse-error 
             (format "Conditional expression for 'if' must be in parens, did not find ')', found ~a" out) ps end))))
        ((if-else)
         (case kind
           ((EOF) 
            (if (advanced?) cur-tok (parse-error "Expected 'else' for 'if' statement" ps pe)))
           ((else) (parse-statement null (getter) 'start getter #f ctor? super-seen?))
           (else
            (if (advanced?)
                cur-tok
                (parse-error
                 (if (and (id-token? tok) (close-to-keyword? tok 'else))
                     (format "Expected 'else' for 'if', found ~a, which might be mispelled or miscapitalized" 
                             (token-value tok))
                     (format "Expected 'else' for 'if', found ~a" out))
                 start end)))))
        ((return)
         (case kind
           ((EOF) (parse-error "Expected ';' to end 'return' statement" ps pe))
           ((SEMI_COLON) (getter))
           (else (parse-error (format "Expected ';' to end 'return' statement, found ~a" out) start end))))
        ;Intermediate & Advanced
        ((statement-or-var)
         (case kind
           ((EOF) (parse-error "Expected remainder of statement" ps pe))
           ((IDENTIFIER) (parse-statement pre cur-tok 'local getter id-ok? ctor? super-seen?))
           (else (parse-statement pre cur-tok 'assign-or-call getter id-ok? ctor? super-seen?))))
        ;Intermediate
        ((assign-or-call)
         (case kind
           ((EOF) (parse-error "Expected remainder of assignment or call" ps pe))
           ((=)
            ;From Assignment
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after '=' for assignment" start end)
                  (parse-statement cur-tok 
                                   (parse-expression null next 'start getter #f #f) 'assign-end getter id-ok? ctor? super-seen?))))
           ((PERIOD) 
            (let ((next (getter)))
              (cond
                ((eof? (get-tok next)) (parse-error "Expected a name after '.'" start end))
                ((id-token? (get-tok next)) 
                 (parse-statement next (getter) 'assign-or-call getter id-ok? ctor? super-seen?))
                (else
                 (parse-error (format "Expected a name after '.', ~a is not a valid name" (format-out (get-tok next)))
                              start (get-end next))))))
           ((O_PAREN) 
            (if (and (advanced?) (close-to-keyword? (get-tok pre) 'for))
                (let* ((next (getter))
                       (next-tok (get-tok next)))
                  (cond
                    ((prim-type? next-tok)
                     (parse-error 
                      (format "Primitive type ~a cannot appear here.~nSection began with ~a, which is close to 'for'. Check spelling and capitalization."
                              (format-out next-tok)
                              (format-out (get-tok pre)))
                      ps (get-end next)))
                    ((c-paren? next-tok)
                     (let ((after-c (getter)))
                       (if (semi-colon? (get-tok after-c))
                           (parse-statement cur-tok (getter) 'end-exp getter id-ok? ctor? super-seen?)
                           (parse-statement cur-tok
                                            (parse-expression next-tok after-c 'dot-op-or-end 
                                                              getter #f #t)
                                            'end-exp getter id-ok? ctor? super-seen?))))
                    (else
                     (parse-statement 
                      cur-tok
                      (parse-expression cur-tok 
                                        (parse-expression cur-tok next-tok 'start getter #f #f)
                                        'method-args getter #f #t)
                      'end-exp getter id-ok? ctor? super-seen?))))
                (parse-statement cur-tok (parse-expression pre cur-tok 'method-call-args getter #f #t)
                                 'end-exp getter id-ok? ctor? super-seen?)))
           (else 
            (cond
              ((and (advanced?) (eq? kind 'O_BRACKET))
               (let* ((next (getter))
                      (next-tok (get-tok next)))
                 (if (eof? next-tok) 
                     (parse-error "Expected an index for array" start end)
                     (let* ((afterOB (parse-expression cur-tok next 'start getter #f #f))
                            (afterOB-tok (get-tok afterOB)))
                       (if (eof? afterOB-tok)
                           (parse-error "Expected a ']' to end array index" start (get-end afterOB))
                           (parse-statement afterOB (getter) 'assign-or-call getter id-ok? ctor? super-seen?))))))
              ((advanced?)
               (parse-statement pre cur-tok 'unary-check getter id-ok? ctor? super-seen?))
              (else
               (parse-error (format "Expected assignment or method call, found ~a, which is not valid for a statement" out)
                            start end))))))
        ((assignment)
         (case kind
           ((EOF) (parse-error "Expected remainder of assignment" ps pe))
           ((=)
            ;From Assignment
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after '=' for assignment" start end)
                  (parse-statement cur-tok 
                                   (parse-expression null next 'start getter #f #f) 'assign-end getter id-ok? ctor? super-seen?))))
           ((O_BRACKET)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (if (eof? next-tok) 
                  (parse-error "Expected an index for array" start end)
                  (let* ((afterOB (parse-expression cur-tok next 'start getter #f #f))
                         (afterOB-tok (get-tok afterOB)))
                    (if (eof? afterOB-tok)
                        (parse-error "Expected a ']' to end array index" start (get-end afterOB))
                        (parse-statement afterOB (getter) 'assignment getter id-ok? ctor? super-seen?))))))
           (else
            (parse-error (format "Expected assignment, found ~a, which is not valid for a statement" out)
                            start end))))
        ;Intermediate - from Assignment, error messages changed
        ((assign-end)
         (cond
           ((eof? tok) (parse-error "Expected a ';' to end assignment" ps pe))
           ((semi-colon? tok) (getter))
           (else (parse-error (format "Expected a ';' to end assignment, found ~a" out) start end))))
        ((unary-check)
         (let ((pre-out (token-value (get-tok pre))))
           (case kind
             ((EOF) (parse-error "Expected remainder of statement" ps pe))
             ((++ --) 
              (let* ((next (getter))
                     (next-tok (get-tok next)))
                (cond
                  ((eof? next-tok) 
                   (parse-error (format "Expected a ';' to end ~a~a" pre-out kind) ps end))
                ((semi-colon? next-tok) (getter))
                (else
                 (parse-error (format "Expected a ';' to end ~a~a, found ~a" pre-out kind (format-out next-tok))
                              ps (get-end next))))))
             (else (parse-error (format "Expected a statement ~a ~a is not the valid start of a statement"
                                        pre-out out) ps end)))))
        ;Intermediate
        ((end-exp)
         (case kind
           ((EOF) (parse-error "Expected ';' or rest of statement" ps pe))
           ((PERIOD)
            (let ((next (getter)))
              (cond
                ((id-token? (get-tok next)) (parse-statement next (getter) 'assign-or-call getter id-ok? ctor? super-seen?))
                (else 
                 (parse-error (format "Expected a name after '.', found ~a" (format-out (get-tok next)))
                              ps (get-end next))))))
           ((SEMI_COLON) (getter))
           (else 
            (parse-error (format "Expected ';' or rest of statement, found ~a" out) ps end))))
        ;Intermediate
        ((c-brace)
         (case kind
           ((EOF) (parse-error "Expected a '}' to close '{'" ps pe))
           ((C_BRACE) (getter))
           (else (parse-error (format "Expected a '}' to close open '{', found ~a" out) start end))))
        ;Intermediate
        ((local)
         (unless id-ok?
           (parse-error 
            (if (advanced?)
                "Found apparent variable declaration directly in an 'if', 'for', 'while', or 'do'. Varaibles declarations must be in blocks"
                "Found apparent variable declaration directly in an 'if', variable declarations must be in blocks")
            ps end))
         (case kind
           ((EOF) (parse-error "Variable declaration requires a name" start end))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (format-out n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Variable declaration has not completed" start end))
                ;Just ended a local field
                ((semi-colon? n-tok) (getter))
                ((comma? n-tok) (parse-statement next (getter) 'local-list getter #t ctor? super-seen?))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (cond
                     ((eof? (get-tok assign-exp))
                      (parse-error (format "Expected an expression to bind to ~a" (token-value tok)) start end))
                     ((and (advanced?) (o-brace? (get-tok assign-exp)))
                      (parse-statement cur-tok (parse-array-init assign-exp (getter) 'start getter) 'local-init-end getter #t ctor?
                                       super-seen?))
                     (else
                      (parse-statement cur-tok (parse-expression null assign-exp 'start getter #f #f) 
                                       'local-init-end getter #t ctor? super-seen?)))))
                ((id-token? n-tok)
                 (parse-error (format "Variables must be separated by commas, ~a not allowed" n-out) start ne))
                (else (parse-error (format "Expected ';' or more variables, found ~a" n-out) start ne)))))
           (else 
            (cond
              ((and (advanced?) (o-bracket? tok))
               (let* ((next (getter))
                      (n-tok (get-tok next)))
                 (cond
                   ((c-bracket? n-tok)
                    (parse-statement next (getter) 'local getter id-ok? ctor? super-seen?))
                   ((o-bracket? n-tok)
                    (parse-error "Array types may not have [[. A closing ] is necessary before opening a new [" start 
                                 (get-end next)))
                   (else (parse-error (format "Array types are of form type[]. ~a is not allowed" (format-out n-tok))
                                      start (get-end next))))))
              ((teaching-assignment-operator? tok)
               (parse-error (format "Expected a type and name before ~a, found ~a ~a" 
                                    kind (format-out (get-tok pre)) kind)
                            ps end))
              (else (parse-error 
                     (if (java-keyword? tok)
                         (format "Expected a name for this variable, cannot be named reserved word ~a" kind)
                         (format "Expected a name for this variable, found ~a" out)) start end))))))
        ;Intermediate
        ((local-list)
         (case kind
           ((EOF) (parse-error "Expected an additional variable after comma" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (format-out n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Variable is not complete" start end))
                ((semi-colon? n-tok) (getter))
                ((comma? n-tok) (parse-statement next (getter) 'local-list getter id-ok? ctor? super-seen?))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (if (eof? (get-tok assign-exp))
                       (parse-error (format "Expected an expression to bind to ~a" (token-value tok)) start end)
                       (parse-statement cur-tok (parse-expression null assign-exp 'start getter #f #f) 
                                        'local-init-end getter id-ok? ctor? super-seen?))))
                ((id-token? n-tok)
                 (parse-error (format "Variables must be separated by commas, ~a not allowed" n-out) start ne))
                (else (parse-error (format "Expected ';' or more variables, found ~a" n-out) start ne)))))
           (else
            (parse-error
             (if (java-keyword? tok)
                 (format "Expected a name for this variable, cannot be named reseved word ~a" kind)
                 (format "Expected a name for this variable, found ~a" out)) start end))))
        ;Intermediate
        ((local-init-end)
         (case kind
           ((EOF) (parse-error "Expected a ';' after variable declaration" ps end))
           ((COMMA) (parse-statement cur-tok (getter) 'local-list getter id-ok? ctor? super-seen?))
           ((SEMI_COLON) (getter))
           ((IDENTIFIER) (parse-error (format "Variables must be separated by commas, ~a not allowed" out) start end))
           (else (parse-error (format "Expected a ';' to end variable declaration, or more variables, found ~a" out) start end))))
        ;Advanced
        ((for) 
         (case kind
           ((EOF) (parse-error "Expected a '(' to begin 'for'" ps pe))
           ((O_PAREN) 
            (parse-statement cur-tok (parse-for cur-tok (getter) 'start getter ctor? super-seen?)
                             'start getter #f ctor? super-seen?))
           (else
            (parse-error (format "Expected a '(' to begin 'for'. Found ~a which is not allowed" out) start end))))
        ;Advanced
        ((do)
         (case kind
           ((EOF) (parse-error "Expeceted a statement and condition for 'do'" ps pe))
           (else (parse-statement pre (parse-statement pre cur-tok 'start getter #f ctor? super-seen?) 
                                  'do-while getter id-ok? ctor? super-seen?))))
        ;Advanced
        ((do-while)
         (case kind
           ((EOF) (parse-error "Expected 'while' and condition for 'do'" ps pe))
           ((while)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected a condition beginning with '(' for 'while' portion of 'do'" ps end))
                ((o-paren? next-tok)
                 (let* ((afterO (getter))
                        (afterO-tok (get-tok afterO)))
                   (cond
                     ((eof? afterO-tok) 
                      (parse-error "Expected a condition expression after '('" (get-start next) (get-end next)))
                     ((c-paren? afterO-tok) (getter))
                     (else
                      (parse-statement afterO (parse-expression null afterO 'start getter #f #f) 'do-while-close
                                       getter id-ok? ctor? super-seen?)))))
                (else
                 (parse-error 
                  (format "Expected a condition beginning with '(' for 'while' portion of 'do'. Found ~a" 
                          (format-out next-tok)) start (get-end next))))))
           (else
            (parse-error (format "Expected 'while' for 'do'. Found ~a which is not allowed here" out) start end))))
        ;Advanced
        ((do-while-close)
         (case kind
           ((EOF) (parse-error "Expected ')' to close condition of 'do'" ps pe))
           ((C_PAREN) 
            (let ((next (getter)))
              (cond
                ((eof? (get-tok next)) (parse-error "Expected ';' to close 'do'" ps end))
                ((semi-colon? (get-tok next)) (getter))
                (else 
                 (parse-error (format "Expected ';' to end 'do'. Found ~a which is not allowed" (format-out (get-tok next)))
                                      (get-start next) (get-end next))))))
           (else
            (parse-error (format "Expected ')' to close condition of 'do'. Found ~a which is not allowed" out) ps end))))                  
        ;Advanced
        ((while) 
         (case kind
           ((EOF) (parse-error "Expected a '(' to begin while condition" ps pe))
           ((O_PAREN)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected a ')' or an expression for while condition" ps end))
                ((c-paren? next-tok)
                 (parse-statement next (getter) 'start getter #f ctor? super-seen?))
                (else
                 (parse-statement cur-tok (parse-expression null next 'start getter #f #f) 
                                  'while-close getter id-ok? ctor? super-seen?)))))
           (else (parse-error (format "Expected a '(' to begin while condition, found ~a" out) ps end))))
        ;Advanced
        ((while-close)
         (case kind
           ((EOF) (parse-error "Expected a ')' to end while condition" ps pe))
           ((C_PAREN) (parse-statement cur-tok (getter) 'start getter #f ctor? super-seen?))
           (else (parse-error (format "Expected a ')' to end while condition, found ~a" out) ps end))))
        ;Advanced
        ((break-continue)
         (case kind
           ((EOF) (parse-error "Expected a ';'" ps pe))
           ((SEMI_COLON) (getter))
           (else (parse-error (format "Expected a ';' to end ~a. Found ~a which is not allowed here" 
                                      (token-name (get-tok pre)) out)
                              ps end))))
        )))
  
  ;parse-for: token token state (->token) bool bool -> token
  (define (parse-for pre cur-tok state getter ctor? super-seen?)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      ;(printf "parse-for: state ~a pre ~a cur-tok ~a~n" state pre cur-tok)
      (case state
        ((start)
         (cond
           ((prim-type? tok)
            (parse-for pre
                       (parse-statement cur-tok (getter) 'local getter #t ctor? super-seen?)
                       'past-inits getter ctor? super-seen?))
           ((id-token? tok)
            (parse-for pre cur-tok 'init-or-expr getter ctor? super-seen?))
           ((semi-colon? tok)
            (parse-for cur-tok (getter) 'past-inits getter ctor? super-seen?))
           (else (parse-for pre 
                            (parse-expression pre cur-tok 'start getter #f #f)
                            'statement-expr-first getter ctor? super-seen?))))
        ((init-or-expr)
         (case kind
           ((EOF) (parse-error "Expected remainder of 'for'" ps pe))
           ((PERIOD)
            (parse-for pre (parse-name (getter) getter #f) 'init-or-expr getter ctor? super-seen?))
           ((IDENTIFIER)
            (parse-for pre (parse-statement cur-tok (getter) 'local getter #t ctor? super-seen?)
                       'past-inits getter ctor? super-seen?))
           (else
            (parse-for pre (parse-expression pre cur-tok 'start getter #f #f)
                       'statement-expr-first getter ctor? super-seen?))))
        ((statement-expr-first)
         (case kind
           ((EOF) (parse-error "Expected remainder of 'for'" ps pe))
           ((COMMA) (parse-for cur-tok (parse-expression cur-tok (getter) 'start getter #f #f) 'statement-expr-first 
                               getter ctor? super-seen?))
           ((SEMI_COLON)
            (parse-for cur-tok (getter) 'past-inits getter ctor? super-seen?))
           (else
            (parse-error (format "Expected a ',' or ';' for list of statement expressions in 'for'. Found ~a" out) 
                         start end))))
        ((past-inits)
         (case kind
           ((EOF) (parse-error "Expected a conditional expression for 'for'" ps pe))
           #;((SEMI_COLON)
            (parse-for cur-tok (getter) 'past-condition getter ctor? super-seen?))
           (else
            (parse-for cur-tok (parse-expression pre cur-tok 'start getter #f #f) 
                       'end-condition getter ctor? super-seen?)
            #;(let ((next (getter)))
              (cond 
                ((eof? (get-tok next)) (parse-error "Expected the rest of 'for'" start end))
                ((semi-colon? (get-tok next)) (parse-for cur-tok next 'end-condition getter 
                                                         ctor? super-seen?))
                (else (parse-for cur-tok (parse-expression cur-tok next 'start getter #f #f) 
                                 'end-condition getter ctor? super-seen?)))))))
        ((end-condition)
         (case kind
           ((EOF) (parse-error "Expected a ';' to end the condition portion of 'for'" ps pe))
           ((SEMI_COLON) (parse-for cur-tok (getter) 'past-condition getter ctor? super-seen?))
           (else
            (parse-error (format "Expected a ';' to end the condition portion of 'for', found ~a" out) start end))))
        ((past-condition)
         (case kind
           ((EOF) (parse-error "Expected a ')' to end the pre-statement portion of 'for'" ps pe))
           ((C_PAREN) (getter))
           (else
            (parse-for pre (parse-expression pre cur-tok 'start getter #f #f)
                       'statement-expr-snd getter ctor? super-seen?))))
        ((statement-expr-snd)
         (case kind
           ((EOF) (parse-error "Expected a ')' to end the pre-statement portion of 'for'" ps pe))
           ((C_PAREN) (getter))
           ((COMMA)
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after ','" start end)
                  (parse-for cur-tok (parse-expression cur-tok next 'start getter #f #f)
                             'statement-expr-snd getter ctor? super-seen?))))
           (else (parse-error (format "Expected a ')' or a ','. Found ~a which is not allowed" out) start end))))
        )))

  ;parse-expression: token token state (->token) bool bool -> token
  (define (parse-expression pre cur-tok state getter statement-ok? stmt-exp?)
    #;(printf "parse-expression state ~a pre ~a cur-tok ~a statement-ok? ~a stmt-exp? ~a ~n" 
            state pre cur-tok statement-ok? stmt-exp?)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (format-out tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      ;(printf "kind ~a~n" kind)
      (case state
        ((start)
         (case kind
           ((EOF) (parse-error "Expected an expression" ps pe))
           ((~ ! -) (parse-expression cur-tok 
                                      (parse-expression cur-tok (getter) 'start getter #f #f) 
                                      'op-or-end getter statement-ok? stmt-exp?))
           ((+)
            (if (or (advanced?) (intermediate?))
                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f stmt-exp?) 
                                  'op-or-end getter statement-ok? stmt-exp?)
                (parse-error "Expected an expression, + cannot begin an expression" start end)))
           ;Advanced
           ((++ --)
            (if (advanced?)
                (parse-expression cur-tok 
                                  (parse-expression cur-tok (getter) 'start getter #f #f)
                                  'dot-op-or-end getter statement-ok? stmt-exp?)
                (parse-error (format "Expected an expression, ~a is not the valid beginning of an expression" out)
                             start end)))
           ((NULL_LIT) 
            (if (or (advanced?) (intermediate?))
                (parse-expression cur-tok (getter) 'dot-op-or-end getter statement-ok? stmt-exp?)
                (parse-error "Expected an expression. null may not be used here" start end)))
           ((TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this IMAGE_SPECIAL)
            (parse-expression cur-tok (getter) 'dot-op-or-end getter statement-ok? stmt-exp?))
           ((super)
            (if (beginner?)
                (parse-error "An expression may not begin with reserved word 'super'" start end)
                (parse-expression cur-tok (getter) 'dot-op-or-end getter statement-ok? stmt-exp?)))
           ((O_PAREN)
            (if (or (advanced?) (intermediate?))
                (parse-expression cur-tok (getter) 'cast-or-parened getter statement-ok? stmt-exp?)
                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f #f) 
                                  'c-paren getter statement-ok? stmt-exp?)))
           ((new) (parse-expression cur-tok (getter) 'alloc-start getter statement-ok? stmt-exp?))
           ((check) 
            (parse-expression pre cur-tok 'check getter statement-ok? stmt-exp?))
           ((IDENTIFIER) (parse-expression cur-tok (getter) 'name getter statement-ok? stmt-exp?))
           ((STRING_ERROR)
            (if (eq? 'STRING_NEWLINE (get-token-name (caddr (token-value tok))))
                (parse-error (format "A string must be contained all on one line, and end in '~a'" #\") start end)
                (parse-error (format "String must end with '~a', which is not found" #\") start end)))
           (else 
            (parse-error (format "Expected an expression, ~a is not the valid beginning of an expression" out) start end))))
        ;Advanced
        ((op-or-end)
         (if stmt-exp?
             (cond
               ((and (advanced?) (unary-end? tok)) (getter))
               (else cur-tok))
             (cond
               ((bin-operator? tok) 
                (parse-expression cur-tok (getter) 'start getter #f stmt-exp?))
               ((and (advanced?) (unary-end? tok)) 
                (parse-expression cur-tok (getter) 'op-or-end getter statement-ok? stmt-exp?))
               ((and (advanced?) (if-exp? tok))
                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f stmt-exp?) 
                                  'if-exp-colon getter #f stmt-exp?))
               ((and (advanced?) (o-bracket? tok))
                (parse-expression cur-tok (getter) 'array-acc getter statement-ok? stmt-exp?))
               ((and (or (advanced?) (intermediate?)) (instanceof-token? tok))
                (parse-expression cur-tok (getter) 'instanceof getter #f stmt-exp?))
               (else cur-tok))))
        ((dot-op-or-end)
         (cond
           ((dot? tok) 
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (name (get-token-name next-tok))
                   (ns (get-start next))
                   (ne (get-end next)))
              (cond 
                ((id-token? next-tok)
                 (let ((afterID (getter)))
                   (cond
                     ((o-paren? (get-tok afterID))
                      (parse-expression next afterID 'method-call-args getter statement-ok? stmt-exp?))
                     ((teaching-assignment-operator? (get-tok afterID))
                      (parse-expression next (parse-expression afterID (getter) 'start getter #f #f)
                                        'assign-end getter statement-ok? stmt-exp?))
                     (else (parse-expression next afterID 'dot-op-or-end getter statement-ok? stmt-exp?)))))
                ((eq? 'this name)
                 (parse-error "Expected a name, 'this' may not appear after a dot" ns ne))
                ((java-keyword? next-tok)
                 (parse-error (format "Expected a name, reserved name ~a may not be a name" name) ns ne))
                (else (parse-error (format "Expected a name, found ~a" (format-out next-tok)) ns ne)))))
           (stmt-exp? (parse-expression pre cur-tok 'op-or-end getter #f stmt-exp?))
           ((bin-operator? tok) (parse-expression cur-tok (getter) 'start getter #f stmt-exp?))
           ;Advanced
             ((and (advanced?) (unary-end? tok)) (parse-expression cur-tok (getter) 'op-or-end getter statement-ok? stmt-exp?))
             ((and (advanced?) (if-exp? tok))
              (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f #f) 'if-exp-colon getter #f stmt-exp?))
             ((and (advanced?) (o-bracket? tok)) (parse-expression cur-tok (getter) 'array-acc getter statement-ok? stmt-exp?))
             ((and (or (advanced?) (intermediate?)) (instanceof-token? tok)) (parse-expression cur-tok (getter) 'instanceof getter #f stmt-exp?))
             (else cur-tok)))
        ;Advanced
        ((array-acc)
         (cond
           ((eof? tok) (parse-error "Expected expression for accessing array" start end))
           ((c-bracket? tok) 
            (parse-error "Expected an expression for accessing array, inbetween [ and ]" ps end))
           (else (parse-expression pre 
                                   (parse-expression pre cur-tok 'start getter #f #f) 
                                   'c-bracket getter statement-ok? stmt-exp?))))
        ;Advanced
        ((c-bracket)
         (case kind
           ((EOF) (parse-error "Expected ] to end array access" ps pe))
           ((C_BRACKET) 
            (let ((next (getter)))
              (if (teaching-assignment-operator? (get-tok next))
                  (parse-expression next (parse-expression next (getter) 'start getter #f #f) 
                                    'assign-end getter statement-ok? stmt-exp?)
                  (parse-expression cur-tok next 'dot-op-or-end getter statement-ok? stmt-exp?))))
           (else (parse-error (format "Expected ] to end array access. Found ~a" out) ps end))))
        ;Advanced
        ((c-bracket-empty-ok)
         (case kind
           ((EOF) (parse-error "Expected ] to end array size specification" ps pe))
           ((C_BRACKET)
            (let ((next (getter)))
              (if (o-bracket? (get-tok next))
                  (let ((afterOB (getter)))
                    (if (c-bracket? (get-tok afterOB))
                        (parse-expression next afterOB 'c-bracket-empty getter statement-ok? stmt-exp?)
                        (parse-expression next (parse-expression next afterOB 'start getter #f stmt-exp?) 
                                          'c-bracket-empty-ok getter statement-ok? stmt-exp?)))
                  (parse-expression cur-tok next 'dot-op-or-end getter statement-ok? stmt-exp?))))
           (else
            (parse-error (format "Expected ] to end array size specification. Found ~a" out) ps end))))
        ;Advanced
        ((c-bracket-empty)
         (case kind
           ((EOF) (parse-error "Expected ] to end array specification" ps pe))
           ((C_BRACKET) 
            (let ((next (getter)))
              (if (o-bracket? (get-tok next))
                  (parse-expression next (getter) 'c-bracket-empty getter statement-ok? stmt-exp?)
                  (parse-expression cur-tok next 'dot-op-or-end getter statement-ok? stmt-exp?))))
           (else (parse-error (format "Expected ] to end array specification. Found ~a" out) ps end))))
        ;Intermediate
        ((cast-or-parened)
         (cond
           ((eof? tok) (parse-error "Expected a name or expression and a )" ps pe))
           ((prim-type? tok) (parse-expression pre (getter) 'cast getter statement-ok? stmt-exp?))
           ((id-token? tok) (parse-expression pre (getter) 'cast-or-parened-close getter statement-ok? stmt-exp?))
           (else (parse-expression pre (parse-expression pre cur-tok 'start getter #f #f) 
                                   'c-paren getter statement-ok? stmt-exp?))))
        ;Intermediate
        ((cast)
         (cond
           ((eof? tok) (parse-error "cast must have close paren and additional expression" ps pe))
           ((c-paren? tok) (parse-expression cur-tok (getter) 'start getter #f stmt-exp?))
           ((and (advanced?) (o-bracket? tok))
            (let ((next (getter)))
              (cond
                ((eof? (get-tok next)) (parse-error "cast to array must have ]" start end))
                ((c-bracket? (get-tok next)) (parse-expression next (getter) 'cast getter statement-ok? stmt-exp?))
                (else (parse-error (format "cast to array must have ]. ~a is not allowed" (format-out (get-tok next)))
                                   start (get-end next))))))
           (else (parse-error (format "cast must have close paren, found ~a instead" out) ps end))))
        ;Intermediate
        ((cast-or-parened-close)
         (cond
           ((eof? tok) (parse-error "Expected a ')'" ps pe))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (case (get-token-name next-tok)
                ((~ ! - + TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this O_PAREN new IDENTIFIER IMAGE_SPECIAL)
                 (parse-expression cur-tok next 'start getter #f stmt-exp?))
                ((super)
                 (if (beginner?)
                     (parse-error "Reserved word 'super' maynot appear in an expression" 
                                  (get-start next) (get-end next))
                     (parse-expression cur-tok next 'start getter #f stmt-exp?)))
                ((NULL_LIT)
                 (if (or (advanced?) (intermediate?))
                     (parse-expression cur-tok next 'start getter #f stmt-exp?)
                     (parse-expression cur-tok next 'dot-op-or-end getter #f stmt-exp?)))
                (else (parse-expression cur-tok next 'dot-op-or-end getter #f stmt-exp?)))))
           ((and (advanced?) (o-bracket? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? (get-tok next)) 
                 (parse-error "cast to array must have ], array access must have expression" start end))
                ((c-bracket? (get-tok next)) (parse-expression next (getter) 'cast getter statement-ok? stmt-exp?))
                (else (parse-expression cur-tok 
                                        (parse-expression cur-tok next 'array-acc getter #f stmt-exp?)
                                        'c-paren getter statement-ok? stmt-exp?)))))
           (else (parse-expression pre (parse-expression pre cur-tok 'name getter #f #f) 'c-paren getter statement-ok? stmt-exp?))))
;           (else (parse-error (format "Expected a ')', found ~a" out) ps end))))
        ((c-paren)
         (cond
           ((eof? tok) (parse-error "Expected a )" ps pe))
           ((c-paren? tok) (parse-expression cur-tok (getter) 'dot-op-or-end getter statement-ok? stmt-exp?))
           (else (parse-error (format "Expression in parens must have an operator or a close paren, found ~a instead" out) ps end))))
        ;Advanced
        ((instanceof)
         (cond
           ((eof? tok) (parse-error "Expected a type for instanceof" ps pe))
           ((id-token? tok)
            (let ((next (getter)))
              (if (dot? (get-tok next))
                  (parse-expression next (parse-name (getter) getter #f) 'instanceof-array getter statement-ok? stmt-exp?)
                  (parse-expression cur-tok next 'op-or-end getter statement-ok? stmt-exp?))))
           ((prim-type? tok) (parse-expression cur-tok (getter) 'instanceof-array getter statement-ok? stmt-exp?))
           ((java-keyword? tok) 
            (parse-error (format "Expected a type for instanceof comparison, found ~a which is not the name of a type" out) 
                         start end)) 
           (else (parse-error (format "Expected a type for instanceof comparison. Found ~a" out) start end))))
        ;Advanced
        ((instanceof-array)
         (case kind
           ((O_BRACE) 
            (if (intermediate?)
                (parse-error "'[' may not follow the name of a type" start end)
                (let ((next (getter)))
                  (if (c-brace? (get-tok next))
                      (parse-expression next (getter) 'instanceof-array getter statement-ok? stmt-exp?)
                      (parse-error (format "Array types are of the form type[], expected ] found ~a" 
                                           (format-out (get-tok next)))
                                   start (get-end next))))))
           (else (parse-expression pre cur-tok 'op-or-end getter statement-ok? stmt-exp?))))
        ;Advanced
        ((if-exp-colon)
         (cond
           ((eof? tok) (parse-error "Expected a :" ps pe))
           ((colon? tok) 
            (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f #f)
                              'op-or-end getter statement-ok? stmt-exp?))
           (else (parse-error (format "Expected a : found ~a" out) start end))))
        ((alloc-start)
         (cond
           ((eof? tok) 
            (parse-error (if (advanced?)
                             "Expected a class name or primitive type for allocation"
                             "Expected a class name for allocation") ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) 
                 (if (advanced?)
                     (parse-error "Expected constructor arguments for class allocation or size for array" start end)
                     (parse-error "Expected constructor arguments for class allocation" start end)))
                ;Advanced
                ((dot? next-tok)
                 (cond
                   ((beginner?)
                    (parse-error "Expected (argument, ...) for an object creation, '.' may not appear here"
                                 (get-start next) (get-end next)))
                   ((advanced?)
                    (parse-expression cur-tok (parse-name (getter) getter #f) 'alloc-open getter statement-ok? stmt-exp?))
                   (else
                    (parse-expression cur-tok (parse-name (getter) getter #f) 'class-args-start getter statement-ok? stmt-exp?))))
                ((o-paren? next-tok) (parse-expression cur-tok next 'class-args-start getter statement-ok? stmt-exp?))
                ;Advanced
                ((and (advanced?) (o-bracket? next-tok))
                 (parse-expression next (getter) 'array-size getter statement-ok? stmt-exp?))
                ((c-paren? next-tok)
                 (parse-error (format "Expected ( to begin constructor arguments for ~a" out) (get-start pre) end))
                ((open-separator? next-tok) 
                 (parse-error (format 
                               (if (advanced?)
                                   "Expected ( to begin constructor arguments, or [ to begin array size, found ~a"
                                   "Expected ( to begin constructor arguments, found ~a") (format-out next-tok))
                              (get-start next) (get-end next)))
                (else 
                 ;Advanced
                 (parse-error
                  (format (if (advanced?)
                              "Expected constructor arguments in parens or array size in []s, found ~a"
                              "Expected constructor arguments in parens, found ~a")
                          (format-out next-tok)) (get-start pre) (get-end next))))))
           ;Advanced
           ((and (advanced?) (prim-type? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected a size for the array" start end))
                ((o-bracket? next-tok)
                 (parse-expression next (getter) 'array-size getter statement-ok? stmt-exp?))
                (else (parse-error (format "Expected a [ to begin array size, found ~a" (format-out next-tok))
                                   start (get-end next))))))
           ((java-keyword? tok)
            ;Advanced
            (parse-error (format
                          (if (advanced?)
                              "Expected a class name or primitive type, reserved word ~a is neither"
                              "Expected a class name, reserved word ~a is not a class") kind) start end))
           (else 
            (parse-error (format (if (advanced?) 
                                     "Expected a class name or primitive type, found ~a"
                                     "Expected a class name, found ~a") out) start end))))
        ;Advanced
        ((alloc-open)
         (case kind
           ((EOF) (parse-error "Expected a ( to begin constructor arguments or [ to specify array size" ps pe))
           ((O_PAREN) (parse-expression pre cur-tok 'class-args-start getter statement-ok? stmt-exp?))
           ((O_BRACKET) (parse-expression cur-tok (getter) 'array-size getter statement-ok? stmt-exp?))
           (else (parse-error (format "Expected a ( to begin constructor arguments or [ to specify size, found ~a" out)
                              ps end))))
        ;Advanced
        ((array-size)
         (case kind
           ((EOF) (parse-error "Expected a size for the array" ps pe))
           ((C_BRACKET) (parse-error "Array allocation must have an expresion for its size. Found ]" ps end))
           (else 
            (parse-expression pre (parse-expression pre cur-tok 'start getter #f stmt-exp?) 
                              'c-bracket-empty-ok getter statement-ok? stmt-exp?))))
        ((class-args-start)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments starting with (" ps pe))
           ((O_PAREN)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected constructor arguments or )" start end))
                ((c-paren? next-tok) (parse-expression next (getter) 'dot-op-or-end getter statement-ok? stmt-exp?))
                (else (parse-expression next (parse-expression cur-tok next 'start getter #f #f) 
                                        'class-args getter statement-ok? stmt-exp?)))))
           (else (parse-error (format "Expected constructor arguments, starting with (, found ~a" out) start end))))
        ((class-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((C_PAREN) (parse-expression cur-tok (getter) 'dot-op-or-end getter statement-ok? stmt-exp?))
           ((COMMA)
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-expression cur-tok (parse-expression cur-tok next 'start getter #f stmt-exp?) 
                                    'class-args getter statement-ok? stmt-exp?))))
           (else
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close constructor arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between expressions in a constructor call, found ~a" out) start end)))))
        ((check)
         (parse-expression cur-tok
                           (parse-expression cur-tok (getter) 'start getter #f stmt-exp?)
                           'check-expect getter statement-ok? stmt-exp?))
        ((check-expect)
         (case kind
           ((EOF) (parse-error "Expected 'expect' and rest of 'check' expression" ps end))
           ((expect)
            (parse-expression pre (parse-expression cur-tok (getter) 'start getter #f stmt-exp?)
                              'within-or-end getter statement-ok? stmt-exp?))
           (else
            (if (close-to-keyword? tok 'expect)
                (parse-error 
                 (format "Expected 'expect' for the intended result of check.~n Found ~a which is similar to 'expect', check spelling and capitolization."
                         out)
                 start end)
                (parse-error (format "Expected 'expect' for the intended result of check. Found ~a which may not appear here."
                                     out)
                             ps end)))))
        ((within-or-end)
         (case kind
           ((within)
            (parse-expression cur-tok (getter) 'start getter #f stmt-exp?))
           (else
            (if (close-to-keyword? tok 'within)
                (parse-error
                 (format "Expected 'within' for range of check, found ~a which is similar to 'within'. Check capitolization and spelling."
                         out))
                cur-tok))))
        ((name)
         (case kind
           ((PERIOD) (parse-expression cur-tok (parse-name (getter) getter #f) 'name getter statement-ok? stmt-exp?))
           ((O_PAREN) (parse-expression pre cur-tok 'method-call-args getter statement-ok? stmt-exp?))
           ((=) (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter #f stmt-exp?) 
                                  'assign-end getter statement-ok? stmt-exp?))
           ((C_BRACKET) cur-tok) 
           (else (parse-expression pre cur-tok 'op-or-end getter statement-ok? stmt-exp?))))
        ((method-call-args)
         (case kind
           ((EOF) (parse-error "Expected method arguments starting with (" ps pe))
           ((O_PAREN)
            (let ((next-tok (getter)))
              (cond
                ((eof? (get-tok next-tok)) (parse-error "Expected method arguments or )" start end))
                ((c-paren? (get-tok next-tok)) 
                 (let ((after-c (getter)))
                   (if (and statement-ok? (or (advanced?) (intermediate?) (semi-colon? (get-tok after-c))))
                       (getter)
                       (parse-expression next-tok after-c 'dot-op-or-end getter statement-ok? stmt-exp?))))
                (else (parse-expression cur-tok (parse-expression cur-tok next-tok 'start getter #f #f)
                                        'method-args getter statement-ok? stmt-exp?)))))
           (else (parse-error (format "Expected method arguments in parens, found ~a" out) start end))))
        ((method-args)
         (case kind
           ((EOF) (parse-error "Expected method arguments or )" ps pe))
           ((C_PAREN) 
            (let ((after-c (getter)))
              (if (and statement-ok? (or (advanced?) (intermediate?) (semi-colon? (get-tok after-c))))
                  (getter)
                  (parse-expression cur-tok after-c 'dot-op-or-end getter statement-ok? stmt-exp?))))
           ((COMMA) 
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-expression cur-tok (parse-expression cur-tok next 'start getter #f stmt-exp?) 
                                    'method-args getter statement-ok? stmt-exp?))))
           (else 
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close method arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between expression in a method call, found ~a" out) start end)))))
        ((assign-end)
         (cond
           ((and statement-ok? (semi-colon? tok)) (getter))
           ((and statement-ok? (eof? tok)) 
            (parse-error "Assignment must end with a ';'" ps end))
           (statement-ok?
            (parse-error (format "Assignment must end with a ';'. Found ~a" out) start end))
           ((beginner?) (parse-error "Fields may not be set in this position, only expressions are permitted here" ps end))
           (else (parse-error "Assignment is not permitted in this position. Only expressions are permitted here" ps end)))))))
  )
