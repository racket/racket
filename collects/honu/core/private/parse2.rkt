#lang racket/base

(define-syntax-rule (require-syntax stuff ...)
                    (require (for-syntax stuff ...)))

;; phase 0
(require ;; "macro2.rkt"
         "literals.rkt"
         "debug.rkt"
         "compile.rkt"
         racket/list
         (prefix-in transformer: "transformer.rkt")
         (prefix-in fixture: "fixture.rkt")
         "operator.rkt"
         macro-debugger/emit
         racket/pretty
         syntax/stx
         syntax/parse/experimental/splicing
         syntax/parse)
;; phase 1
(require-syntax racket/base
                "debug.rkt")

;; phase -1
(require (for-template racket/base
                       racket/splicing
                       (only-in "literals.rkt" %racket)
                       "compile.rkt"
                       "syntax.rkt"
                       "extra.rkt"))

(provide parse parse-all)

#;
(define-literal-set literals
                    [honu-macro])

(define (get-value what)
  (syntax-local-value what (lambda () #f)))

#;
(define (get-value what)
  (debug "what is ~a\n" what)
  (with-syntax ([what what])
    (let-syntax ([v (lambda (stx)
                      (debug "get ~a\n" #'what)
                      (with-syntax ([x (syntax-local-value #'what (lambda () #f))])
                        #'x))])
      (v))))

#;
(define (get-value check)
  (eval-syntax
    (with-syntax ([check check])
      #'(syntax-local-value check #'check (lambda () #f))))) 

(define (bound-to-operator? check)
  (let ([value (get-value check)])
    (debug 2 "operator? ~a ~a\n" check value)
    (transformer:honu-operator? value)))

(define (bound-to-fixture? check)
  (let ([value (get-value check)])
    (debug 2 "fixture? ~a ~a\n" check value)
    (fixture:fixture? value)))

(define (bound-to-macro? check)
  (let ([value (get-value check)])
    (debug 2 "macro? ~a ~a\n" check value)
    (transformer:honu-transformer? value))
  #;
  (let ([value (syntax-local-value check (lambda () #f))])
    (transformer:honu-transformer? value)))

(define (honu-macro? something)
  (and (identifier? something)
       (bound-to-macro? something)))

(define (honu-operator? something)
  (and (identifier? something)
       (bound-to-operator? something)))

(define (honu-fixture? something)
  (and (identifier? something)
       (bound-to-fixture? something)))

(define (semicolon? what)
  (define-literal-set check (semicolon))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug "Semicolon? ~a ~a\n" what is)
  is)

(define (comma? what)
  (define-literal-set check (honu-comma))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug 2 "Comma? ~a ~a\n" what is)
  is)

(define-literal-set argument-stuff [honu-comma])

(define (parse-arguments arguments)
  (define-syntax-class val
    [pattern x:identifier #:when (equal? 'val (syntax-e #'x))])
  (let loop ([out '()]
             [arguments arguments])
    (syntax-parse arguments #:literal-sets (argument-stuff)
      [(x:val name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(x:val name:identifier)
       (loop (cons #'name out) #'())]
      [(name:identifier)
       (loop (cons #'name out) #'())]
      [() (reverse out)])))

(define (parse-comma-expression arguments)
  (if (null? (syntax->list arguments))
    '()
    (let loop ([used '()]
               [rest arguments])
      (if (empty-syntax? rest)
        (reverse used)
        (syntax-parse rest #:literal-sets (cruft)
          [(honu-comma more ...)
           (loop used #'(more ...))]
          [else
            (let-values ([(parsed unparsed)
                          ;; FIXME: don't strip all stops, just comma
                          (parse (strip-stops rest))])
              (loop (if parsed
                      (cons parsed used)
                      used)
                    unparsed))])))))

(define (stopper? what)
  (define-literal-set check (honu-comma semicolon colon))
  (define is (and (identifier? what)
                  ((literal-set->predicate check) what)))
  (debug 2 "Comma? ~a ~a\n" what is)
  is)

(provide do-parse-rest)
(define (do-parse-rest stx parse-more)
  (syntax-parse stx #:literal-sets (cruft)
    [(semicolon semicolon ... rest ...)
     (do-parse-rest #'(rest ...) parse-more)]
    [(stuff ...)
     (debug "Parse rest ~a\n" (syntax->datum #'(stuff ...)))
     (define-values (parsed unparsed)
                    (parse (strip-stops #'(stuff ...))))
     (debug "Parse more: ~a unparsed ~a\n" parsed unparsed)
     (define output (if parsed
                      parsed
                      #;
                      (honu->racket parsed)
                      #'(void)))
     (debug "Output ~a unparsed ~a\n"
            (syntax->datum output)
            (syntax->datum unparsed))
     (with-syntax ([output output]
                   [(unparsed-out ...) unparsed]
                   [parse-more parse-more])
       (if (null? (syntax->datum #'(unparsed-out ...)))
         (if (parsed-syntax? #'output)
           #'output
           #;
           #'(parse-more output)
           (with-syntax ([(out ...) #'output])
             #'(parse-more out ...)))
         #;
         #'(begin (parse-more output unparsed-out ...))
         (if (parsed-syntax? #'output)
           #'(begin output (parse-more unparsed-out ...))
           #;
           #'(parse-more output unparsed-out ...)
           (with-syntax ([(out ...) #'output])
             #'(begin (parse-more out ...) (parse-more unparsed-out ...))))))]
    [() #'(begin)]))

(define (do-parse-rest/local stx)
  (define name (gensym 'local-parser))
  (define local-parser (with-syntax ([name name])
                         #'(define-syntax (name stx)
                             (syntax-case stx ()
                               [(_ stuff (... ...))
                                (debug "Properties on first element ~a\n" (syntax-property-symbol-keys (stx-car #'(stuff (... ...)))))
                                (do-parse-rest #'(stuff (... ...)) #'name)]))))
  (with-syntax ([local local-parser]
                #;
                [parsed (do-parse-rest stx name)])
    (with-syntax ([stx stx]
                  [name name])
      (debug "Create local parser for ~a properties ~a\n" (syntax->datum #'stx) (syntax-property-symbol-keys #'stx))
      ;; sort of a hack, if the input is already parsed then don't deconstruct it
      ;; otherwise the input is a honu expression so we need to splice it in
      (define with-local
        (if (parsed-syntax? #'stx)
          #'(begin local (unexpand-honu-syntax (name stx)))
          (with-syntax ([(inside ...) #'stx])
            #'(begin local (unexpand-honu-syntax (name inside ...))))))
      (emit-local-step #'stx with-local #:id #'do-parse-rest/local)
      (parsed-syntax with-local))))

#|
(provide do-parse-rest-macro)
(define-syntax (do-parse-rest-macro stx)
  (syntax-case stx ()
    [(_ stuff ...)
     (do-parse-rest #'(stuff ...) #'do-parse-rest-macro)]))
|#

(provide honu-body)
(define-syntax-class honu-body
  #:literal-sets (cruft)
  [pattern (#%braces code ...)
           #:with result
           (racket-syntax (let ()
                            (define-syntax (parse-more stx)
                              (syntax-case stx ()
                                [(_ stuff (... ...))
                                 (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
                            (parse-more code ...)))])

(provide honu-delayed)
(define-syntax-class honu-delayed
  [pattern any #:with result (racket-syntax
                               (let ()
                                 (define-syntax (parse-more stx)
                                   (syntax-case stx ()
                                     [(_ stuff (... ...))
                                      (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
                                 (parse-more any)))])

(provide honu-function)
(define-splicing-syntax-class honu-function #:literal-sets (cruft)
  [pattern (~seq function:identifier (#%parens args ...) body:honu-body)
           #:with result
           (with-syntax ([(parsed-arguments ...)
                          (parse-arguments #'(args ...))])
             (racket-syntax (define (function parsed-arguments ...)
                              body.result)))])

(define (definition? code)
  (define (contains-define? code)
    (syntax-parse code #:literals (define define-honu-syntax)
      [(define x ...) #t]
      [(define-honu-syntax x ...) #t]
      [else #f]))
  (and (parsed-syntax? code)
       (contains-define? code)))

;; E = macro
;;   | E operator E
;;   | [...]
;;   | f(...)
;;   | { ... }
;;   | (...)

;; 1 + 1
;; ^
;;  left: identity
;;  current: 1
;; 1 + 1
;;   ^
;;  left: (lambda (x) (+ 1 x))
;;  current: #f
;; 1 + 1
;;     ^
;;  left: (lambda (x) (+ 1 x))
;;  current: 1
;;
;; 1 + 1 * 2
;;       ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: #f
;;
;; 1 + 1 * 2
;;         ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: 2

;; parse one form
;; return the parsed stuff and the unparsed stuff
(define (parse input)
  (define (do-macro head rest precedence left current stream)
    (if current
      (values (left current) stream)
      (begin
        (debug "Honu macro at phase ~a: ~a\n" (syntax-local-phase-level) head)
        (let-values ([(parsed unparsed terminate?)
                      ((syntax-local-value head)
                       (with-syntax ([head head]
                                     [(rest ...) rest])
                         (datum->syntax #'head
                                        (syntax->list #'(head rest ...))
                                        #'head #'head))
                       #f)])
          #;
          (emit-remark parsed)
          #;
          (emit-local-step stream parsed #:id #'do-macro)
          (with-syntax ([parsed parsed]
                        [rest unparsed])
            (debug "Output from macro ~a\n" (pretty-format (syntax->datum #'parsed)))
            #;
            (do-parse #'(parsed ... rest ...)
                      precedence left current)
            ;; (debug "Remove repeats from ~a\n" #'parsed)
            (define re-parse (remove-repeats #'parsed)
              #;
              (with-syntax ([(x ...) #'parsed])
                (debug "Properties on parsed ~a\n" (syntax-property-symbol-keys #'parsed))
                (do-parse-rest/local #'parsed)))
            (debug "Reparsed ~a\n" (pretty-format (syntax->datum re-parse)))
            #;
            (define re-parse (let-values ([(re-parse re-unparse)
                                           (parse #'parsed)])
                               (with-syntax ([(re-parse* ...) re-parse]
                                             [(re-unparse* ...) re-unparse])
                                 (datum->syntax re-parse
                                                (syntax->list
                                                  #'(%racket
                                                      (begin (re-parse* ...)
                                                             (let ()
                                                               (define-syntax (parse-more stx)
                                                                 (do-parse-rest stx #'parse-more))
                                                               (parse-more re-unparse* ...)))))
                                                re-parse re-parse))))
            #;
            (debug "Reparsed output ~a\n" (pretty-format (syntax->datum re-parse)))
            (define terminate (definition? re-parse))
            (debug "Terminate? ~a\n" terminate)
            (if terminate
              (values (left re-parse)
                      #'rest)
              (do-parse #'rest precedence
                        left re-parse)))))))
  (define (do-parse stream precedence left current)
    (define-syntax-class atom
      ;; [pattern x:identifier #:when (not (stopper? #'x))]
      [pattern x:identifier #:when (not (free-identifier=? #'#%braces #'x))]
      [pattern x:str]
      [pattern x:number])

    (debug "parse ~a precedence ~a left ~a current ~a properties ~a\n"
           (syntax->datum stream) precedence left current
           (syntax-property-symbol-keys stream))
    (define final (if current current #f))
    (if (parsed-syntax? stream)
      (values (left stream) #'())
    (syntax-parse stream #:literal-sets (cruft)
      #;
      [x:id (values #'x #'())]
      [()
       (values (left final) #'())]
      ;; dont reparse pure racket code
      [(%racket racket)
       (debug "Native racket expression ~a\n" #'racket)
       (if current
         (values (left current) stream)
         (values (left #'racket) #'()))
       #;
       (if current
         (values (left current) stream)
         (values (left #'racket) #'(rest ...)))]
      ;; for expressions that can keep parsing
      #;
      [((%racket-expression racket) rest ...)
       (if current
         (values (left current) stream)
         (do-parse #'(rest ...)
                   precedence left
                   #'racket))]
      #;
      [(%racket-expression racket rest ...)
       (if current
         (values (left current) stream)
         (do-parse #'(rest ...)
                   precedence left
                   #'racket))]
      [(head rest ...)
       (debug 2 "Not a special expression..\n")
       (cond
         [(honu-macro? #'head)
          (debug "Macro ~a\n" #'head)
          (do-macro #'head #'(rest ...) precedence left current stream)]
         [(parsed-syntax? #'head)
          (debug "Parsed syntax ~a\n" #'head)
          (emit-local-step #'head #'head #:id #'do-parse)
          (do-parse #'(rest ...) precedence left #'head)]
         [(honu-fixture? #'head)
          (debug 2 "Fixture ~a\n" #'head)
          (define transformer (fixture:fixture-ref (syntax-local-value #'head) 0))
          (define-values (output rest) (transformer current stream))
          (do-parse rest precedence left output)]
         [(honu-operator? #'head)
          (define new-precedence (transformer:honu-operator-ref (syntax-local-value #'head) 0))
          (define association (transformer:honu-operator-ref (syntax-local-value #'head) 1))
          (define binary-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 2))
          (define unary-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 3))
          (define higher
            (case association
              [(left) >]
              [(right) >=]))
          (debug "precedence old ~a new ~a higher? ~a\n" precedence new-precedence (higher new-precedence precedence))
          (if (higher new-precedence precedence)
            (let-values ([(parsed unparsed)
                          (do-parse #'(rest ...) new-precedence
                                      (lambda (stuff)
                                        (define right (parse-all stuff))
                                        (define output
                                          (if current
                                            (if binary-transformer
                                              (binary-transformer (parse-all-expression current) right)
                                              (error 'binary "cannot be used as a binary operator in ~a" #'head))
                                            (if unary-transformer
                                              (unary-transformer right)
                                              (error 'unary "cannot be used as a unary operator in ~a" #'head))))
                                        #;
                                        (debug "Binary transformer ~a\n" binary-transformer)
                                        #;
                                        (emit-local-step stuff output #:id binary-transformer)
                                        (with-syntax ([out (parse-all output)])
                                          #'out))

                                    #f)])
              (do-parse unparsed precedence left parsed))
            ;; if we have a unary transformer then we have to keep parsing
            (if unary-transformer
              (if current
                (values (left current) stream)
                (do-parse #'(rest ...) new-precedence
                                      (lambda (stuff)
                                        (define right (parse-all stuff))
                                        (define output (unary-transformer right))
                                        ;; apply the left function because
                                        ;; we just went ahead with parsing without
                                        ;; caring about precedence
                                        (with-syntax ([out (left (parse-all output))])
                                          #'out))
                                    #f))
              ;; otherwise we have a binary transformer (or no transformer..??)
              ;; so we must have made a recursive call to parse, just return the
              ;; left hand
              (values (left current) stream))
            )]
         
         #;
         [(stopper? #'head)
          (debug "Parse a stopper ~a\n" #'head)
          (values (left final)
                  stream)]
         [else
           (define-splicing-syntax-class no-left
             [pattern (~seq) #:when (and (= precedence 0) (not current))])
           (syntax-parse #'(head rest ...) #:literal-sets (cruft)
             #;
             [(semicolon . rest)
              (debug "Parsed a semicolon, finishing up with ~a\n" current)
              (values (left current) #'rest)]
             [body:honu-body
               (if current
                 (values (left current) stream)
                 (values (left #'body.result) #'())
                 #;
                 (do-parse #'(rest ...) precedence left #'body.result))]
             #;
             [((semicolon more ...) . rest)
              #;
              (define-values (parsed unparsed)
                             (do-parse #'(more ...)
                                       0
                                       (lambda (x) x)
                                       #f))
              #;
              (when (not (stx-null? unparsed))
                (raise-syntax-error 'parse "found unparsed input" unparsed))
              (values (parse-all #'(more ...)) #'rest)]
             #;
             [(left:no-left function:honu-function . rest)
              (values #'function.result #'rest)]
             [else 
               (debug "Parse a single thing ~a\n" (syntax->datum #'head))
               (syntax-parse #'head
                     #:literal-sets (cruft)
                     #;
                     [(%racket x)
                      (debug 2 "Native racket expression ~a\n" #'x)
                      (if current
                        (values (left current) stream)
                        (do-parse #'(rest ...) precedence left #'head))]
                     [x:atom
                       (debug 2 "atom ~a current ~a\n" #'x current)
                       (if current
                         (values (left current) stream)
                         (do-parse #'(rest ...) precedence left (racket-syntax x)))]
                     ;; [1, 2, 3] -> (list 1 2 3)
                     [(#%brackets stuff ...)
                      (define-literal-set wheres (honu-where))
                      (define-literal-set equals (honu-equal))
                      (syntax-parse #'(stuff ...) #:literal-sets  (cruft wheres equals)
                        [(work:honu-expression 
                          colon (~seq variable:id honu-equal list:honu-expression (~optional honu-comma)) ...
                          (~seq honu-where where:honu-expression (~optional honu-comma)) ...)
                         (define filter (if (attribute where)
                                          (with-syntax ([(where.result ...) (map honu->racket (syntax->list #'(where.result ...)))])
                                            #'((#:when where.result) ...))
                                          #'()))
                         (define comprehension
                           (with-syntax ([((filter ...) ...) filter]
                                         [(list.result ...) (map honu->racket (syntax->list #'(list.result ...)))]
                                         [work.result (honu->racket #'work.result)])
                             (racket-syntax (for/list ([variable list.result]
                                                       ...
                                                       filter ... ...)
                                              work.result))))
                         (if current
                           (values (left current) stream)
                           (do-parse #'(rest ...) precedence left comprehension))]
                        [else
                          (debug "Current is ~a\n" current)
                          (define value (with-syntax ([(data ...)
                                                       (parse-comma-expression #'(stuff ...))])
                                          (debug "Create list from ~a\n" #'(data ...))
                                          (racket-syntax (list data ...))))
                          (define lookup (with-syntax ([(data ...)
                                                        (parse-comma-expression #'(stuff ...))]
                                                       [current current])
                                           (racket-syntax (do-lookup current data ...))))
                          (if current
                            ;; (values (left current) stream)
                            (do-parse #'(rest ...) precedence left lookup)
                            (do-parse #'(rest ...) precedence left value))])]
                     ;; block of code
                     [body:honu-body
                       (if current
                        (values (left current) stream)
                        (do-parse #'(rest ...) precedence left #'body.result))] 
                     ;; expression or function application
                     [(#%parens args ...)
                      (debug "Maybe function call with ~a\n" #'(args ...))
                      (if current
                        ;; FIXME: 9000 is an arbitrary precedence level for
                        ;; function calls
                        (if (> precedence 9000)
                          (let ()
                            (debug 2 "higher precedence call ~a\n" current)
                            (define call (with-syntax ([current (left current)]
                                                       [(parsed-args ...)
                                                        (parse-comma-expression #'(args ...)) ])
                                           (racket-syntax (current parsed-args ...))))
                            (do-parse #'(rest ...) 9000 (lambda (x) x) call))
                          (let ()
                            (debug 2 "function call ~a\n" left)
                            (define call (with-syntax ([current current]
                                                       [(parsed-args ...)
                                                        (parse-comma-expression #'(args ...)) ])
                                           (debug "Parsed args ~a\n" #'(parsed-args ...))
                                           (racket-syntax (current parsed-args ...))))
                            (do-parse #'(rest ...) precedence left call)))
                        (let ()
                          (debug "inner expression ~a\n" #'(args ...))
                          (define-values (inner-expression unparsed) (parse #'(args ...)))
                          (when (not (empty-syntax? unparsed))
                            (error 'parse "expression had unparsed elements ~a" unparsed))
                          (do-parse #'(rest ...) precedence left inner-expression)))

                      #;
                      (do-parse #'(rest ...)
                                0
                                (lambda (x) x)
                                (left (with-syntax ([current current]
                                                    [(parsed-args ...)
                                                     (if (null? (syntax->list #'(args ...)))
                                                       '()
                                                       (list (parse #'(args ...))))])
                                        #'(current parsed-args ...))))
                      #;
                      (error 'parse "function call")]
                     #;
                     [else (if (not current)
                             (error 'what "dont know how to parse ~a" #'head)
                             (values (left current) stream))]
                     [else (error 'what "dont know how to parse ~a" #'head)])])])])))

  (define-values (parsed unparsed)
                 (do-parse input 0 (lambda (x) x) #f))
  (values ;; (parsed-syntax parsed)
          parsed
          unparsed))

(define (empty-syntax? what)
  (syntax-parse what
    [() #t]
    [else #f]))

(provide parse-one)
(define (parse-one code)
  (parse (strip-stops code)))

;; keep parsing some expression until only a parsed term remains
(define (parse-all-expression code)
  (define-values (parsed unparsed)
                 (parse code))
  (when (not (empty-syntax? unparsed))
    (raise-syntax-error 'parse-all-expression "expected no more syntax" code))
  (if (parsed-syntax? parsed)
    parsed
    (parse-all-expression parsed)))

(define (parse-all code)
  (let loop ([all '()]
             [code code])
    (define-values (parsed-original unparsed)
                   (parse (strip-stops code)))
    (define parsed (if (parsed-syntax? parsed-original)
                     parsed-original
                     (let-values ([(out rest)
                                  (parse parsed-original)])
                       (when (not (empty-syntax? rest))
                         (raise-syntax-error 'parse-all "expected no more syntax" parsed-original))
                       out)))
    (debug "Parsed ~a unparsed ~a all ~a\n"
           (if parsed (syntax->datum parsed) parsed)
           (if unparsed (syntax->datum unparsed) unparsed)
           all)
    (if (empty-syntax? unparsed)
      (with-syntax ([(use ...) (reverse (if parsed
                                          (cons parsed all)
                                          all))])
        (debug "Nothing left to parse. Use ~a\n" #'(use ...))
        ;; If multiple things then wrap inside a begin
        (syntax-parse #'(use ...)
          [(x z y ...)
           (emit-remark "Parsed all" #'(begin use ...))
           (racket-syntax (begin use ...))]
          [(x) (racket-syntax x)]))
      (loop (cons parsed all)
            unparsed))))

(provide parsed-things)
;; rest will be some subset of full
(define (parsed-things full rest)
  (define full-datum (syntax->datum full))
  (define rest-datum (syntax->datum rest))
  (- (length full-datum) (length rest-datum)))

(provide honu-expression)
(define-primitive-splicing-syntax-class (honu-expression)
  #:attributes (result)
  #:description "expression"
  (lambda (stx fail)
    (define context (gensym))
    (debug "[~a] honu expression syntax class on ~a\n" context stx)
    (if (or (stx-null? stx)
            #;
            (stopper? (stx-car stx)))
      (begin
        (debug "[~a] failed\n" context)
        (fail))
      (let ()
        (define-values (parsed unparsed)
                       (parse stx))
        (debug "[~a] expression parsed ~a\n" context (if parsed (syntax->datum parsed) parsed))
        (debug 2 "[~a] Parsed things ~a\n" context (parsed-things stx unparsed))
        (if (parsed-syntax? parsed)
          (list (parsed-things stx unparsed)
                parsed)
          (list (parsed-things stx unparsed)
                (parse-all parsed)))))))

(provide honu-expression-list)
(define-splicing-syntax-class (honu-expression-list)
  #:literal-sets (cruft)
  [pattern (~seq (~seq each:honu-expression (~optional honu-comma)) ...)
           #:with (each_result ...)
           #'(each.result ...)
           #;
           (with-syntax ([(each ...) (add-between (syntax->list #'(each.result ...)) #'honu-comma)])
             #'(each ...))
           ])

(provide honu-identifier)
(define-splicing-syntax-class honu-identifier
                              #:literal-sets (cruft)
  [pattern (~and (~not semicolon)
                 x:id) #:with result #'x])

(provide honu-number)
(define-splicing-syntax-class honu-number
                              #:literal-sets (cruft)
  [pattern x:number #:with result #'x])


(provide identifier-comma-list)
(define-splicing-syntax-class identifier-comma-list
                              #:literal-sets (cruft)
  [pattern (~seq (~seq name:id (~optional honu-comma) ...) ...)]) 

(provide honu-expression/comma)
(define-splicing-syntax-class honu-expression/comma
  [pattern (~seq x ...) #:with (result ...) (parse-comma-expression #'(x ...))])
