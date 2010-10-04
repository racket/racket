#lang scheme

(provide define-keywords 
         ;; (define-keywords (name1:identifier ... spec:expr) ...)
         ;; constraint: the first name is the original name
         ;; and it is also the name of the field in the class
         function-with-arity expr-with-check except err 
         ->args
         ->kwds-in
         clauses-use-kwd)

(require 
 (for-template "syn-aux-aux.ss" 
               scheme
               (rename-in lang/prim (first-order->higher-order f2h))))

(require (for-syntax syntax/parse))
(define-syntax (define-keywords stx)
  (syntax-parse stx 
    [(define-keywords the-list (kw:identifier ... coerce:expr) ...)
     #'(begin
         (provide kw ...) ...
         (define-syntaxes (kw ...)
           (values (lambda (x)
                     (raise-syntax-error 'kw "used out of context" x))
                   ...))
         ...
         (define-for-syntax the-list
           (apply append 
                  (list 
                   (let* ([x (list (list #'kw ''kw) ...)]
                          [f (caar x)])
                     (map (lambda (x) 
                            (define clause-name (car x))
                            (define clause-spec (cadr x))
                            (list clause-name f (coerce clause-spec)))
                          x))
                   ...))))]))

#;
(define-syntax-rule
  (define-keywords the-list (kw coerce) ...)
  (begin
    (provide kw ...) 
    (define-syntax kw
      (lambda (x)
        (raise-syntax-error 'kw "used out of context" x)))
    ...
    (define-for-syntax the-list
      (list (list #'kw (coerce ''kw)) ...))))

#|
  transform the clauses into the initial arguments specification 
  for a new expression that instantiates the appropriate class
  
  ensure that the initial state (state0) is not in the shape of a clause

  ensure that all clauses mention only keywords specified in AllSpec or PartSpec
  move the contracts from AppSpecl and PartSpec to the clauses 
  
  run ->rec? over all used keywords to discover the presence of special clauses
  
  if anything fails, use the legal keyword to specialize the error message
|#
(define (->args tag stx state0 clauses AllSpec PartSpec ->rec? legal)
  (define msg (format "not a legal clause in a ~a description" legal))
  (define Spec (append AllSpec PartSpec))
  (define kwds (map (compose (curry datum->syntax stx) car) Spec))
  (define spec (clauses-use-kwd (syntax->list clauses) ->rec? msg kwds))
  (duplicates? tag spec)
  (not-a-clause tag stx state0 kwds)
  (map (lambda (x) 
         (define kw (car x))
         (define-values (key coercion)
           (let loop ([kwds kwds][Spec Spec])
             (if (free-identifier=? (car kwds) kw)
                 ;; -- the original keyword, which is also the init-field name
                 ;; -- the coercion that comes with it 
                 (values (cadar Spec) (caddar Spec))
                 (loop (cdr kwds) (cdr Spec)))))
         (list (mk-kwd key) (coercion (cdr x))))
       spec))

;; Syntax -> Syntax 
;; eventually: convert syntax to keyword
(define (mk-kwd key)
  (define key:id (symbol->string (syntax-e key)))
  (define key:wd (string->keyword key:id))
  ;  (displayln key:wd)
  key)

;; Symbol Syntax Syntax [Listof Kw] -> true
;; effect: if state0 looks like a clause, raise special error 
(define (not-a-clause tag stx state0 kwds)
  (syntax-case state0 ()
    [(kw . E) 
     ((->kwds-in kwds) #'kw) 
     (raise-syntax-error tag "missing initial state" stx)]
    [_ #t]))

;; Symbol [Listof kw] -> true
;; effect: raise syntax error about duplicated clause 
(define (duplicates? tag lox)
  (let duplicates? ([lox lox])
    (cond
      [(empty? lox) false]
      [else
       (let* ([f (caar lox)]
              [id (syntax-e f)]
              [x (memf (lambda (x) (free-identifier=? (car x) f)) (rest lox))])
         (if x 
             (raise-syntax-error tag (format "duplicate ~a clause" id) (cdar x))
             (duplicates? (rest lox))))])))

;; check whether rec? occurs, produce list of keywords 
(define (clauses-use-kwd stx:list ->rec? legal-clause kwds)
  (define kwd-in? (->kwds-in kwds))
  (define double (string-append legal-clause ", ~a has been redefined"))
  (map (lambda (stx)
         (syntax-case stx ()
           [(kw . E) (kwd-in? #'kw) (begin (->rec? #'kw #'E) (cons #'kw stx))]
           [(kw . E)
            (let ([kw (syntax-e #'kw)])
              (if (member kw (map syntax-e kwds))
                  (raise-syntax-error #f (format double kw) stx)
                  (raise-syntax-error #f legal-clause stx)))]
           [_ (raise-syntax-error #f legal-clause stx)]))
       stx:list))

;; [Listof SyntaxIdentifier] -> (Syntax -> Boolean)
(define (->kwds-in kwds)
  (lambda (k)
    (and (identifier? k) (for/or ([n kwds]) (free-identifier=? k n)))))

(define-syntax (expr-with-check stx)
  (syntax-case stx ()
    [(_ check> msg)
     #`(lambda (tag)
         (lambda (p)
           (syntax-case p ()
             [(_ x) #`(check> #,tag x)]
             [_ (err tag p msg)])))]))

(define-syntax function-with-arity 
  (syntax-rules (except)
    [(_ arity)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           [_ (err tag p)])))]
    [(_ arity except extra)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           extra
           [_ (err tag p)])))]))

(define (err spec p . xtras)
  (raise-syntax-error (cadr spec)
                      (if (null? xtras)
                          "illegal specification"
                          (string-append "illegal specification: " (car xtras)))
                      p))
