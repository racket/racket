#lang racket/base
(require racket/extflonum
         "match.rkt"
         "quoted.rkt")

(provide convert-for-serialize)

;; Some quoted Racket values cannot be serialized and deserialized
;; automatically by Scheme: keywords (because they need to be interned
;; when reading code), strings and byte strings (ditto), regexps
;; (because they contain function pointers), etc.
;;
;; For those kinds of values, lift a construction of the quoted value
;; out and replace the use of a quoted value with a variable
;; reference. This lifting can interefere with optimizations, so only
;; lift as a last resort.

(define (convert-for-serialize bodys for-cify?)
  (define lifted-eq-constants (make-hasheq))
  (define lifted-equal-constants (make-hash))
  (define lift-bindings null)
  (define lifts-count 0)
  (define (add-lifted rhs)
    ;; FIXME: make sure these `id`s don't collide with anything
    (define id (string->symbol (format "q:~a" lifts-count)))
    (set! lifts-count (add1 lifts-count))
    (set! lift-bindings (cons (list id rhs) lift-bindings))
    id)
  (define new-bodys
    (for/list ([v (in-list bodys)])
      (cond
        [(convert-any? v for-cify?)
         (define (convert v)
           (match v
             [`(quote ,q)
              (cond
                [(lift-quoted? q for-cify?)
                 (make-construct q add-lifted lifted-eq-constants lifted-equal-constants for-cify?)]
                [else v])]
             [`(lambda ,formals ,body ...)
              `(lambda ,formals ,@(convert-function-body body))]
             [`(case-lambda [,formalss ,bodys ...] ...)
              `(case-lambda ,@(for/list ([formals (in-list formalss)]
                                         [body (in-list bodys)])
                                `[,formals ,@(convert-function-body body)]))]
             [`(define-values ,ids ,rhs)
              `(define-values ,ids ,(convert rhs))]
             [`(let-values ([,idss ,rhss] ...) ,bodys ...)
              `(let-values ,(for/list ([ids (in-list idss)]
                                       [rhs (in-list rhss)])
                              `[,ids ,(convert rhs)])
                 ,@(convert-body bodys))]
             [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
              `(letrec-values ,(for/list ([ids (in-list idss)]
                                          [rhs (in-list rhss)])
                                 `[,ids ,(convert rhs)])
                 ,@(convert-body bodys))]
             [`(if ,tst ,thn ,els)
              `(if ,(convert tst) ,(convert thn) ,(convert els))]
             [`(with-continuation-mark ,key ,val ,body)
              `(with-continuation-mark ,(convert key) ,(convert val) ,(convert body))]
             [`(begin ,exps ...)
              `(begin . ,(convert-body exps))]
             [`(begin0 ,exps ...)
              `(begin0 . ,(convert-body exps))]
             [`(set! ,id ,rhs)
              `(set! ,id ,(convert rhs))]
             [`(#%variable-reference) v]
             [`(#%variable-reference ,_) v]
             [`(,rator ,exps ...)
              `(,(convert rator) ,@(convert-body exps))]
             [`,_
              (cond
                [(and for-cify?
                      (not (symbol? v))
                      (lift-quoted? v for-cify?))
                 (convert `(quote ,v))]
                [else v])]))
         (define (convert-body body)
           (for/list ([e (in-list body)])
             (convert e)))
         (define (convert-function-body body)
           (if for-cify?
               ;; Detect the function-name pattern and avoid
               ;; mangling it:
               (match body
                 [`((begin (quote ,name) ,body . ,bodys))
                  `((begin (quote ,name) ,@(convert-body (cons body bodys))))]
                 [`,_ (convert-body body)])
               (convert-body body)))
         (convert v)]
        [else v])))
  (values new-bodys
          (reverse lift-bindings)))

;; v is a form or a list of forms
(define (convert-any? v for-cify?)
  (let convert-any? ([v v])
    (match v
      [`(quote ,q) (lift-quoted? q for-cify?)]
      [`(lambda ,formals ,body ...)
       (convert-any? body)]
      [`(case-lambda [,formalss ,bodys ...] ...)
       (convert-any? bodys)]
      [`(define-values ,ids ,rhs)
       (convert-any? rhs)]
      [`(let-values ([,idss ,rhss] ...) ,bodys ...)
       (or (convert-any? rhss)
           (convert-any? bodys))]
      [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
       (or (convert-any? rhss)
           (convert-any? bodys))]
      [`(if ,tst ,thn ,els)
       (or (convert-any? tst)
           (convert-any? thn)
           (convert-any? els))]
      [`(with-continuation-mark ,key ,val ,body)
       (or (convert-any? key)
           (convert-any? val)
           (convert-any? body))]
      [`(begin ,exps ...)
       (convert-any? exps)]
      [`(begin0 ,exps ...)
       (convert-any? exps)]
      [`(set! ,id ,rhs)
       (convert-any? rhs)]
      [`(#%variable-reference) #f]
      [`(#%variable-reference ,_) #f]
      [`(,exps ...)
       (for/or ([exp (in-list exps)])
         (convert-any? exp))]
      [`,_ (and for-cify?
                (not (symbol? v))
                (lift-quoted? v for-cify?))])))

;; Construct an expression to be lifted
(define (make-construct q add-lifted lifted-eq-constants lifted-equal-constants for-cify?)
  (define (quote? e) (and (pair? e) (eq? 'quote (car e))))
  (let make-construct ([q q])
    (define lifted-constants (if (or (string? q) (bytes? q))
                                 lifted-equal-constants
                                 lifted-eq-constants))
    (cond
      [(hash-ref lifted-constants q #f)
       => (lambda (id) id)]
      [else
       (define rhs
         (cond
           [(path? q) `(bytes->path ,(path->bytes q)
                                    ',(path-convention-type q))]
           [(regexp? q)
            `(,(if (pregexp? q) 'pregexp 'regexp) ,(object-name q))]
           [(byte-regexp? q)
            `(,(if (byte-pregexp? q) 'byte-pregexp 'byte-regexp) ,(object-name q))]
           [(keyword? q)
            `(string->keyword ,(keyword->string q))]
           [(hash? q)
            `(,(cond
                 [(hash-eq? q) 'hasheq]
                 [(hash-eqv? q) 'hasheqv]
                 [else 'hash])
              ,@(apply append
                       (for/list ([(k v) (in-hash q)])
                         (list (make-construct k)
                               (make-construct v)))))]
           [(string? q) `(datum-intern-literal ,q)]
           [(bytes? q) `(datum-intern-literal ,q)]
           [(pair? q)
            (if (list? q)
                (let ([args (map make-construct q)])
                  (if (andmap quote? args)
                      `(quote ,q)
                      `(list ,@(map make-construct q))))
                (let ([a (make-construct (car q))]
                      [d (make-construct (cdr q))])
                  (if (and (quote? a) (quote? d))
                      `(quote ,q)
                      `(cons ,a ,d))))]
           [(vector? q)
            (let ([args (map make-construct (vector->list q))])
              (if (and (andmap quote? args)
                       (not (impersonator? q)))
                  `(quote ,q)
                  `(vector ,@args)))]
           [(box? q)
            (let ([arg (make-construct (unbox q))])
              (if (and (quote? arg)
                       (not (impersonator? q)))
                  `(quote ,q)
                  `(box ,arg)))]
           [(prefab-struct-key q)
            => (lambda (key)
                 `(make-prefab-struct ',key ,@(map make-construct
                                                   (cdr (vector->list (struct->vector q))))))]
           [(extflonum? q)
            `(string->number ,(format "~a" q) 10 'read)]
           [else `(quote ,q)]))
       (cond
         [(and (quote? rhs)
               (or (not for-cify?)
                   (not (lift-quoted? (cadr rhs) #t))))
          rhs]
         [else
          (define id (add-lifted rhs))
          (hash-set! lifted-constants q id)
          id])])))
