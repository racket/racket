;;; test-sets.scm  --  Jens Axel Søgaard

;;; Automatic test generator

(require (lib "list.ss")
         (lib "67.ss" "srfi"))

(define types
  '(
    ;(singleton    : elm         -> set)
    
    ;;(set          : elm ...     -> set)
    ;(set          : elm     -> set)
    
    ;(size         : set         -> integer)
    ;(elements     : set         -> (list elm))
    
    (union        : set set     -> set)
    (intersection : set set     -> set)
    (difference   : set set     -> set)
    
    ;(empty?       : set         -> boolean)
    
    ;(insert1      : elm set     -> set)
    
    ;(member?      : elm set     -> boolean)
    ;;(member       : elm set     -> (union elm #f))
    
    ;(subset?      : set set     -> boolean)
    ;(equal?       : set set     -> boolean)
    
    ;(insert       : elm set -> set)
    (insert       : elm set  -> set)
    
    ; (remove       : set elm ... -> set)
    (remove       : elm set -> set)
    ;(remove1      : elm set     -> set)
    ))

; build-base-level-expr : type -> expression
;   build a small "base" expression of a given type;
;   the base expressions are used by build-expr to
;   to build bigger expressions
(define (build-base-level-expr type)
  (case type
    [(set) `(insert* '(,(random 10) ,(random 10) ,(random 10)) (empty))]
    [(elm) (random 10)]
    [else  (error 'build-base-level-expr "no base level exprs known for ~a" type)]))

; build-expr : type level -> expression
;  return an expression that evaluates to a value of the type type.
(define (build-expr type level)
  (define (build-sub-expr type) (build-expr type (- level 1)))
  (cond
    [(or (= level 0)
         (empty? (operations/result-type type)))
     (build-base-level-expr type)]
    [else         
     (let* ((operation (random-element-of (operations/result-type type)))
            (arg-exprs (map build-sub-expr (operation-argument-types operation))))
       `(,operation ,@arg-exprs))]))

; operations : -> (list symbol)
;   return list of all known operations
(define (operations)
  (map car types))

; operation-member : symbol -> type
;   return the description for a given operation,
;   return #f for an unknown operation
(define (operation-member operation)
  (ormap (lambda (type) 
           (if (eq? (first type) operation) 
               type 
               #f))
         types))

; operation-type : symbol -> type
;   return the type for a given operation
;   e.g. (operation-type 'insert) ==> (set elm ... -> set)
(define (operation-type operation)
  (let ((type (operation-member operation)))
    (when (not type) (error 'operation-type "no type associated with ~a" operation))
    (rest (rest type))))

; before-arrow : arrow-type -> type
;   return the input types
;   e.g. (before-arrow '(a b -> c))  ==> (a b)
(define (before-arrow type)
  (when (not (member '-> type)) (error 'before-arrow "no arrow in type: ~a" type))
  (do ((type   type (cdr type))
       (before '()  (cons (car type) before)))
    ((eq? (first type) '->)
     (reverse! before))))

; after-arrow : arrow-type -> type
;   return the return-type
;   e.g. (after-arrow '(a b -> c))  ==>  c
(define (after-arrow type)
  (when (not (member '-> type)) (error 'after-arrow "no arrow in type: ~a" type))
  (first (rest (member '-> type))))

; operation-result-type : symbol -> type
;   return the result type of an operation
;   e.g. (operation-result-type 'insert)  ==>  set
(define (operation-result-type operation)
  (let ((type (operation-member operation)))
    (when (not type) (error 'operation-result-type "unknown operation: ~a" operation))
    (after-arrow type)))

; (operation-argument-types 'insert) ==>  (set elm)
(define (operation-argument-types operation)
  (let ((type (operation-member operation)))
    (when (not type) (error 'operation-argument-types "unknown operation: ~a" operation))
    (rest (rest (before-arrow type)))))

; (operations/result-type 'boolean)  ==>  (empty? member? subset? equal?)
(define (operations/result-type type)
  (filter (lambda (op)
            (equal? (operation-result-type op) type))
          (operations)))


; random-element-of : (list alpha) -> alpha
;   return a random element of the list
(define (random-element-of l)
  (list-ref l (random (length l))))


(define (test-expr expr namespaces compare)
  (define (test-module namespace)
    (parameterize ([current-namespace namespace])
      (eval expr)))
  (printf "~a~n" expr)
  (let ((results (map test-module namespaces)))
    (cond 
      [(not (andmap (lambda (r) (equal? (first results) r))
                    (rest results)))
       (error "FAILED:\n"
              expr "\n" results)]
      [else (printf "~a~n" results)])))

; test : integer (list string) compare-function -> 
;  build run cnt random expressions, and evaluate them
;  in the context of each module - complain if not
;  all modules agree on the value of an expression
(define (test cnt modules compare)
  (define (module->namespace module)
    (let ((n (make-namespace)))
      (parameterize ([current-namespace n])
        (namespace-require '(lib "list.ss"))
        (namespace-require module)
        (current-namespace))))
  (do ((i 0 (+ i 1))) 
    ((= i cnt) (void))
    (let ([namespaces (map module->namespace modules)])
      (test-expr `(mergesort (elements ,(build-expr 'set (random 8))) <) 
                 namespaces compare))))


(test 1000 '("../list-set.scm" "../red-black-tree-set.scm") default-compare)

