#lang scheme/base

(require "output.rkt" (for-syntax scheme/base syntax/kerncase))

(provide module-begin/text begin/text include/text begin/collect
         process-begin/text)

(begin-for-syntax
  (define definition-ids ; ids that don't require forcing
    (syntax->list #'(define-values define-syntaxes begin-for-syntax
                     require provide #%require #%provide)))
  (define stoplist (append definition-ids (kernel-form-identifier-list)))
  (define (definition-id? id)
    (and (identifier? id)
         (ormap (λ (i) (free-identifier=? id i)) definition-ids)))
  (define (definition? x)
    (syntax-case x () [(id . rest) (and (definition-id? #'id) #'id)] [_ #f]))
  (define (begin?->list x)
    (syntax-case x (begin) [(begin x ...) (syntax->list #'(x ...))] [_ #f]))
  ;; This function is used to group a syntax list into triplets of consecutive
  ;; scribble indentation syntaxes, an input expression, and scribble newlines.
  ;; It is used to ignore indentations before a definition and newlines after
  ;; it.  See the following test cases for how it works.
  (define (group-by pred? xs fun)
    (let loop ([xs xs] [before '()] [cur #f] [after '()] [r '()])
      (define (add) (cons (fun (reverse before) cur (reverse after)) r))
      (if (null? xs)
        (reverse (if (or cur (pair? before) (pair? after)) (add) r))
        (let* ([x (car xs)] [xs (cdr xs)] [p (pred? x)])
          (cond [(eq? '> p) (loop xs before cur (cons x after) r)]
                [(eq? '< p) (if (or cur (pair? after))
                              (loop xs (list x) #f '() (add))
                              (loop xs (cons x before) cur after r))]
                [(or cur (pair? after)) (loop xs '() x '() (add))]
                [else (loop xs before x '() r)])))))
  (define (group-stxs stxs fun)
    (group-by (λ (stx)
                (define p (syntax-property stx 'scribble))
                (cond [(and (pair? p) (eq? (car p) 'newline)) '>]
                      [(eq? 'indentation p) '<]
                      [else #f]))
              stxs fun))
  #; ; tests for this
  (for-each
   (λ (t)
     (define r (group-by (λ (x)
                           (cond [(number? x) '<] [(symbol? x) '>] [else #f]))
                         (car t)
                         list))
     (unless (equal? r (cadr t)) (printf "FAILURE: ~s -> ~s\n" (car t) r)))
   '([() ()]
     [("a") ((() "a" ()))]
     [("a" "b") ((() "a" ()) (() "b" ()))]
     [(1 "a" x) (((1) "a" (x)))]
     [(1 2 3 "a" x y z) (((1 2 3) "a" (x y z)))]
     [(1 2 3 "a" "b" x y z) (((1 2 3) "a" ()) (() "b" (x y z)))]
     [(1 2 "a" x 3 "b" y z) (((1 2) "a" (x)) ((3) "b" (y z)))]
     [(1 2 "a" 3 "b" y z) (((1 2) "a" ()) ((3) "b" (y z)))]
     [(1 2 "a" 3 x "b" y z) (((1 2) "a" ()) ((3) #f (x)) (() "b" (y z)))]
     [(1 2 "a" 3 4 x "b" y z) (((1 2) "a" ()) ((3 4) #f (x)) (() "b" (y z)))]
     [(1 2 "a" 3 w x "b" y z) (((1 2) "a" ()) ((3) #f (w x)) (() "b" (y z)))]
     [(1) (((1) #f ()))]
     [(x) ((() #f (x)))]
     [(1 2 3) (((1 2 3) #f ()))]
     [(x y z) ((() #f (x y z)))]
     [(1 2 3 x y z) (((1 2 3) #f (x y z)))]
     [(1 x 2 y 3 z) (((1) #f (x)) ((2) #f (y)) ((3) #f (z)))]
     [(1 x y 2 3 z) (((1) #f (x y)) ((2 3) #f (z)))]
     [(1 2 x 3) (((1 2) #f (x)) ((3) #f ()))]
     [(w x 3 y z) ((() #f (w x)) ((3) #f (y z)))])))

(define-syntax (toplevel-decorate stx)
  (define context (syntax-local-context))
  (syntax-case stx ()
    [(this decor (pre ...) expr (post ...))
     (let ([expr* (local-expand #'expr context stoplist)])
       (define pre?  (not (null? (syntax-e #'(pre ...)))))
       (define post? (not (null? (syntax-e #'(post ...)))))
       (define (wrap expr)
         (if (or pre? post?)
           #`(begin #,@(if pre?  #'((decor 'pre)  ...) #'())
                    #,expr
                    #,@(if post? #'((decor 'post) ...) #'()))
           expr))
       (cond [(begin?->list expr*)
              => (λ (xs)
                   (if (null? xs)
                     (if (or pre? post?)
                       #'(begin (decor 'pre) ... (decor 'post) ...)
                       expr*)
                     #`(process-begin/text begin decor
                                           pre ... #,@xs post ...)))]
             [(definition? expr*) expr*] ; dump pre/post
             [else (wrap #`(decor #,expr*))]))]))

(define-syntax (process-begin/text stx)
  (define (process-body decor body)
    (group-stxs
     (syntax->list body)
     (λ (pre expr post)
       (with-syntax ([decor decor])
         (if (not expr) ; no need to decorate these
           (with-syntax ([(x ...) (append pre post)]) #`(decor '(x ...)))
           (with-syntax ([pre  pre]
                         [post post])
             #`(toplevel-decorate decor pre #,expr post)))))))
  (syntax-case stx ()
    [(_ beginner decor expr ...)
     ;; add a dummy define and throw it away, to get rid of initial newlines
     (with-syntax ([(_ expr ...) (process-body #'decor #'((define) expr ...))])
       #'(beginner expr ...))]))

;; module-begin for text files
(define-syntax-rule (module-begin/text expr ...)
  (#%plain-module-begin
   (port-count-lines! (current-output-port))
   (process-begin/text begin output expr ...)))

;; `begin'-like utility that allows definitions and collects values
(define-for-syntax (split-collect-body exprs ctx)
  (let loop ([exprs exprs]      ; expressions to scan
             [ds '()] [es '()]) ; collected definitions and expressions
    (if (null? exprs)
      (values (reverse ds) (reverse es) '())
      (let ([expr* (local-expand (car exprs) ctx stoplist (car ctx))])
        (syntax-case expr* (begin define-syntaxes define-values)
          [(begin x ...)
           (loop (append (syntax->list #'(x ...)) (cdr exprs)) ds es)]
          [(define-syntaxes (id ...) rhs)
           (andmap identifier? (syntax->list #'(id ...)))
           (if (null? es)
             (let ([ids (syntax->list #'(id ...))])
               (syntax-local-bind-syntaxes
                ids (local-transformer-expand #'rhs 'expression '()) (car ctx))
               (loop (cdr exprs) (cons expr* ds) es))
             ;; return the unexpanded expr, to be re-expanded later, in the
             ;; right contexts
             (values (reverse ds) (reverse es) exprs))]
          [(define-values (id ...) rhs)
           (andmap identifier? (syntax->list #'(id ...)))
           (if (null? es)
             (begin (syntax-local-bind-syntaxes
                     (syntax->list #'(id ...)) #f (car ctx))
                    (loop (cdr exprs) (cons expr* ds) es))
             ;; same note here
             (values (reverse ds) (reverse es) exprs))]
          [_ (loop (cdr exprs) ds (cons expr* es))])))))
(define-syntax (begin/collect* stx) ; helper, has a boolean flag first
  (define-values [exprs always-list?]
    (let ([exprs (syntax->list stx)])
      (if (and (pair? exprs) (pair? (cdr exprs)))
        (values (cddr exprs) (syntax-e (cadr exprs)))
        (raise-syntax-error #f "bad syntax" stx))))
  (define context
    (cons (syntax-local-make-definition-context)
          (let ([old (syntax-local-context)]) (if (list? old) old '()))))
  (define-values (defns nondefns rest) (split-collect-body exprs context))
  (define body
    (cond [(pair? rest) #`(list* #,@nondefns (begin/collect* #t #,@rest))]
          [(and (not always-list?) (= 1 (length nondefns))) (car nondefns)]
          [else #`(list #,@nondefns)]))
  (begin0
   (local-expand (if (null? defns) body #`(let () #,@defns #,body))
                 context stoplist (car context))
   (internal-definition-context-seal (car context))))
(define-syntax-rule (begin/collect x ...) (begin/collect* #f x ...))

;; begin for templates (allowing definition blocks)
(define-syntax (begin/text stx)
  (syntax-case stx ()
    [(begin/text expr ...)
     #'(process-begin/text begin/collect begin expr ...)]))

;; include for templates
(require (for-syntax scheme/base (prefix-in scribble: "../reader.rkt"))
         scheme/include)
(define-syntax-rule (include/text path-spec)
  (begin/text
   (include-at/relative-to/reader path-spec path-spec path-spec
     (let ([xs #f])
       (λ (src inp)
         (unless xs
           (set! xs (scribble:read-syntax-inside src inp))
           (when (syntax? xs) (set! xs (or (syntax->list xs) (list xs)))))
         (if (null? xs)
           eof
           (let ([x (car xs)])
             (set! xs (cdr xs))
             (if (and (null? xs)
                      (let ([p (syntax-property x 'scribble)])
                        (and (pair? p) (eq? (car p) 'newline))))
               eof ; throw away the last newline from the included file
               x))))))))
