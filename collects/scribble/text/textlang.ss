#lang scheme/base

(require (for-syntax scheme/base syntax/kerncase)
         scheme/promise "../text.ss")

(provide (except-out (all-from-out scheme/base) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out scheme/promise "../text.ss"))

(begin-for-syntax
  (define definition-ids ; ids that don't require forcing
    (syntax->list #'(define-values define-syntaxes define-values-for-syntax
                     require provide #%require #%provide)))
  (define stoplist (append definition-ids (kernel-form-identifier-list)))
  (define (definition-id? id)
    (and (identifier? id)
         (ormap (lambda (i) (free-identifier=? id i)) definition-ids)))
  (define (newline-stx? stx)
    (let ([p (syntax-property stx 'scribble)])
      (and (pair? p) (eq? (car p) 'newline))))
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
    (group-by (lambda (stx)
                (let ([p (syntax-property stx 'scribble)])
                  (cond [(and (pair? p) (eq? (car p) 'newline)) '>]
                        [(eq? 'indentation p) '<]
                        [else #f])))
              stxs fun))
  #; ; tests for this
  (for-each
   (lambda (t)
     (let ([r (group-by (lambda (x)
                          (cond [(number? x) '<] [(symbol? x) '>] [else #f]))
                        (car t)
                        list)])
       (unless (equal? r (cadr t)) (printf "FAILURE: ~s -> ~s\n" (car t) r))))
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
  (let ([context (syntax-local-context)])
    (syntax-case stx ()
      [(this pre expr post)
       (let ([expr* (local-expand #'expr context stoplist)])
         (syntax-case expr* (begin)
           ;; perhaps we should dig inside for more pre/posts
           [(begin x ...) #'(begin pre (this x) ... post)]
           ;; dump pre/post
           [(id . rest) (definition-id? #'id) expr*]
           [_ #`(begin pre (output #,expr*) post)]))])))

(define-syntax (module-begin stx)
  (define (process-body body)
    (group-stxs
     (syntax->list body)
     (lambda (pre expr post)
       (if (not expr) ; no need to decorate these
         (with-syntax ([(x ...) (append pre post)]) #`(output '(x ...)))
         (with-syntax ([pre  (if (null? pre)  #'(begin) #`(output '#,pre))]
                       [post (if (null? post) #'(begin) #`(output '#,post))])
           #`(toplevel-decorate pre #,expr post))))))
  (syntax-case stx ()
    [(_ expr ...)
     ;; add a dummy define and throw it away, to get rid of initial newlines
     (with-syntax ([(_ expr ...) (process-body #'((define) expr ...))])
       #'(#%module-begin expr ...))]))
