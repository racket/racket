#|

incompatible changes to be done:

  - make contexts be bound to just the context to avoid a separate `hole' thingy

|#

(module reduction-semantics mzscheme
  (require "private/matcher.ss"
           "private/term.ss"
           (lib "contract.ss")
           (lib "etc.ss"))
  (require-for-syntax (lib "list.ss"))

  
  ;; type red = (make-red compiled-pat ((listof (cons sym tst) (union string #f)) -> any)
  (define-struct red (contractum reduct name))

  
  (provide reduction
           reduction/name
           reduction/context
           reduction/context/name
           language
           plug
	   compiled-lang?
           red?
           term
           term-let
           none?
           (rename red-name reduction->name))
  
  (provide match-pattern
           compile-pattern
           make-bindings bindings-table bindings?
           mtch? mtch-bindings mtch-context  mtch-hole
           make-rib rib? rib-name rib-exp)

  
  (provide/contract
   (language->predicate (compiled-lang? symbol? . -> . (any/c . -> . boolean?)))
   (reduce ((listof (lambda (x) (red? x))) any/c . -> . (listof any/c)))
   (reduce/tag-with-reduction ((listof (lambda (x) (red? x))) any/c . -> . (listof any/c)))
   (give-name ((λ (x) (red? x)) string? . -> . red?))
   (variable-not-in (any/c symbol? . -> . symbol?))
   (compatible-closure ((lambda (x) (red? x))
                        compiled-lang?
                        symbol?
                        . -> .
                        (lambda (x) (red? x))))
   (context-closure ((lambda (x) (red? x))
                     compiled-lang?
                     any/c
                     . -> .
                     (lambda (x) (red? x)))))

  

  ;; give-name : red (union string #f) -> red
  ;; gives the reduction the given name
  (define (give-name red name) (make-red (red-contractum red) (red-reduct red) name))
  
  ;; build-red : language pattern ((listof (cons sym tst)) -> any) (union string #f) -> red
  (define (build-red lang contractum reduct name)
    (make-red (compile-pattern lang contractum) reduct name))
  
  (define (compatible-closure red lang nt)
    (context-closure red lang `(cross ,nt)))
  
  (define (context-closure red lang pattern)
    (let ([new-name (gensym 'context-closure)])
      (make-red (compile-pattern
                 lang
                 `(in-hole (name ,new-name ,pattern)
                           ,(red-contractum red)))
                (lambda (bindings)
                  (let ([context (lookup-binding bindings new-name)]
                        [res ((red-reduct red) bindings)])
                    (plug context res)))
		#f)))
  
  (define-syntax-set (reduction/context reduction reduction/name reduction/context/name language)
    
    ;; (reduction/context lang ctxt pattern expression ...)
    (define (reduction/context/proc stx)
      (syntax-case stx ()
        [(_ lang-exp ctxt pattern bodies ...)
         (let-values ([(names names/ellipses) (extract-names (syntax pattern))])
	   (when (null? (syntax->list (syntax (bodies ...))))
	     (raise-syntax-error #f "missing result expression" stx))
           (with-syntax ([(names ...) names]
                         [(names/ellipses ...) names/ellipses]
                         [side-condition-rewritten (rewrite-side-conditions (syntax pattern))])
             (syntax 
              (build-red lang-exp
                         `(in-hole (name context ctxt) side-condition-rewritten)
                         (lambda (bindings)
                           (term-let ([context (lookup-binding bindings 'context)]
                                      [names/ellipses (lookup-binding bindings 'names)] ...)
                                     (plug
                                      (term context)
                                      (begin
                                        (void)
                                        bodies ...))))
                         #f))))]))
    
   (define (reduction/context/name/proc stx)
      (syntax-case stx ()
        [(_ name-exp lang-exp ctxt pattern bodies ...)
         #'(give-name (reduction/context lang-exp ctxt pattern bodies ...)
                      name-exp)]))
    
    
    ;; (reduction lang pattern expression ...)
    (define (reduction/proc stx)
      (syntax-case stx ()
        [(_ lang-exp pattern bodies ...)
         (let-values ([(names names/ellipses) (extract-names (syntax pattern))])
	   (when (null? (syntax->list (syntax (bodies ...))))
	     (raise-syntax-error #f "missing result expression" stx))
           (with-syntax ([(name-ellipses ...) names/ellipses]
                         [(name ...) names]
                         [side-condition-rewritten (rewrite-side-conditions (syntax pattern))])
             (syntax 
              (build-red lang-exp
                         `side-condition-rewritten
                         (lambda (bindings)
                           (term-let ([name-ellipses (lookup-binding bindings 'name)] ...)
                             bodies ...))
                         #f))))]))
    
    (define (reduction/name/proc stx)
      (syntax-case stx ()
        [(_ name-exp lang-exp pattern bodies ...)
         #`(give-name (reduction lang-exp pattern bodies ...) name-exp)]))
    
    (define (language/proc stx)
      (syntax-case stx ()
        [(_ (name rhs ...) ...)
         (andmap identifier? (syntax->list (syntax/loc stx (name ...))))
         (with-syntax ([((r-rhs ...) ...) (map (lambda (rhss) (map rewrite-side-conditions (syntax->list rhss)))
                                               (syntax->list (syntax ((rhs ...) ...))))])
           (syntax/loc stx
             (compile-language (list (make-nt 'name (list (make-rhs `r-rhs) ...)) ...))))]
        [(_ (name rhs ...) ...)
         (for-each
          (lambda (name)
            (unless (identifier? name)
              (raise-syntax-error 'language "expected name" stx name)))
          (syntax->list (syntax (name ...))))]
        [(_ x ...)
         (for-each
          (lambda (x)
            (syntax-case x ()
              [(name rhs ...)
               (void)]
              [_
               (raise-syntax-error 'language "malformed non-terminal" stx x)]))
          (syntax->list (syntax (x ...))))]))
    
    (define (rewrite-side-conditions orig-stx)
      (let loop ([term orig-stx])
        (syntax-case term (side-condition)
          [(side-condition pat exp)
           (let-values ([(names names/ellipses) (extract-names (syntax pat))])
             (with-syntax ([(name ...) names]
                           [(name/ellipses ...) names/ellipses])
               (syntax/loc term
                 (side-condition
                  pat
                  ,(lambda (bindings)
                     (term-let ([name/ellipses (lookup-binding bindings 'name)] ...)
                               exp))))))]
          [(terms ...)
           (map loop (syntax->list (syntax (terms ...))))]
          [else term])))

    (define-struct id/depth (id depth))
    
    ;; extract-names : syntax -> (values (listof syntax) (listof syntax[x | (x ...) | ((x ...) ...) | ...]))
    (define (extract-names orig-stx)
      (let* ([dups
              (let loop ([stx orig-stx]
                         [names null]
                         [depth 0])
                (syntax-case stx (name in-hole in-named-hole side-condition)
                  [(name sym pat)
                   (identifier? (syntax sym))
                   (loop (syntax pat) 
                         (cons (make-id/depth (syntax sym) depth) names)
                         depth)]
                  [(in-named-hole hlnm sym pat1 pat2)
                   (identifier? (syntax sym))
                   (loop (syntax pat1)
                         (loop (syntax pat2) names depth)
                         depth)]
                  [(in-hole pat1 pat2)
                   (loop (syntax pat1)
                         (loop (syntax pat2) names depth)
                         depth)]
                  [(side-condition pat e)
                   (loop (syntax pat) names depth)]
                  [(pat ...)
                   (let i-loop ([pats (syntax->list (syntax (pat ...)))]
                                [names names])
                     (cond
                       [(null? pats) names]
                       [else 
                        (if (or (null? (cdr pats))
                                (not (identifier? (cadr pats)))
                                (not (module-identifier=? (quote-syntax ...)
                                                          (cadr pats))))
                            (i-loop (cdr pats)
                                    (loop (car pats) names depth))
                            (i-loop (cdr pats)
                                    (loop (car pats) names (+ depth 1))))]))]
                  [x
                   (and (identifier? (syntax x))
                        (has-underscore? (syntax x)))
                   (cons (make-id/depth (syntax x) depth) names)]
                  [else names]))]
             [no-dups (filter-duplicates dups)])
        (values (map id/depth-id no-dups)
                (map build-dots no-dups))))
    
    ;; build-dots : id/depth -> syntax[x | (x ...) | ((x ...) ...) | ...]
    (define (build-dots id/depth)
      (let loop ([depth (id/depth-depth id/depth)])
        (cond
          [(zero? depth) (id/depth-id id/depth)]
          [else (with-syntax ([rest (loop (- depth 1))]
                              [dots (quote-syntax ...)])
                  (syntax (rest dots)))])))

    (define (has-underscore? x)
      (memq #\_ (string->list (symbol->string (syntax-e x)))))
    
    (define (filter-duplicates dups)
      (let loop ([dups dups])
        (cond
          [(null? dups) null]
          [else 
           (cons
            (car dups)
            (filter (lambda (x) 
                      (let ([same-id? (module-identifier=? (id/depth-id x)
                                                           (id/depth-id (car dups)))])
                        (when same-id?
                          (unless (equal? (id/depth-depth x)
                                          (id/depth-depth (car dups)))
                            (error 'reduction "found the same binder, ~s, at different depths, ~a and ~a"
                                   (syntax-object->datum (id/depth-id x))
                                   (id/depth-depth x)
                                   (id/depth-depth (car dups)))))
                        (not same-id?)))
                    (loop (cdr dups))))]))))
  
  (define (language->predicate lang id)
    (let ([p (compile-pattern lang id)])
      (lambda (x)
	(and (match-pattern p x) #t))))
    
  ;; reduce : (listof red) exp -> (listof exp)
  (define (reduce reductions exp)
    (reduce/internal reductions exp (λ (red) (λ (mtch) ((red-reduct red) (mtch-bindings mtch))))))
  
  ; reduce/tag-with-reductions : (listof red) exp -> (listof (list red exp))
  (define (reduce/tag-with-reduction reductions exp)
    (reduce/internal reductions exp (λ (red) (λ (mtch) (list red ((red-reduct red) (mtch-bindings mtch)))))))
  
  ; reduce/internal : (listof red) exp (red -> match -> X) -> listof X
  (define (reduce/internal reductions exp f)
    (let loop ([reductions reductions]
               [acc null])
      (cond
        [(null? reductions) acc]
        [else (let ([red (car reductions)])
                (let ([mtchs (match-pattern (red-contractum red) exp)])
                  (if mtchs
                      (loop (cdr reductions)
                            (map/mt
                             (f red) 
                             mtchs
                             acc))
                      (loop (cdr reductions) acc))))])))
  
  ;; map/mt : (a -> b) (listof a) (listof b) -> (listof b)
  ;; map/mt is like map, except it uses the last argument
  ;; instaed of the empty list
  (define (map/mt f l mt-l)
    (let loop ([l l])
      (cond
        [(null? l) mt-l]
        [else (cons (f (car l)) (loop (cdr l)))])))
    
  (define re:gen-d #rx".*[^0-9]([0-9]+)$")
  (define (variable-not-in sexp var)
    (let* ([var-str (symbol->string var)]
           [nums (let loop ([sexp sexp]
                            [nums null])
                   (cond
                     [(pair? sexp) (loop (cdr sexp) (loop (car sexp) nums))]
                     [(symbol? sexp) (let* ([str (symbol->string sexp)]
                                            [match (regexp-match re:gen-d str)])
                                       (if (and match
                                                (is-prefix? var-str str))
                                           (cons (string->number (cadr match)) nums)
                                           nums))]
                     [else nums]))])
      (if (null? nums)
          (string->symbol (format "~a1" var))
          (string->symbol (format "~a~a" var (+ 1 (apply max nums)))))))
  
  (define (is-prefix? str1 str2)
    (and (<= (string-length str1) (string-length str2))
         (equal? str1 (substring str2 0 (string-length str1))))))
