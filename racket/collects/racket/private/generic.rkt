#lang racket/base
(require (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/stx
                     syntax/boundmap)
         "generic-methods.rkt"
         (only-in racket/private/arity arity-includes?))

(provide define-primitive-generics
         define-primitive-generics/derived
         define/generic
         raise-support-error
         (struct-out exn:fail:support))

(begin-for-syntax

  (define (keyword-stx? v)
    (keyword? (syntax->datum v)))

  (define (check-identifier! stx)
    (unless (identifier? stx)
      (wrong-syntax stx "expected an identifier")))

  (define (free-id-table) (make-free-identifier-mapping))
  (define (free-id-ref table id default)
    (free-identifier-mapping-get table id (lambda () default)))
  (define (free-id-set! table id value)
    (free-identifier-mapping-put! table id value)))

(define-syntax (define-primitive-generics/derived stx)
  (syntax-case stx ()
    [(_ original
        (self-name generic-name
                   property-name
                   accessor-name
                   predicate-name
                   supported-name)
        #:fast-defaults ([fast-pred fast-disp fast-defn ...] ...)
        #:defaults ([default-pred default-disp default-defn ...] ...)
        #:fallbacks [fallback-defn ...]
        #:derive-properties ([derived-prop derived-impl] ...)
        [method-name . method-signature]
        ...)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'generic-name)
       (check-identifier! #'predicate-name)
       (check-identifier! #'property-name)
       (check-identifier! #'accessor-name)
       (check-identifier! #'supported-name)
       (check-identifier! #'self-name)
       (define methods (syntax->list #'(method-name ...)))
       (for-each check-identifier! methods)

       (define n (length methods))
       (define method-indices (for/list ([i (in-range n)]) i))
       (define fast-preds (syntax->list #'(fast-pred ...)))
       (define default-preds (syntax->list #'(default-pred ...)))
       (define (generate-methods . ignore) (generate-temporaries methods))
       (define (transpose-methods names)
         (map cdr (apply map list methods names)))

       (define fasts-by-type (map generate-methods fast-preds))
       (define fasts-by-method (transpose-methods fasts-by-type))
       (define defaults-by-type (map generate-methods default-preds))
       (define defaults-by-method (transpose-methods defaults-by-type))

       (define/with-syntax size n)
       (define/with-syntax [method-index ...] method-indices)
       (define/with-syntax contract-str
         (format "~s" (syntax-e #'predicate-name)))

       (define/with-syntax (fast-pred-name ...)
         (generate-temporaries fast-preds))
       (define/with-syntax (fast-disp-name ...)
         (generate-temporaries #'(fast-disp ...)))
       (define/with-syntax (fast-disp-expr ...)
         (for/list ([stx (in-list (syntax->list #'(fast-disp ...)))]
                    [id (in-list (syntax->list #'(fast-pred-name ...)))])
           (if (eq? (syntax-e stx) '#:same)
               id
               stx)))

       (define/with-syntax (default-pred-name ...)
         (generate-temporaries default-preds))
       (define/with-syntax (default-disp-name ...)
         (generate-temporaries #'(default-disp ...)))
       (define/with-syntax (default-disp-expr ...)
         (for/list ([stx (in-list (syntax->list #'(default-disp ...)))]
                    [id (in-list (syntax->list #'(default-pred-name ...)))])
           (if (eq? (syntax-e stx) '#:same)
               id
               stx)))

       (define/with-syntax ([fast-by-method ...] ...) fasts-by-method)
       (define/with-syntax ([fast-by-type ...] ...) fasts-by-type)
       (define/with-syntax ([default-by-method ...] ...) defaults-by-method)
       (define/with-syntax ([default-by-type ...] ...) defaults-by-type)
       (define/with-syntax [fallback ...] (generate-methods))

       (define/with-syntax forward-declaration
         (if (eq? (syntax-local-context) 'top-level)
             #'(define-syntaxes (fast-pred-name ...
                                 fast-disp-name ...
                                 default-pred-name ...
                                 default-disp-name ...
                                 fast-by-method ... ...
                                 default-by-method ... ...
                                 fallback ...)
                 (values))
             #'(begin)))

       #'(begin
           (define-syntax generic-name
             (make-generic-info (quote-syntax generic-name)
                                (quote-syntax property-name)
                                (quote-syntax prop:pred)
                                (quote-syntax accessor-name)
                                (list (quote-syntax method-name) ...)
                                (list (quote-syntax method-name) ...)))
           (define (prop:guard x info)
             (unless (and (vector? x) (= (vector-length x) 'size))
               (raise-argument-error 'generic-name
                                     (format "expected a vector of length ~a"
                                             'size)
                                     x))
             (check-generic-method generic-name
                                   method-name
                                   method-signature
                                   (vector-ref x 'method-index)
                                   original)
             ...
             x)
           (define-values (property-name prop:pred accessor-name)
             (make-struct-type-property
              'generic-name
              prop:guard
              (list
               (cons derived-prop
                     (lambda (impl)
                       (let ([method-name (vector-ref impl 'method-index)] ...)
                         derived-impl)))
               ...)
              #t))
           forward-declaration
           (define (predicate-name self-name)
             (or (fast-pred-name self-name)
                 ...
                 (prop:pred self-name)
                 (default-pred-name self-name)
                 ...))
           (define (supported-name self-name . syms)
             (define (bad-sym sym)
               (raise-argument-error 'supported-name
                                     (format "~s" '(or/c 'method-name ...))
                                     sym))
             (cond
               [(fast-pred-name self-name)
                (for/and ([sym (in-list syms)])
                  (case sym
                    [(method-name) (procedure? fast-by-type)]
                    ...
                    [else (bad-sym sym)]))]
               ...
               [(prop:pred self-name)
                (define table (accessor-name self-name))
                (for/and ([sym (in-list syms)])
                  (case sym
                    [(method-name)
                     (procedure? (vector-ref table 'method-index))]
                    ...
                    [else (bad-sym sym)]))]
               [(default-pred-name self-name)
                (for/and ([sym (in-list syms)])
                  (case sym
                    [(method-name) (procedure? default-by-type)]
                    ...
                    [else (bad-sym sym)]))]
               ...
               [else (raise-argument-error 'supported-name
                                           'contract-str
                                           self-name)]))

           (define-generic-method
             method-name
             method-signature
             self-name
             (or (cond
                   [(fast-disp-name self-name) fast-by-method]
                   ...
                   [(prop:pred self-name)
                    (vector-ref (accessor-name self-name) 'method-index)]
                   [(default-disp-name self-name) default-by-method]
                   ...)
                 fallback)
             original)
           ...
           (define fast-pred-name fast-pred) ...
           (define fast-disp-name fast-disp-expr) ...
           (define default-pred-name default-pred) ...
           (define default-disp-name default-disp-expr) ...
           (define-values (fast-by-type ...)
             (generic-methods generic-name fast-defn ...))
           ...
           (define-values (default-by-type ...)
             (generic-methods generic-name default-defn ...))
           ...
           (define-values (fallback ...)
             (generic-methods generic-name fallback-defn ...))))]))

(define-syntax (define-primitive-generics stx)
  (syntax-case stx ()
    [(_ . args)
     #`(define-primitive-generics/derived #,stx . args)]))

(begin-for-syntax

  (define (method-formals/application name-stx proc-stx self-id sig-stx)

    (define (check-method-signature!)
      (define dup (check-duplicate-identifier ids))
      (when dup (wrong-syntax dup "duplicate method argument"))
      (for ([id (in-list non-req)]
            #:when (free-identifier=? id self-id))
        (wrong-syntax id
                      "the generic name must be used as ~a"
                      "a required, by-position argument"))
      (define matches
        (for/list ([id (in-list req)]
                   #:when (free-identifier=? id self-id))
          id))
      (unless (pair? matches)
        (wrong-syntax sig-stx
                      "did not find ~a \"~a\" among ~a to ~s"
                      "the generic name" (syntax-e self-id)
                      "the required, by-position arguments"
                      (syntax-e name-stx)))
      (when (pair? (cdr matches))
        (wrong-syntax (cadr matches)
                      "found ~a among the arguments to ~s"
                      "more than one occurrence of the generic name"
                      (syntax-e name-stx))))

    (define (method-formals)
      (define/with-syntax [req-name ...] req)
      (define/with-syntax [opt-name ...] opt)
      (define/with-syntax ([req-arg ...] ...) req-kw)
      (define/with-syntax ([opt-key opt-val] ...) opt-kw)
      (define/with-syntax ([opt-arg ...] ...)
        #'([opt-key [opt-val default-arg]] ...))
      (define/with-syntax tail (or rest '()))
      #'(req-name ...
         [opt-name default-arg] ...
         req-arg ... ...
         opt-arg ... ...
         . tail))

    (define (method-application)
      (define app-count (* (add1 (length opt)) (expt 2 (length opt-kw))))
      (if (<= app-count app-threshold)
          (by-position req opt rest
                       (lambda (pos tail)
                         (by-keyword req-kw opt-kw
                                     (lambda (keys vals)
                                       (make-application pos keys vals tail)))))
          (brute-force-application)))

    (define app-threshold 64)

    (define (brute-force-application)
      (define/with-syntax [r ...] req)
      (define/with-syntax [o ...] opt)
      (define/with-syntax ([key val] ...)
        (sort (append req-kw opt-kw) keyword<?
              #:key (compose syntax-e stx-car)))
      (define/with-syntax tail (if rest rest #'(quote ())))
      (define/with-syntax f proc-stx)
      (define/with-syntax [tmp.ks tmp.vs tmp.k tmp.v tmp.args tmp.arg]
        (generate-temporaries '(ks vs k v args arg)))
      #'(let ()
          (define-values (tmp.ks tmp.vs)
            (for/lists
                (tmp.ks tmp.vs)
                ([tmp.k (in-list '(key ...))]
                 [tmp.v (in-list (list val ...))]
                 #:unless (eq? tmp.v default-arg))
              (values tmp.k tmp.v)))
          (define tmp.args
            (for/list ([tmp.arg (in-list (list* o ... tail))]
                       #:unless (eq? tmp.arg default-arg))
              tmp.arg))
          (keyword-apply f tmp.ks tmp.vs r ... tmp.args)))

    (define (push lst x) (append lst (list x)))

    (define (by-position req opt tail make-app)
      (cond
        [tail #`(if (pair? #,tail)
                    #,(make-app (append req opt) tail)
                    #,(by-position req opt #f make-app))]
        [(null? opt) (make-app req tail)]
        [else
         (define/with-syntax arg (car opt))
         #`(if (eq? arg default-arg)
               #,(make-app req #f)
               #,(by-position (push req (car opt)) (cdr opt) tail make-app))]))

    (define (by-keyword req opt make-app)
      (cond
        [(null? opt) (make-app (map car req) (map cadr req))]
        [else
         (define/with-syntax arg (cadr (car opt)))
         #`(if (eq? arg default-arg)
               #,(by-keyword req (cdr opt) make-app)
               #,(by-keyword (push req (car opt)) (cdr opt) make-app))]))

    (define (make-application pos keys vals tail)
      (define/with-syntax f proc-stx)
      (define/with-syntax [arg ...] pos)
      (define/with-syntax ([kw ...] ...) (map list keys vals))
      (define/with-syntax x (generate-temporary 'x))
      (if tail
          (with-syntax ([rest tail])
            #'(apply f kw ... ... arg ... rest))
          #'(f kw ... ... arg ...)))

    (define-values (req req-kw opt opt-kw rest)
      (parse-method-signature sig-stx))
    (define req-kw-ids (map cadr req-kw))
    (define opt-kw-ids (map cadr opt-kw))
    (define rest-ids (if rest (list rest) '()))
    (define non-req (append opt req-kw-ids opt-kw-ids rest-ids))
    (define ids (append req non-req))

    (check-method-signature!)
    (list (method-formals)
          (method-application)))

  (define (parse-method-signature stx)
    (syntax-case stx ()
      [(kw [val] . args)
       (and (keyword-stx? #'kw) (identifier? #'val))
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values req req-kw opt (cons (list #'kw #'val) opt-kw) rest))]
      [(kw val . args)
       (and (keyword-stx? #'kw) (identifier? #'val))
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values req (cons (list #'kw #'val) req-kw) opt opt-kw rest))]
      [(kw other . args)
       (keyword-stx? #'kw)
       (wrong-syntax #'other
                     "expected required or optional identifier")]
      [(kw . args)
       (keyword-stx? #'kw)
       (wrong-syntax #'kw
                     "expected a required or optional identifier following ~s"
                     (syntax-e #'kw))]
      [([val] . args)
       (identifier? #'val)
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (when (pair? req)
           (wrong-syntax (car req)
                         "required arguments must precede optional arguments"))
         (values req req-kw (cons #'val opt) opt-kw rest))]
      [(val . args)
       (identifier? #'val)
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values (cons #'val req) req-kw opt opt-kw rest))]
      [(other . args)
       (wrong-syntax #'other
                     "expected a keyword or a required or optional identifier")]
      [rest (identifier? #'rest) (values '() '() '() '() #'rest)]
      [() (values '() '() '() '() #f)]
      [other
       (wrong-syntax #'other
                     "expected an identifier or an empty list")])))

(define default-arg
  (gensym 'default-arg))

       
;; Converts 0-based index to an ordinal string
;; 0 => 1st
;; 1 => 2nd
(define (pos->ord n)
  (define n/1base (add1 n))
  (string-append
   (number->string n/1base)
   (case n/1base
     [(11 12 13) "th"]
     [else (case (remainder n/1base 10)
             [(1) "st"]
             [(2) "nd"]
             [(3) "rd"]
             [else "th"])])))

(define-syntax (define-generic-method stx)
  (syntax-case stx ()
    [(_ method-name
        method-signature
        self-name
        proc
        original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'method-name)
       (check-identifier! #'self-name)
       (define/with-syntax proc-name (generate-temporary #'method-name))
       (define/with-syntax [method-formals method-apply]
         (method-formals/application #'method-name
                                     #'proc-name
                                     #'self-name
                                     #'method-signature))

       ; compute extra error info for default method
       (define-values (req req-kw opt opt-kw rest)
         (parse-method-signature #'method-signature))

       ;; (0-based) pos of the "self" argument, for error reporting purposes;
       ;; method signature already checked so self-name is in signature
       (define self-i
         (for/first
           ([(id i) (in-indexed req)] #:when (free-identifier=? id #'self-name))
           i))

       ; removes the lst element at (0-based) index pos
       (define (remove-at lst pos)
         (let loop ([i 0] [lst lst])
           (cond [(null? lst) '()]
                 [else (if (= i pos)
                           (cdr lst)
                           (cons (car lst) (loop (add1 i) (cdr lst))))])))
       (define (stx-drop-last stx)
         (reverse (cdr (reverse (syntax->list stx)))))
         
       (define/with-syntax (req-name ...) req)
       (define/with-syntax ((reqkw-key reqkw-val) ...) req-kw)
       (define/with-syntax (opt-name ...) opt)
       (define/with-syntax ((optkw-key optkw-val) ...) opt-kw)
       (define/with-syntax arg-labels/restargs ; labels for args
         #'((symbol->string 'req-name) ... 
            (string-append (symbol->string 'opt-name) " (optional)") ...
            (string-append "#:" (keyword->string 'reqkw-key)) ...
            (string-append "#:" (keyword->string 'optkw-key) " (optional)")  ...
            "rest args"))
       (define/with-syntax arg-labels ; drop restargs if none
         (if rest #'arg-labels/restargs (stx-drop-last #'arg-labels/restargs)))
       (define/with-syntax arg-vals/restargs ; arg values
         #`(req-name ... opt-name ... reqkw-val ... optkw-val ... #,rest))
       (define/with-syntax arg-vals ; drop restargs if none
         (if rest #'arg-vals/restargs (stx-drop-last #'arg-vals/restargs)))
       (define/with-syntax bad-arg ; only the bad arg
         (list-ref (stx->list #'arg-vals) self-i))
       (define/with-syntax other-arg-labels ; other arg labels
         (remove-at (stx->list #'arg-labels) self-i))
       (define/with-syntax other-args ; other args
         (remove-at (stx->list #'arg-vals) self-i))
       (define/with-syntax (other-labels+args ...) ; other args interleaved
         (apply append (stx-map list #'other-arg-labels #'other-args)))
       (define/with-syntax err-fmt-str
         (string-append "contract violation:\n"
                        "expected: ~a\n"
                        "given: ~v\n"
                        (if (null? (syntax->list #'other-arg-labels))
                            "argument position: ~a"
                            (string-append "argument position: ~a\n"
                                           "other arguments...:"))))
       (define/with-syntax contract-str
         (format "~s?" (syntax-e #'self-name)))
       (define/with-syntax self-i-stx self-i)
       
       #'(define (method-name . method-formals)
           (define proc-name proc)
           (when (void? proc-name)
             (raise-arguments-error
              'method-name
              (format err-fmt-str 'contract-str bad-arg (pos->ord self-i-stx))
              other-labels+args ...))
           (unless proc-name
             (raise-support-error 'method-name self-name))
           method-apply))]))

(struct exn:fail:support exn:fail [] #:transparent)

(define (raise-support-error name v)
  (raise
   (exn:fail:support
    (format "~a: not implemented for ~e" name v)
    (current-continuation-marks))))

(define-syntax (check-generic-method stx)
  (syntax-case stx ()
    [(check-generic-method
       generic-name
       method-name
       method-signature
       method-expr
       original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'generic-name)
       (check-identifier! #'method-name)
       (define-values (req req-kw opt opt-kw rest)
         (parse-method-signature #'method-signature))
       (define/with-syntax req-n (length req))
       (define/with-syntax opt-n (length opt))
       (define/with-syntax rest? (identifier? rest))
       (define/with-syntax [req-key ...]
         (sort (map car req-kw) keyword<? #:key syntax-e))
       (define/with-syntax [opt-key ...]
         (sort (map car opt-kw) keyword<? #:key syntax-e))
       #'(check-method 'generic-name
                       'method-name
                       method-expr
                       'req-n
                       'opt-n
                       'rest?
                       '(req-key ...)
                       '(opt-key ...)))]))

(define (check-method who what v req-n opt-n rest? req-kws opt-kws)
  (when v

    (unless (procedure? v)
      (define msg "generic method definition is not a function")
      (raise-arguments-error who msg (format "~s" what) v))

    (define (arity-error why)
      (define msg
        (format "generic method definition has an incorrect arity; ~a" why))
      (raise-arguments-error who msg (format "~s" what) v))

    (define arity (procedure-arity v))

    (cond
      [rest?
       (unless (arity-includes? arity (arity-at-least req-n))
         (arity-error
           (format "expected a procedure that accepts ~a or more arguments"
                   req-n)))]
      [(zero? opt-n)
       (unless (arity-includes? arity req-n)
         (arity-error (format "expected a procedure that accepts ~a ~a"
                              req-n
                              (if (= 1 req-n) "argument" "arguments"))))]
      [else
       (for ([i (in-range req-n (+ req-n opt-n 1))])
         (unless (arity-includes? arity i)
           (arity-error (format "~a ~a required ~a and up to ~a optional ~a"
                                "expected a procedure that accepts"
                                req-n
                                (if (= 1 req-n) "argument" "arguments")
                                opt-n
                                (if (= 1 opt-n) "argument" "arguments")))))])

    (define-values (v-req-kws v-opt-kws) (procedure-keywords v))

    (define (keyword-subset? xs ys)
      (cond
        [(null? xs) #t]
        [(null? ys) #f]
        [else
         (define x (car xs))
         (define y (car ys))
         (cond
           [(keyword<? x y) #f]
           [(keyword<? y x) (keyword-subset? xs (cdr ys))]
           [else (keyword-subset? (cdr xs) (cdr ys))])]))

    (unless (and (keyword-subset? v-req-kws req-kws)
                 (or (not v-opt-kws)
                     (and (keyword-subset? req-kws v-opt-kws)
                          (keyword-subset? opt-kws v-opt-kws))))
      (define r (keywords-message #t req-kws))
      (define o (keywords-message #f opt-kws))
      (arity-error (format "expected a procedure that accepts ~a~a" r o)))))

(define (keywords-message required? kws)
  (cond
    [(null? kws) (if required? "no required keyword arguments" "")]
    [(null? (cdr kws))
     (format "~athe ~a keyword argument ~s"
             (if required? "" " and ")
             (if required? "required" "optional")
             (car kws))]
    [(null? (cddr kws))
     (format "~athe ~a keyword arguments ~s and ~s"
             (if required? "" " and ")
             (if required? "required" "optional")
             (car kws)
             (cadr kws))]
    [else
     (define strs
       (let loop ([kws kws])
         (cond
           [(null? (cdr kws)) (list (format "and ~s" (car kws)))]
           [else (cons (format "~s, " (car kws)) (loop (cdr kws)))])))
     (format "~athe ~a keyword arguments ~a"
             (if required? "" " and ")
             (if required? "required" "optional")
             (apply string-append strs))]))
