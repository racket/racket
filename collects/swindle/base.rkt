;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> The `base' module defines some basic low-level syntactic extensions to
;;> Racket.  It can be used by itself to get these extensions.

#lang mzscheme

(provide (all-from-except mzscheme
          #%module-begin #%top #%app define let let* letrec lambda
          keyword? keyword->string string->keyword))

;;>> (#%module-begin ...)
;;>   `base' is a language module -- it redefines `#%module-begin' to load
;;>   itself for syntax definitions.
(provide (rename module-begin~ #%module-begin))
(define-syntax (module-begin~ stx)
  (let ([e (if (syntax? stx) (syntax-e stx) stx)])
    (if (pair? e)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%plain-module-begin)
              (datum->syntax-object
               stx (list (quote-syntax require-for-syntax) 'swindle/base))
              (cdr e))
       stx)
      (raise-syntax-error #f "bad syntax" stx)))
  ;; This doesn't work anymore (from 203.4)
  ;; (syntax-rules ()
  ;;   [(_ . body) (#%plain-module-begin
  ;;                (require-for-syntax swindle/base) . body)])
  )

;;>> (#%top . id)
;;>   This special syntax is redefined to make keywords (symbols whose names
;;>   begin with a ":") evaluate to themselves.
(provide (rename top~ #%top))
(define-syntax (top~ stx)
  (syntax-case stx ()
    [(_ . x)
     (let ([s (syntax-e #'x)])
       (if (and (symbol? s)
                (not (eq? s '||))
                (eq? #\: (string-ref (symbol->string s) 0)))
         (syntax/loc stx (#%datum . x))
         (syntax/loc stx (#%top . x))))]))

;;>> (#%app ...)
;;>   Redefined so it is possible to apply using dot notation: `(foo x . y)'
;;>   is the same as `(apply foo x y)'.  This is possible only when the last
;;>   (dotted) element is an identifier.
(provide (rename app~ #%app))
(define-syntax (app~ stx)
  (syntax-case stx ()
    [(_ x ...) (syntax/loc stx (#%app x ...))]
    [(_ . x)
     (let loop ([s (syntax-e #'x)] [r '()])
       (cond [(list? s) (syntax/loc stx (#%app . x))]
             [(pair? s) (loop (cdr s) (cons (car s) r))]
             [else (let ([e (and (syntax? s) (syntax-e s))])
                     (if (or (null? e) (pair? e))
                       (loop e r)
                       (quasisyntax/loc stx
                         (#%app apply . #,(reverse (cons s r))))))]))]))

;; these are defined as normal bindings so code that uses this module can use
;; them, but for the syntax level of this module we need them too.
(define-for-syntax (keyword*? x)
  (and (symbol? x) (not (eq? x '||))
       (eq? (string-ref (symbol->string x) 0) #\:)))
(define-for-syntax (syntax-keyword? x)
  (keyword*? (if (syntax? x) (syntax-e x) x)))

;;>> (define id-or-list ...)
;;>   The standard `define' form is modified so defining :keywords is
;;>   forbidden, and if a list is used instead of an identifier name for a
;;>   function then a curried function is defined.
;;>     => (define (((plus x) y) z) (+ x y z))
;;>     => plus
;;>     #<procedure:plus>
;;>     => (plus 5)
;;>     #<procedure:plus:1>
;;>     => ((plus 5) 6)
;;>     #<procedure:plus:2>
;;>     => (((plus 5) 6) 7)
;;>     18
;;>   Note the names of intermediate functions.
;;>
;;>   In addition, the following form can be used to define multiple values:
;;>     => (define (values a b) (values 1 2))
(provide (rename define~ define))
(define-syntax (define~ stx)
  ;; simple version
  ;; (syntax-case stx ()
  ;;   [(_ (name arg ...) body ...)
  ;;    #`(define~ name (lambda~ (arg ...) body ...))]
  ;;   [(_ name body ...) #'(define name body ...)])
  ;; this version makes created closures have meaningful names
  ;; also -- forbid using :keyword identifiers
  ;; also -- make (define (values ...) ...) a shortcut for define-values (this
  ;;   is just a patch, a full solution should override `define-values', and
  ;;   also deal with `let...' and `let...-values' and lambda binders)
  ;; also -- if the syntax is top-level, then translate all defines into a
  ;;   define with (void) followed by a set! -- this is for the problem of
  ;;   defining something that is provided by some module, and re-binding a
  ;;   syntax
  (define top-level? (eq? 'top-level (syntax-local-context)))
  (syntax-case* stx (values)
                ;; compare symbols if at the top-level
                (if top-level?
                  (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
                  module-identifier=?)
    [(_ name expr) (identifier? #'name)
     (cond [(syntax-keyword? #'name)
            (raise-syntax-error #f "cannot redefine a keyword" stx #'name)]
           [top-level?
            (syntax/loc stx
              (begin (define-values (name) (void)) (set! name expr)))]
           [else
            (syntax/loc stx (define-values (name) expr))])]
    [(_ (values name ...) expr)
     (cond [(ormap (lambda (id) (and (syntax-keyword? id) id))
                   (syntax->list #'(name ...)))
            => (lambda (id)
                 (raise-syntax-error #f "cannot redefine a keyword" stx id))]
           [top-level?
            (syntax/loc stx
              (begin (define name (void)) ... (set!-values (name ...) expr)))]
           [else (syntax/loc stx (define-values (name ...) expr))])]
    [(_ names body0 body ...) (pair? (syntax-e #'names))
     (let loop ([s #'names] [args '()])
       (syntax-case s ()
         [(name . arg) (loop #'name (cons #'arg args))]
         [name
          (let ([sym (syntax-object->datum #'name)])
            (let loop ([i    (sub1 (length args))]
                       [as   (reverse (cdr args))]
                       [body #'(begin body0 body ...)])
              (if (zero? i)
                (cond [(syntax-keyword? #'name)
                       (raise-syntax-error
                        #f "cannot redefine a keyword" stx #'name)]
                      [top-level?
                       (quasisyntax/loc stx
                         (begin (define name (void))
                                (set! name (lambda~ #,(car args) #,body))))]
                      [else
                       (quasisyntax/loc stx
                         (define name (lambda~ #,(car args) #,body)))])
                (loop (sub1 i) (cdr as)
                      (syntax-property
                       (quasisyntax/loc stx (lambda~ #,(car as) #,body))
                       'inferred-name
                       (string->symbol (format "~a:~a" sym i)))))))]))]))

;;>> (let ([id-or-list ...] ...) ...)
;;>> (let* ([id-or-list ...] ...) ...)
;;>> (letrec ([id-or-list ...] ...) ...)
;;>   All standard forms of `let' are redefined so they can generate
;;>   functions using the same shortcut that `define' allows.  This includes
;;>   the above extension to the standard `define'.  For example:
;;>     => (let ([((f x) y) (+ x y)]) ((f 1) 2))
;;>     3
;;>   It also includes the `values' keyword in a similar way to `define'.
;;>   For example:
;;>     => (let ([(values i o) (make-pipe)]) i)
;;>     #<pipe-input-port>
(provide (rename let~ let) (rename let*~ let*) (rename letrec~ letrec))
(define-syntaxes (let~ let*~ letrec~)
  (let* ([process
          (lambda (stx var0 val0 . flat?)
            (syntax-case var0 (values)
              [(values var ...) (null? flat?) #`((var ...) . #,val0)]
              [_ (let loop ([var var0] [args '()])
                   (if (identifier? var)
                     (if (null? args)
                       (let ([val (syntax->list val0)])
                         (if (and (pair? val) (null? (cdr val)))
                           (list (if (null? flat?) (list var) var) (car val))
                           (raise-syntax-error
                            #f "bad binding" stx #`(#,var0 #,@val0))))
                       (let ([sym (syntax-e var)])
                         (let loop ([i   (sub1 (length args))]
                                    [as  (reverse args)]
                                    [val val0])
                           (if (< i 0)
                             (list (if (null? flat?) (list var) var)
                                   (car (syntax->list val)))
                             (loop (sub1 i) (cdr as)
                                   (let ([val #`((lambda~ #,(car as) #,@val))])
                                     (if (zero? i)
                                       val
                                       (syntax-property
                                        val 'inferred-name
                                        (if (zero? i)
                                          sym
                                          (string->symbol
                                           (format "~a:~a" sym i)))))))))))
                (syntax-case var ()
                  [(var . args1) (loop #'var (cons #'args1 args))])))]))]
         [mk-bindings
          (lambda (stx bindings . flat?)
            (syntax-case bindings ()
              [((var val more ...) ...)
               (datum->syntax-object
                #'bindings
                (map (lambda (x y) (apply process stx x y flat?))
                     (syntax->list #'(var ...))
                     (syntax->list #'((val more ...) ...)))
                #'bindings)]))]
         [mk-let
          (lambda (tag . lbl)
            (lambda (stx)
              (syntax-case stx ()
                [(_ label bindings body0 body ...)
                 (and (identifier? #'label) (pair? lbl))
                 (quasisyntax/loc stx
                   (#,(car lbl) label #,(mk-bindings stx #'bindings #t)
                    body0 body ...))]
                [(_ bindings body0 body ...)
                 (quasisyntax/loc stx
                   (#,tag #,(mk-bindings stx #'bindings) body0 body ...))])))])
    (values (mk-let #'let-values #'let)
            (mk-let #'let*-values)
            (mk-let #'letrec-values))))

;;>> (lambda formals body ...)
;;>   The standard `lambda' is extended with Lisp-like &-keywords in its
;;>   argument list.  This extension is available using the above short
;;>   syntax.  There is one important difference between these keywords and
;;>   Lisp: some &-keywords are used to access arguments that follow the
;;>   keyword part of the arguments.  This makes it possible to write
;;>   procedures that can be invoked as follows:
;;>     (f <required-args> <optional-args> <keyword-args> <additional-args>)
;;>   (Note: do not use more keywords after the <additional-args>!)
;;>
;;>   Available &-keywords are:
(provide (rename lambda~ lambda))
(define-syntax (lambda~ stx)
  (define (process-optional-arg o)
    (syntax-case o ()
      [(var default) (identifier? #'var) (list #'var #'default)]
      [(var) (identifier? #'var) (list #'var #'#f)]
      [var (identifier? #'var) (list #'var #'#f)]
      [var (raise-syntax-error #f "not a valid &optional spec" stx #'var)]))
  (define (process-keyword-arg k)
    (define (key var)
      (datum->syntax-object
       k
       (string->symbol
        (string-append ":" (symbol->string (syntax-object->datum var))))
       k k))
    (syntax-case k ()
      [(var key default)
       (and (identifier? #'var) (syntax-keyword? #'key))
       (list #'var #'key #'default)]
      [(var default) (identifier? #'var) (list #'var (key #'var) #'default)]
      [(var) (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (raise-syntax-error #f "not a valid &key spec" stx #'var)]))
  (syntax-case stx ()
    [(_ formals expr0 expr ...)
     (let ([vars       '()]
           [opts       '()]
           [keys       '()]
           [rest       #f]  ; keys and all (no optionals)
           [rest-keys  #f]  ; like the above, minus specified keys
           [body       #f]  ; stuff that follows all keywords
           [all-keys   #f]  ; all keys, excluding body
           [other-keys #f]) ; unprocessed keys, excluding body
       ;; relations:
       ;;   rest = (append all-keys body)
       ;;   rest-keys = (append other-keys body)
       (let loop ([state #f] [args #'formals])
         (syntax-case args ()
           [() #f]
           [(v . xs)
            (let* ([v #'v]
                   [k (if (symbol? v) v (and (identifier? v) (syntax-e v)))]
                   [x (and k (symbol->string k))])
              (cond
               ;; check &-keywords according to their name, so something like
               ;;  (let ([&rest 1]) (lambda (&rest r) ...))
               ;; works as expected
               [(and x (> (string-length x) 0) (eq? #\& (string-ref x 0)))
                (case k
;;>   * &optional, &opt, &opts: denote an optional argument, possibly with a
;;>     default value (if the variable is specified as `(var val)').
;;>       => ((lambda (x &optional y [z 3]) (list x y z)) 1)
;;>       (1 #f 3)
;;>       => ((lambda (x &optional y [z 3]) (list x y z)) 1 2 #f)
;;>       (1 2 #f)
                  [(&optional &optionals &opt &opts)
                   (if state
                     (raise-syntax-error
                      #f "misplaced &optional argument" stx #'formals)
                     (loop 'o #'xs))]
;;>   * &keys, &key: a keyword argument -- the variable should be specified
;;>     as `x' or `(x)' to be initialized by an `:x' keyword, `(x v)' to
;;>     specify a default value `v', and `(x k v)' to further specify an
;;>     arbitrary keyword `k'.
;;>       => ((lambda (&key x [y 2] [z :zz 3]) (list x y z)) :x 'x :zz 'z)
;;>       (x 2 z)
;;>     Note that keyword values take precedence on the left, and that
;;>     keywords are not verified:
;;>       => ((lambda (&key y) y) :y 1 :z 3 :y 2)
;;>       1
                  [(&key &keys)
                   (if (memq state '(#f o r!))
                     (loop 'k #'xs)
                     (raise-syntax-error
                      #f "misplaced &keys argument" stx #'formals))]
;;>   * &rest: a `rest' argument which behaves exactly like the Scheme dot
;;>     formal parameter (actually a synonym for it: can't use both).  Note
;;>     that in case of optional arguments, the rest variable holds any
;;>     arguments that were not used for defaults, but using keys doesn't
;;>     change its value.  For example:
;;>       => ((lambda (x &rest r) r) 1 2 3)
;;>       (2 3)
;;>       => ((lambda (x &optional y &rest r) r) 1)
;;>       ()
;;>       => ((lambda (x &optional y &rest r) r) 1 2 3)
;;>       (3)
;;>       => ((lambda (x &optional y . r) r) 1 2 3)
;;>       (3)
;;>       => ((lambda (x &key y &rest r) (list y r)) 1 :y 2 3 4)
;;>       (2 (:y 2 3 4))
;;>       => ((lambda (x &key y &rest r) (list y r)) 1 :y 2 3 4 5)
;;>       (2 (:y 2 3 4 5))
;;>     Note that the last two examples indicate that there is no error if
;;>     the given argument list is not balanced.
                  [(&rest)
                   (if (pair? (syntax-e #'xs))
                     (loop 'r #'xs)
                     (raise-syntax-error
                      #f "no name for &rest argument" stx #'formals))]
;;>   * &rest-keys: similar to `&rest', but all specified keys are removed
;;>     with their values.
;;>       => ((lambda (x &key y &rest r) r) 1 :x 2 :y 3)
;;>       (:x 2 :y 3)
;;>       => ((lambda (x &key y &rest-keys r) r) 1 :x 2 :y 3)
;;>       (:x 2)
                  [(&rest-keys)
                   (if (pair? (syntax-e #'xs))
                     (loop 'rk #'xs)
                     (raise-syntax-error
                      #f "no name for &rest-keys argument" stx #'formals))]
;;>   * &body: similar to `&rest-keys', but all key/values are removed one
;;>     by one until a non-key is encountered.  (Warning: this is *not* the
;;>     same as in Common Lisp!)
;;>       => ((lambda (x &key y &body r) r) 1 :x 2 :y 3)
;;>       ()
;;>       => ((lambda (x &key y &body r) r) 1 :x 2 :y 3 5 6)
;;>       (5 6)
                  [(&body &rest-all-keys) ; &rest-all-keys for compatibility
                   (if (pair? (syntax-e #'xs))
                     (loop 'b #'xs)
                     (raise-syntax-error
                      #f "no name for &body argument"
                      stx #'formals))]
;;>   * &all-keys: the list of all keys+vals, without a trailing body.
;;>       => ((lambda (&keys x y &all-keys r) r) :x 1 :z 2 3 4)
;;>       (:x 1 :z 2)
                  [(&all-keys)
                   (if (pair? (syntax-e #'xs))
                     (loop 'ak #'xs)
                     (raise-syntax-error
                      #f "no name for &all-keys argument"
                      stx #'formals))]
;;>   * &other-keys: the list of unprocessed keys+vals, without a trailing
;;>     body.
;;>       => ((lambda (&keys x y &other-keys r) r) :x 1 :z 2 3 4)
;;>       (:z 2)
                  [(&other-keys)
                   (if (pair? (syntax-e #'xs))
                     (loop 'ok #'xs)
                     (raise-syntax-error
                      #f "no name for &other-keys argument"
                      stx #'formals))]
;;>
;;>   Finally, here is an example where all &rest-like arguments are
;;>   different:
;;>     => ((lambda (&keys x y
;;>                  &rest r
;;>                  &rest-keys rk
;;>                  &body b
;;>                  &all-keys ak
;;>                  &other-keys ok)
;;>           (list r rk b ak ok))
;;>         :z 1 :x 2 2 3 4)
;;>     ((:z 1 :x 2 2 3 4) (:z 1 2 3 4) (2 3 4) (:z 1 :x 2) (:z 1))
;;>   Note that the following invariants hold:
;;>   * rest = (append all-keys body)
;;>   * rest-keys = (append other-keys body)
                  [else (raise-syntax-error
                         #f "unknown lambda &-keyword" stx v)])]
               [(not (or x (memq state '(o k))))
                (raise-syntax-error #f "not an identifier" stx v)]
               [else
                (let ([test (lambda (var name)
                              (if var
                                (raise-syntax-error
                                 #f (format "too many &~a arguments" name)
                                 stx #'formals)
                                (set! state 'r!)))])
                  (case state
                    [(#f) (set! vars (cons v vars))]
                    [(o)  (set! opts (cons v opts))]
                    [(k)  (set! keys (cons v keys))]
                    [(r!) (raise-syntax-error
                           #f "second identifier after a &rest or similar"
                           stx v)]
                    [(r)  (test rest       'rest      ) (set! rest v)]
                    [(rk) (test rest-keys  'rest-keys ) (set! rest-keys v)]
                    [(b)  (test body       'body      ) (set! body v)]
                    [(ak) (test all-keys   'all-keys  ) (set! all-keys v)]
                    [(ok) (test other-keys 'other-keys) (set! other-keys v)]
                    [else (raise-syntax-error #f "bad lambda formals" stx v)])
                  (loop state #'xs))]))]
           [v (loop state #'(&rest v))]))
       (set! vars (reverse vars))
       (set! opts (map process-optional-arg (reverse opts)))
       (set! keys (map process-keyword-arg  (reverse keys)))
       (when (and (or rest-keys body all-keys other-keys) (not rest))
         (set! rest #'rest))
       (cond
        ;; non-trivial case -- full processing
        [(or (pair? opts) (pair? keys) rest-keys body all-keys other-keys)
         (unless rest (set! rest #'rest))
         ;; other-keys is computed from all-keys
         (when (and other-keys (not all-keys)) (set! all-keys #'all-keys))
         (quasisyntax/loc stx
           (lambda (#,@vars . #,rest)
             (let*-values
                 (#,@(map (lambda (o)
                            #`[(#,(car o))
                               (if (pair? #,rest)
                                 (begin0 (car #,rest)
                                   (set! #,rest (cdr #,rest)))
                                 #,(cadr o))])
                          opts)
                  #,@(map (lambda (k)
                            #`[(#,(car k))
                               (getarg #,rest #,(cadr k)
                                       (lambda () #,(caddr k)))])
                          keys)
                  #,@(if rest-keys
                       #`([(#,rest-keys)
                           (filter-out-keys '#,(map cadr keys) #,rest)])
                       #'())
                  #,@(cond
                      ;; At most one scan for body, all-keys, other-keys.  This
                      ;; could be much shorter by always using keys/args, but a
                      ;; function call is not a place to spend time on.
                      [(and body all-keys)
                       #`([(#,all-keys #,body)
                           ;; inlined keys/args
                           (let loop ([args #,rest] [keys '()])
                             (cond [(or (null? args)
                                        (null? (cdr args))
                                        (not (keyword*? (car args))))
                                    (values (reverse keys) args)]
                                   [else (loop (cddr args)
                                               (list* (cadr args) (car args)
                                                      keys))]))])]
                      [body
                       #`([(#,body)
                           (let loop ([args #,rest])
                             (if (or (null? args)
                                     (null? (cdr args))
                                     (not (keyword*? (car args))))
                               args
                               (loop (cddr args))))])]
                      [all-keys
                       #`([(#,all-keys)
                           ;; inlined keys/args, not returning args
                           (let loop ([args #,rest] [keys '()])
                             (cond [(or (null? args)
                                        (null? (cdr args))
                                        (not (keyword*? (car args))))
                                    (reverse keys)]
                                   [else (loop (cddr args)
                                               (list* (cadr args) (car args)
                                                      keys))]))])]
                      [else #'()])
                  #,@(if other-keys
                       #`([(#,other-keys) ; use all-keys (see above)
                           (filter-out-keys '#,(map cadr keys) #,all-keys)])
                       #'()))
               expr0 expr ...)))]
        ;; common cases: no optional, keyword, or other fancy stuff
        [(null? vars)
         (quasisyntax/loc stx
           (lambda #,(or rest #'()) expr0 expr ...))]
        [else
         (quasisyntax/loc stx
           (lambda (#,@vars . #,(or rest #'())) expr0 expr ...))]))]))

;; Keyword utilities
(provide (rename keyword*? keyword?) syntax-keyword?
         (rename keyword->string* keyword->string)
         (rename string->keyword* string->keyword)
         ;; also provide the builtin as `real-keyword'
         (rename keyword? real-keyword?)
         (rename keyword->string real-keyword->string)
         (rename string->keyword string->real-keyword))
;;>> (keyword? x)
;;>   A predicate for keyword symbols (symbols that begin with a ":").
;;>   (Note: this is different from Racket's keywords!)
(define (keyword*? x)
  (and (symbol? x) (not (eq? x '||))
       (eq? (string-ref (symbol->string x) 0) #\:)))
;;>> (syntax-keyword? x)
;;>   Similar to `keyword?' but also works for an identifier (a syntax
;;>   object) that contains a keyword.
(define (syntax-keyword? x)
  (keyword*? (if (syntax? x) (syntax-e x) x)))
;;>> (keyword->string k)
;;>> (string->keyword s)
;;>   Convert a Swindle keyword to a string and back.
(define (keyword->string* k)
  (if (keyword*? k)
    (substring (symbol->string k) 1)
    (raise-type-error 'keyword->string "keyword" k)))
(define (string->keyword* s)
  (if (string? s)
    (string->symbol (string-append ":" s))
    (raise-type-error 'string->keyword "string" s)))

;; Keyword searching utilities (note: no errors for odd length)
(provide getarg syntax-getarg getargs keys/args filter-out-keys)
;;>> (getarg args keyword [not-found])
;;>   Searches the given list of arguments for a value matched with the
;;>   given keyword.  Similar to CL's `getf', except no error checking is
;;>   done for an unbalanced list.  In case no value is found, the optional
;;>   default value can be used -- this can be either a thunk, a promise, or
;;>   any other value that will be used as is.  For a repeated keyword the
;;>   leftmost occurrence is used.
(define (getarg args keyword . not-found)
  (let loop ([args args])
    (cond [(or (null? args) (null? (cdr args)))
           (and (pair? not-found)
                (let ([x (car not-found)])
                  (cond [(procedure? x) (x)]
                        [(promise? x) (force x)]
                        [else x])))]
          [(eq? (car args) keyword) (cadr args)]
          [else (loop (cddr args))])))
;;>> (syntax-getarg syntax-args keyword [not-found])
;;>   Similar to `getarg' above, but the input is a syntax object of a
;;>   keyword-value list.
(define (syntax-getarg syntax-args keyword . not-found)
  (when (syntax? keyword) (set! keyword (syntax-e keyword)))
  (let loop ([args syntax-args])
    (syntax-case args ()
      [(key arg . more)
       (if (eq? (syntax-e #'key) keyword) #'arg (loop #'more))]
      [_ (and (pair? not-found)
              (let ([x (car not-found)])
                (cond [(procedure? x) (x)]
                      [(promise? x) (force x)]
                      [else x])))])))
;;>> (getargs initargs keyword)
;;>   The same as `getarg' but return the list of all key values matched --
;;>   no need for a default value.  The result is in the same order as in
;;>   the input.
(define (getargs initargs keyword)
  (define (scan tail)
    (cond [(null? tail) '()]
          [(null? (cdr tail)) (error 'getargs "keyword list not balanced.")]
          [(eq? (car tail) keyword) (cons (cadr tail) (scan (cddr tail)))]
          [else (scan (cddr tail))]))
  (scan initargs))
;;>> (keys/args args)
;;>   The given argument list is scanned and split at the point where there
;;>   are no more keyword-values, and the two parts are returned as two
;;>   values.
;;>     => (keys/args '(:a 1 :b 2 3 4 5))
;;>     (:a 1 :b 2)
;;>     (3 4 5)
(define (keys/args args)
  (let loop ([args args] [keys '()])
    (cond [(or (null? args) (null? (cdr args)) (not (keyword*? (car args))))
           (values (reverse keys) args)]
          [else (loop (cddr args) (list* (cadr args) (car args) keys))])))
;;>> (filter-out-keys outs args)
;;>   The keywords specified in the outs argument, with their matching
;;>   values are filtered out of the second arguments.
(define (filter-out-keys outs args)
  (let loop ([as args] [r '()])
    (cond [(null? as) (reverse r)]
          [(null? (cdr as)) (reverse (cons (car as) r))]
          [else
           (loop (cddr as)
                 (if (memq (car as) outs) r (list* (cadr as) (car as) r)))])))
