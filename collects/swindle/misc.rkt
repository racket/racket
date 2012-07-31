;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> A lot of miscellaneous functionality that is needed for Swindle, or
;;> useful by itself.

#lang s-exp swindle/base

(require mzlib/list)   (provide (all-from mzlib/list))
(require mzlib/etc)    (provide (all-from mzlib/etc))
(require mzlib/string) (provide (all-from mzlib/string))

;; these are needed to make regexp-case work in scheme/base too
(require (rename scheme/base base-else else) (rename scheme/base base-=> =>))

;; ----------------------------------------------------------------------------
;;>>... Convenient syntax definitions

;;>> (define* ...)
;;>   Like `define', except that the defined identifier is automatically
;;>   `provide'd.  Doesn't provide the identifier if outside of a module
;;>   context.
(provide define*)
(define-syntax (define* stx)
  (syntax-case stx ()
    [(_ x . xs)
     (memq (syntax-local-context) '(module module-begin))
     (let ([name (let loop ([x #'x])
                   (syntax-case x () [(x . xs) (loop #'x)] [_ x]))])
       (if name
         #`(begin (provide #,name) (define x . xs))
         #`(define x . xs)))]
    [(_ x . xs) #`(define x . xs)]))
;;>> (make-provide-syntax orig-def-syntax provide-def-syntax)
;;>   Creates `provide-def-syntax' as a syntax that is the same as
;;>   `orig-def-syntax' together with an automatic `provide' form for the
;;>   defined symbol, which should be either the first argument or the first
;;>   identifier in a list (it does not work for recursive nesting).  The
;;>   `provide' form is added only if the form appears at a module
;;>   top-level.  The convention when this is used is to use a "*" suffix
;;>   for the second identifier.
(provide make-provide-syntax)
(define-syntax make-provide-syntax
  (syntax-rules ()
    [(_ form form*)
     (define-syntax (form* stx)
       (syntax-case stx ()
         [(_ (id . as) . r)
          (memq (syntax-local-context) '(module module-begin))
          #'(begin (provide id) (form (id . as) . r))]
         [(_ id . r)
          (memq (syntax-local-context) '(module module-begin))
          #'(begin (provide id) (form id . r))]
         [(_ . r) #'(form . r)]))]))
;;>> (define-syntax* ...)
;;>   Defined as the auto-provide form of `define-syntax'.
(provide define-syntax*)
(make-provide-syntax define-syntax define-syntax*)

;;>> (defsyntax ...)
;;>> (defsyntax* ...)
;;>> (letsyntax (local-syntaxes ...) ...)
;;>   These are just shorthands for `define-syntax', `define-syntax*', and
;;>   `let-syntax'.  This naming scheme is consistent with other definitions
;;>   in this module (and the rest of Swindle).
(define-syntax* defsyntax
  (syntax-rules () [(_ . args) (define-syntax . args)]))
(make-provide-syntax defsyntax defsyntax*) (provide defsyntax*)
(define-syntax* letsyntax
  (syntax-rules () [(_ . args) (let-syntax . args)]))

;;>> (defsubst name body)
;;>> (defsubst* name body)
;;>> (letsubst ([name body] ...) letbody ...)
;;>   These are convenient ways of defining simple pattern transformer
;;>   syntaxes (simple meaning they're much like inlined functions).  In
;;>   each of these forms, the `name' can be either a `(name arg ...)' which
;;>   will define a simple macro or an identifier which will define a
;;>   symbol-macro.  For example:
;;>     => (defsubst (my-if cond then else)
;;>          (if (and cond (not (eq? 0 cond))) then else))
;;>     => (my-if 1 (echo 2) (echo 3))
;;>     2
;;>     => (my-if 0 (echo 2) (echo 3))
;;>     3
;;>     => (define x (list 1 2 3))
;;>     => (defsubst car-x (car x))
;;>     => car-x
;;>     1
;;>     => (set! car-x 11)
;;>     => x
;;>     (11 2 3)
;;>   Actually, if a `(name arg ...)' is used, then the body can have more
;;>   pattern/expansions following -- but since this form translates to a
;;>   usage of `syntax-rules', the `name' identifier should normally be `_'
;;>   in subsequent patterns.  For example:
;;>     => (defsubst (my-if cond then else)
;;>                    (if (and cond (not (eq? 0 cond))) then else)
;;>                  (_ cond then)
;;>                    (and cond (not (eq? 0 cond)) then))
;;>     => (my-if 0 1)
;;>     #f
;;>   Finally, note that since these are just patterns that get handled by
;;>   syntax-rules, all the usual pattern stuff applies, like using `...'.

(defsyntax defsubst-process
  (syntax-rules ()
    [(_ name (acc ...)) (define-syntax name (syntax-rules () acc ...))]
    [(_ name (acc ...) n+a subst . more)
     (defsubst-process name (acc ... (n+a subst)) . more)]))
(defsyntax* defsubst
  (syntax-rules ()
    [(_ (name . args) subst)
     (define-syntax name
       (syntax-rules () [(name . args) subst]))]
    [(_ (name . args) subst . more)
     (defsubst-process name () (name . args) subst . more)]
    [(_ name subst)
     (define-syntax (name stx)
       (syntax-case stx () ; syntax-rules won't handle identifier expansion
         ;; doesn't matter here, but see `letsubst' for an explanation on `___'
         [(___ . args) (syntax/loc stx (subst . args))]
         [___          (syntax/loc stx subst)]))]))
(make-provide-syntax defsubst defsubst*) (provide defsubst*)

;; a let version of the above
(defsyntax* (letsubst stx)
  (syntax-case stx ()
    [(_ ([name body] ...) letbody ...)
     (quasisyntax/loc stx
       (let-syntax
           #,(map
              (lambda (name body)
                ;; use `___' in the following, if we use `name', then it would
                ;; not be possible to make an X subst that expand to something
                ;; with the previous X, so (let ([x 1]) (letsubst ([x x]) x))
                ;; will loop forever instead of returning 1.
                (syntax-case name ()
                  [(name . args)
                   (quasisyntax/loc body
                     (name (syntax-rules () [(___ . args) #,body])))]
                  [name (identifier? #'name)
                   (quasisyntax/loc body
                     (name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(___ . args) (syntax/loc stx (#,body . args))]
                          [___          (syntax/loc stx #,body)]))))]))
              (syntax-e #'(name ...)) (syntax-e #'(body ...)))
         letbody ...))]))

;;>> (defmacro name body)
;;>> (defmacro* name body)
;;>> (letmacro ([name body] ...) letbody ...)
;;>   These are just like Racket's define-macro (from mzlib/defmacro) with
;;>   two major extensions:
;;>   * If `name' is a simple identifier then a symbol-macro is defined (as
;;>     with `defsubst' above).
;;>   * A `letmacro' form for local macros is provided.

(require (for-syntax (submod compatibility/defmacro dmhelp)))
(provide defmacro letmacro)
(define-syntaxes (defmacro letmacro)
  (let ()
    (define (syntax-null? x)
      (or (null? x) (and (syntax? x) (null? (syntax-e x)))))
    (define (syntax-pair? x)
      (or (pair? x) (and (syntax? x) (pair? (syntax-e x)))))
    (define (syntax-car x)   (if (pair? x) (car x) (car (syntax-e x))))
    (define (syntax-cdr x)   (if (pair? x) (cdr x) (cdr (syntax-e x))))
    (define (check-args stx name args)
      (unless (identifier? name)
        (raise-syntax-error
         #f "expected an identifier for the macro name" stx name))
      (let loop ([args args])
        (cond [(syntax-null? args) 'ok]
              [(identifier? args) 'ok]
              [(syntax-pair? args)
               (unless (identifier? (syntax-car args))
                 (raise-syntax-error
                  #f "expected an identifier for a macro argument"
                  stx (syntax-car args)))
               (loop (syntax-cdr args))]
              [else
               (raise-syntax-error
                #f "not a valid argument sequence after the macro name"
                stx)])))
    (values
     (lambda (stx) ; defmacro
       (syntax-case stx ()
         [(_ (name . args) body0 body ...)
          (begin
            (check-args stx #'name #'args)
            #'(define-syntax name
                (let ([p (lambda args body0 body ...)])
                  (lambda (stx)
                    (let ([l (syntax->list stx)])
                      (unless (and l (procedure-arity-includes?
                                      p (sub1 (length l))))
                        (raise-syntax-error #f "bad form" stx))
                      (let ([ht (make-hash-table)])
                        (datum->syntax-object
                         stx
                         (dm-subst
                          ht (apply p (cdr (dm-syntax->datum stx ht))))
                         stx)))))))]
         [(_ name body) (identifier? #'name)
          #'(define-syntax name
              (lambda (stx)
                (syntax-case stx ()
                  [(_ . xs) (quasisyntax/loc stx
                              (#,(datum->syntax-object stx body stx) . xs))]
                  [_ (datum->syntax-object stx body stx)])))]))
     (lambda (stx) ; letmacro
       (syntax-case stx ()
         [(_ ([name body] ...) letbody ...)
          (quasisyntax/loc stx
            (let-syntax
                #,(map
                   (lambda (name body)
                     (if (identifier? name)
                       (quasisyntax/loc body
                         (#,name
                          (lambda (stx)
                            (syntax-case stx ()
                              [(_1 . xs)
                               (quasisyntax/loc stx
                                 (#,(datum->syntax-object stx body stx)
                                  . xs))]
                              [_1 (datum->syntax-object stx #,body stx)]))))
                       (syntax-case name ()
                         [(name . args)
                          (begin
                            (check-args stx #'name #'args)
                            (quasisyntax/loc body
                              (name
                               (let ([p (lambda args #,body)])
                                 (lambda (stx)
                                   (let ([l (syntax->list stx)])
                                     (unless
                                         (and l (procedure-arity-includes?
                                                 p (sub1 (length l))))
                                       (raise-syntax-error #f "bad form" stx))
                                     (let ([ht (make-hash-table)])
                                       (datum->syntax-object
                                        stx
                                        (dm-subst
                                         ht (apply p (cdr (dm-syntax->datum
                                                           stx ht))))
                                        stx))))))))])))
                   (syntax-e #'(name ...)) (syntax-e #'(body ...)))
              letbody ...))])))))
(make-provide-syntax defmacro defmacro*) (provide defmacro*)

;; ----------------------------------------------------------------------------
;;>>... Controlling syntax

;;>> (define-syntax-parameter name default)
;;>> (define-syntax-parameter* name default)
;;>   Creates a `syntax parameter'.  Syntax parameters are things that you
;;>   can use just like normal parameters, but they are syntax transformers,
;;>   and the information they store can be used by other syntax
;;>   transformers.  The purpose of having them around is to parameterize
;;>   the way syntax transformation is used -- so they should be used as
;;>   global option changes, not for frequent side effect: they change their
;;>   value at syntax expansion time.  Note that using it stores the literal
;;>   syntax that is passed to them -- there is no way to evaluate the given
;;>   argument, for example, if some parameter expects a boolean -- then
;;>   `(not #t)' will not work!  The syntax parameter itself is invoked
;;>   wither with no arguments to retrieve its value, or with an argument to
;;>   set it.  Retrieving or setting the value in this way is meaningful
;;>   only in an interactive context since using it in a function just
;;>   expands to the current value:
;;>     => (define-syntax-parameter -foo- 1)
;;>     => (-foo-)
;;>     1
;;>     => (define (foo) (-foo-))
;;>     => (-foo- 2)
;;>     => (-foo-)
;;>     2
;;>     => (foo)
;;>     1
(defsyntax* define-syntax-parameter
  (syntax-rules ()
    [(_ name default)
     (define-syntax name
       (let ([p (make-parameter #'default)])
         (lambda stx
           (if (null? stx)
             (p) ; when the value is used in other transformers
             (syntax-case (car stx) ()
               [(_ new) (begin (p #'new) #'(void))]
               [(_)     (p)])))))]))
(make-provide-syntax define-syntax-parameter define-syntax-parameter*)
(provide define-syntax-parameter*)

;; ----------------------------------------------------------------------------
;;>>... Setters and more list accessors

;;>> (set-caar! place x)
;;>> (set-cadr! place x)
;;>> (set-cdar! place x)
;;>> (set-cddr! place x)
;;>> (set-caaar! place x)
;;>> (set-caadr! place x)
;;>> (set-cadar! place x)
;;>> (set-caddr! place x)
;;>> (set-cdaar! place x)
;;>> (set-cdadr! place x)
;;>> (set-cddar! place x)
;;>> (set-cdddr! place x)
;;>> (set-caaaar! place x)
;;>> (set-caaadr! place x)
;;>> (set-caadar! place x)
;;>> (set-caaddr! place x)
;;>> (set-cadaar! place x)
;;>> (set-cadadr! place x)
;;>> (set-caddar! place x)
;;>> (set-cadddr! place x)
;;>> (set-cdaaar! place x)
;;>> (set-cdaadr! place x)
;;>> (set-cdadar! place x)
;;>> (set-cdaddr! place x)
;;>> (set-cddaar! place x)
;;>> (set-cddadr! place x)
;;>> (set-cdddar! place x)
;;>> (set-cddddr! place x)
;;>   These are all defined so it is possible to use `setf!' from "setf.rkt"
;;>   with these standard and library-provided functions.
#|
(define* set-caar!   (lambda (p v) (set-car! (car p) v)))
(define* set-cadr!   (lambda (p v) (set-car! (cdr p) v)))
(define* set-cdar!   (lambda (p v) (set-cdr! (car p) v)))
(define* set-cddr!   (lambda (p v) (set-cdr! (cdr p) v)))
(define* set-caaar!  (lambda (p v) (set-car! (caar p) v)))
(define* set-caadr!  (lambda (p v) (set-car! (cadr p) v)))
(define* set-cadar!  (lambda (p v) (set-car! (cdar p) v)))
(define* set-caddr!  (lambda (p v) (set-car! (cddr p) v)))
(define* set-cdaar!  (lambda (p v) (set-cdr! (caar p) v)))
(define* set-cdadr!  (lambda (p v) (set-cdr! (cadr p) v)))
(define* set-cddar!  (lambda (p v) (set-cdr! (cdar p) v)))
(define* set-cdddr!  (lambda (p v) (set-cdr! (cddr p) v)))
(define* set-caaaar! (lambda (p v) (set-car! (caaar p) v)))
(define* set-caaadr! (lambda (p v) (set-car! (caadr p) v)))
(define* set-caadar! (lambda (p v) (set-car! (cadar p) v)))
(define* set-caaddr! (lambda (p v) (set-car! (caddr p) v)))
(define* set-cadaar! (lambda (p v) (set-car! (cdaar p) v)))
(define* set-cadadr! (lambda (p v) (set-car! (cdadr p) v)))
(define* set-caddar! (lambda (p v) (set-car! (cddar p) v)))
(define* set-cadddr! (lambda (p v) (set-car! (cdddr p) v)))
(define* set-cdaaar! (lambda (p v) (set-cdr! (caaar p) v)))
(define* set-cdaadr! (lambda (p v) (set-cdr! (caadr p) v)))
(define* set-cdadar! (lambda (p v) (set-cdr! (cadar p) v)))
(define* set-cdaddr! (lambda (p v) (set-cdr! (caddr p) v)))
(define* set-cddaar! (lambda (p v) (set-cdr! (cdaar p) v)))
(define* set-cddadr! (lambda (p v) (set-cdr! (cdadr p) v)))
(define* set-cdddar! (lambda (p v) (set-cdr! (cddar p) v)))
(define* set-cddddr! (lambda (p v) (set-cdr! (cdddr p) v)))
|#

;;>> (1st list)
;;>> (2nd list)
;;>> (3rd list)
;;>> (4th list)
;;>> (5th list)
;;>> (6th list)
;;>> (7th list)
;;>> (8th list)
;;>   Quick list accessors -- no checking is done, which makes these
;;>   slightly faster than the bindings provided by mzlib/list.
(define* 1st car)
(define* 2nd cadr)
(define* 3rd caddr)
(define* 4th cadddr)
(define* 5th (lambda (x) (car (cddddr x))))
(define* 6th (lambda (x) (cadr (cddddr x))))
(define* 7th (lambda (x) (caddr (cddddr x))))
(define* 8th (lambda (x) (cadddr (cddddr x))))

;;>> (set-1st! list x)
;;>> (set-2nd! list x)
;;>> (set-3rd! list x)
;;>> (set-4th! list x)
;;>> (set-5th! list x)
;;>> (set-6th! list x)
;;>> (set-7th! list x)
;;>> (set-8th! list x)
;;>   Setter functions for the above.
#|
(define* set-1st! set-car!)
(define* set-2nd! set-cadr!)
(define* set-3rd! set-caddr!)
(define* set-4th! set-cadddr!)
(define* set-5th! (lambda (p v) (set-car! (cddddr p) v)))
(define* set-6th! (lambda (p v) (set-car! (cdr (cddddr p)) v)))
(define* set-7th! (lambda (p v) (set-car! (cddr (cddddr p)) v)))
(define* set-8th! (lambda (p v) (set-car! (cdddr (cddddr p)) v)))
|#

;;>> (head pair)
;;>> (tail pair)
;;>> (set-head! pair x)
;;>> (set-tail! pair x)
;;>   Synonyms for `first', `rest', `set-first!', `set-rest!'.
(define* head first)
(define* tail rest)
;(define* set-head! set-first!)
;(define* set-tail! set-rest!)

;;>> (set-second! list x)
;;>> (set-third! list x)
;;>> (set-fourth! list x)
;;>> (set-fifth! list x)
;;>> (set-sixth! list x)
;;>> (set-seventh! list x)
;;>> (set-eighth! list x)
;;>   Defined to allow `setf!' with these mzlib/list functions.  Note that
;;>   there is no error checking (unlike the accessor functions which are
;;>   provided by mzlib/list).
#|
(define* set-second!  set-2nd!)
(define* set-third!   set-3rd!)
(define* set-fourth!  set-4th!)
(define* set-fifth!   set-5th!)
(define* set-sixth!   set-6th!)
(define* set-seventh! set-7th!)
(define* set-eighth!  set-8th!)
|#

;;>> (nth list n)
;;>> (nthcdr list n)
;;>   Functions for pulling out the nth element and the nth tail of a list.
;;>   Note the argument order which is unlike the one in CL.
(define* nth list-ref)
(define* (nthcdr l n)
  (if (zero? n) l (nthcdr (cdr l) (- n 1))))

;;>> (list-set! list n x)
;;>> (set-nth! list n x)
;;>   A function to set the nth element of a list, also provided as
;;>   `set-nth!' to allow using `setf!' with `nth'.
#|
(define* (list-set! lst index new)
  (set-car! (nthcdr lst index) new))
(define* set-nth! list-set!)
|#

;;>> (set-list-ref! list n x)
;;>> (set-vector-ref! vector n x)
;;>> (set-string-ref! string n x)
;;>   These are defined as `list-set!', `vector-set!', and `string-set!', so
;;>   the accessors can be used with `setf!'.
; (define* set-list-ref!   list-set!)
(define* set-vector-ref! vector-set!)
(define* set-string-ref! string-set!)

;;>> (last list)
;;>> (set-last! list x)
;;>   Accessing a list's last element, and modifying it.
(define* (last l)
  (car (last-pair l)))
#|
(define* (set-last! l x)
  (set-car! (last-pair l) x))
|#

;;>> (set-unbox! box x)
;;>   Allow using `setf!' with `unbox'.  Note: this is an alias for
;;>   `set-box!' which is an inconsistent name with other Scheme `set-foo!'
;;>   functions -- the result is that you can also do `(setf! (box foo) x)'
;;>   and bogusly get the same effect.
(define* set-unbox! set-box!)

;;>> (set-hash-table-get! table key [default] value)
;;>   This is defined to be able to `setf!' into a `hash-table-get'
;;>   accessor.  The form that `setf!' assembles always puts the new value
;;>   last, but it is still useful to have a default thunk which results in
;;>   an optional argument in an unusual place (and this argument is ignored
;;>   by this, which is why it is defined as a macro).  For example:
;;>     => (define t (make-hash-table))
;;>     => (inc! (hash-table-get t 'foo))
;;>     hash-table-get: no value found for key: foo
;;>     => (inc! (hash-table-get t 'foo (thunk 0)))
;;>     => (hash-table-get t 'foo)
;;>     1
(defsubst*
  (set-hash-table-get! table key value) (hash-table-put! table key value)
  (_ table key thunk value)             (hash-table-put! table key value))

;; ----------------------------------------------------------------------------
;;>>... Utilities

;;>> (eprintf fmt-string args ...)
;;>   Same as `printf' but it uses `current-error-port'.
(define* (eprintf . args)
  (apply fprintf (current-error-port) args))

;;>> concat
;;>   A shorter alias for `string-append'.
(define* concat string-append)

;;>> (symbol-append sym ...)
;;>   Self explanatory.
(define* (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

;;>> (maptree func tree)
;;>   Applies given function to a tree made of cons cells, and return the
;;>   results tree with the same shape.
(define* (maptree f x)
  (let loop ([x x])
    (cond [(list? x) (map loop x)]
          [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
          [else (f x)])))

;;>> (map! func list ...)
;;>   Same as `map' -- but destructively modifies the first list to hold the
;;>   results of applying the function.  Assumes all lists have the same
;;>   length.
#|
(define* (map! f l . rest)
  (if (null? rest)
    (let loop ([xs l])
      (if (null? xs) l (begin (set-car! xs (f (car xs))) (loop (cdr xs)))))
    (let loop ([xs l] [ls rest])
      (if (null? xs) l (begin (set-car! xs (apply f (car xs) (map car ls)))
                              (loop (cdr xs) (map cdr ls)))))))
|#

;;>> (maptree! func tree)
;;>   Same as `maptree' -- but destructively modifies the list to hold the
;;>   results of applying the function.
#|
(define* (maptree! f x)
  (if (pair? x)
    (begin (let loop ([x x])
             (defsubst (do-part get set)
               (let ([y (get x)])
                 (cond [(pair? y) (loop y)]
                       [(not (null? y)) (set x (f y))])))
             (do-part car set-car!)
             (do-part cdr set-cdr!))
           x)
    (f x))) ; can't be destructive here
|#

;;>> (mappend func list ...)
;;>> (mappend! func list ...)
;;>   Common idiom for doing a `(map func list ...)' and appending the
;;>   results.  `mappend!' uses `append!'.
(define* (mappend f . ls)
  (apply append (apply map f ls)))
#|
(define* (mappend! f . ls)
  (apply append! (apply map f ls)))
|#

;;>> (mapply func list-of-lists)
;;>   Apply the given `func' on every list in `list-of-lists' and return the
;;>   results list.
(define* (mapply f ls)
  (map (lambda (args) (apply f args)) ls))

;;>> (negate predicate?)
;;>   Returns a negated predicate function.
(define* (negate pred?)
  (lambda x (not (pred? . x))))

;;>> (position-of x list)
;;>   Finds `x' in `list' and returns its index.
(define* (position-of x lst)
  (let loop ([i 0] [l lst])
    (cond [(null? l) #f]
          [(eq? x (car l)) i]
          [else (loop (add1 i) (cdr l))])))

;;>> (find-if predicate? list)
;;>   Find and return an element of `list' which satisfies `predicate?', or
;;>   #f if none found.
(define* (find-if pred? l)
  (let loop ([l l])
    (cond [(null? l) #f]
          [(pred? (car l)) (car l)]
          [else (loop (cdr l))])))

;;>> (some predicate? list ...)
;;>> (every predicate? list ...)
;;>   Similar to Racket's `ormap' and `andmap', except that when multiple
;;>   lists are given, the check stops as soon as the shortest list ends.

(define* (some pred? l . rest)          ; taken from slib/comlist.scm,
  (cond [(null? rest)                   ; modified to check only up to the
         (let mapf ([l l])              ; length of the shortest list.
           (and (not (null? l))
                (or (pred? (car l)) (mapf (cdr l)))))]
        [else (let mapf ([l l] [rest rest])
                (and (not (or (null? l) (memq '() rest)))
                     (or (apply pred? (car l) (map car rest))
                         (mapf (cdr l) (map cdr rest)))))]))

(define* (every pred? l . rest)         ; taken from slib/comlist.scm
  (cond [(null? rest)                   ; modified to check only up to the
         (let mapf ([l l])              ; length of the shortest list.
           (or (null? l)
               (and (pred? (car l)) (mapf (cdr l)))))]
        [else (let mapf ([l l] [rest rest])
                (or (null? l) (if (memq '() rest) #t #f)
                    (and (apply pred? (car l) (map car rest))
                         (mapf (cdr l) (map cdr rest)))))]))

;;>> (with-output-to-string thunk)
;;>   Run `thunk' collecting generated output into a string.
(define* (with-output-to-string thunk)
  (let ([str (open-output-string)])
    (parameterize ([current-output-port str]) (thunk))
    (get-output-string str)))

;;>> (1+ x)
;;>> (1- x)
;;>   Synonyms for `add1' and `sub1'.
(define* 1+ add1)
(define* 1- sub1)

;; ----------------------------------------------------------------------------
;;>>... Multi-dimensional hash-tables
;; Using lists of `eq?' keys, based on Racket's hash tables (MzScheme doesn't
;; have custom hashes).  Use weak hash-tables so no space is redundantly
;; wasted.

;;>> (make-l-hash-table)
;;>> (l-hash-table-get table keys [failure-thunk])
;;>> (l-hash-table-put! table keys value)
;;>> (set-l-hash-table-get! table key [default] value)
;;>   These functions are similar to Racket's hash-table functions, except
;;>   that they work with a list of keys (compared with `eq?').  If it was
;;>   possible to use a custom equality hash-table, then then would use
;;>   something like
;;>     (lambda (x y) (and (= (length x) (length y)) (andmap eq? x y))).
;;>   The implementation uses a hash-table of hash-tables, all of them weak,
;;>   since it is supposed to be used for memoization.
;;>
;;>   `set-l-hash-table-get!' is defined to work with `setf!'.

;; Internal values, used below.
(define *nothing* (list "*"))
(define (return-nothing) *nothing*)

(defsubst l-hash-vector-length 10)

(define* (make-l-hash-table)
  (make-vector (add1 l-hash-vector-length) *nothing*))

(define* (l-hash-table-get table keys . thunk)
  (let ([len (length keys)])
    (let loop ([obj (vector-ref table (min len l-hash-vector-length))]
               [keys (if (< len l-hash-vector-length) keys (cons len keys))])
      (cond [(eq? obj *nothing*)
             (if (null? thunk)
               (error 'l-hash-table-get "no value found.") ((car thunk)))]
            [(null? keys) obj]
            [(not (hash-table? obj))
             (error 'l-hash-table-get "got to a premature value.")]
            [else (loop (hash-table-get obj (car keys) return-nothing)
                        (cdr keys))]))))

(define* (l-hash-table-put! table keys value)
  (let* ([len (length keys)]
         [obj (vector-ref table (min len l-hash-vector-length))])
    (when (eq? obj *nothing*)
      (set! obj (if (zero? len) value (make-hash-table 'weak)))
      (vector-set! table (min len l-hash-vector-length) obj))
    (unless (zero? len)
      (let loop ([obj obj]
                 [keys (if (< len l-hash-vector-length) keys (cons len keys))])
        (cond [(not (hash-table? obj))
               (error 'l-hash-table-put! "got to a premature value.")]
              [(null? (cdr keys)) (hash-table-put! obj (car keys) value)]
              [else (let ([value (hash-table-get
                                  obj (car keys) return-nothing)])
                      (when (eq? value *nothing*)
                        (set! value (make-hash-table 'weak))
                        (hash-table-put! obj (car keys) value))
                      (loop value (cdr keys)))])))))

(defsubst*
  (set-l-hash-table-get! table key value) (l-hash-table-put! table key value)
  (_ table key thunk value)               (l-hash-table-put! table key value))

;; Simple memoization.

;;>> (memoize func)
;;>   Return a memoized version of `func'.  Note that if `func' is
;;>   recursive, it should be arranged for it to call the memoized version
;;>   rather then call itself directly.
(define* (memoize f)
  (let ([table (make-l-hash-table)])
    (lambda args
      (l-hash-table-get
       table args
       (thunk
         (let ([r (apply f args)]) (l-hash-table-put! table args r) r))))))

;;>> (memoize! func-name)
;;>   Changes the given function binding to a memoized version.
(defsubst* (memoize! f) (set! f (memoize f)))

;; ---------------------------------------------------------------------------
;;>>... Generic iteration and list comprehension
;; Idea originated in a post on c.l.s by Based on Phil Bewig (July 2002), but
;; went light years beyond that.

;;>> (collect [dir] (var base expr) clause ...)
;;>   Sophisticated iteration syntax.  The iteration is specified by the
;;>   given clauses, where `var' serves as an accumulator variable that
;;>   collects a value beginning with `base' and continuing with `expr' --
;;>   similar to a single binding in a `do' form with a variable, an initial
;;>   value and an update expression.  But there are much more iteration
;;>   options than a `do' form: this form supports a generic
;;>   list-comprehension and related constructs.  Forms that use this
;;>   construct are:
;;>

;;>> (loop-for clause ...)
;;>   Use when no value collection is needed, and the default for
;;>   expressions is to do them instead of using them as a filter.
;;>   Implemented as:
;;>     (collect => (acc (void) acc) do clause ...)
(defsubst* (loop-for clause ...)
  (collect => (acc (void) acc) do clause ...))
;;>

;;>> (list-of expr clause ...)
;;>   Implemented as:
;;>     (reverse! (collect (acc '() (cons expr acc)) clause ...))
(defsubst* (list-of expr clause ...)
  (reverse (collect (acc '() (cons expr acc)) clause ...)))
;;>

;;>> (sum-of expr clause ...)
;;>   Implemented as:
;;>     (collect (acc 0 (+ expr acc)) clause ...)
(defsubst* (sum-of expr clause ...)
  (collect (acc 0 (+ expr acc)) clause ...))
;;>

;;>> (product-of expr clause ...)
;;>   Implemented as:
;;>     (collect (acc 1 (* expr acc)) clause ...)
(defsubst* (product-of expr clause ...)
  (collect (acc 1 (* expr acc)) clause ...))
;;>

;;>> (count-of clause ...)
;;>   Only count matching cases, implemented as:
;;>     (sum-of 1 clause ...)
(defsubst* (count-of clause ...)
  (sum-of 1 clause ...))
;;>

;;>   Each clause is either:
;;>   * (v <- ...):     a binding generator clause;
;;>   * (v <- ... and v <- ...): parallel generator clauses;
;;>   * (v is is-expr): bind `v' to the result of `is-expr';
;;>   * while expr:     a `while' keyword followed by an expression will
;;>                     abort the whole loop if that expression evaluates to
;;>                     #f;
;;>   * until expr:     an `until' keyword followed by an expression will
;;>                     abort the whole loop if that expression evaluates to
;;>                     a non-#f value;
;;>   * when ...:       filter by the following expressions -- if an
;;>                     expression evaluates to #f, stop processing this
;;>                     iteration (default for all macros except for
;;>                     `loop-for');
;;>   * unless ...:     filter by the negation of the following expressions;
;;>   * do ...:         execute the following expressions, used for side
;;>                     effects (default for the `loop-for' macro);
;;>   * expr:           expression is used according to the current mode set
;;>                     by a `when', `unless', or `do', keyword that
;;>                     precedes it.
;;>   The effect of this form is to iterate each generator variable
;;>   according to generating `<-' clauses (see below for these) and
;;>   parallel clauses, and evaluate the `expr' with each combination, which
;;>   composes a result out of iteration-bound values and an accumulated
;;>   result.  Generation is done in a nested fashion, where the rightmost
;;>   generator spin fastest.  Parallel generators (specified with an infix
;;>   `and') make all iterations happen simultaneously, ending as soon as
;;>   the first one ends.  An `is' clause is used for binding arbitrary
;;>   variables, a `do' clause is used to execute code for general
;;>   side-effects, and other clauses are used to filter results before
;;>   continuing down the clause list.  Each clause can use variables bound
;;>   by previous clauses, and the `expr' can use all bound variables as
;;>   well as the given accumulator variable.
;;>
;;>   An optional first token can be used to specify the direction which is
;;>   used to accumulate the result.  It can be one of these two tokens:
;;>   `<=': A "backward" collection, the default (similar to `foldl');
;;>   `=>': A "forward" collection (similar to `foldr').
;;>   The default "backward" direction works by generating an accumulator
;;>   carrying loop, as in this code (this code is for demonstration, not
;;>   what `collect' creates):
;;>     (let loop ([x foo] [acc '()])
;;>       (if (done? x) acc (loop (next x) (cons (value x) acc))))
;;>   which is a common Scheme idiom for such operations.  The problem is
;;>   that this accumulation happens in reverse -- requiring reversing the
;;>   final result (which is done by the `list-of' macro).  A "forward"
;;>   direction does a naive recursive loop:
;;>     (let loop ([x foo])
;;>       (if (done? x) '() (cons (value x) (loop (next x)))))
;;>   collecting values in the correct order, but the problem is that it
;;>   keeps a computation context which makes memory consumption
;;>   inefficient.  The default style is usually preferred, since reversing
;;>   a list is a cheap operation, but it is not possible when infinite
;;>   lists (streams) are used since it is impossible to reverse them.  In
;;>   these cases, the "forward" style should be used, but the `expr' must
;;>   take care not to evaluate the iteration "variable" immediately, using
;;>   `delay' or a similar mechanism (this "variable" is not bound to a
;;>   value but substituted with an expression (a symbol macro)).  For
;;>   example, here's a quick lazy list usage:
;;>     => (defsubst (lcons x y) (delay (cons x y)))
;;>     => (define (lcar s) (car (force s)))
;;>     => (define (lcdr s) (cdr (force s)))
;;>     => (define x (collect (_ '() (lcons x _)) (x <- 0 ..)))
;;>     ; loops indefinitely
;;>     => (define x (collect => (_ '() (lcons x _)) (x <- 0 ..)))
;;>     => (lcar (lcdr (lcdr x)))
;;>     2
;;>   Note that the `loop-for' macro uses a "forward" direction, but this is
;;>   only because it is slightly faster since it doesn't require an extra
;;>   binding.
;;>   [The direction can be changed for a single part by using a "<-!"
;;>   keyword instead of "<-", but this is an experimental feature since I
;;>   don't know if it's actually useful for anything.  Do not try to mix
;;>   this with the `while' and `until' keywords which are implemented
;;>   differently based on the direction.]
;;>

(defsyntax* (collect stx)
  (define (split id stxs)
    (let loop ([stxs '()] [stxss '()]
               [l (if (syntax? stxs) (syntax->list stxs) stxs)])
      (cond [(null? l) (reverse (cons (reverse stxs) stxss))]
            [(and (identifier? (car l)) (module-identifier=? id (car l)))
             (loop '() (cons (reverse stxs) stxss) (cdr l))]
            [else (loop (cons (car l) stxs) stxss (cdr l))])))
  (define (gen-loop generate add-aux! &optional hacked)
    (with-syntax ([generate generate]
                  [(cur step done? value)
                   (generate-temporaries '(cur step done? value))])
      (add-aux! #'((cur step done? value) (apply values generate)))
      (with-syntax ([value #'(if value (value cur) cur)])
        (with-syntax ([value (if hacked
                               #`(let ([r value]) (set! #,hacked r) r)
                               #'value)])
          #'(cur cur (step cur) (and done? (done? cur)) value)))))
  (define (gen var args add-aux! hack-var! &optional seq?)
    (define (hack!) (when (and seq? hack-var!) (hack-var! var)))
    (define (gen1 arg) (if seq? arg (gen-loop arg add-aux!)))
    (with-syntax ([v var])
      (syntax-case args (then until while .. ..<)
;;>   Generator forms are one of the following ("..", "then", "until",
;;>   "while" are literal tokens), see below for what values are generated:
;;>   * (v <- sequence):
;;>     iterate `v' on values from `sequence';
        [(arg)            (gen1 #'(collect-iterator arg))]
;;>   * (v <- 1st [2nd] .. [last]):
;;>     iterate on an enumerated range, including last element of range;
        [(a b ..  z)      (gen1 #'(collect-numerator a b  z        ))]
        [(a b ..   )      (gen1 #'(collect-numerator a b  #f       ))]
        [(a   ..  z)      (gen1 #'(collect-numerator a #f z        ))]
        [(a   ..   )      (gen1 #'(collect-numerator a #f #f       ))]
;;>   * (v <- 1st [2nd] ..< last):
;;>     iterate on an enumerated range, excluding last element of range;
        [(a b ..< z)      (gen1 #'(collect-numerator a b  z  '<    ))]
        [(a   ..< z)      (gen1 #'(collect-numerator a #f z  '<    ))]
;;>   * (v <- 1st [2nd] .. while last):
;;>     iterate on an enumerated range, excluding last element of range;
        [(a b .. while z) (gen1 #'(collect-numerator a b  z  'while))]
        [(a   .. while z) (gen1 #'(collect-numerator a #f z  'while))]
;;>   * (v <- 1st [2nd] .. until last):
;;>     iterate on an enumerated range, excluding last element of range;
        [(a b .. until z) (gen1 #'(collect-numerator a b  z  'until))]
        [(a   .. until z) (gen1 #'(collect-numerator a #f z  'until))]
;;>   * (v <- x then next-e [{while|until} cond-e]):
;;>     start with the `x' expression, continue with the `next-e' expression
;;>     (which can use `v'), do this while/until `cond-e' is true if a
;;>     condition is given;
        [(arg then next) (hack!)
         (if seq? ; making seq? => convert to composable funcs
           #'(list arg (lambda (v) next) #f #f)
           #'(v arg next #f v))]
        [(arg then next while cond) (hack!)
         (if seq?
           #'(list arg (lambda (v) next) (lambda (v) (not cond)) #f)
           #'(v arg next (not cond) v))]
        [(arg then next until cond) (hack!)
         (if seq?
           #'(list arg (lambda (v) next) (lambda (v) cond) #f)
           #'(v arg next cond v))]
;;>   * (v <- x {while|until} cond-e):
;;>     repeat using the `x' expression while/until `cond-e' is true;
        [(arg while cond) (hack!)
         (if seq?
           #'(list #f #f #f (lambda (_) (if cond arg collect-final)))
           #'(v #f #f #f (begin (set! v arg) (if cond v collect-final))))]
        [(arg until cond) (hack!)
         (if seq?
           #'(list #f #f #f (lambda (_) (if cond collect-final arg)))
           #'(v #f #f #f (begin (set! v arg) (if cond collect-final v))))]
;;>   * (v <- func arg ...):
;;>     applies `func' to `arg ...', the result is expected to be some
;;>     "iterator value" which is used to do the iteration -- iteration
;;>     values are created by `collect-iterator' and `collect-numerator',
;;>     see below for their description and return values.
;;>   * (v <- gen1 <- gen2 <- ...):
;;>     generator clauses can have multiple parts specified by more `<-'s,
;;>     all of them will run sequentially;
        [(f x ...)
         (let ([argss (split #'<- args)])
           (if (= 1 (length argss))
             (gen1 #'(f x ...))
             (let ([hacked #f])
               (with-syntax
                   ([(gen ...)
                     (map (lambda (as)
                            (gen var as add-aux!
                                 (lambda (v) (set! hacked v) (hack-var! v))
                                 #t))
                          argss)])
                 (gen-loop #'(sequential-generators gen ...)
                           add-aux! hacked)))))])))
  (define-values (acc base0 expr clauses fwd?)
    (syntax-case stx (<= =>)
      [(_ <= (acc base expr) clause ...)
       (values #'acc #'base #'expr #'(clause ...) #f)]
      [(_ => (acc base expr) clause ...)
       (values #'acc #'base #'expr #'(clause ...) #t)]
      [(_ (acc base expr) clause ...)
       (values #'acc #'base #'expr #'(clause ...) #f)]))
  (define need-break? #f)
  (define loop-body
    (let c-loop ([base base0] [clauses clauses] [mode 'when] [rev? #f])
      (syntax-case clauses (<- <-! is do when unless while until)
        [() (if (if rev? (not fwd?) fwd?)
              #`(letsubst ([#,acc #,base]) #,expr)
              expr)]
        [((var <-! arg ...) rest ...)
         (c-loop base #'((var <- arg ...) rest ...) mode 'rev!)]
        [((var <- arg ...) rest ...)
;;>   * (v1 <- gen1 ... and v2 <- gen2 ...):
;;>     finally, an infix `and' specifies parallel generators, binding
;;>     several variables.
         (let ([rev? (if (eq? 'rev! rev?) #t #f)]
               [gens (split #'and #'(var <- arg ...))]
               [loop-id (car (generate-temporaries '(loop)))]
               [aux '()] [hacked-vars '()])
           (for-each
            (lambda (g)
              (syntax-case g (<-)
                [(var <- arg ...) (identifier? #'var) #f]
                [_ (raise-syntax-error
                    #f "expected a generator clause" stx g)]))
            gens)
           (with-syntax ([((var <- arg ...) ...) gens])
             ;; Hack needed: generator variables are defined later in the loop
             ;; just before their code, after the place where the expression
             ;; appear in setup code.  This is usually not a problem since
             ;; functions are applied the same, but when using expression
             ;; iteration (`then') in a sequential range which is in
             ;; simultaneous iteration where real expressions are turned to
             ;; functions (which are define before variables the might
             ;; reference).  This could be eliminated, restricting expressions
             ;; from referencing variables that are bound in parallel, but this
             ;; is usually the power of using expression (which can be claimed
             ;; redundant).  The hack is doing this:
             ;;  (let ([x #f] ...)
             ;;    ... (let ([x (let ([r value]) (set! x r) r)])))
             ;; The problem is that the extra junk makes it run twice slower,
             ;; so do this only for bindings that has the above scenario
             ;; (parallel of sequential of expression generators).  To test it,
             ;; do this:
             ;;  (list-of (list c x y)
             ;;    (c <- 1 .. 5 and x <- 1 <- 'x then y
             ;;                 and y <- 1 <- 'y then x))
             ;; but this always works:
             ;;  (list-of (list c x y)
             ;;    (c <- 1 .. 5 and x <- 'x then y and y <- 'y then x))
             (with-syntax ([((cur fst next done? value) ...)
                            (map (lambda (v as)
                                   (gen v as
                                        (lambda (a) (set! aux (cons a aux)))
                                        (lambda (v)
                                          (set! hacked-vars
                                                (cons v hacked-vars)))))
                                 (syntax->list #'(var ...))
                                 (syntax->list #'((arg ...) ...)))]
                           [loop loop-id]
                           [(aux ...) (reverse aux)] [acc acc] [base base])
               (with-syntax
                   ([body
                     (let* ([fwd? (if rev? (not fwd?) fwd?)]
                            [return (if fwd? #'base #'acc)]
                            [body (if fwd?
                                    (c-loop #`(#,loop-id next ...)
                                            #'(rest ...) mode rev?)
                                    #`(loop next ...
                                            #,(c-loop #'acc #'(rest ...)
                                                      mode rev?)))])
                       #`(let-values (aux ...)
                           (let loop ([cur fst] ...
                                      #,@(if fwd? #'() #'((acc base))))
                             (if (or done? ...)
                               #,return
                               #,(let vloop ([vars (syntax->list #'(var ...))]
                                             [values (syntax->list
                                                      #'(value ...))])
                                   (if (null? vars)
                                     body
                                     #`(let ([#,(car vars) #,(car values)])
                                         (if (eq? #,(car vars) collect-final)
                                           #,return
                                           #,(vloop (cdr vars)
                                                    (cdr values))))))))))])
                 (if (null? hacked-vars)
                   #'body
                   (with-syntax ([(var ...) (reverse hacked-vars)])
                     #'(let ([var #f] ...) body)))))))]
        [((var is is-expr) rest ...)
         #`(let ([var is-expr]) #,(c-loop base #'(rest ...) mode rev?))]
        [(while cond rest ...)
         #`(if cond
             #,(c-loop base #'(rest ...) mode rev?)
             #,(if (if rev? (not fwd?) fwd?)
                 base0 (begin (set! need-break? #t) #`(break #,base))))]
        [(until cond rest ...)
         #`(if cond
             #,(if (if rev? (not fwd?) fwd?)
                 base0 (begin (set! need-break? #t) #`(break #,base)))
             #,(c-loop base #'(rest ...) mode rev?))]
        [(do     rest ...) (c-loop base #'(rest ...) 'do     rev?)]
        [(when   rest ...) (c-loop base #'(rest ...) 'when   rev?)]
        [(unless rest ...) (c-loop base #'(rest ...) 'unless rev?)]
        [(expr rest ...)
         (with-syntax ([cont (c-loop base #'(rest ...) mode rev?)])
           (case mode
             [(when)   #`(if expr cont #,base)]
             [(unless) #`(if expr #,base cont)]
             [(do)     #`(begin expr cont)]))])))
  (if need-break?
    #`(let/ec break #,loop-body) loop-body))
;;>

(define (sequential-generators gen . rest)
  (let-values ([(new) #f] [(fst step done? value) (values . gen)])
    (define (next!)
      (and (pair? rest)
           (begin (set! gen   (car rest)) (set! rest  (cdr rest))
                  (set! fst   (1st gen))  (set! step  (2nd gen))
                  (set! done? (3rd gen))  (set! value (4th gen))
                  #t)))
    (list fst
          (lambda (x)
            (let ([r (step (if new (begin0 new (set! new #f)) x))])
              (if (and done? (done? r)) (if (next!) fst collect-final) r)))
          (lambda (x)
            (and (null? rest)
                 (or (eq? x collect-final) (and done? (done? x)))))
          (lambda (x)
            (let ([r (if value (value x) x)])
              (if (eq? r collect-final)
                (let* ([n? (next!)] [r (and n? (if value (value fst) fst))])
                  (set! new fst)
                  (if (or (not n?) (done? fst)) collect-final r))
                r))))))

(define (function->iterator f &optional done? include-last?)
  (define arity
    (cond [(procedure-arity-includes? f 0) 0]
          [(procedure-arity-includes? f 1) 1]
          [else (error 'function->iterator
                       "don't know how to iterate over function ~e" f)]))
  (when (and done? include-last?)
    (set! done?
          (let ([d? done?])
            (lambda (x) (when (d? x) (set! f (lambda _ collect-final))) #f))))
  (when (eq? 1 arity) (set! f (function-iterator f collect-final)))
  (list (void) void #f
        (if done?
          (lambda (_)
            (let ([x (f)])
              (if (or (eq? x collect-final) (done? x)) collect-final x)))
          (lambda (_) (f)))))

;;>   Iteration is possible on one of the following sequence values:
(define* (collect-iterator seq)
  (define (out-of-range r) (lambda (x) (<= r x)))
  (cond
;;>   * list: iterate over the list's element;
   [(list? seq) (list seq cdr null? car)]
;;>   * vector: iterate over the vector's elements;
   [(vector? seq) (list 0 add1 (out-of-range (vector-length seq))
                        (lambda (i) (vector-ref seq i)))]
;;>   * string: iterate over characters in the string;
   [(string? seq) (list 0 add1 (out-of-range (string-length seq))
                        (lambda (i) (string-ref seq i)))]
;;>   * integer n: iterate on values from 0 to n-1;
   [(integer? seq) (list 0 add1 (out-of-range seq) #f)]
;;>   * procedure f:
   [(procedure? seq)
;;>     - if f accepts zero arguments, begin with (f) and iterate by
;;>       re-applying (f) over and over, so the only way to end this
;;>       iteration is by returning `collect-final' (see below);
;;>     - otherwise, if f accepts one argument, it is taken as a generator
;;>       function: it is passed a one-argument procedure `yield' which can
;;>       be used to suspend its execution returning the given value, and it
;;>       will be continued when more values are required (see
;;>       `function-iterator' below);
    (function->iterator seq)]
;;>   * hash-table: iterate over key-value pairs -- this is done with a
;;>     generator function:
;;>       (lambda (yield)
;;>         (hash-table-for-each seq (lambda (k v) (yield (cons k v)))))
   [(hash-table? seq)
    (collect-iterator (lambda (yield)
                        (hash-table-for-each
                         seq (lambda (k v) (yield (cons k v))))))]
;;>   * other values: repeated infinitely.
   [else (list seq identity #f #f)]))
;;>   Note that iteration over non-lists is done efficiently, iterating over
;;>   a vector `v' is better than iterating over `(vector->list v)'.
;;>

;;>   Enumeration is used whenever a ".." token is used to specify a range.
;;>   There are different enumeration types based on different input types,
;;>   and all are modified by the token used:
;;>   * "..": a normal inclusive range;
;;>   * "..<": a range that does not include the last element;
;;>   * ".. while": a range that continues while a predicate is true;
;;>   * ".. until": a range that continues until a predicate is true.
;;>   The "..<" token extends to predicates in the expected way: the element
;;>   that satisfies the predicate is the last one and it is not included in
;;>   the enumeration -- unlike "..".
;;>   These are the possible types that can be used with an enumeration:
(define* (collect-numerator from second to &optional flag)
  (define (check-type pred? &optional not-to)
    (and (pred? from) (or (not second) (pred? second))
         (or not-to (not to) (pred? to))))
  (define (to->pred)
    (and to (let ([to (if (and (procedure? to)
                               (procedure-arity-includes? to 1))
                        to (lambda (x) (equal? x to)))])
              (if (eq? 'while flag) (negate to) to))))
  (when (and (memq flag '(while until))
             (not (and (procedure? to) (procedure-arity-includes? to 1))))
    (set! to (lambda (x) (equal? x to))))
;;>   * num1 [num2] .. [num3]: go from num1 to num3 in num3 in num2-num1
;;>     steps, if num2 is not given then use +1/-1 steps, if num3 is not
;;>     given don't stop;
;;>   * num1 [num2] .. pred: go from num1 by num2-num1 steps (defaults to
;;>     1), up to the number that satisfies the given predicate;
  (cond [(check-type number?)
         (let* ([step
                 (cond [second (- second from)]
                       [(and (number? to) (> from to)) -1]
                       [else 1])]
                [gt?
                 (case flag
                   [(#f) (if (positive? step) > <)]
                   [(<)  (if (positive? step) >= <=)]
                   [else (error 'collect-numerator "internal error")])])
           (list from
                 (lambda (x) (+ x step))
                 (if (number? to) (lambda (x) (gt? x to)) #f)
                 #f))]
;;>   * char1 [char2] .. [char3/pred]: the same as with numbers, but on
;;>     character ranges;
        [(check-type char? #t)
         (let ([numerator (collect-numerator
                           (char->integer from)
                           (and second (char->integer second))
                           (cond [(char? to) (char->integer to)]
                                 [(and (procedure? to)
                                       (procedure-arity-includes? to 1))
                                  (compose to integer->char)]
                                 [else to])
                           flag)])
           (list (1st numerator) (2nd numerator) (3rd numerator)
                 integer->char))]
;;>   * func .. [pred/x]: use `func' the same way as in an iterator above,
;;>     use `pred' to identify the last element, if `pred' is omitted repeat
;;>     indefinitely;
        [(and (procedure? from) (not second))
         (let ([to (to->pred)])
           (function->iterator from to (and (not flag) to)))]
;;>   * fst [next] .. [pred]: start with `fst', continue by repeated
;;>     applications of the `next' function on it, and use `pred' to
;;>     identify the last element, if `pred' is omitted repeat indefinitely,
;;>     if `next' is omitted repeat `fst', and if both `fst' and `next' are
;;>     numbers or characters then use their difference for stepping.  (Note
;;>     that to repeat a function value you should use `identity' as for
;;>     `next' or the function will be used as described above.)
        [else
         (cond [(and (number? from) (number? second))
                (let ([d (- second from)]) (set! second (lambda (x) (+ x d))))]
               [(not second) (set! second identity)]
               [(not (and (procedure? second)
                          (procedure-arity-includes? second 1)))
                (error 'collect-numerator
                       "don't know how to enumerate ~e ~e .. ~e"
                       from second to)])
         (if (not to)
           (list from second #f #f)
           (let ([to (to->pred)])
             (if (or flag (not to))
               (list from second to #f)
               (let ([almost-done? (to from)] [done? #f])
                 (list from (lambda (x)
                              (if almost-done?
                                (set! done? #t)
                                (let ([next (second x)])
                                  (when (to next) (set! almost-done? #t))
                                  next)))
                       (lambda (_) done?) #f)))))]))
;;>

;;>   Here is a long list of examples for clarification, all using
;;>   `list-of', but the generalization should be obvious:
;;>     => (list-of x [x <- '(1 2 3)])
;;>     (1 2 3)
;;>     => (list-of (list x y) [x <- '(1 2 3)] [y <- 1 .. 2])
;;>     ((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))
;;>     => (list-of (format "~a~a~a" x y z)
;;>                 [x <- '(1 2)] [y <- #(a b)] [z <- "xy"])
;;>     ("1ax" "1ay" "1bx" "1by" "2ax" "2ay" "2bx" "2by")
;;>     => (list-of (+ x y) [x <- '(1 2 3)] [y <- 20 40 .. 100])
;;>     (21 41 61 81 101 22 42 62 82 102 23 43 63 83 103)
;;>     => (list-of (+ x y) [x <- '(1 2 3) and y <- 20 40 .. 100])
;;>     (21 42 63)
;;>     => (list-of y [x <- 0 .. and y <- '(a b c d e f g h i)] (even? x))
;;>     (a c e g i)
;;>     => (list-of y [x <- 0 .. and y <- '(a b c d e f g h i)]
;;>          when (even? x) do (echo y))
;;>     a
;;>     c
;;>     e
;;>     g
;;>     i
;;>     (a c e g i)
;;>     => (list-of (list x y) [x <- 3 and y <- 'x])
;;>     ((0 x) (1 x) (2 x))
;;>     => (list-of (list x y) [x <- 3 and y <- 'x ..])
;;>     ((0 x) (1 x) (2 x))
;;>     => (list-of (list x y) [x <- #\0 .. and y <- '(a b c d)])
;;>     ((#\0 a) (#\1 b) (#\2 c) (#\3 d))
;;>     => (list-of x [x <- '(1 2 3) then (cdr x) until (null? x)])
;;>     ((1 2 3) (2 3) (3))
;;>     => (list-of (list x y)
;;>          [x <- '(1 2 3) then (cdr y) until (null? x) and
;;>           y <- '(10 20 30) then (cdr x) until (null? y)])
;;>     (((1 2 3) (10 20 30)) ((20 30) (2 3)) ((3) (30)))
;;>     => (list-of x [x <- (lambda (yield) 42)])
;;>     ()
;;>     => (list-of x [x <- (lambda (yield) (yield 42))])
;;>     (42)
;;>     => (list-of x [x <- (lambda (yield) (yield (yield 42)))])
;;>     (42 42)
;;>     => (list-of x [x <- (lambda (yield)
;;>                           (for-each (lambda (x) (echo x) (yield x))
;;>                                     '(3 2 1 0)))])
;;>     3
;;>     2
;;>     1
;;>     0
;;>     (3 2 1 0)
;;>     => (list-of x [x <- (lambda (yield)
;;>                           (for-each (lambda (x) (echo x) (yield (/ x)))
;;>                                     '(3 2 1 0)))])
;;>     3
;;>     2
;;>     1
;;>     0
;;>     /: division by zero
;;>     => (list-of x
;;>          [c <- 3 and
;;>           x <- (lambda (yield)
;;>                  (for-each (lambda (x) (echo x) (yield (/ x)))
;;>                            '(3 2 1 0)))])
;;>     3
;;>     2
;;>     1
;;>     (1/3 1/2 1)
;;>     => (define h (make-hash-table))
;;>     => (set! (hash-table-get h 'x) 1
;;>              (hash-table-get h 'y) 2
;;>              (hash-table-get h 'z) 3)
;;>     => (list-of x [x <- h])
;;>     ((y . 2) (z . 3) (x . 1))
;;>     => (list-of x [x <- 4 <- 4 .. 0 <- '(1 2 3)])
;;>     (0 1 2 3 4 3 2 1 0 1 2 3)
;;>     => (list-of (list x y)
;;>          [x <- 1 .. 3 <- '(a b c) and
;;>           y <- (lambda (y) (y 'x) (y 'y)) <- "abcd"])
;;>     ((1 x) (2 y) (3 #\a) (a #\b) (b #\c) (c #\d))
;;>
;;>   Note that parallel iteration is useful both for enumerating results,
;;>   and for walking over a finite prefix of an infinite iteration.
;;>
;;>   The following is an extensive list of various ranges:
;;>     => (list-of x [x <- 0 .. 6])
;;>     (0 1 2 3 4 5 6)
;;>     => (list-of x [x <- 0 ..< 6])
;;>     (0 1 2 3 4 5)
;;>     => (list-of x [x <- 0 .. -6])
;;>     (0 -1 -2 -3 -4 -5 -6)
;;>     => (list-of x [x <- 0 ..< -6])
;;>     (0 -1 -2 -3 -4 -5)
;;>     => (list-of x [x <- 0 2 .. 6])
;;>     (0 2 4 6)
;;>     => (list-of x [x <- 0 2 ..< 6])
;;>     (0 2 4)
;;>     => (list-of x [x <- 0 -2 ..< -6])
;;>     (0 -2 -4)
;;>     => (list-of x [x <- #\a .. #\g])
;;>     (#\a #\b #\c #\d #\e #\f #\g)
;;>     => (list-of x [x <- #\a ..< #\g])
;;>     (#\a #\b #\c #\d #\e #\f)
;;>     => (list-of x [x <- #\a #\c .. #\g])
;;>     (#\a #\c #\e #\g)
;;>     => (list-of x [x <- #\a #\c ..< #\g])
;;>     (#\a #\c #\e)
;;>     => (list-of x [x <- #\g #\e ..< #\a])
;;>     (#\g #\e #\c)
;;>     => (list-of x [x <- 6 5 .. zero?])
;;>     (6 5 4 3 2 1 0)
;;>     => (list-of x [x <- 6 5 ..< zero?])
;;>     (6 5 4 3 2 1)
;;>     => (list-of x [x <- 6 5 .. until zero?])
;;>     (6 5 4 3 2 1)
;;>     => (list-of x [x <- 6 5 .. while positive?])
;;>     (6 5 4 3 2 1)
;;>     => (list-of x [x <- '(1 2 3) cdr .. null?])
;;>     ((1 2 3) (2 3) (3) ())
;;>     => (list-of x [x <- '(1 2 3) cdr ..< null?])
;;>     ((1 2 3) (2 3) (3))
;;>     => (list-of x [x <- '(1 2 3) cdr .. until null?])
;;>     ((1 2 3) (2 3) (3))
;;>     => (list-of x [x <- '(1 2 3) cdr .. while pair?])
;;>     ((1 2 3) (2 3) (3))
;;>     => (list-of x [x <- #\a #\d .. while char-alphabetic?])
;;>     (#\a #\d #\g #\j #\m #\p #\s #\v #\y)
;;>     => (list-of x [x <- #\a #\d .. char-alphabetic?])
;;>     (#\a)
;;>     => (list-of x [x <- #\a #\d ..< char-alphabetic?])
;;>     ()
;;>     => (list-of x [x <- 0 1 .. positive?])
;;>     (0 1)
;;>     => (list-of x [x <- 1 2 .. positive?])
;;>     (1)
;;>     => (list-of x [x <- 1 2 ..< positive?])
;;>     ()
;;>     => (list-of x [x <- '(a b c) ..< pair?])
;;>     ()
;;>     => (list-of x [x <- '(a b c) .. pair?])
;;>     ((a b c))
;;>     => (list-of x [x <- '(a b c) cdr .. pair?])
;;>     ((a b c))
;;>     => (list-of x [x <- read-line .. eof-object?])
;;>     ...list of remaining input lines, including #<eof>...
;;>     => (list-of x [x <- read-line ..< eof-object?])
;;>     ...list of remaining input lines, excluding #<eof>...
;;>     => (list-of x [x <- read-line ..< eof])
;;>     ...the same...
;;>

;;>> collect-final
;;>   This value can be used to terminate iterations: when it is returned as
;;>   the iteration value (not the state), the iteration will terminate
;;>   without using it.
(define* collect-final (list "*"))

;;>> (function-iterator f [final-value])
;;>   `f' is expected to be a function that can accept a single input value.
;;>   It is applied on a `yield' function that can be used to return a value
;;>   at any point.  The return value is a function of no argument, which
;;>   returns on every application values that were passed to `yield'.  When
;;>   `f' terminates, the final result of the iterated return value depends
;;>   on the optional argument -- if none was supplied, the actual return
;;>   value is returned, if a thunk was supplied it is applied for a return
;;>   value, and if any other value was given it is returned.  After
;;>   termination, calling the iterated function again results in an error.
;;>   (The supplied `yield' function returns its supplied value to the
;;>   calling context when resumed.)
;;>     => (define (foo yield) (yield 1) (yield 2) (yield 3))
;;>     => (define bar (function-iterator foo))
;;>     => (list (bar) (bar) (bar))
;;>     (1 2 3)
;;>     => (bar)
;;>     3
;;>     => (bar)
;;>     function-iterator: iterated function #<procedure:foo> exhausted.
;;>     => (define bar (function-iterator foo 'done))
;;>     => (list (bar) (bar) (bar) (bar))
;;>     (1 2 3 done)
;;>     => (bar)
;;>     function-iterator: iterated function #<procedure:foo> exhausted.
;;>     => (define bar (function-iterator foo (thunk (error 'foo "done"))))
;;>     => (list (bar) (bar) (bar))
;;>     (1 2 3)
;;>     => (bar)
;;>     foo: done
(define* (function-iterator f . finally)
  (define ret #f)
  (define (done)
    (set! cnt (thunk (error 'function-iterator
                            "iterated function ~e exhausted." f))))
  (define cnt
    (cond [(null? finally) (thunk (let ([r (f yield)]) (done) (ret r)))]
          [(and (procedure? (car finally))
                (procedure-arity-includes? (car finally) 0))
           (thunk (f yield) (done) (ret ((car finally))))]
          [else (thunk (f yield) (done) (ret (car finally)))]))
  (define (yield v) (let/cc k (set! cnt (thunk (k v))) (ret v)))
  (thunk (let/cc ret1 (set! ret ret1) (cnt))))

;;>> (collect-iterator sequence)
;;>> (collect-numerator from second to [flag])
;;>   These functions are used to construct iterations.  `collect-iterator'
;;>   is the function used to create iteration over a sequence object and it
;;>   is used by `(x <- sequence)' forms of `collect'.  `collect-numerator'
;;>   create range iterations specified with `(x <- from second to)' forms,
;;>   where unspecified values are passed as `#f', and the flag argument is
;;>   a `<', `while', or `until' symbol for ranges specified with "..<",
;;>   ".. while" and ".. until".  These functions are available for
;;>   implementing new iteration constructs, for example:
;;>     => (define (in-values producer)
;;>          (collect-iterator (call-with-values producer list)))
;;>     => (list-of x [x <- in-values (thunk (values 1 2 3))])
;;>     (1 2 3)
;;>   The return value that specifies an iteration is a list of four items:
;;>   1. the initial state value;
;;>   2. a `step' function that gets a state and returns the next one;
;;>   3. a predicate for the end state (#f for none);
;;>   4. a function that computes a value from the state variable.
;;>   But usually the functions are more convenient.
;;>
;;>   Finally, remember that you can return `collect-final' as the value to
;;>   terminate any iteration.

;; ----------------------------------------------------------------------------
;;>>... Convenient printing

;;>> *echo-display-handler* [h]
;;>> *echo-write-handler*   [h]
;;>   Currently, Racket's I/O can be customized only on a per port basis.
;;>   This means that installing the object printing generic later will
;;>   change only the standard ports, and for new ports a handleres should
;;>   always be installed.  This means that `echos' will not work with
;;>   objects since it uses a new port -- so use these parameters to allow
;;>   to change them later to the Swindle printer.
(define* *echo-display-handler* (make-parameter display))
(define* *echo-write-handler* (make-parameter write))

;;>> (echo arg ...)
;;>   This is a handy printout utility that offers an alternative approach
;;>   to `printf'-like output (it's a syntax, but it can be used as a
;;>   regular function too, see below).  When applied, it simply prints its
;;>   arguments one by one, using certain keywords to control its behavior:
;;>   * :>e     - output on the current-error-port;
;;>   * :>o     - output on the current-output-port (default);
;;>   * :>s     - accumulate output in a string which is the return value
;;>               (string output sets `:n-' as default (unless
;;>               pre-specified));
;;>   * :> p    - output on the given port `p', or a string if `#f';
;;>   * :>> o   - use `o', a procedure that gets a value and a port, as the
;;>               output handler (the procedure can take one value and
;;>               display it on the current output port);
;;>   * :d      - use `display' output (default);
;;>   * :w      - use `write' output;
;;>   * :d1 :w1 - change to a `display' or `write' output just for the next
;;>               argument;
;;>   * :s-     - no spaces between arguments;
;;>   * :s+     - add spaces between arguments (default);
;;>   * :n-     - do not print a final newline;
;;>   * :n+     - terminate the output with a newline (default);
;;>   * :n      - output a newline now;
;;>   * : or :: - avoid a space at this point;
;;>   * :\{     - begin a list construct (see below).
;;>   Keywords that require additional argument are ignored if no argument
;;>   is given.
;;>
;;>   Recursive processing of a list begins with a `:\{' and ends with a
;;>   `:\}' (which can be simpler if `read-curly-brace-as-paren' is off).
;;>   Inside a list context, values are inspected and any lists cause
;;>   iteration for all elements.  In each iteration, all non-list arguments
;;>   are treated normally, but lists are dissected and a single element is
;;>   printed in each step, terminating when the shortest list ends (and
;;>   repeating a last `dotted' element of a list):
;;>     => (define abc '(a b c))
;;>     => (echo :\{ "X" abc :\})
;;>     X a X b X c
;;>     => (echo :\{ "X" abc '(1 2 3 4) :\})
;;>     X a 1 X b 2 X c 3
;;>     => (echo :\{ "X" abc '(1 . 2) :\})
;;>     X a 1 X b 2 X c 2
;;>   Inside a list context, the `:^' keyword can be used to stop this
;;>   iteration if it is the last:
;;>     => (echo :s- :\{ abc :^ ", " :\})
;;>     a, b, c
;;>   Nesting of lists is also simple, following these simple rules, by
;;>   nesting the `:\{' ... `:\}' construct:
;;>     => (echo :s- :\{ "<" :\{ '((1 2) (3 4 5) 6 ()) :^ "," :\} ">"
;;>                      :^ "-" :\})
;;>     <1,2>-<3,4,5>-<6>-<>
;;>   Note that this example is similar to the CL `format':
;;>     (format t "~{<~{~a~^,~}>~^-~}" '((1 2) (3 4 5) 6 ()))
;;>   except that `echo' treats a dotted element (a non-list in this case)
;;>   as repeating as needed.
;;>
;;>   There are two additional special keywords that are needed only in
;;>   uncommon situations:
;;>   * :k-  - turn off keyword processing
;;>   * :k+  - turn keyword processing on
;;>   Usually, when `echo' is used, it is processed by a macro that detects
;;>   all keywords, even if there is a locally bound variable with a keyword
;;>   name.  This means that keywords are only ones that are syntactically
;;>   so, not expressions that evaluate to keywords.  The two cases where
;;>   this matters are -- when `echo' is used for its value (using it as a
;;>   value, not in a head position) no processing is done so all keywords
;;>   will just get printed; and when `echo' is used in a context where a
;;>   variable has a keyword name and you want to use its value (which not a
;;>   great idea anyway, so there is no way around it).  The first case is
;;>   probably more common, so the variable `echo:' is bound to a special
;;>   value that will force treating the next value as a keyword (if it
;;>   evaluates to one) -- it can also be used to turn keyword processing on
;;>   (which means that all keyword values will have an effect).  Here is a
;;>   likely examples where `echo:' should be used:
;;>     => (define (echo-values vals)
;;>          (apply echo "The given values are:" echo: :w vals))
;;>     => (echo-values '("a" "b" "c"))
;;>     The given values are: "a" "b" "c"
;;>     => (echo-values '(:a :b :c))
;;>     The given values are: :a :b :c
;;>   And here are some tricky examples:
;;>     => (echo :>s 2)
;;>     "2"
;;>     => (define e echo)                 ; `e' is the real `echo' function
;;>     => (e :>s 2)                       ; no processing done here
;;>     :>s 2
;;>     => (e echo: :>s 2)                 ; explicit key
;;>     "2"
;;>     => (e echo: :k+ :>s 2)             ; turn on keywords
;;>     "2"
;;>     => (let ([:>s 1]) (echo :>s 2))    ; `:>s' was processed by `echo'
;;>     "2"
;;>     => (let ([:>s 1]) (e :>s 2))       ; `:>s' was not processed
;;>     1 2
;;>     => (let ([:>s 1]) (e echo: :>s 2)) ; `:>s' is not a keyword here!
;;>     1 2
;;>     => (let ([:>s 1]) (echo echo: :>s 2)) ; `echo:' not needed
;;>     "2"
;;>
;;>   Finally, it is possible to introduce new keywords to `echo'.  This is
;;>   done by calling it with the `:set-user' keyword, which expects a
;;>   keyword to attach a handler to, and the handler itself.  The handler
;;>   can be a simple value or a keyword that will be used instead:
;;>     => (echo :set-user :foo "foo")
;;>     => (echo 1 :foo 2)
;;>     1 foo 2
;;>     => (echo :set-user :foo :n)
;;>     => (echo 1 :foo 2)
;;>     1
;;>     2
;;>   The `:set-user' keyword can appear with other arguments, it has a
;;>   global effect in any case:
;;>     => (echo 1 :foo :set-user :foo "FOO" 2 :foo 3
;;>              :set-user :foo "bar" :foo 4)
;;>     1
;;>     2 FOO 3 bar 4
;;>     => (echo 1 :foo 2)
;;>     1 bar 2
;;>   If the handler is a function, then when this keyword is used, the
;;>   function is applied on arguments pulled from the remaining `echo'
;;>   arguments that follow (if the function can get any number of
;;>   arguments, then all remaining arguments are taken).  The function can
;;>   work in two ways: (1) when it is called, the `current-output-port'
;;>   will be the one that `echo' currently prints to, so it can just print
;;>   stuff; (2) if the function returns a list (or a single value which is
;;>   not `#f' or `void'), then these values will be used instead of the
;;>   taken arguments.  Some examples:
;;>     => (echo :set-user :foo (thunk "FOO") 1 :foo 2)
;;>     1 FOO 2
;;>     => (echo :set-user :add1 add1 1 :add1 2)
;;>     1 3
;;>     => (echo :set-user :+1 (lambda (n) (list n '+1= (add1 n))) :+1 2)
;;>     2 +1= 3
;;>     => (echo :set-user :<> (lambda args (append '("<") args '(">")))
;;>              :<> 1 2 3)
;;>     < 1 2 3 >
;;>   Care should be taken when user keywords are supposed to handle other
;;>   keywords -- the `echo:' tag will usually be among the arguments except
;;>   when `:k+' was used and an argument value was received.  This exposes
;;>   the keyword treatment hack and might change in the future.
;;>
;;>   To allow user handlers to change settings temporarily, there are
;;>   `:push' and `:pop' keywords that will save and restore the current
;;>   state (space and newline flags, output type and port etc).  For
;;>   example:
;;>     => (echo :set-user :@
;;>              (lambda (l)
;;>                (echo-quote
;;>                 list :push :s- :\{ "\"" l "\"" :^ ", " :\} :pop)))
;;>     => (echo 1 :@ '(2 3 4) 5)
;;>     1 "2", "3", "4" 5
;;>   The above example shows another helper tool -- the `echo-quote'
;;>   syntax: `(echo-quote head arg ...)' will transform into `(head ...)',
;;>   where keyword arguments are prefix with the `echo:' tag.  Without it,
;;>   things would look much worse.
;;>
;;>   In addition to `:set-user' there is an `:unset-user' keyword which
;;>   cancels a keyword handler.  Note that built-in keywords cannot be
;;>   overridden or unset.

;;>> (echo-quote head arg ...) [h]
;;>   This macro will result in `(head arg ...)', where all keywords in the
;;>   argument list are preceded with the `echo:' tag.  It is a convenient
;;>   form to use for defining new echo keyword handlers.
(defsyntax* (echo-quote stx)
  (define (process args)
    (syntax-case args ()
      [() #'()]
      [(x . more) (with-syntax ([more (process #'more)])
                    (if (syntax-keyword? #'x)
                      ;; `datum' protects from using a local binding
                      #'(echo: (#%datum . x) . more) #'(x . more)))]
      [x #'x])) ; only in case of (echo ... . x)
  (syntax-case stx ()
    [(_ head . args) (quasisyntax/loc stx (head . #,(process #'args)))]))

(provide (rename echo-syntax echo))
(defsyntax (echo-syntax stx)
  (syntax-case stx ()
    [(_ . args) (syntax/loc stx (echo-quote echo . args))]
    [_ #'echo]))

;; A table for user-defined keywords
(define echo-user-table (make-hash-table))

;; Make an echo keyword handler for a given procedure.  The handler gets the
;; current list of arguments and returns the new list of arguments.
(define (make-echo-handler keyword proc)
  (let* ([arity (procedure-arity proc)]
         [at-least (and (arity-at-least? arity)
                        (arity-at-least-value arity))]
         [required (or at-least arity)])
    (unless (integer? required)
      (error 'echo "handler function for `~.s' has bad arity" keyword))
    (lambda (args)
      (if (< (length args) required)
        (error 'echo "user-keyword `~.s' didn't get enough arguments" keyword)
        (let*-values ([(proc-args rest-args)
                       (if at-least
                         (values args '())
                         (let loop ([rest args] [args '()] [n required])
                           (if (zero? n)
                             (values (reverse args) rest)
                             (loop (cdr rest) (cons (car rest) args)
                                   (sub1 n)))))]
                      [(result) (apply proc proc-args)])
          (cond [(list? result) (append result rest-args)]
                [(and result (not (void? result)))
                 (if (keyword? result)
                   (list* echo: result rest-args) (cons result rest-args))]
                [else rest-args]))))))

(define (echo . args)
  (define break: "break:")
  (define call:  "call:")
  (let ([printer (*echo-display-handler*)] [out (current-output-port)]
        [spaces? #t] [newline? 'x] [first? #t] [str? #f] [keys? #f]
        [states '()])
    (define (getarg) (begin0 (car args) (set! args (cdr args))))
    (define (push-state!)
      (set! states (cons (list printer out spaces? newline? first? str? keys?)
                         states)))
    (define (pop-state!)
      (if (null? states)
        (error 'echo "tried to restore a state, but none saved")
        (let ([s (car states)])
          (set! states (cdr states))
          (set!-values (printer out spaces? newline? first? str? keys?)
                       (apply values s)))))
    (define (set-out! arg)
      (set! out (or arg (open-output-string)))
      (set! str? (not arg))
      (unless (output-port? out)
        (error 'echo "expected an output-port or #f, given ~e" out)))
    (define (printer1! hparam)
      (unless (or (null? args) (eq? echo: (car args)))
        (let ([p (hparam)])
          (unless (eq? printer p)
            (let ([v (getarg)] [op printer])
              (set! printer p)
              (set! args (list* v echo: :>> op args)))))))
    (define (process-list)
      (define level 1)
      (define ((do-lists args))
        ;; this returns a thunk so the whole thing is not expanded in one shot
        (let loop ([args args] [cars '()] [cdrs '()] [last? '?])
          (if (null? args)
            (reverse
             (if last? cars (list* (do-lists (reverse cdrs)) call: cars)))
            (let* ([1st (car args)] [p? (pair? 1st)])
              (if (and last? (eq? 1st break:))
                (reverse cars)
                (if (null? 1st)
                  '()
                  (loop (cdr args)
                        (if (eq? 1st break:)
                          cars (cons (if p? (car 1st) 1st) cars))
                        (cons (if p? (cdr 1st) 1st) cdrs)
                        (if p?
                          (or (eq? last? #t) (null? (cdr 1st)))
                          last?))))))))
      (let loop ([l-args '()])
        (define (pop-key-tags)
          (when (and (pair? l-args) (eq? echo: (car l-args)))
            (set! l-args (cdr l-args)) (pop-key-tags)))
        (when (null? args)
          (error 'echo "found a `~.s' with no matching `~.s'" :\{ :\}))
        (let ([arg (getarg)])
          (define (next) (loop (cons arg l-args)))
          (cond
           [(eq? arg echo:) (set! keys? (or keys? 'just-one)) (next)]
           [(and keys? (keyword? arg))
            (unless (eq? keys? #t) (set! keys? #f))
            (case arg
              [(:\})
               (set! level (sub1 level))
               (if (zero? level)
                 (begin
                   (pop-key-tags)
                   (set! args (append ((do-lists (reverse l-args))) args)))
                 (next))]
              [(:\{)
               (set! level (add1 level)) (next)]
              [(:^)
               (when (eq? 1 level) (set! arg break:) (pop-key-tags))
               (next)]
              [else (next)])]
           [else (next)]))))
    (let loop ()
      (unless (null? args)
        (let ([arg (getarg)])
          (cond
           [(eq? arg call:) (set! args (append ((getarg)) args))]
           [(eq? arg echo:) (set! keys? (or keys? 'just-one))]
           [(and keys? (keyword? arg))
            (unless (eq? keys? #t) (set! keys? #f))
            (case arg
              [(:>e)    (set-out! (current-error-port))]
              [(:>o)    (set-out! (current-output-port))]
              [(:>s)    (set-out! #f)]
              [(:>)     (unless (or (null? args) (eq? echo: (car args)))
                          (set-out! (getarg)))]
              [(:>>)    (unless (or (null? args) (eq? echo: (car args)))
                          (let ([p (getarg)])
                            (set! printer (if (eq? 1 (procedure-arity p))
                                            (lambda (x _) (p x)) p))))]
              [(:d)     (set! printer (*echo-display-handler*))]
              [(:w)     (set! printer (*echo-write-handler*))]
              [(:d1)    (printer1! *echo-display-handler*)]
              [(:w1)    (printer1! *echo-write-handler*)]
              [(:s-)    (set! spaces? (and spaces? (not first?) 'just-one))]
              [(:s+)    (set! spaces? #t)]
              [(:n-)    (set! newline? #f)]
              [(:n+)    (set! newline? #t)]
              [(:n)     (newline out) (set! first? #t)]
              [(:: :)   (set! first? #t)]
              [(:push)  (push-state!)]
              [(:pop)   (pop-state!)]
              [(:\{)    (process-list)]
              [(:\} :^) (error 'echo "unexpected list keyword `~.s'" arg)]
              [(:k-)    (set! keys? #f)]
              [(:k+)    (set! keys? #t)]
              [(:set-user :unset-user)
               (let loop ([keyword echo:])
                 (if (null? args)
                   (error 'echo "expecting a keyword+handler after `~.s'" arg)
                   (let ([x (getarg)])
                     (cond
                      [(eq? keyword echo:) (loop x)]
                      [(not (keyword? keyword))
                       (error 'echo "got a `~.s' with a non-keyword `~.s'"
                              arg keyword)]
                      [(eq? arg :unset-user)
                       (hash-table-put! echo-user-table keyword #f)]
                      [(eq? x echo:) (loop keyword)]
                      [else (let ([handler (if (procedure? x)
                                             (make-echo-handler keyword x) x)])
                              (hash-table-put! echo-user-table keyword handler)
                              (when (and newline? (not (eq? #t newline))
                                         (null? args))
                                (set! newline? #f)))]))))]
              [else
               (let ([user (hash-table-get echo-user-table arg (thunk #f))])
                 (if user
                   (set! args
                         (cond [(procedure? user) (user args)]
                               [(keyword? user) (list* echo: user args)]
                               [else (cons user args)]))
                   (error 'echo "unknown keyword: `~.s'" arg)))])]
           [first?  (printer arg out) (set! first? #f)]
           [spaces? (display " " out) (printer arg out)
                    (unless (eq? spaces? #t) (set! spaces? #f))]
           [else (printer arg out)])
          (loop))))
    (when (and newline? (or (not str?) (eq? newline? #t))) (newline out))
    (when str? (get-output-string out))))

;;>> (echos arg ...)
;;>   Just uses `echo' with `:>s'.
(provide (rename echos-syntax echos))
(defsyntax (echos-syntax stx)
  (syntax-case stx ()
    [(_ . args) (syntax/loc stx (echo-syntax :>s . args))]
    [_ #'echos]))
(define (echos . args)
  (echo echo: :>s . args))

;;>> echo:
;;>   See the `echo' description for usage of this value.
(define* echo: "echo:")

;; ----------------------------------------------------------------------------
;; Simple macros

;;>> (named-lambda name args body ...)
;;>   Like `lambda', but the name is bound to itself in the body.
(defsubst* (named-lambda name args . body)
  (letrec ([name (lambda args . body)]) name))

;;>> (thunk body ...)
;;>   Returns a procedure of no arguments that will have the given body.
(defsubst* (thunk body ...) (lambda () body ...))

;;>> (while condition body ...)
;;>> (until condition body ...)
;;>   Simple looping constructs.
(defsubst* (while cond body ...)
  (let loop () (when cond (begin body ... (loop)))))
(defsubst* (until cond body ...)
  (while (not cond) body ...))

;;>> (dotimes (i n) body ...)
;;>   Loop `n' times, evaluating the body when `i' is bound to 0,1,...,n-1.
(defsubst* (dotimes [i n] body0 body ...)
  (let ([n* n])
    (let loop ([i 0])
      (when (< i n*) body0 body ... (loop (add1 i))))))

;;>> (dolist (x list) body ...)
;;>   Loop with `x' bound to elements of `list'.
(defsubst* (dolist [x lst] body0 body ...)
  (for-each (lambda (x) body0 body ...) lst))

;;>> (no-errors body ...)
;;>   Execute body, catching all errors and returning `#f' if one occurred.
(defsubst* (no-errors body ...)
  (with-handlers ([void (lambda (x) #f)]) body ...))
;;>> (no-errors* body ...)
;;>   Execute body, catching all errors and returnsthe exception if one
;;>   occured.
(defsubst* (no-errors* body ...)
  (with-handlers ([void identity]) body ...))

;;>> (regexp-case string clause ...)
;;>   Try to match the given `string' against several regexps.  Each clause
;;>   has one of the following forms:
;;>   * (re => function): if `string' matches `re', apply `function' on the
;;>     resulting list.
;;>   * ((re args ...) body ...): if `string' matches `re', bind the tail of
;;>     results (i.e, excluding the whole match result) to the given
;;>     arguments and evaluate the body.  The whole match result (the first
;;>     element of `regexp-match') is bound to `match'.
;;>   * (re body ...): if `string' matches `re', evaluate the body -- no
;;>     match results are available.
;;>   * (else body ...): should be the last clause which is evaluated if all
;;>     previous cases failed.
(defsyntax* (regexp-case stx)
  (define (do-clause c)
    (syntax-case c (else base-else => base-=>)
      [(else body ...) c]
      [(base-else body ...) #'(else body ...)]
      [(re => func) #'((regexp-match re s) => (lambda (r) (apply func r)))]
      [(re base-=> func) #'((regexp-match re s) => (lambda (r) (apply func r)))]
      [((re . args) body ...)
       #`((regexp-match re s) =>
          (lambda (r)
            (apply (lambda (#,(datum->syntax-object c 'match c) . args)
                     body ...)
                   r)))]
      [(re body ...) #'((regexp-match re s) body ...)]))
  (syntax-case stx ()
    [(_ str clause ...)
     #`(let ([s str])
         (cond #,@(map do-clause (syntax->list #'(clause ...)))))]))
