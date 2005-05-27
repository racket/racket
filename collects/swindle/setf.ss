;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> This module provides the forms `setf!', `psetf!', and `setf!-values' for
;;> generic setters, much like CL's `setf', and `psetf', and a form similar
;;> to MzScheme's `set!-values'.  Note that when these are later re-exported
;;> (by `turbo'), they are renamed as `set!', `pset!', and `set!-values'
;;> (overriding the built-in `set!' and `set!-values').  Also, note that
;;> this just defines the basic functionality, the `misc' module defines
;;> many common setters.

(module setf mzscheme

;;>> (setf! place value ...)
;;>   Expand `(setf! (foo ...) v)' to `(set-foo! ... v)'.  The generated
;;>   `set-foo!' identifier has the same syntax context as `foo', which
;;>   means that to use this for some `foo' you need to define `set-foo!'
;;>   either as a function or a syntax in the same definition context of
;;>   `foo'.  The nice feature that comes out of this and the syntax system
;;>   is that examples like the following work as expected:
;;>     (let ([foo car] [set-foo! set-car!]) (setf! (foo a) 11))
;;>
;;>   `place' gets expanded before this processing is done so macros work
;;>   properly.  If the place is not a form, then this will just use the
;;>   standard `set!'.
;;>
;;>   Another extension of the original `set!' is that it allows changing
;;>   several places in sequence -- `(setf! x a y b)' will set `x' to `a'
;;>   and then set `y' to `b'.
;; Original idea thanks to Eric Kidd who stole it from Dylan
(provide setf!)
(define-syntax (setf! stx)
  (define (expand-place stx)
    (syntax-case stx ()
      [(_ var val)
       (quasisyntax/loc stx
         (_ #,(local-expand
               #'var 'expression
               (append '(#%app #%top #%datum)
                       (map (lambda (s) (datum->syntax-object #'var s #f))
                            '(#%app #%top #%datum))))
            val))]
      [else stx]))
  (syntax-case (expand-place stx) ()
    [(_ (getter args ...) val)
     (if (identifier? #'getter)
       (quasisyntax/loc stx
         (#,(datum->syntax-object
             #'getter
             (string->symbol
              (string-append "set-" (symbol->string (syntax-e #'getter)) "!"))
             #'getter #'getter)
          args ... val))
       (raise-syntax-error #f "not an identifier" stx #'getter))]
    [(_ var val)
     (syntax/loc stx (set! var val))]
    [(_ var val more ...)
     (let loop ([vas #'(var val more ...)] [r '()])
       (syntax-case vas ()
         [(v a more ...)
          (loop #'(more ...) (cons (syntax/loc stx (_ v a)) r))]
         [() (quasisyntax/loc stx (begin #,@(reverse! r)))]
         [_ (raise-syntax-error #f "uneven number of forms" stx)]))]))

;;>> (psetf! place value ...)
;;>   This is very similar to `setf!' above, except that the change to the
;;>   places is done *simultaneously*.  For example, `(setf! x y y x)'
;;>   switches the values of the two variables.
;; This could have been expressed using `setf!-values', but that would lead to
;; an unnecessary creation of a values tuple.
(provide psetf!)
(define-syntax (psetf! stx)
  (syntax-case stx ()
    ;; optimize common case
    [(_ place val) (syntax/loc stx (setf! place val))]
    [(_ more ...)
     (let loop ([vars '()] [vals '()] [more (syntax->list #'(more ...))])
       (cond
        [(null? more)
         (let ([vars (reverse vars)]
               [vals (reverse vals)]
               [tmps (generate-temporaries (map (lambda (x) 'x) vars))])
           (quasisyntax/loc stx
             (let #,(map (lambda (t v) #`(#,t #,v)) tmps vals)
               #,@(map (lambda (v t) #`(setf! #,v #,t)) vars tmps))))]
        [(null? (cdr more))
         (raise-syntax-error #f "uneven number of forms" stx)]
        [else (loop (cons (car more) vars) (cons (cadr more) vals)
                    (cddr more))]))]))

;;>> (setf!-values (place ...) expr)
;;>   This is a version of `setf!', that works with multiple values.  `expr'
;;>   is expected to evaluate to the correct number of values, and these are
;;>   then put into the specified places which can be an place suited to
;;>   `setf!'.  Note that no duplication of identifiers is checked, if an
;;>   identifier appears more than once then it will have the last assigned
;;>   value.
(provide setf!-values)
(define-syntax (setf!-values stx)
  (syntax-case stx ()
    ;; optimize common case
    [(_ (place) val) (syntax/loc stx (setf! place val))]
    [(_ (place ...) values)
     (with-syntax ([(temp ...) (datum->syntax-object
                                #'(place ...)
                                (generate-temporaries #'(place ...))
                                #'(place ...))])
       (syntax/loc stx
         (let-values ([(temp ...) values])
           (setf! place temp) ...)))]))

;;>> (set-values! places ... values-expr)
;;>> (set-list! places ... list-expr)
;;>> (set-vector! places ... vector-expr)
;;>   These are defined as special forms that use `setf!-values' to set the
;;>   given places to the appropriate components of the third form.  This
;;>   allows foing the following:
;;>     => (define (values a b c) (values 1 2 3))
;;>     => (setf! (values a b c) (values 11 22 33))
;;>     => (list a b c)
;;>     (11 22 33)
;;>     => (setf! (list a b c) (list 111 222 333))
;;>     => (list a b c)
;;>     (111 222 333)
;;>     => (setf! (list a b c) (list 1111 2222 3333))
;;>     => (list a b c)
;;>     (1111 2222 3333)
;;>   Furthermore, since the individual setting of each place is eventually
;;>   done with `setf!', then this can be used recursively:
;;>     => (set! (list a (vector b) (vector c c)) '(2 #(3) #(4 5)))
;;>     => (list a b c)
;;>     (2 3 5)
(provide set-values! set-list! set-vector!)
(define-syntaxes (set-values! set-list! set-vector!)
  (let ([make-setter
         (lambda (convert)
           (lambda (stx)
             (syntax-case stx ()
               [(_ x y ...)
                (let loop ([args (syntax->list #'(x y ...))] [as '()])
                  (if (null? (cdr args))
                    (quasisyntax/loc stx
                      (setf!-values #,(datum->syntax-object
                                       #'(x y ...) (reverse! as) #'(x y ...))
                                    #,(convert (car args))))
                    (loop (cdr args) (cons (car args) as))))])))])
    (values
     ;; set-values!
     (make-setter (lambda (x) x))
     ;; set-list!
     (make-setter (lambda (x) #`(apply values #,x)))
     ;; set-vector!
     (make-setter (lambda (x) #`(apply values (vector->list #,x)))))))

(provide shift! rotate! inc! dec! push! pop!)
(define-syntaxes (shift! rotate! inc! dec! push! pop!)
  (let* ([protect-indexes
          (lambda (place body)
            (syntax-case place ()
              [(getter . xs)
               (let ([bindings+expr
                      (let loop ([xs #'xs]
                                 [bindings '()]
                                 [expr (list #'getter)]
                                 [all-ids? #t])
                        (syntax-case xs ()
                          [() (and (not all-ids?)
                                   (cons (reverse! bindings) (reverse! expr)))]
                          [(x . xs)
                           (let ([new (datum->syntax-object
                                       #'x (gensym) #'x)])
                             (loop #'xs
                                   (cons (list new #'x) bindings)
                                   (cons new expr)
                                   (and (identifier? #'x) all-ids?)))]
                          [x (and (not (and all-ids? (identifier? #'x)))
                                  (let ([new (datum->syntax-object
                                              #'x (gensym) #'x)])
                                    (cons (reverse! (cons (list new #'x)
                                                          bindings))
                                          (append! (reverse! expr) new))))]))])
                 (if bindings+expr
                   #`(let #,(car bindings+expr) #,(body (cdr bindings+expr)))
                   (body place)))]
              [_ (body place)]))]
         [protect-indexes-list
          (lambda (places body)
            (let loop ([ps places] [r '()])
              (if (null? ps)
                (body (reverse! r))
                (protect-indexes (car ps) (lambda (p)
                                            (loop (cdr ps) (cons p r)))))))])
    (values
;;>> (shift! place ... newvalue)
;;>   This is similar to CL's `shiftf' -- it is roughly equivalent to
;;>     (begin0 place1
;;>             (psetf! place1 place2
;;>                     place2 place3
;;>                     ...
;;>                     placen newvalue))
;;>   except that it avoids evaluating index subforms twice, for example:
;;>     => (let ([foo (lambda (x) (printf ">>> ~s\n" x) x)]
;;>              [a '(1)] [b '(2)])
;;>          (list (shift! (car (foo a)) (car (foo b)) 3) a b))
;;>     >>> (1)
;;>     >>> (2)
;;>     (1 (2) (3))
     ;; --- shift!
     (lambda (stx)
       (syntax-case stx ()
         [(_ x y more ...)
          (protect-indexes-list (syntax->list #'(x y more ...))
            (lambda (vars)
              (let loop ([vs vars] [r '()])
                (if (null? (cdr vs))
                  (quasisyntax/loc stx
                    (let ([v #,(car vars)])
                      (psetf! #,@(datum->syntax-object
                                  #'(x y more ...)
                                  (reverse! r)
                                  #'(x y more ...)))
                      v))
                  (loop (cdr vs) (list* (cadr vs) (car vs) r))))))]))
;;>> (rotate! place ...)
;;>   This is similar to CL's `rotatef' -- it is roughly equivalent to
;;>     (psetf! place1 place2
;;>             place2 place3
;;>             ...
;;>             placen place1)
;;>   except that it avoids evaluating index subforms twice.
     ;; --- rotate!
     (lambda (stx)
       (syntax-case stx ()
         [(_ x) #'(void)]
         [(_ x xs ...)
          (protect-indexes-list (syntax->list #'(x xs ...))
            (lambda (vars)
              (let loop ([vs vars] [r '()])
                (if (null? (cdr vs))
                  (quasisyntax/loc stx
                    (psetf! #,@(datum->syntax-object
                                #'(x xs ...)
                                (reverse! (list* (car vars) (car vs) r))
                                #'(x xs ...))))
                  (loop (cdr vs) (list* (cadr vs) (car vs) r))))))]))
;;>> (inc! place [delta])
;;>> (dec! place [delta])
;;>> (push! x place)
;;>> (pop! place)
;;>   These are some simple usages of `setf!'.  Note that they also avoid
;;>   evaluating any indexes twice.
     ;; --- inc!
     (lambda (stx)
       (syntax-case stx ()
         [(_ p) #'(_ p 1)]
         [(_ p d) (protect-indexes #'p
                    (lambda (p) #`(setf! #,p (+ #,p d))))]))
     ;; --- dec!
     (lambda (stx)
       (syntax-case stx ()
         [(_ p) #'(_ p 1)]
         [(_ p d) (protect-indexes #'p
                    (lambda (p) #`(setf! #,p (- #,p d))))]))
     ;; --- push!
     (lambda (stx)
       (syntax-case stx ()
         [(_ x p) (protect-indexes #'p
                    (lambda (p) #`(setf! #,p (cons x #,p))))]))
     ;; --- pop!
     (lambda (stx)
       (syntax-case stx ()
         [(_ p) (protect-indexes #'p
                  (lambda (p)
                    #`(let ([p1 #,p])
                        (begin0 (car p1) (setf! #,p (cdr p1))))))])))))

)
