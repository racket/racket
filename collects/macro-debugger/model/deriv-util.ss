
(module deriv-util mzscheme
  (require "deriv.ss"
           (lib "plt-match.ss"))
  (provide IntW
           ErrW
           AnyQ
           IntQ
           
           $$
           $$I
           $$E
           Wrap
           lift/wrap
           rewrap
           rewrap/nt
           outer-rewrap
           lift/deriv-e1
           lift/deriv-e2
           wrapped?)

  ;; IntW
  ;; Matches only interrupted wraps
  (define-match-expander IntW
    (syntax-rules ()
      [(IntW S (var ...))
       (struct interrupted-wrap (_ (struct S (var ...))))]
      [(IntW S (var ...) tag)
       (struct interrupted-wrap (tag (struct S (var ...))))]))

  ;; ErrW
  ;; Matches only error wraps
  (define-match-expander ErrW
    (syntax-rules ()
      [(ErrW S (var ...))
       (struct error-wrap (_ _ (struct S (var ...))))]
      [(ErrW S (var ...) exn)
       (struct error-wrap (exn _ (struct S (var ...))))]
      [(ErrW S (var ...) tag exn)
       (struct error-wrap (exn tag (struct S (var ...))))]))
  
  ;; AnyQ matcher
  ;; Matches unwrapped, interrupted wrapped, or error wrapped
  (define-match-expander AnyQ
    (syntax-rules ()
      [(AnyQ S (var ...))
       (or (struct S (var ...))
           (struct interrupted-wrap (_ (struct S (var ...))))
           (struct error-wrap (_ _ (struct S (var ...)))))]
      [(AnyQ S (var ...) exni)
       (or (and (struct S (var ...))
                (app (lambda (_) #f) exni))
           (and (struct interrupted-wrap (tag (struct S (var ...))))
                (app (lambda (ew) (cons #f (interrupted-wrap-tag ew))) exni))
           (and (struct error-wrap (exn tag (struct S (var ...))))
                (app (lambda (ew) (cons (error-wrap-exn ew) (error-wrap-tag ew))) exni)))]))

  ;; IntQ
  ;; Matches interrupted wraps and unwrapped structs
  (define-match-expander IntQ
    (syntax-rules ()
      [(IntQ S (var ...))
       (or (struct S (var ...))
           (struct interrupted-wrap (_ (struct S (var ...)))))]
      [(IntQ S (var ...) tag)
       (or (and (struct S (var ...))
                (app (lambda (_) #f) tag))
           (struct interrupted-wrap (tag (struct S (var ...)))))]))

  ;; $$ match form
  ;; ($$ struct-name (var ...) info)
  ;; If normal instance of struct-name, binds info to #f
  ;; If interrupted-wrapped, binds info to (cons #f symbol/#f)
  ;; If error-wrapped, binds info to (cons exn symbol/#f)
  (define-match-expander $$
    (lambda (stx)
      (syntax-case stx ()
        [($$ S (var ...) info)
         #'(or (and (struct S (var ...))
                    (app (lambda (_) #f) info))
               (and (struct interrupted-wrap (tag (struct S (var ...))))
                    (app (lambda (ew) (cons #f (interrupted-wrap-tag ew))) info))
               (and (struct error-wrap (exn tag (struct S (var ...))))
                    (app (lambda (ew) (cons (error-wrap-exn ew) (error-wrap-tag ew)))
                         info)))]
        [($$ S (var ...))
         #'(struct S (var ...))])))
  
  (define-match-expander $$I
    (lambda (stx)
      (syntax-case stx ()
        [($$I S (var ...))
         #'(or (struct interrupted-wrap (tag (struct S (var ...))))
               (struct S (var ...)))]
        [($$I S (var ...) tag)
         #'(or (struct interrupted-wrap (tag (struct S (var ...))))
               (and (app (lambda (_) #f) tag)
                    (struct S (var ...))))])))
  
  (define-match-expander $$E
    (lambda (stx)
      (syntax-case stx ()
        [($$E S (var ...))
         #'(or (struct interrupted-wrap (_tag (struct S (var ...))))
               (struct error-wrap (_exn _tag (struct S (var ...))))
               (struct S (var ...)))])))

  (define-match-expander Wrap
    (syntax-rules ()
      [(Wrap x)
       (or (struct interrupted-wrap (_tag x))
           (struct error-wrap (_exn _tag x))
           x)]))
  
  ;; lift/wrap : ('a -> 'b) boolean -> Wrap('a) -> Wrap('b)
  (define (lift/wrap f preserve-tag?)
    (lambda (x)
      (match x
        [(struct interrupted-wrap (tag inner))
         (make-interrupted-wrap (and preserve-tag? tag) (f inner))]
        [(struct error-wrap (exn tag inner))
         (make-error-wrap exn (and preserve-tag? tag) (f inner))]
        [x
         (f x)])))

  ;; rewrap : Wrap('a) 'b -> Wrap('b)
  (define (rewrap x y)
    (if (wrapped? y)
        y
        ((lift/wrap (lambda (x) y) #t) x)))
  
  ;; rewrap/nt : Wrap('a) 'b -> Wrap('b)
  (define (rewrap/nt x y)
    (if (wrapped? y)
        y
        ((lift/wrap (lambda (x) y) #f) x)))

  (define (outer-rewrap x y)
    (if (and (wrapped? x) (not (wrapped? y)))
        (make-interrupted-wrap #f y)
        y))
  
  (define (lift/deriv-e1 x)
    (match x
      [(AnyQ deriv (e1 _)) e1]))

  (define (lift/deriv-e2 x)
    (match x
      [(AnyQ deriv (_ e2)) e2]))
  
  (define (wrapped? x)
    (or (interrupted-wrap? x)
        (error-wrap? x)))

;  (define-match-expander $$E
;    (lambda (stx)
;      (syntax-case stx (@)
;        [($$E S (var ...))
;         #'($$ S (var ...) _exni)]
;        [($$E S (var ...) @ tag)
;         #'($$ S (var ...) (cons #f tag))]
;        [($$E S (var ...) @ tag exn)
;         #'($$ S (var ...) (cons exn tag))])))
  
  )
