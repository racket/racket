;;;
;;; DISPATCHING
;;;

(module dispatching mzscheme
  (provide (all-defined))
  
  (require-for-syntax "expansion.scm" "generators.scm")
  (require "expansion.scm" "generators.scm")
  
  (define-generator (:dispatched form-stx)
    (syntax-case form-stx (index)
      [(_ var (index i) dispatch expr1 expr2 ...)
       #'(:parallel (:integers i)
                    (:dispatched var dispatch expr1 expr2 ...))]
      [(_ var dispatch expr1 expr2 ...)
       #'(:do (let ([d dispatch]
                    [args (list expr1 expr2 ...)]
                    [g #f]
                    [empty (list #f)])
                (set! g (d args))
                (if (not (procedure? g))
                    (error "unrecognized arguments in dispatching"
                           args (d '()))))
              ((var (g empty)))
              (not (eq? var empty))
              (let ())
              #t
              ((g empty)))]))
  
  ; Despite the name, this isn't a generator.
  ; It's syntax used to make a first-order generator from generator syntax.
  ; TODO: insert ec-simplify
  (define-syntax :generator-proc
    (lambda (form-stx)
      (syntax-case form-stx (:do let) 
        [(_ (gen expr1 expr2 ...))
         (with-syntax ([(var) (generate-temporaries #'(empty))])
           (let ([loop (generator->loop #'(gen var expr1 expr2 ...))])
             (with-syntax
                 ([(obs (oc ...) ((lv li) ...)
                        ne1? (((i ...) v) ...) (ic ...) 
                        ne2? (ls ...) )
                   (loop-stx loop)])
               #'(let-values obs
                   oc ...
                   (let ((lv li) ... (ne2 #t))
                     (let-values (((i) #f) ... ...) ; v not yet valid
                       (lambda (empty)
                         (if (and ne1? ne2)
                             (begin 
                               (set!-values (i ...) v) ...
                               ic ...
                               (let ((value var))
                                 (if ne2?
                                     (begin (set! lv ls) ...)
                                     (set! ne2 #f) )
                                 value ))
                             empty ))))))))])))
  
  (define (dispatch-union d1 d2)
    (lambda (args)
      (let ((g1 (d1 args)) (g2 (d2 args)))
        (if g1
            (if g2 
                (if (null? args)
                    (append (if (list? g1) g1 (list g1)) 
                            (if (list? g2) g2 (list g2)) )
                    (error "dispatching conflict" args (d1 '()) (d2 '())) )
                g1 )
            (if g2 g2 #f) ))))
  
  (define (make-initial-:-dispatch)
    (lambda (args)
      (case (length args)
        ((0) 'SRFI42)
        ((1) (let ((a1 (car args)))
               (cond
                 ((list? a1)
                  (:generator-proc (:list a1)) )
                 ((string? a1)
                  (:generator-proc (:string a1)) )
                 ((vector? a1)
                  (:generator-proc (:vector a1)) )
                 ((and (integer? a1) (exact? a1))
                  (:generator-proc (:range a1)) )
                 ((real? a1)
                  (:generator-proc (:real-range a1)) )
                 ((input-port? a1)
                  (:generator-proc (:port a1)) )
                 (else
                  #f ))))
        ((2) (let ((a1 (car args)) (a2 (cadr args)))
               (cond
                 ((and (list? a1) (list? a2))
                  (:generator-proc (:list a1 a2)) )
                 ((and (string? a1) (string? a1))
                  (:generator-proc (:string a1 a2)) )
                 ((and (vector? a1) (vector? a2))
                  (:generator-proc (:vector a1 a2)) )
                 ((and (integer? a1) (exact? a1) (integer? a2) (exact? a2))
                  (:generator-proc (:range a1 a2)) )
                 ((and (real? a1) (real? a2))
                  (:generator-proc (:real-range a1 a2)) )
                 ((and (char? a1) (char? a2))
                  (:generator-proc (:char-range a1 a2)) )
                 ((and (input-port? a1) (procedure? a2))
                  (:generator-proc (:port a1 a2)) )
                 (else
                  #f ))))
        ((3) (let ((a1 (car args)) (a2 (cadr args)) (a3 (caddr args)))
               (cond
                 ((and (list? a1) (list? a2) (list? a3))
                  (:generator-proc (:list a1 a2 a3)) )
                 ((and (string? a1) (string? a1) (string? a3))
                  (:generator-proc (:string a1 a2 a3)) )
                 ((and (vector? a1) (vector? a2) (vector? a3))
                  (:generator-proc (:vector a1 a2 a3)) )
                 ((and (integer? a1) (exact? a1) 
                       (integer? a2) (exact? a2)
                       (integer? a3) (exact? a3))
                  (:generator-proc (:range a1 a2 a3)) )
                 ((and (real? a1) (real? a2) (real? a3))
                  (:generator-proc (:real-range a1 a2 a3)) )
                 (else
                  #f ))))
        (else
         (letrec ((every? 
                   (lambda (pred args)
                     (if (null? args)
                         #t
                         (and (pred (car args))
                              (every? pred (cdr args)) )))))
           (cond
             ((every? list? args)
              (:generator-proc (:list (apply append args))) )
             ((every? string? args)
              (:generator-proc (:string (apply string-append args))) )
             ((every? vector? args)
              (:generator-proc (:list (apply append (map vector->list args)))) )
             (else
              #f )))))))
  
  (define :-dispatch
    (make-initial-:-dispatch) )
  
  (define (:-dispatch-ref)
    :-dispatch )
  
  (define (:-dispatch-set! dispatch)
    (if (not (procedure? dispatch))
        (error "not a procedure" dispatch) )
    (set! :-dispatch dispatch) )
  
  (define-generator (: form-stx)
    (define (raise-error culprit)
      (raise-syntax-error
       '|: |
       "expected either (: <var> <expr>) or (: <var> (index <var>) <expr>), got:"
       form-stx culprit))
    (syntax-case form-stx (index)
      [(_ var (index i) arg1 arg ...)
       (begin
         (unless (identifier? #'var) (raise-error #'var))
         (unless (identifier? #'i) (raise-error #'i))
         (syntax/loc form-stx
           (:dispatched var (index i) :-dispatch arg1 arg ...)))]
      [(_ var arg1 arg ...)
       (unless (identifier? #'var) (raise-error #'var))
       (syntax/loc form-stx
         (:dispatched var :-dispatch arg1 arg ...))]
      [_ (raise-error form-stx)]))
  
  )
