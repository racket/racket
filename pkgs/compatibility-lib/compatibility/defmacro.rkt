#lang racket/base

;; defmacro - for legacy macros only

(require (for-syntax racket/base syntax/stx))

(provide define-macro
         defmacro)

(define-syntax define-macro
  (lambda (stx)
    (syntax-case stx ()
      [(_ (name . args) proc0 proc ...)
       (begin
         (unless (identifier? (syntax name))
           (raise-syntax-error
            #f
            "expected an identifier for the macro name"
            stx
            (syntax name)))
         (let loop ([args (syntax args)])
           (cond
            [(stx-null? args) 'ok]
            [(identifier? args) 'ok]
            [(stx-pair? args)
             (unless (identifier? (stx-car args))
               (raise-syntax-error
                #f
                "expected an identifier for a macro argument"
                stx
                (stx-car args)))
             (loop (stx-cdr args))]
            [else (raise-syntax-error
                   #f
                   "not a valid argument sequence after the macro name"
                   stx)]))
         (syntax
          (define-macro name (lambda args proc0 proc ...))))]
      [(_ name proc)
       (begin
         (unless (identifier? (syntax name))
           (raise-syntax-error
            #f
            "expected an identifier for the macro name"
            stx
            (syntax name)))
         (syntax
          (define-syntax name
            (let ([p proc])
              (unless (procedure? p)
                (raise-type-error
                 'define-macro
                 "procedure (arity 1)"
                 p))
              (lambda (stx)
                (let ([l (syntax->list stx)])
                  (unless (and l (procedure-arity-includes? p (sub1 (length l))))
                    (raise-syntax-error
                     #f
                     "bad form"
                     stx))
                  (let ([ht (make-hash)])
                    (datum->syntax
                     stx
                     (dm-subst
                      ht
                      (apply p (cdr (dm-syntax->datum stx ht))))
                     stx))))))))])))

(define-syntax defmacro
  (syntax-rules ()
      [(_ name formals body1 body ...)
       (define-macro (name . formals) body1 body ...)]))

;; helper submodule
;;
;; defined as a submodule because swindle requires it
(module dmhelp racket/base
  (require syntax/stx)

  (provide dm-syntax->datum
           dm-subst)

  ;; `dm-syntax->datum' is like syntax-object->datum, but it also
  ;; builds a hash table that maps generated data to original syntax
  ;; objects. The hash table can then be used with `dm-subst' to
  ;; replace each re-used, unmodified datum with the original syntax
  ;; object.

  (define (dm-syntax->datum stx ht)
    ;; Easiest to handle cycles by letting `syntax-object->datum'
    ;;  do all the work.
    (let ([v (syntax->datum stx)])
      (let loop ([stx stx][v v])
        (let ([already (hash-ref ht v (lambda () #f))])
          (if already
              (hash-set! ht v #t) ;; not stx => don't subst later
              (hash-set! ht v stx))
          (cond
           [(stx-pair? stx)
            (loop (stx-car stx) (car v))
            (loop (stx-cdr stx) (cdr v))]
           [(stx-null? stx) null]
           [(vector? (syntax-e stx))
            (for-each
             loop
             (vector->list
              (syntax-e stx))
             (vector->list v))]
           [(box? (syntax-e stx))
            (loop (unbox (syntax-e stx))
                  (unbox v))]
           [else (void)])))
      v))

  (define (dm-subst ht v)
    (define cycle-ht (make-hash))
    (let loop ([v v])
      (if (hash-ref cycle-ht v (lambda () #f))
          v
          (begin
            (hash-set! cycle-ht v #t)
            (let ([m (hash-ref ht v (lambda () #f))])
              (cond
               [(syntax? m) m] ;; subst back!
               [(pair? v) (cons (loop (car v))
                                (loop (cdr v)))]
               [(vector? v) (list->vector
                             (map
                              loop
                              (vector->list v)))]
               [(box? v) (box (loop (unbox v)))]
               [else v])))))))

;; this require has to be here after the submodule
(require (for-syntax 'dmhelp))
