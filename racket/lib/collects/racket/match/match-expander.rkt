#lang racket/base

(require (for-syntax racket/base "stxtime.rkt"))

(provide define-match-expander)

(begin-for-syntax
  (define make-match-expander
    (let ()
      (define-struct match-expander (match-xform legacy-xform macro-xform)
        #:property prop:set!-transformer 
        (λ (me stx)
          (define xf (match-expander-macro-xform me))
          (if (set!-transformer? xf)
              ((set!-transformer-procedure xf) stx)
              (syntax-case stx (set!)
                [(set! . _)
                 (raise-syntax-error #f "cannot mutate syntax identifier" stx)]
                [_ (xf stx)])))
        #:property prop:match-expander (struct-field-index match-xform)
        #:property prop:legacy-match-expander (struct-field-index legacy-xform))
      (values make-match-expander))))

(define-syntax (define-match-expander stx)
  (define (lookup v alist)
    (cond [(assoc v alist) => cadr]
          [else #f]))
  (define (parse args)
    (let loop ([args args]
               [alist '()])
      (if (null? args)
        alist
        (let* ([stx-v (car args)]
               [v (syntax-e stx-v)])
          (cond
            [(not (keyword? v))
             (raise-syntax-error #f "argument must be a keyword" stx stx-v)]
            [(not (memq v '(#:expression #:plt-match #:match)))
             (raise-syntax-error
              #f (format "keyword argument ~a is not a correct keyword" v)
              stx stx-v)]
            [else
             (loop (cddr args) (cons (list v (cadr args)) alist))])))))
  (syntax-case stx ()
    [(_ id kw . rest)
     (keyword? (syntax-e #'kw))
     (let* ([args (syntax->list #'(kw . rest))]
            [parsed-args (parse args)])
       (with-syntax
           ([legacy-xform (lookup '#:match parsed-args)]
            [match-xform (lookup '#:plt-match parsed-args)]
            [macro-xform
             (or (lookup '#:expression parsed-args)
                 #'(lambda (stx)
                     (raise-syntax-error
                      #f "this match expander must be used inside match"
                      stx)))])
         (if (identifier? #'macro-xform)
           (syntax/loc stx
             (define-syntax id
               (make-match-expander
                match-xform
                legacy-xform
                (lambda (stx)
                  (syntax-case stx (set!)
                    [(nm . args) #'(macro-xform . args)]
                    [nm (identifier? #'nm) #'macro-xform]
                    [(set! . _)
                     (raise-syntax-error #f "match expander cannot be target of a set!" stx)])))))
           (syntax/loc stx
             (define-syntax id
               (make-match-expander match-xform legacy-xform macro-xform))))))]
    ;; implement legacy syntax
    [(_ id plt-match-xform match-xform std-xform)
     #'(define-match-expander id #:plt-match plt-match-xform
                                 #:match match-xform
                                 #:expression std-xform)]
    [(_ id plt-match-xform std-xform)
     #'(define-match-expander id #:plt-match plt-match-xform
                                 #:expression std-xform)]
    [(_ id plt-match-xform)
     #'(define-match-expander id #:plt-match plt-match-xform)]
    ;; error checking
    [_ (raise-syntax-error #f "invalid use of define-match-expander" stx)]))
