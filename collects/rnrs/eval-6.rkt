#lang scheme/base

(require (only-in r6rs)
         (only-in r6rs/private/prelims)
         scheme/mpair
         r6rs/private/parse-ref)

(provide (rename-out [r6rs:eval eval])
         environment)

(define-namespace-anchor anchor)

(define (mpair->pair p)
  (cond
   [(mpair? p) (cons (mpair->pair (mcar p))
                     (mpair->pair (mcdr p)))]
   [(vector? p) (list->vector
                 (map mpair->pair
                      (vector->list p)))]
   [else p]))

(define (show v)
  (printf "~s\n" v)
  v)

(define (r6rs:eval expr env)
  (eval (datum->syntax #f `(#%expression ,(mpair->pair expr))) env))

(define (environment . specs)
  (let ([reqs
         (map (lambda (spec)
                (syntax->datum
                 (datum->syntax
                  #f
                  (parse-import
                   #'here
                   (mpair->pair spec)
                   (lambda (msg orig stx)
                     (error 'environment "~a: ~e" msg spec))))))
              specs)])
    (let ([ns (namespace-anchor->empty-namespace anchor)])
      ;; Make sure all modules are instantiated here:
      (parameterize ([current-namespace ns])
        (namespace-require '(rename scheme/base #%base-require require))
        (namespace-require '(only scheme/base #%expression))
        (eval `(#%base-require r6rs/private/prelims 
                               . ,(datum->syntax #'here (apply append reqs)))))
      ns)))

