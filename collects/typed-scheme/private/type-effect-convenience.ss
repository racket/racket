#lang scheme/base  
(require "../utils/utils.ss")

(require (rep type-rep effect-rep)
         (utils tc-utils)
         scheme/list
         scheme/match
         "type-comparison.ss"
         "type-effect-printer.ss"
         "union.ss"
         "subtype.ss"
         "type-utils.ss" 
         "type-abbrev.ss"
         scheme/promise
         (for-syntax stxclass)
         (for-syntax scheme/base)
         (for-template scheme/base scheme/contract scheme/tcp))

(provide (all-defined-out)
         (all-from-out "type-abbrev.ss")
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function make-Latent-Restrict-Effect make-Latent-Remove-Effect)

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (Un/eff . args)
  (apply Un (map tc-result-t args)))


(define-syntax (make-env stx)
  (syntax-case stx ()
    [(_ e ...)
     #`(list
        #,@(map (lambda (e)
                  (syntax-case e ()
                    [(nm ty)
                     (identifier? #'nm)
                     #`(list  #'nm ty)]
                    [(e ty extra-mods ...)
                     #'(let ([x (list (let ([new-ns
                                             (let* ([ns (make-empty-namespace)])
                                               (namespace-attach-module (current-namespace)
                                                                        'scheme/base
                                                                        ns)
                                               ns)])
                                        (parameterize ([current-namespace new-ns])
                                          (namespace-require 'scheme/base)
                                          (namespace-require 'extra-mods) ...
                                          e))
                                      ty)])
                         ;(display x) (newline)
                         x)]))
                (syntax->list #'(e ...))))]))

;; if t is of the form (Pair t* (Pair t* ... (Listof t*)))
;; return t*
;; otherwise, return t
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
        [(Mu: var (Union: (list (Value: '()) (Pair: _ (F: var))))) t*]
        [(Pair: t1 t2)
         (let ([t-new (loop t2)])
           (if (type-equal? (-lst t1) t-new)
               t-new
               (exit t)))]
        [_ (exit t)]))))



(define (opt-fn args opt-args result)
  (apply cl->* (for/list ([i (in-range (add1 (length opt-args)))])                         
                 (make-Function (list (make-arr* (append args (take opt-args i)) result))))))

(define-syntax-rule (->opt args ... [opt ...] res)
  (opt-fn (list args ...) (list opt ...) res))
