#lang racket/base

;; this file cheats to define types for unexported variables that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 racket/promise
 string-constants/string-constant 
 (for-syntax racket/base syntax/parse (only-in unstable/syntax syntax-local-eval)
             (utils tc-utils)
             (env init-envs)          
             (except-in (rep filter-rep object-rep type-rep) make-arr)
             (types convenience union)
             (only-in (types convenience) [make-arr* make-arr])))

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ...)
     (with-syntax ([(id ...)
                    (for/list ([expr (syntax->list #'(id-expr ...))])
                      (syntax-local-eval expr))])
       #`(begin
           (define-for-syntax initial-env
             (make-env
              [id ty] ...))
           (define-for-syntax (initialize-env)
             (initialize-type-env initial-env))
           (provide (for-syntax initialize-env))))]))


(define-initial-env initialize-special
  ;; make-promise
  [(syntax-parse (local-expand #'(delay 3) 'expression null)
     #:context #'make-promise
     [(_ mp . _) #'mp])
   (-poly (a) (-> (-> a) (-Promise a)))]
  ;; language
  [(syntax-parse (local-expand #'(this-language) 'expression null)
     #:context #'language
     [lang #'lang])
   -Symbol]
  ;; qq-append
  [(syntax-parse (local-expand #'`(,@'() 1) 'expression null)
     #:context #'qq-append
     [(_ qqa . _) #'qqa])
   (-poly (a b) 
         (cl->*
          (-> (-lst a) (-val '()) (-lst a))
          (-> (-lst a) (-lst b) (-lst (*Un a b)))))]
  ;; make-sequence
  [(syntax-parse (local-expand #'(for ([x '()]) x) 'expression #f)
     #:context #'make-sequence
     #:literals (let-values quote)
     [(let-values ([_ (m-s '(_) '())]) . _)
      #'m-s])
   (-poly (a) 
          (let ([seq-vals 
                 (lambda ([a a])
                   (-values (list 
                             (-> Univ a)
                             (-> Univ Univ)
                             Univ
                             (-> Univ Univ)
                             (-> a Univ)
                             (-> Univ a Univ))))])
            (-> Univ (-seq a) (seq-vals))
            #;
            (cl->* (-> Univ (-lst a) (seq-vals))
                   (-> Univ (-vec a) (seq-vals))
                   (-> Univ -String (seq-vals -Char))
                   (-> Univ -Bytes (seq-vals -Nat))
                   (-> Univ -Input-Port (seq-vals -Nat)))))]
  ;; in-range
  [(syntax-parse (local-expand #'(in-range 1) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-PositiveFixnum -Fixnum [-Nat] . ->opt . (-seq -PositiveFixnum))
          (-NonnegativeFixnum [-Fixnum -Nat] . ->opt . (-seq -NonnegativeFixnum))
          (-Fixnum [-Fixnum -Integer] . ->opt . (-seq -Fixnum))
          (-ExactPositiveInteger -Integer [-Nat] . ->opt . (-seq -ExactPositiveInteger))
          (-Nat [-Integer -Nat] . ->opt . (-seq -Nat))
          (-Integer [-Integer -Integer] . ->opt . (-seq -Integer)))]
  ;; in-naturals
  [(syntax-parse (local-expand #'(in-naturals) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-> -ExactPositiveInteger (-seq -ExactPositiveInteger))
          (-> -Integer (-seq -Nat)))]
  ;; in-list
  [(syntax-parse (local-expand #'(in-list '(1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-vector
  [(syntax-parse (local-expand #'(in-vector (vector 1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (->opt (-vec a) [-Integer (-opt -Integer) -Integer] (-seq a)))]
  ;; in-string
  [(syntax-parse (local-expand #'(in-string "abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -String [-Integer (-opt -Integer) -Integer] (-seq -Char))]
  ;; in-bytes
  [(syntax-parse (local-expand #'(in-bytes #"abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -Bytes [-Integer (-opt -Integer) -Integer] (-seq -Byte))]
  ;; in-port
  [(syntax-parse (local-expand #'(in-port) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [(-> -Input-Port Univ) -Input-Port] (-seq Univ))]
  ;; in-input-port-bytes
  [(syntax-parse (local-expand #'(in-input-port-bytes (open-input-bytes #"abc")) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-> -Input-Port (-seq -Byte))]
  ;; in-input-port-chars
  [(syntax-parse (local-expand #'(in-input-port-chars (open-input-string "abc")) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-> -Input-Port (-seq -Char))]
  ;; in-lines
  [(syntax-parse (local-expand #'(in-lines) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [-Input-Port -Symbol] (-seq -String))]
  ;; in-bytes-lines
  [(syntax-parse (local-expand #'(in-bytes-lines) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt [-Input-Port -Symbol] (-seq -Bytes))])

