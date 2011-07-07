#lang racket/base

;; this file cheats to define types for unexported variables that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 racket/promise
 string-constants/string-constant
 (for-syntax racket/base syntax/parse (only-in racket/syntax syntax-local-eval)
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
     [(let-values ([_ (m-s '(_) '())]) . _) #'m-s])
   (-poly (a b)
          (let ([seq-vals
                 (lambda (a)
                   (-values (list
                             (-> Univ (-values a))
                             (-> Univ Univ)
                             Univ
                             (Un (-> Univ Univ) (-val #f))
                             (Un (->* a Univ) (-val #f))
                             (Un (->* (cons Univ a) Univ) (-val #f)))))])
            (cl->*
             (-> Univ (-seq a) (seq-vals (list a)))
             (-> Univ (-seq a b) (seq-vals (list a b))))))]
  ;; in-range
  [(syntax-parse (local-expand #'(in-range 1) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-PosFixnum -Fixnum [-Nat] . ->opt . (-seq -PosFixnum))
          (-NonNegFixnum [-Fixnum -Nat] . ->opt . (-seq -NonNegFixnum))
          (-Fixnum [-Fixnum -Int] . ->opt . (-seq -Fixnum))
          (-PosInt -Int [-Nat] . ->opt . (-seq -PosInt))
          (-Nat [-Int -Nat] . ->opt . (-seq -Nat))
          (-Int [-Int -Int] . ->opt . (-seq -Int)))]
  ;; in-naturals
  [(syntax-parse (local-expand #'(in-naturals) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (cl->* (-> -PosInt (-seq -PosInt))
          (-> -Int (-seq -Nat)))]
  ;; in-list
  [(syntax-parse (local-expand #'(in-list '(1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-vector
  [(syntax-parse (local-expand #'(in-vector (vector 1 2 3)) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a) (->opt (-vec a) [-Int (-opt -Int) -Int] (-seq a)))]
  ;; in-string
  [(syntax-parse (local-expand #'(in-string "abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -String [-Int (-opt -Int) -Int] (-seq -Char))]
  ;; in-bytes
  [(syntax-parse (local-expand #'(in-bytes #"abc") 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (->opt -Bytes [-Int (-opt -Int) -Int] (-seq -Byte))]
  ;; in-hash and friends
  [(syntax-parse (local-expand #'(in-hash #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq a b)))]
  [(syntax-parse (local-expand #'(in-hash-keys #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq a)))]
  [(syntax-parse (local-expand #'(in-hash-values #hash((1 . 2))) 'expression #f)
     [(i-n _ ...)
      #'i-n])
   (-poly (a b) (-> (-HT a b) (-seq b)))]
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
   (->opt [-Input-Port -Symbol] (-seq -Bytes))]
  ;; check-in-bytes-lines
  [(syntax-parse (local-expand #'(for ([i (in-bytes-lines 0)]) i)
                               'expression #f)
                 #:literals (let-values let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)                   
      #'c])
   (-> Univ Univ Univ)]
  ;; check-in-lines
  [(syntax-parse (local-expand #'(for ([i (in-lines 0)]) i)
                               'expression #f)
                 #:literals (let-values #%app let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)                   
      #'c])
   (-> Univ Univ Univ)]
  ;; check-in-port
  [(syntax-parse (local-expand #'(for ([i (in-port 0)]) i)
                               'expression #f)
                 #:literals (let-values #%app let)
     [(let-values ((_ (let _ (c . _) . _))
                   . _)
        . _)                   
      #'c])
   (-> Univ Univ Univ)]
  ;; from the expansion of `with-syntax'
  [(syntax-parse (local-expand #'(with-syntax ([x 1]) #'(x)) 'expression null)
     #:literals (let-values #%plain-app #%plain-lambda if letrec-syntaxes+values)
     [(let-values _
        (let-values _
          (let-values _
            (if _
                (let-values _ (letrec-syntaxes+values _ _ (#%plain-app (#%plain-lambda _ (#%plain-app apply-pattern-substitute _ _ _)) _)))
                _))))
      #'apply-pattern-substitute])
   (->* (list (-Syntax Univ) Univ) Univ Any-Syntax)]

  [(syntax-parse (local-expand #'(with-syntax ([x 1]) #'(x)) 'expression null)
     #:literals (let-values #%plain-app #%plain-lambda if letrec-syntaxes+values)
    [(let-values _ (let-values _
                     (let-values _ (if _ _ (let-values _
                                             (if _ (let-values _ (letrec-syntaxes+values _ _ (#%plain-app with-syntax-fail _))) _))))))
      #'with-syntax-fail])
   (-> (-Syntax Univ) (Un))]
  )

