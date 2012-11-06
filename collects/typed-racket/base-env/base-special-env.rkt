#lang racket/base

;; this file cheats to define types for unexported variables that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 racket/promise
 string-constants/string-constant
 racket/private/kw racket/file racket/port syntax/parse racket/path
 (for-template (only-in racket/private/kw kw-expander-proc kw-expander-impl)
               racket/base racket/file racket/port racket/path racket/list)
 (utils tc-utils)
 (env init-envs)
 (except-in (rep filter-rep object-rep type-rep) make-arr)
 (rename-in (types abbrev numeric-tower union) [make-arr* make-arr])
 (for-syntax racket/base syntax/parse (only-in racket/syntax syntax-local-eval)))

(define (make-template-identifier what where)
  (let ([name (module-path-index-resolve (module-path-index-join where #f))])
    (parameterize ([current-namespace (make-empty-namespace)])
      (namespace-attach-module (current-namespace) ''#%kernel)
      (parameterize ([current-module-declare-name name])
        (eval `(,#'module any '#%kernel
                 (#%provide ,what)
                 (define-values (,what) #f))))
      (namespace-require `(for-template ,name))
      (namespace-syntax-introduce (datum->syntax #f what)))))


(define-initial-env initialize-special
  ;; make-promise
  [(make-template-identifier 'delay 'racket/private/promise)
   (-poly (a) (-> (-> a) (-Promise a)))]
  ;; language
  [(make-template-identifier 'language 'string-constants/string-constant)
   -Symbol]
  ;; qq-append
 [(make-template-identifier 'qq-append 'racket/private/qq-and-or)
  (-poly (a b)
        (cl->*
         (-> (-lst a) (-val '()) (-lst a))
         (-> (-lst a) (-lst b) (-lst (Un a b)))))]
  ;; make-sequence
  [(make-template-identifier 'make-sequence 'racket/private/for)
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
             (-> Univ -Byte         (seq-vals (list -Byte)))
             (-> Univ -Index        (seq-vals (list -Index)))
             ;; Generous. Negative numbers aren't allowed.
             (-> Univ -Fixnum       (seq-vals (list -NonNegFixnum)))
             (-> Univ -Int          (seq-vals (list -Nat)))
             (-> Univ (-seq a) (seq-vals (list a)))
             (-> Univ (-seq a b) (seq-vals (list a b))))))]
  ;; in-range
  [(make-template-identifier 'in-range 'racket/private/for)
   (cl->* (-> -Byte (-seq -Byte))
          (-> -Index (-seq -Index))
          (-> -Fixnum (-seq -Fixnum))
          (-> -Real (-seq -Int))
          (->opt -PosInt -Byte [-Int] (-seq -PosByte))
          (->opt -Nat -Byte [-Int] (-seq -Byte))
          (->opt -PosInt -Index [-Int] (-seq -PosIndex))
          (->opt -Nat -Index [-Int] (-seq -Index))
          (->opt -Nat -NonNegFixnum [-Int] (-seq -NonNegFixnum))
          (->opt -PosInt -Fixnum [-Nat] (-seq -PosFixnum))
          (->opt -Nat -Fixnum [-Nat] (-seq -NonNegFixnum))
          (->opt -Nat -Nat [-Int] (-seq -Nat))
          (->opt -PosInt -Int [-Nat] (-seq -PosInt))
          (->opt -Nat -Int [-Nat] (-seq -Nat))
          ;; could add cases that guarantee lists of negatives, etc.
          (->opt -Int -Real [-Int] (-seq -Int))
          (->opt -Rat -Real [-Rat] (-seq -Rat))
          (->opt -Flonum -Real [-Flonum] (-seq -Flonum))
          (->opt -SingleFlonum -Real [-SingleFlonum] (-seq -SingleFlonum))
          (->opt -InexactReal -Real [-InexactReal] (-seq -InexactReal))
          (->opt -Real -Real [-Real] (-seq -Real)))]
  ;; in-naturals
  [(make-template-identifier 'in-naturals 'racket/private/for)
   (cl->* (-> -PosInt (-seq -PosInt))
          (-> -Int (-seq -Nat)))]
  ;; in-list
  [(make-template-identifier 'in-list 'racket/private/for)
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-vector
  [(make-template-identifier 'in-vector 'racket/private/for)
   (-poly (a) (->opt (-vec a) [-Int (-opt -Int) -Int] (-seq a)))]
  ;; in-string
  [(make-template-identifier 'in-string 'racket/private/for)
   (->opt -String [-Int (-opt -Int) -Int] (-seq -Char))]
  ;; in-bytes
  [(make-template-identifier 'in-bytes 'racket/private/for)
   (->opt -Bytes [-Int (-opt -Int) -Int] (-seq -Byte))]
  ;; in-hash and friends
  [(make-template-identifier 'in-hash 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq a b)))]
  [(make-template-identifier 'in-hash-keys 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq a)))]
  [(make-template-identifier 'in-hash-values 'racket/private/for)
   (-poly (a b) (-> (-HT a b) (-seq b)))]
  ;; in-port
  [(make-template-identifier 'in-port 'racket/private/for)
   (->opt [(-> -Input-Port Univ) -Input-Port] (-seq Univ))]
  ;; in-input-port-bytes
  [(make-template-identifier 'in-input-port-bytes 'racket/private/for)
   (-> -Input-Port (-seq -Byte))]
  ;; in-input-port-chars
  [(make-template-identifier 'in-input-port-chars 'racket/private/for)
   (-> -Input-Port (-seq -Char))]
  ;; in-lines
  [(make-template-identifier 'in-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -String))]
  ;; in-bytes-lines
  [(make-template-identifier 'in-bytes-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -Bytes))]
  ;; check-in-bytes-lines
  [(make-template-identifier 'check-in-bytes-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-lines
  [(make-template-identifier 'check-in-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-port
  [(make-template-identifier 'check-in-port 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; from the expansion of `with-syntax'
  [(make-template-identifier 'apply-pattern-substitute 'racket/private/stxcase)
   (->* (list (-Syntax Univ) Univ) Univ Any-Syntax)]
  ;; same
  [(make-template-identifier 'with-syntax-fail 'racket/private/with-stx)
   (-> (-Syntax Univ) (Un))]
  ;; from the expansion of `make-temp-file`
  [(make-template-identifier 'make-temporary-file/proc 'racket/file)
   (->opt [-String (Un -Pathlike (-val 'directory) (-val #f)) (-opt -Pathlike)] -Path)]
  ;; from the (lifted) portion of the expansion of keyword lambdas
  [(make-template-identifier 'make-required 'racket/private/kw)
   (-> Univ Univ Univ Univ Univ)]
  [(make-template-identifier 'missing-kw 'racket/private/kw)
   (->* (list Univ) Univ Univ)]
  )
