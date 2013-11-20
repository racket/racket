#lang racket

(require "test-utils.rkt"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (rep type-rep filter-rep object-rep)
         (for-syntax (rename-in (types utils union numeric-tower abbrev filter-ops)
                                [Un t:Un]
                                [-> t:->]))
         (utils tc-utils utils)
         (utils mutated-vars)

         rackunit rackunit/text-ui
         syntax/parse
         racket/file racket/port
         (for-syntax syntax/kerncase syntax/parse racket/syntax
                     (types abbrev numeric-tower utils)
                     (utils mutated-vars) (env mvar-env)
                     (utils tc-utils) (typecheck typechecker))
         typed-racket/base-env/prims
         typed-racket/base-env/base-types
         (only-in typed-racket/typed-racket do-standard-inits))

(begin-for-syntax (do-standard-inits))

(define-syntax-rule (tc-e/t e t) (tc-e e #:ret (ret t (-FS -top -bot))))

(define-syntax (tc-e stx)
  (syntax-parse stx
    [(_ expr ty) (syntax/loc stx (tc-e expr #:ret (ret ty)))]
    [(_ a #:ret b)
     (quasisyntax/loc stx
       (check-tc-result-equal? (format "~a ~a" #,(syntax-line stx) 'a)
                               #,(let ([ex (local-expand #'a 'expression null)])
                                   (find-mutated-vars ex mvar-env)
                                   (tc-expr ex))
                               #,(syntax-local-eval #'b)))]))

(define (typecheck-special-tests)
  (test-suite
   "Special Typechecker tests"
   ;; should work but don't -- need expected type
   #|
[tc-e (for/list ([(k v) (in-hash #hash((1 . 2)))]) 0) (-lst -Zero)]
[tc-e (in-list (list 1 2 3)) (-seq -Integer)]
[tc-e (in-vector (vector 1 2 3)) (-seq -Integer)]
|#

   [tc-e (in-hash #hash((1 . 2))) (-seq -Integer -Integer)]
   [tc-e (in-hash-keys #hash((1 . 2))) (-seq -Integer)]
   [tc-e (in-hash-values #hash((1 . 2))) (-seq -Integer)]

   (tc-e (file->string "tmp") -String)
   (tc-e (file->string "tmp" #:mode 'binary) -String)
   (tc-e (file->string "tmp" #:mode 'text) -String)

   (tc-e (file->bytes "tmp") -Bytes)
   (tc-e (file->bytes "tmp" #:mode 'binary) -Bytes)
   (tc-e (file->bytes "tmp" #:mode 'text) -Bytes)

   (tc-e (file->list "tmp") (-lst Univ))
   (tc-e ((inst file->list Any) "tmp" #:mode 'binary) (-lst Univ))
   (tc-e ((inst file->list Any) "tmp" #:mode 'text) (-lst Univ))

   (tc-e (file->list "tmp" (lambda (x) "string")) (-lst -String))
   (tc-e ((inst file->list String) "tmp" (lambda (x) "string") #:mode 'binary) (-lst -String))
   (tc-e ((inst file->list String) "tmp" (lambda (x) "string") #:mode 'text) (-lst -String))

   (tc-e (file->lines "tmp") (-lst -String))
   (tc-e (file->lines "tmp" #:mode 'text) (-lst -String))
   (tc-e (file->lines "tmp" #:line-mode (first (shuffle '(linefeed return return-linefeed any any-one)))
                      #:mode 'binary)  (-lst -String))


   (tc-e (file->bytes-lines "tmp") (-lst -Bytes))
   (tc-e (file->bytes-lines "tmp" #:mode 'text) (-lst -Bytes))
   (tc-e (file->bytes-lines "tmp" #:line-mode (first (shuffle '(linefeed return return-linefeed any any-one)))
                            #:mode 'binary)  (-lst -Bytes))

   (tc-e (display-to-file "a" "tmp" #:mode (if (= 1 2) 'binary 'text)
                          #:exists (first (shuffle '(error append update replace truncate truncate/replace))))
         -Void)

   (tc-e (write-to-file "a" "tmp" #:mode (if (= 1 2) 'binary 'text)
                        #:exists (first (shuffle '(error append update replace truncate truncate/replace))))
         -Void)


   (tc-e (display-lines-to-file (list 2 'esha "esht") "tmp" #:separator #f
                                #:mode (if (= 1 2) 'binary 'text)
                                #:exists (first (shuffle '(error append update replace truncate truncate/replace))))
         -Void)

   (tc-e (get-preference 'pref (lambda () 'error) 'timestamp #f #:use-lock? #t #:timeout-lock-there #f #:lock-there #f) Univ)


   (tc-e (make-handle-get-preference-locked .3 'sym (lambda () 'eseh) 'timestamp #f #:lock-there #f #:max-delay .45)
         (t:-> -Pathlike Univ))

   (tc-e (call-with-file-lock/timeout #f 'exclusive (lambda () 'res) (lambda () 'err)
                                      #:lock-file "lock"
                                      #:delay .01
                                      #:max-delay .2) (one-of/c 'res 'err))

   (tc-e (make-derived-parameter current-input-port
                                 (lambda: ((s : String)) (open-input-file s))
                                 object-name) (-Param -String Univ))

   ;; exception handling
   [tc-e (with-handlers ([void (λ (x) (values 0 0))]) (values  "" ""))
         #:ret (ret (list (t:Un -Zero -String) (t:Un -Zero -String)))]

   (tc-e (make-temporary-file) -Path)
   (tc-e (make-temporary-file "ee~a") -Path)
   (tc-e (make-temporary-file "ee~a" 'directory) -Path)
   (tc-e (make-temporary-file "ee~a" "temp" "here") -Path)

   ;; more sequences
   [tc-e (sequence-ref (in-directory) 0) -Path]
   [tc-e (in-mlist (ann (mcons 'a null) (MListof 'a))) (-seq (-val 'a))]
   [tc-e (in-producer (λ () 'hi)) (-seq (-val 'hi))]
   [tc-e (in-producer (λ: ([x : String]) 'hi) symbol? "foo")
         (-seq (-val 'hi))]
   [tc-e (in-value 'hi) (-seq (-val 'hi))]
   [tc-e (in-indexed '(a b c)) (-seq (one-of/c 'a 'b 'c) -Nat)]
   [tc-e (in-sequences '(a b) '(z y)) (-seq (one-of/c 'a 'b 'z 'y))]
   [tc-e (in-cycle '(a b) '(z y)) (-seq (one-of/c 'a 'b 'z 'y))]
   [tc-e (in-parallel '(a b) '(z y)) (-seq (one-of/c 'a 'b) (one-of/c 'z 'y))]
   [tc-e (in-values-sequence (in-parallel '(a b) '(z y)))
         (-seq (-pair (one-of/c 'a 'b) (-pair (one-of/c 'z 'y) (-val null))))]
   [tc-e (in-values-sequence '(a b c))
         (-seq (-pair (one-of/c 'a 'b 'c) (-val null)))]
   [tc-e (in-values*-sequence (in-parallel '(a b) '(z y)))
         (-seq (-pair (one-of/c 'a 'b) (-pair (one-of/c 'z 'y) (-val null))))]
   [tc-e (in-values*-sequence '(a b c))
         (-seq (one-of/c 'a 'b 'c))]
   ))


(define-go typecheck-special-tests)
(provide typecheck-special-tests)
