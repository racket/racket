(load-relative "loadtest.rktl")

(Section 'modprot)
(require racket/file)

;; ============================================================

;; Use '#%kernel everywhere so we're only checking the directly
;; intended taints and protections.

(define zero
  '(module zero '#%kernel

     (define-values (prot) '(8))
     
     (#%provide (protect prot))))

;; - - - - - - - - - - - - - - - - - - - -

(define one
  '(module one '#%kernel
     (#%require 'zero
                (for-syntax '#%kernel))
     
     (define-values (unexp) '(5))
     (define-syntaxes (stx)
       (lambda (stx) (quote-syntax '(13))))

     (define-syntaxes (nab)
       (lambda (stx)
         (datum->syntax
          stx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e stx))
                (quote-syntax (make-rename-transformer (quote-syntax unexp)))))))
     (define-syntaxes (pnab)
       (lambda (stx)
         (datum->syntax
          stx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e stx))
                (quote-syntax (make-rename-transformer (quote-syntax prot)))))))
     (define-syntaxes (snab)
       (lambda (xstx)
         (datum->syntax
          xstx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e xstx))
                (quote-syntax (make-rename-transformer (quote-syntax stx)))))))

     (define-syntaxes (nfnab)
       (lambda (stx)
         (datum->syntax
          stx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e stx))
                (quote-syntax (make-rename-transformer (syntax-property (quote-syntax unexp)
                                                                        'not-free-identifier=?
                                                                        #t)))))))
     (define-syntaxes (nfpnab)
       (lambda (stx)
         (datum->syntax
          stx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e stx))
                (quote-syntax (make-rename-transformer (syntax-property (quote-syntax prot)
                                                                        'not-free-identifier=?
                                                                        #t)))))))
     (define-syntaxes (nfsnab)
       (lambda (xstx)
         (datum->syntax
          xstx
          (list (quote-syntax define-syntaxes)
                (cdr (syntax-e xstx))
                (quote-syntax (make-rename-transformer (syntax-property (quote-syntax stx)
                                                                        'not-free-identifier=?
                                                                        #t)))))))
     
     (#%provide nab
                pnab
                snab
                nfnab
                nfpnab
                nfsnab)))

;; - - - - - - - - - - - - - - - - - - - -

(define two/no-protect
  '(module two '#%kernel
     (#%require 'one)

     (define-values (normal) '(10))
     
     (nab nabbed)
     (pnab pnabbed)
     (snab snabbed)
     (nfnab nfnabbed)
     (nfpnab nfpnabbed)
     (nfsnab nfsnabbed)

     (#%provide normal
                nabbed
                pnabbed
                snabbed
                nfnabbed
                nfpnabbed
                nfsnabbed)))

;; - - - - - - - - - - - - - - - - - - - -

(define two/protect
  '(module two '#%kernel
     (#%require 'one)

     (define-values (normal) '(10))
     
     (nab nabbed)
     (pnab pnabbed)
     (snab snabbed)
     (nfnab nfnabbed)
     (nfpnab nfpnabbed)
     (nfsnab nfsnabbed)
     
     (#%provide (protect normal
                         nabbed
                         pnabbed
                         snabbed
                         nfnabbed
                         nfpnabbed
                         nfsnabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/nabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n" 
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax nabbed)))))
             nabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/pnabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n" 
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax pnabbed)))))
             pnabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/snabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n"
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax snabbed)))))
             snabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/nfnabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n" 
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax nfnabbed)))))
             nfnabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/nfpnabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n" 
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax nfpnabbed)))))
             nfpnabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/nfsnabbed
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n"
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax nfsnabbed)))))
             nfsnabbed))))

;; - - - - - - - - - - - - - - - - - - - -

(define three/normal
  '(module three '#%kernel
     (#%module-begin
      (#%require 'two)
      (#%app printf "~s ~s\n" 
             (resolved-module-path-name
              (module-path-index-resolve (car (identifier-binding (quote-syntax normal)))))
             normal))))

;; - - - - - - - - - - - - - - - - - - - -

(define unsafe
  '(module unsafe '#%kernel
     (#%require '#%unsafe)
     (display unsafe-car)))

(require (only-in racket/unsafe/ops unsafe-car)
         compiler/zo-structs
         compiler/zo-marshal
         (only-in '#%linklet primitive->compiled-position))

;; - - - - - - - - - - - - - - - - - - - -

(define (xeval e)
  (eval
   (if (bytes? e)
       (parameterize ([read-accept-compiled #t])
         (read (open-input-bytes e)))
       e)))

(define (mp-try-all zero one two/no-protect two/protect 
                    three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed 
                    three/normal
                    get-one-inspector get-three-inspector fail-pnab? fail-prot? fail-three? np-ok? fail-three-comp?
                    #:via-2-ok? [via-2-ok? #f]
                    #:unprot-ok? [unprot-ok? #f]
                    #:early-ok? [early-ok? #f])
  (let ([try
         (lambda (two three v fail-three?)
           (let ([ns (make-base-namespace)]
                 [p (open-output-bytes)])
             (parameterize ([current-namespace ns]
                            [current-output-port p])
               (xeval zero)
               (parameterize ([current-code-inspector (get-one-inspector)])
                 (xeval one)
                 (xeval two)
                 (parameterize ([current-code-inspector (get-three-inspector)])
                   (with-handlers ([(lambda (x) fail-three?)
                                    (lambda (exn)
                                      (printf "~a\n" (exn-message exn)))])
                     (xeval three)
                     (when fail-three? (error "wrong")))
                   (with-handlers ([values (lambda (exn)
                                             (printf "~a\n" (exn-message exn)))])
                     (eval '(#%require 'three))))))
             (test #t regexp-match?
                   (if (byte-regexp? v) v (byte-regexp (string->bytes/utf-8 (format "~a\n" v))))
                   (get-output-bytes p))))])
    (try two/no-protect three/nabbed (if (and fail-prot? (not early-ok?)) #rx#"unexported" #rx#"one .5.") fail-three?)
    (try two/no-protect three/nfnabbed (if (and fail-prot? (not np-ok?) (not unprot-ok?)) #rx#"unexported .* unexp" #rx#"two .5.") fail-three?)
    (try two/no-protect three/pnabbed (if (and fail-pnab? (not early-ok?)) #rx#"protected" #rx#"zero .8.") fail-three?)
    (try two/no-protect three/nfpnabbed (if (and fail-pnab? (not np-ok?) (not unprot-ok?)) #rx#"protected .* prot" #rx#"two .8.") (or fail-three? fail-three-comp?))
    (try two/no-protect three/snabbed (if (and fail-prot? (not np-ok?) (not via-2-ok?) (not early-ok?)) #rx#"unexported .* stx" #rx#"one .13.") fail-three?)
    (try two/no-protect three/nfsnabbed #rx#"two .13." fail-three?)
    (try two/no-protect three/normal #rx#"two .10." fail-three?)
    (try two/protect three/nabbed (if fail-prot? #rx#"unexported" #rx#"one .5.") fail-three?)
    (try two/protect three/pnabbed (if fail-pnab? #rx#"protected" #rx#"zero .8.") fail-three?)
    (try two/protect three/snabbed (if (and fail-prot? (not np-ok?) (not via-2-ok?)) #rx#"unexported .* stx" #rx#"one .13.") fail-three?)
    (try two/protect three/normal  (if fail-prot? #rx#"protected" #rx#"two .10.") fail-three?)))

(define (unsafe-try unsafe get-inspector unsafe-fail? unsafe-ref-fail? read-fail?)
  (let ([ns (make-base-namespace)]
        [p (open-output-bytes)])
    (parameterize ([current-namespace ns]
                   [current-output-port p]
                   [current-code-inspector (get-inspector)])
      (with-handlers ([values (lambda (exn)
                                (printf "~a\n" (exn-message exn)))])
        (eval unsafe)
        (unless unsafe-fail?
          (dynamic-require ''unsafe #f))))
    (test (or unsafe-fail? unsafe-ref-fail?) regexp-match? #rx"protected" (get-output-bytes p)))
  (let-values ([(i o) (make-pipe)])
    (if (bytes? unsafe)
        (write-bytes unsafe o)
        (write unsafe o))
    (close-output-port o)
    (parameterize ([read-accept-compiled #t]
                   [current-code-inspector (get-inspector)])
      (if read-fail?
          (err/rt-test (void (read i)) exn:fail:read?)
          (test #t not (not (read i)))))))

;; - - - - - - - - - - - - - - - - - - - -

(define-values (zero-zo one-zo two/no-protect-zo two/protect-zo 
                        three/nabbed-zo three/pnabbed-zo three/snabbed-zo 
                        three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
                        three/normal-zo unsafe-zo-bytes)
  (apply
   values
   (let ([ns (make-base-namespace)])
     (parameterize ([current-namespace ns])
       (map (lambda (c)
              (let ([c (compile c)]
                    [p (open-output-bytes)])
                (write c p)
                (eval c)
                (get-output-bytes p)))
            (list zero one two/no-protect two/protect 
                  three/nabbed three/pnabbed three/snabbed 
                  three/nfnabbed three/nfpnabbed three/nfsnabbed 
                  three/normal unsafe))))))

(define-values (zero-c one-c two/no-protect-c two/protect-c 
                       three/nabbed-c three/pnabbed-c three/snabbed-c 
                       three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
                       three/normal-c unsafe-c)
  (apply
   values
   (let ([ns (make-base-namespace)])
     (parameterize ([current-namespace ns])
       (map (lambda (s) (let ([c (compile s)])
                          (eval c)
                          c))
            (list zero one two/no-protect two/protect 
                  three/nabbed three/pnabbed three/snabbed 
                  three/nfnabbed three/nfpnabbed three/nfsnabbed 
                  three/normal unsafe))))))

;; - - - - - - - - - - - - - - - - - - - -

(define unsafe-zo (parameterize ([read-accept-compiled #t])
                    (read (open-input-bytes unsafe-zo-bytes))))

(displayln "source, no inspector change:")
(mp-try-all zero one two/no-protect two/protect 
            three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed
            three/normal
            current-code-inspector current-code-inspector #f #f #f #f #f)
(unsafe-try unsafe current-code-inspector #f #f #f)

(displayln "zo, no inspector change:")
(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo 
            three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
            three/normal-zo
            current-code-inspector current-code-inspector #f #f #f #f #f)
(unsafe-try unsafe-zo current-code-inspector #f #f #f)

(displayln "compiled, no inspector change:")
(mp-try-all zero-c one-c two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            current-code-inspector current-code-inspector #f #f #f #f #f)
(unsafe-try unsafe-c current-code-inspector #f #f #f)

(displayln "compiled; changing inspectors does not affect access:")
(mp-try-all zero one two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            make-inspector current-code-inspector #f #f #f #f #f)
(mp-try-all zero one two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            current-code-inspector make-inspector #f #f #f #f #f)
(unsafe-try unsafe-c make-inspector #f #f #f)

(displayln "just source, weaken inspector:")
(mp-try-all zero one two/no-protect two/protect 
            three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed 
            three/normal
            current-code-inspector make-inspector #t #t #t #f #f #:unprot-ok? #t #:early-ok? #t)

;; ----------------------------------------

(err/rt-test (parameterize ([current-code-inspector (make-inspector (current-code-inspector))])
               (dynamic-require 'racket/unsafe/ops 'unsafe-s16vector-ref)))

(err/rt-test (parameterize ([current-code-inspector (make-inspector (current-code-inspector))])
               (eval '(define-syntax foo (dynamic-require 'racket/unsafe/ops 'unsafe-s16vector-ref)))))

(let ([n (make-base-namespace)])
  (eval '(module m racket/base
           (require (for-syntax racket/unsafe/ops))
           (provide (for-syntax unsafe-s16vector-ref))))
  (parameterize ([current-code-inspector (make-inspector (current-code-inspector))])
    (err/rt-test
     (eval '(module n racket/base
              (require (for-syntax racket/base) 'm)
              (begin-for-syntax unsafe-s16vector-ref)))
     exn:fail:syntax?)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parameterize ([current-namespace (make-base-namespace)]
               [current-code-inspector (make-inspector)])
  (eval
   ;; This compilation is intended to inline a call to `gen-for-each`,
   ;; and the test is meant to ensure that the reference is allowed
   (compile '(lambda (f) (for-each f '(1 2 3 4 5))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parameterize ([current-namespace (make-base-namespace)])
  (define strong (current-code-inspector))
  (define weak (make-inspector (current-code-inspector)))

  (eval '(module A racket/base ;; A is controlled by strong, not by weak
           (require (for-syntax racket/base))
           (define s1 #'secret-val)
           (define secret-val 'secret)
           (define-syntax (A-identity stx)
             (syntax-case stx ()
               [(_ id) #`(quote-syntax #,(datum->syntax #'id 'secret-val))]))
           (provide s1 A-identity)))

  (eval '(require 'A))

  (define s1 (eval 's1))

  (define (get-id s)
    (syntax-case s () [(_ id) #'id]))

  (define s2 (get-id (expand `(A-identity ,s1))))
  (define s2-weak1 (parameterize ([current-code-inspector weak])
                     ;; Ends up with no inspector, since we're not
                     ;; in a macro expansion:
                     (datum->syntax s1 'secret-val)))
  (define s2-weak2 (parameterize ([current-code-inspector weak])
                     ;; Ends up with weak inspector, since no inspector
                     ;; on `s2-weak1 turns into a weak inspector:
                     (get-id (expand `(A-identity ,s2-weak1)))))

  (define s3-weak
    (parameterize ((current-code-inspector strong))
      ;; Doesn't get strong inspector back:
      (datum->syntax s2-weak1 'secret-val)))

  (parameterize ([current-code-inspector weak])
    (test 'secret eval s1)
    (test 'secret eval s2)
    (err/rt-test (eval s2-weak1) exn:fail:syntax?)
    (err/rt-test (eval s2-weak2) exn:fail:syntax?)
    (err/rt-test (eval s3-weak) exn:fail:syntax?))

  (parameterize ([current-code-inspector strong])
    (test 'secret eval s1)
    (test 'secret eval s2)
    (test 'secret eval s2-weak1)
    (err/rt-test (eval s2-weak2) exn:fail:syntax?)
    (test 'secret eval s3-weak))

  (parameterize ([current-code-inspector weak])
    (let ([s4-weak
           ;; Try to get `struct` to synthesize a `secret-val`
           ;; that has the `racket/base` inspector:
           (syntax-case (expand (datum->syntax s1 '(struct secret (val)))) ()
             [(_ a b (_ (_ _ _ c) . _) . _) #'c])])
      (err/rt-test (eval s4-weak) exn:fail:syntax?))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define work-dir (make-temporary-file "deputy-check-~a" 'directory))
  
  (define m '(module m racket/base
               (provide m)
               (define (m) '(this is m))))

  (define n '(module n racket/base
               (require "am.rkt")
               (m)))

  (define (make e f)
    (parameterize ([current-directory work-dir]
                   [current-load-relative-directory work-dir])
      (define c (parameterize ([current-namespace  (make-base-namespace)])
                  (compile e)))
      (call-with-output-file*
       f
       #:exists 'truncate
       (lambda (o)
         (write c o)))))

  (make m "am.rkt")
  (make n "an.rkt")

  (parameterize ([current-namespace  (make-base-namespace)])
    (parameterize ([current-code-inspector  (make-inspector)])
      (parameterize ([current-module-declare-name
                      (make-resolved-module-path
                       (build-path work-dir "am.rkt"))])
        (eval m)))
    ;; attempt to import from a module with a weaker code inspector
    (err/rt-test (dynamic-require (build-path work-dir "an.rkt") #f)
                 exn:fail?
                 "weaker code inspector"))

  (delete-directory/files work-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check syntax-local-apply-transformer runs the transformer with
; the weaker of the calling macro's code inspector and the inspector
; associated with the binding of the binding-id argument.
(parameterize ([current-namespace (make-base-namespace)])
  ; Trusted DSL expander
  (eval '(module Exp racket
           (provide exp)
           (define-syntax (exp stx)
             (syntax-case stx ()
               [(_ (m . rest))
                (syntax-local-apply-transformer
                 (syntax-local-value #'m) #'m
                 'expression #f
                 #'(m . rest))]))))

  ; Trusted module protecting secret data, but exposing an unrelated id for-syntax
  (eval '(module Secret racket
           (provide (protect-out secret)
                    secret-m
                    (for-syntax internal-id))
           (define-for-syntax internal-id #'not-secret)
           (define secret 'secret)
           ; Should be allowed to use `datum->syntax` in its own macros
           (define-syntax (secret-m stx) #`(begin #,(datum->syntax internal-id 'secret) 'safe))))

  ; Sandboxed code...
  (parameterize ([current-code-inspector (make-inspector (current-code-inspector))])
    (eval '(require 'Exp 'Secret (for-syntax racket/base)))
    ; Trusted DSL macro can use datum->syntax to generate reference to secret
    (test 'safe eval '(exp (secret-m)))
    ; Sandboxed attacker cannot use datum->syntax in a DSL macro to access protected import
    ; even given id with secret module scopes but different symbol.
    (define attack-1
      '(let-syntax ([attack-m (lambda (stx)
                                (datum->syntax internal-id 'secret))])
         (exp (attack-m))))
    (err/rt-test (eval attack-1) exn:fail:syntax?)
    ; Sandboxed macro cannot trick syntax-local-apply-transformer directly into allowing it
    ; use of datum->syntax
    (define attack-2
      '(let-syntax ([attack-m (lambda (stx)
                                (syntax-local-apply-transformer
                                 (lambda ()
                                   (datum->syntax internal-id 'secret))
                                 #'secret-m
                                 'expression #f))])
         (attack-m)))

    (err/rt-test (eval attack-2) exn:fail:syntax?)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make sure that re-exporting a protected binding is allowed consistently

(module provides-a-protected-binding-to-reexport racket/base
  (define binding (gensym))
  (provide (protect-out binding)))

(parameterize ([current-code-inspector (make-inspector)])
  (test #t syntax? (expand '(module module-that-reexports-protected '#%kernel
                              (#%require 'provides-a-protected-binding-to-reexport)
                              (#%provide binding))))
  (test #t syntax? (expand '(module module-that-reexports-protected racket/base
                              (require 'provides-a-protected-binding-to-reexport)
                              (provide binding))))
  (test #t syntax? (expand '(module module-that-reexports-protected racket/base
                              (require 'provides-a-protected-binding-to-reexport)
                              (provide (all-from-out 'provides-a-protected-binding-to-reexport))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(report-errs)
