(load-relative "loadtest.rktl")

(Section 'modprot)

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

(define (xeval e)
  (eval
   (if (bytes? e)
       (parameterize ([read-accept-compiled #t]
                      ;; The read-time inspector is supposed to
                      ;; be irrelevant; only the declaration-time
                      ;; inspector should matter
                      [current-code-inspector (make-inspector)])
         (read (open-input-bytes e)))
       e)))

(define (mp-try-all zero one two/no-protect two/protect 
                    three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed 
                    three/normal
                    get-one-inspector get-three-inspector fail-pnab? fail-prot? fail-three? np-ok? fail-three-comp?)
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
    (try two/no-protect three/nabbed (if fail-prot? #rx#"unexported .* unexp" #rx#"one .5.") fail-three?)
    (try two/no-protect three/nfnabbed (if (and fail-prot? (not np-ok?)) #rx#"unexported .* unexp" #rx#"two .5.") fail-three?)
    (try two/no-protect three/pnabbed (if fail-pnab? #rx#"protected .* prot" #rx#"zero .8.") fail-three?)
    (try two/no-protect three/nfpnabbed (if (and fail-pnab? (not np-ok?)) #rx#"protected .* prot" #rx#"two .8.") (or fail-three? fail-three-comp?))
    (try two/no-protect three/snabbed (if (and fail-prot? np-ok?) #rx#"unexported .* stx" #rx#"one .13.") fail-three?)
    (try two/no-protect three/nfsnabbed #rx#"two .13." fail-three?)
    (try two/no-protect three/normal #rx#"two .10." fail-three?)
    (try two/protect three/nabbed (if fail-prot? #rx#"unexported .* unexp" #rx#"one .5.") fail-three?)
    (try two/protect three/pnabbed (if fail-pnab? #rx#"protected .* prot" #rx#"zero .8.") fail-three?)
    (try two/protect three/snabbed (if (and fail-prot? np-ok?) #rx#"unexported .* stx" #rx#"one .13.") fail-three?)
    (try two/protect three/normal  (if fail-prot? #rx#"protected .* normal" #rx#"two .10.") fail-three?)))

;; - - - - - - - - - - - - - - - - - - - -

(define-values (zero-zo one-zo two/no-protect-zo two/protect-zo 
                        three/nabbed-zo three/pnabbed-zo three/snabbed-zo 
                        three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
                        three/normal-zo)
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
                  three/normal))))))

(define-values (zero-c one-c two/no-protect-c two/protect-c 
                        three/nabbed-c three/pnabbed-c three/snabbed-c 
                        three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
                        three/normal-c)
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
                  three/normal))))))

;; - - - - - - - - - - - - - - - - - - - -

;; source, no inspector change:
(mp-try-all zero one two/no-protect two/protect 
            three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed
            three/normal
            current-code-inspector current-code-inspector #f #f #f #f #f)

;; zo, no inspector change:
(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo 
            three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
            three/normal-zo
            current-code-inspector current-code-inspector #f #f #f #f #f)

;; compiled, no inspector change:
(mp-try-all zero-c one-c two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            current-code-inspector current-code-inspector #f #f #f #f #f)

;; compiled; changing inspectors does not affect access:
(mp-try-all zero one two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            make-inspector current-code-inspector #f #f #f #f #f)
(mp-try-all zero one two/no-protect-c two/protect-c 
            three/nabbed-c three/pnabbed-c three/snabbed-c three/nfnabbed-c three/nfpnabbed-c three/nfsnabbed-c 
            three/normal-c
            current-code-inspector make-inspector #f #f #f #f #f)

;; zo and source; changing inspector affects access in various ways-----------------

(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo 
            three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
            three/normal-zo
            make-inspector current-code-inspector #t #f #f #f #t)

(mp-try-all zero one two/no-protect two/protect 
            three/nabbed three/pnabbed three/snabbed-zo three/nfnabbed three/nfpnabbed three/nfsnabbed-zo 
            three/normal
            make-inspector current-code-inspector #t #f #t #f #t)

(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo 
            three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/nfnabbed-zo three/nfpnabbed-zo three/nfsnabbed-zo 
            three/normal-zo
            current-code-inspector make-inspector #t #t #f #f #f)

(mp-try-all zero one two/no-protect two/protect 
            three/nabbed three/pnabbed three/snabbed three/nfnabbed three/nfpnabbed three/nfsnabbed 
            three/normal
            current-code-inspector make-inspector #t #t #t #t #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
