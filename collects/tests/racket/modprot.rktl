(load-relative "loadtest.rktl")

(Section 'modprot)

;; ============================================================

;; Use '#%kernel everywhere so we're only checking the directly
;; intended certifications and protections.

(define zero
  '(module zero '#%kernel

     (define-values (prot) 8)
     
     (#%provide (protect prot))))

;; - - - - - - - - - - - - - - - - - - - -

(define one
  '(module one '#%kernel
     (#%require 'zero
                (for-syntax '#%kernel))
     
     (define-values (unexp) 5)
     (define-syntaxes (stx)
       (lambda (stx) (quote-syntax 13)))

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
     
     (#%provide nab
                pnab
                snab)))

;; - - - - - - - - - - - - - - - - - - - -

(define two/no-protect
  '(module two '#%kernel
     (#%require 'one)

     (define-values (normal) 10)
     
     (nab nabbed)
     (pnab pnabbed)
     (snab snabbed)

     (#%provide normal
                nabbed
                pnabbed
                snabbed)))

;; - - - - - - - - - - - - - - - - - - - -

(define two/protect
  '(module two '#%kernel
     (#%require 'one)

     (define-values (normal) 10)
     
     (nab nabbed)
     (pnab pnabbed)
     (snab snabbed)
     
     (#%provide (protect normal
                         nabbed
                         pnabbed
                         snabbed))))

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
       (parameterize ([read-accept-compiled #t])
         (read (open-input-bytes e)))
       e)))

(define (mp-try-all zero one two/no-protect two/protect three/nabbed three/pnabbed three/snabbed three/normal
                    get-one-inspector get-three-inspector fail-pnab? fail-prot? fail-three? np-ok?)
  (let ([try
         (lambda (two three v)
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
                     (xeval three))
                   (with-handlers ([values (lambda (exn)
                                             (printf "~a\n" (exn-message exn)))])
                     (eval '(#%require 'three))))))
             (test #t regexp-match?
                   (if (byte-regexp? v) v (byte-regexp (string->bytes/utf-8 (format "~a\n" v))))
                   (get-output-bytes p))))])
    (try two/no-protect three/nabbed (if (and fail-prot? (not np-ok?)) #rx#"unexported .* unexp" #rx#"one 5"))
    (try two/no-protect three/pnabbed (if (and fail-pnab? (not np-ok?)) #rx#"protected .* prot" #rx#"zero 8"))
    (try two/no-protect three/snabbed #rx#"one 13")
    (try two/no-protect three/normal #rx#"two 10")
    (try two/protect three/nabbed (if fail-prot? #rx#"unexported .* unexp" #rx#"one 5"))
    (try two/protect three/pnabbed (if fail-pnab? #rx#"protected .* prot" #rx#"zero 8"))
    (try two/protect three/snabbed (if (and fail-prot? np-ok?) #rx#"unexported .* stx" #rx#"one 13"))
    (try two/protect three/normal  (if fail-prot? #rx#"protected .* normal" #rx#"two 10"))))

;; - - - - - - - - - - - - - - - - - - - -

(define-values (zero-zo one-zo two/no-protect-zo two/protect-zo 
                        three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/normal-zo)
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
            (list zero one two/no-protect two/protect three/nabbed three/pnabbed three/snabbed three/normal))))))

;; - - - - - - - - - - - - - - - - - - - -


(mp-try-all zero one two/no-protect two/protect three/nabbed three/pnabbed three/snabbed three/normal
            current-code-inspector current-code-inspector #f #f #f #f)
                    
(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/normal-zo
            current-code-inspector current-code-inspector #f #f #f #f)

(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/normal-zo
            make-inspector current-code-inspector #t #f #f #f)

(mp-try-all zero one two/no-protect two/protect three/nabbed three/pnabbed three/snabbed-zo three/normal
            make-inspector current-code-inspector #t #f #t #f)

(mp-try-all zero-zo one-zo two/no-protect-zo two/protect-zo three/nabbed-zo three/pnabbed-zo three/snabbed-zo three/normal-zo
            current-code-inspector make-inspector #t #t #f #f)

(mp-try-all zero one two/no-protect two/protect three/nabbed three/pnabbed three/snabbed three/normal
            current-code-inspector make-inspector #t #t #t #t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
