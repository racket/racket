#lang racket

(require "test-util.rkt"
         drracket/check-syntax
         redex/pict
         redex/reduction-semantics
         (for-syntax setup/path-to-relative)
         setup/path-to-relative)

(module test racket/base)

(reset-count)

(define-syntax (identifier stx)
  (syntax-case stx ()
    [(_ x)
     (identifier? #'x)
     #`(let ([p (open-input-string (format "~s" 'x))])
         (port-count-lines! p)
         (set-port-next-location! 
          p
          #,(syntax-line #'x)
          #,(syntax-column #'x)
          #,(syntax-position #'x))
         (read-syntax '#,(and (path? (syntax-source #'x))
                              (path->relative-string/library (syntax-source #'x)))
                      p))]))

(define (source stx)
  (list (and (path? (syntax-source stx))
             (path->relative-string/library (syntax-source stx)))
        (syntax-line stx)
        (syntax-column stx)))

(define (expected-arrows bindings)
  (for/fold ([arrs (set)]) ([binding bindings])
            (for/fold ([arrs arrs]) ([bound (cdr binding)])
                      (set-add arrs
                               (list (source (car binding))
                                     (source bound))))))

(define (expected-rename-class binding)
  (apply set (map source binding)))

(define collector%
  (class (annotations-mixin object%)
    (super-new)
    (define/override (syncheck:find-source-object stx)
      stx)
    (define/override (syncheck:add-arrow start-source-obj
                                         start-left
                                         start-right
                                         end-source-obj
                                         end-left
                                         end-right
                                         actual?
                                         phase-level)
      (set! arrows 
            (set-add arrows 
                     (list (source start-source-obj)
                           (source end-source-obj)))))
    (define arrows (set))
    (define/public (collected-arrows) arrows)))

(define-namespace-anchor module-anchor)
(define module-namespace 
  (namespace-anchor->namespace module-anchor))

;; judgment forms
(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define language-def-name (identifier L))
  (define language-use-name (identifier L))
  
  (define mode-name (identifier J))
  (define contract-name (identifier J))
  (define conclusion-name (identifier J))
  (define premise-name (identifier J))
  (define render-name (identifier J))
  (define holds-name (identifier J))
  
  (define language-binding 
    (list language-def-name language-use-name))
  (define judgment-form-binding
    (list mode-name contract-name conclusion-name premise-name render-name holds-name))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(let ()
                 (define-language #,language-def-name)
                 (define-judgment-form #,language-use-name
                   #:mode (#,mode-name)
                   #:contract (#,contract-name)
                   [(#,conclusion-name)
                    (#,premise-name)])
                 (render-judgment-form #,render-name)
                 (judgment-holds (#,holds-name)))))
    (done))
  
  (test (send annotations collected-arrows)
        (expected-arrows
         (list language-binding judgment-form-binding))))

;; metafunctions
(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define language-def-name (identifier L))
  (define language-use-name (identifier L))
  
  (define contract-name (identifier f))
  (define lhs-name (identifier f))
  (define rhs-name (identifier f))
  (define render-name (identifier f))
  (define term-name (identifier f))
  
  (define language-binding
    (list language-def-name language-use-name))
  (define metafunction-binding
    (list contract-name lhs-name rhs-name render-name term-name))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(let ()
                 (define-language #,language-def-name)
                 (define-metafunction #,language-use-name
                   #,contract-name : () -> ()
                   [(#,lhs-name) (#,rhs-name)])
                 (render-metafunction #,render-name)
                 (term (#,term-name)))))
    (done))
  
  (test (send annotations collected-arrows)
        (expected-arrows
         (list language-binding metafunction-binding))))

;; define-term
(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define def-name (identifier x))
  (define use-name (identifier x))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(let ()
                 (define-term #,def-name a)
                 (term (#,use-name b)))))
    (done))
  
  (test (send annotations collected-arrows) 
        (expected-arrows
         (list (list def-name use-name)))))

;; extended language
(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define base-lang-def (identifier L1))
  (define base-lang-use1 (identifier L1))
  (define base-lang-use2 (identifier L1))
  (define base-lang-use3 (identifier L1))
  (define extended-lang-def (identifier L2))
  (define extended-lang-use (identifier L2))
  (define base-nt-name (identifier e))
  (define base-nt-use (identifier e))
  (define base-nt-alias (identifier f))
  (define alias-use (identifier f))
  (define extended-nt-name (identifier e))
  (define extended-nt-use (identifier e))
  
  (define base-lang-bindings
    (list base-lang-def base-lang-use1 base-lang-use2 base-lang-use3))
  (define extended-lang-bindings
    (list extended-lang-def extended-lang-use))
  (define base-nt-bindings
    (list base-nt-name extended-nt-name base-nt-use))
  (define extended-nt-bindings
    (list extended-nt-name extended-nt-use))
  (define alias-bindings
    (list base-nt-alias alias-use))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(let ()
                 (define-language #,base-lang-def
                   ((#,base-nt-name #,base-nt-alias) number))
                 (define-extended-language #,extended-lang-def #,base-lang-use1
                   (#,extended-nt-name .... variable))
                 (redex-match #,base-lang-use2 #,base-nt-use 1)
                 (redex-match #,extended-lang-use #,extended-nt-use 'x)
                 (redex-match #,base-lang-use3 #,alias-use 2))))
    (done))
  
  (test (send annotations collected-arrows)
        (expected-arrows
         (list base-lang-bindings
               extended-lang-bindings
               base-nt-bindings
               extended-nt-bindings
               alias-bindings))))
  
    

(print-tests-passed 'check-syntax-test.rkt)
