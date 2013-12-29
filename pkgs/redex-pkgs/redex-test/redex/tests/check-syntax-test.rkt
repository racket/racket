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

(print-tests-passed 'check-syntax-test.rkt)
