#lang racket

(require "test-util.rkt"
         drracket/check-syntax
         redex/pict
         redex/reduction-semantics)

(reset-count)

(define (source stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)))

(define arrow-collector%
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
(let ([annotations (new arrow-collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define language-def-name #'L)
  (define language-use-name #'L)
  
  (define mode-name #'J)
  (define contract-name #'J)
  (define conclusion-name #'J)
  (define premise-name #'J)
  (define render-name #'J)
  (define holds-name #'J)
  
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
        (set (list (source language-def-name) (source language-use-name))
             (list (source mode-name) (source contract-name))
             (list (source mode-name) (source conclusion-name))
             (list (source mode-name) (source premise-name))
             (list (source mode-name) (source render-name))
             (list (source mode-name) (source holds-name)))))

;; metafunctions
(let ([annotations (new arrow-collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define language-def-name #'L)
  (define language-use-name #'L)
  
  (define contract-name #'f)
  (define lhs-name #'f)
  (define rhs-name #'f)
  (define render-name #'f)
  (define term-name #'f)
  
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
        (set (list (source language-def-name) (source language-use-name))
             (list (source contract-name) (source lhs-name))
             (list (source contract-name) (source rhs-name))
             (list (source contract-name) (source render-name))
             (list (source contract-name) (source term-name)))))

(print-tests-passed 'check-syntax-test.rkt)