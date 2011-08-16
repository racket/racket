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
    (define/override (syncheck:add-rename-menu id	 
                                               all-ids	 
                                               new-name-interferes?)
      (match all-ids
        [(list (list ids _ _) ...)
         (set! renames (cons ids renames))]))
    (define renames '())
    (define/public (collected-rename-class stx)
      (for/fold ([class (set)]) ([ids renames])
                (if (for/or ([id ids])
                            (equal? (source stx) (source id)))
                    (set-union class (apply set (map source ids)))
                    class)))
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
  
  (define language-def-name #'L)
  (define language-use-name #'L)
  
  (define mode-name #'J)
  (define contract-name #'J)
  (define conclusion-name #'J)
  (define premise-name #'J)
  (define render-name #'J)
  (define holds-name #'J)
  
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
         (list language-binding judgment-form-binding)))
  (test (send annotations collected-rename-class language-def-name)
        (expected-rename-class language-binding))
  (test (send annotations collected-rename-class mode-name)
        (expected-rename-class judgment-form-binding)))

;; metafunctions
(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define language-def-name #'L)
  (define language-use-name #'L)
  
  (define contract-name #'f)
  (define lhs-name #'f)
  (define rhs-name #'f)
  (define render-name #'f)
  (define term-name #'f)
  
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
         (list language-binding metafunction-binding)))
  (test (send annotations collected-rename-class language-def-name)
        (expected-rename-class language-binding))
  (test (send annotations collected-rename-class contract-name)
        (expected-rename-class metafunction-binding)))

(print-tests-passed 'check-syntax-test.rkt)