#lang racket/base

(require drracket/check-syntax
         racket/class
         rackunit)

(check-true
 (let ()
   (define add-arrow-called? #f)
   
   (define annotations
     (new (class (annotations-mixin object%)
            (super-new)
            (define/override (syncheck:find-source-object stx)
              (if (eq? 'the-source (syntax-source stx))
                  'yep
                  #f))
            (define/override (syncheck:add-arrow . args)
              (set! add-arrow-called? #t)))))
   
   (define-values (add-syntax done)
     (make-traversal (make-base-namespace)
                     (current-directory)))
   
   (parameterize ([current-annotations annotations]
                  [current-namespace (make-base-namespace)])
     (add-syntax (expand
                  (read-syntax
                   'the-source
                   (open-input-string
                    (format "~s"
                            `(module m racket/base
                               (define x 4)
                               x
                               (let ([y 1]) y)))))))
     (done))
   add-arrow-called?))

(check-true
 (let ()
   (define add-arrow-called? #f)
   
   (define annotations
     (new (class (annotations-mixin object%)
            (super-new)
            (define/override (syncheck:find-source-object stx)
              (if (eq? 'the-source (syntax-source stx))
                  'yep
                  #f))
            (define/override (syncheck:add-arrow . args)
              (set! add-arrow-called? #t)))))
   
   (define-values (add-syntax done)
     (make-traversal (make-base-namespace) #f))
   
   (parameterize ([current-annotations annotations]
                  [current-namespace (make-base-namespace)])
     (add-syntax (expand
                  (read-syntax
                   'the-source
                   (open-input-string
                    (format "~s"
                            `(module m racket/base
                               (define x 4)
                               x
                               (let ([y 1]) y)))))))
     (done))
   add-arrow-called?))
