#lang scheme/base

(require (for-syntax scheme/base syntax/boundmap scheme/list syntax/kerncase)
         scribble/scheme scribble/decode scribble/manual scribble/struct)

(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-numbers (make-free-identifier-mapping))
  (define (get-chunk-number id)
    (let ([n (add1 (free-identifier-mapping-get chunk-numbers id
                                                (lambda () 0)))])
      (free-identifier-mapping-put! chunk-numbers id n)
      n)))

(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ name expr ...) 
     ;; no need for more error checking, using chunk for the code will do that
     (identifier? #'name)
     (let ([n (get-chunk-number #'name)]
           [str (symbol->string (syntax-e #'name))])

       (syntax-local-lift-expression #'(quote-syntax (a-chunk name expr ...)))
       
       (if (n . > . 1)
           (let ([str
                  (format 
                   "need to handle secondary tags: ~a ~a\n"
                   n 
                   str)])
             #`(begin
                 (italic #,str)))
           (with-syntax ([tag str]
                         [str str]
                         [((for-label-mod ...) ...)
                          (map (lambda (expr)
                                 (syntax-case expr (require)
                                   [(require mod ...)
                                    #'(mod ...)]
                                   [else null]))
                               (syntax->list #'(expr ...)))])
             #`(begin
                 (require (for-label for-label-mod ... ...))
                 ;; why does this happen twice?
                 #;
                 (define-syntax name (make-element-id-transformer
                                      (lambda (stx) #'(chunkref name))))
                 (make-splice
                  (list (make-toc-element
                         #f
                         (list (elemtag '(chunk tag)
                                        (bold (italic (scheme name)) " ::=")))
                         (list (smaller (elemref '(chunk tag) #:underline? #f
                                                 str))))
                        (schemeblock expr ...)))))))]))

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([str (format "~a" (syntax-e #'id))])
       #'(elemref '(chunk str) #:underline? #f str))]))


(provide (all-from-out scheme/base
                       scribble/manual)
         chunk)
