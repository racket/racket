#lang racket/base

(require (for-syntax syntax/context
                     syntax/kerncase
                     syntax/transformer
                     racket/base
                     syntax/parse/pre))

(provide block)

(define-syntax block
  (make-expression-transformer
   (syntax-parser
     [(_ defn-or-expr ...)
      (quasisyntax/loc this-syntax
        (let () (block-trampoline #,(generate-expand-context #t) #f defn-or-expr ...)))])))

(define-syntax block-trampoline
  (syntax-parser
    [(_ _ #f) #'(void)]
    [(_ _ #t) #'(begin)]
    [(_ context-v follows-expr? defn-or-expr more ...)
     (syntax-parse (local-expand #'defn-or-expr
                                 (list (syntax-e #'context-v))
                                 (kernel-form-identifier-list))
       #:literal-sets [kernel-literals]
       [(head:begin defn-or-expr ...)
        (syntax-track-origin #'(block-trampoline context-v follows-expr? defn-or-expr ... more ...)
                             this-syntax
                             (syntax-local-introduce #'head))]
       [({~or define-values define-syntaxes} . _)
        #`(begin #,this-syntax (block-trampoline context-v #f more ...))]
       [_
        #`(begin #,this-syntax (block-trampoline context-v #t more ...))])]))
