#lang scheme/base

(require "honu.ss"
         (for-syntax "debug.ss")
         (for-syntax scheme/base))

(provide honu-macro)

(define-for-syntax (extract-conventions pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       ;; todo: export honu attributes for syntax/parse
       (loop (cons #'(any expr) out)
             #'(rest ...))
       #;
       (loop (cons #'(any attribute) out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (loop out #'(rest1 rest ...))]
      [(foo) out])))

(define-for-syntax (extract-patterns pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       (loop (cons #'any out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (let ([f (if (eq? (syntax->datum #'foo) 'crackers)
                  #'(... ...)
                  #'foo)])
         (loop (cons f out)
               #'(rest1 rest ...)))]
      [(foo) (reverse (cons #'foo out))])))

(define-honu-syntax honu-macro
  (lambda (stx ctx)
    (debug "Original macro: ~a\n" (syntax->datum stx))
    (syntax-case stx (#%parens #%braces)
      [(_ (#%parens honu-literal ...)
          (#%braces (#%braces name pattern ...))
          (#%braces (#%braces template ...))
          . rest)
       (with-syntax ([(conventions ...)
                      (extract-conventions #'(pattern ...))]
                     [(raw-patterns ...)
                      (extract-patterns #'(pattern ...))])
         (values
           (syntax/loc
             stx
             (begin
               #|
               (define honu-literal (lambda () (error 'honu-literal "cant use this")))
               ...
               |#
               (define-honu-syntax name
                 (lambda (stx ctx)
                   (debug "Try to match against pattern ~a. Literals ~a\n" '(name raw-patterns ... . rrest) '(honu-literal ...))
                   (debug "stx is ~a\n" (syntax->datum stx))
                   ;; (printf "head is ~a\n" (stx-car stx))
                   ;; (printf "= is ~a\n" =)
                   (debug "my matcher ~a\n"
                           (syntax-case stx (to set! do honu-end honu-literal ...)
                             [(name q set! v to m do bb (... ...) honu-end) (syntax->datum #'(bb (... ...)))]
                             [(name raw-patterns ...)
                              'ok2]
                             [(name pattern ...) 'ok5]
                             [(name v (... ...) honu-literal ...) 'ok4]
                             [(name v (... ...)) 'ok3]
                             #;
                             [(name v (... ...)) (syntax->datum #'(v (... ...)))]
                             [else 'bad]))
                   #;
                   (debug "case pattern ~a\n"
                           #'(syntax-case stx
                               (honu-literal ...)
                               [(name pattern ...)
                                #'(honu-unparsed-block
                                    #f obj 'obj #f ctx
                                    template ...)]))

                   (let ([result (syntax-case stx
                                   #;
                                   (to set! do honu-end)
                                   (honu-literal ...)
                                   #;
                                   [(name q set! v to m do bb (... ...) honu-end) (syntax->datum #'(bb (... ...)))]
                                   [(name pattern ...) 'ok]
                                   [(name raw-patterns ...)
                                    #'(honu-unparsed-block
                                        #f obj 'obj #f ctx
                                        template ...)]
                                   [else 'fail-boat])])
                     (debug "result was ~a\n" result))
                   (syntax-case stx (honu-literal ...)
                     [(name raw-patterns ... . rrest)
                      (values
                        #'(honu-unparsed-block
                            #f obj 'obj #f ctx
                            template ...)
                        #'rrest)])))
               #;
               (define-honu-syntax name
                 (lambda (stx ctx)
                   (define-conventions honu-conventions conventions ...)
                   #;
                   (debug "Hello from ~a transformer. Syntax is ~a\n" 'name (syntax->datum stx))
                   (syntax-parse stx
                                 #:literals (honu-literal ...)
                                 #:conventions (honu-conventions)
                                 [(name raw-patterns ... . rrest)
                                  (values
                                    #'(honu-unparsed-block
                                        #f obj 'obj #f ctx
                                        template ...)
                                    #'rrest)])))))
           #'rest))])
    ))
