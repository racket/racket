#lang scribble/doc

@(require (for-syntax scheme/base
                      syntax/boundmap 
                      scheme/list
                      (prefix-in scr: scribble/reader)
                      compiler/cm-accomplice))

@(require scribble/manual
          scribble/struct
          scribble/basic
          scribble/decode)

@(define :make-splice make-splice)

@(define-syntax (chunk stx)
   (syntax-case stx ()
     [(_ name expr ...)
      (begin
        (unless (identifier? #'name)
          (raise-syntax-error #f "expected a chunk name" stx #'name))
        (unless (regexp-match #rx"^<.*>$" (symbol->string (syntax-e #'name)))
          (raise-syntax-error #f "chunk names must begin and end with angle brackets, <...>"
                              stx 
                              #'name))
        #`(:make-splice
           (list
            (italic #,(format "~a = " (syntax-e #'name)))
            (schemeblock expr ...))))]))

@;{the two lines below seem like they shoudl work, but they loop forever; probably the read-syntax-inside vs read-syntax difference. If they did work, then all of the stuff below could go away}
@;(require scheme/include)
@;(include/reader "chat-noir-literate.ss" scr:read-syntax-inside)

@;{ stolen from include.ss. Should probably be refactored to just have one of these.}
@(define-for-syntax (give-lexical-content ctx content)
   (let loop ([content content])
     (cond
       [(pair? content)
        (cons (loop (car content))
              (loop (cdr content)))]
       [(null? content) null]
       [else
        (let ([v (syntax-e content)])
          (datum->syntax
           ctx
           (cond
             [(pair? v) 
              (loop v)]
             [(vector? v)
              (list->vector (loop (vector->list v)))]
             [(box? v)
              (box (loop (unbox v)))]
             [else
              v])
           content
           content))])))

@(define-syntax (content-elsewhere stx)
   (syntax-case stx ()
     [(_ fn)
      (string? (syntax-e #'fn))
      (let ([fn (syntax-e #'fn)])
        (register-external-file (path->complete-path fn))
        (call-with-input-file fn
          (λ (port)
            (port-count-lines! port)
            (let ([reader-line (read-line port)])
              (unless (regexp-match #rx"^#reader" reader-line)
                (raise-syntax-error #f (format "expected a #reader line, found ~s" reader-line) stx))
              (let* ([content (scr:read-syntax-inside fn port)]
                     [w/context (give-lexical-content stx content)])
                #`(begin #,@w/context))))))]))


@content-elsewhere["chat-noir-literate.ss"]
