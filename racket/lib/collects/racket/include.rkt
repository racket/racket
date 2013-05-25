#lang racket/base

(require (for-syntax racket/base
                     syntax/path-spec
                     mzlib/private/increader
                     compiler/cm-accomplice))

(provide include
         include-at/relative-to
         include/reader
         include-at/relative-to/reader)

(define-syntax (do-include stx)
  (syntax-case stx ()
    [(_ orig-stx ctx loc fn reader)
     ;; Parse the file name
     (let ([orig-c-file (resolve-path-spec (syntax fn) (syntax loc) (syntax orig-stx))]
           [ctx (syntax ctx)]
           [loc (syntax loc)]
           [reader (syntax reader)]
           [orig-stx (syntax orig-stx)]
           [rkt->ss (lambda (p)
                      (let ([b (path->bytes p)])
                        (if (regexp-match? #rx#"[.]rkt$" b)
                            (path-replace-suffix p #".ss")
                            p)))])

       (let ([c-file (if (file-exists? orig-c-file)
                         orig-c-file
                         (let ([p2 (rkt->ss orig-c-file)])
                           (if (file-exists? p2)
                               p2
                               orig-c-file)))])
         
         (register-external-file c-file)

         (let ([read-syntax (if (syntax-e reader)
                                (reader-val
                                 (let loop ([e (syntax->datum
                                                (local-expand reader 'expression null))])
                                   (cond
                                    [(reader? e) e]
                                    [(pair? e) (or (loop (car e))
                                                   (loop (cdr e)))]
                                    [else #f])))
                                (lambda (src in)
                                  (parameterize ([read-accept-reader #t])
                                    (read-syntax src in))))])
           (unless (and (procedure? read-syntax)
                        (procedure-arity-includes? read-syntax 2))
             (raise-syntax-error
              #f
              "reader is not a procedure of two arguments"
              orig-stx))

           ;; Open the included file
           (let ([p (with-handlers ([exn:fail?
                                     (lambda (exn)
                                       (raise-syntax-error
                                        #f
                                        (format
                                         "can't open include file (~a)"
                                         (if (exn? exn)
                                             (exn-message exn)
                                             exn))
                                        orig-stx
                                        c-file))])
                      (open-input-file c-file))])
             (port-count-lines! p)
             ;; Read expressions from file
             (let ([content
                    (let loop ()
                      (let ([r (with-handlers ([exn:fail?
                                                (lambda (exn)
                                                  (close-input-port p)
                                                  (raise-syntax-error
                                                   #f
                                                   (format
                                                    "read error (~a)"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn))
                                                   orig-stx))])
                                 (read-syntax c-file p))])
                        (if (eof-object? r)
                            null
                            (cons r (loop)))))])
               (close-input-port p)
               ;; Preserve src info for content, but set its
               ;; lexical context to be that of the include expression
               (let ([lexed-content
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
                             content))]))])
                 (datum->syntax
                  (quote-syntax here)
                  `(begin ,@lexed-content)
                  orig-stx)))))))]))

(define-syntax (include stx)
  (syntax-case stx ()
    [(_ fn)
     (with-syntax ([_stx stx])
       (syntax/loc stx (do-include _stx _stx _stx fn #f)))]))

(define-syntax (include-at/relative-to stx)
  (syntax-case stx ()
    [(_ ctx loc fn)
     (with-syntax ([_stx stx])
       (syntax/loc stx (do-include _stx ctx loc fn #f)))]))
      
(define-syntax (include/reader stx)
  (syntax-case stx ()
    [(_ fn reader)
     ;; Expand to do-include:
     (with-syntax ([_stx stx])
       (syntax/loc stx 
         (do-include _stx _stx _stx fn 
                     (letrec-syntax ([the-reader (lambda (stx)
                                                   (datum->syntax
                                                    #'here
                                                    (make-reader reader)))])
                       the-reader))))]))
    
(define-syntax (include-at/relative-to/reader stx)
  (syntax-case stx ()
    [(_ ctx loc fn reader)
     (with-syntax ([_stx stx])
       (syntax/loc stx 
         (do-include _stx ctx loc fn 
                     (letrec-syntax ([the-reader (lambda (stx)
                                                   (datum->syntax
                                                    #'here
                                                    (make-reader reader)))])
                       the-reader))))]))
