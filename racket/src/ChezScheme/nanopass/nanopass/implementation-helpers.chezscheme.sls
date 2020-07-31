;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details
#!chezscheme
(library (nanopass implementation-helpers)
  (export
    ;; formatting
    format printf pretty-print

    ;; listy stuff
    iota make-list list-head

    ;; gensym stuff (related to nongenerative languages)
    gensym regensym

    ;; source-information stuff
    syntax->source-information
    source-information-source-file
    source-information-byte-offset-start
    source-information-char-offset-start
    source-information-byte-offset-end
    source-information-char-offset-end
    source-information-position-line
    source-information-position-column
    source-information-type
    provide-full-source-information

    ;; library export stuff (needed for when used inside module to
    ;; auto-indirect export things)
    indirect-export

    ;; compile-time environment helpers
    make-compile-time-value

    ;; code organization helpers
    module

    ;; useful for warning items
    warningf errorf

    ;; used to get the best performance from hashtables
    eq-hashtable-set! eq-hashtable-ref

    ;; debugging support
    trace-lambda trace-define-syntax trace-let trace-define
          
    ;; needed to know what code to generate
    optimize-level

    ;; the base record, so that we can use gensym syntax
    define-nanopass-record

    ;; failure token so that we can know when parsing fails with a gensym
    np-parse-fail-token

    ;; handy syntactic stuff
    with-implicit

    ;; abstraction of the grabbing the syntactic environment that will work in
    ;; Chez, Ikarus, & Vicare
    with-compile-time-environment

    ;; apparently not neeaded (or no longer needed)
    ; scheme-version= scheme-version< scheme-version> scheme-version>=
    ; scheme-version<= with-scheme-version gensym? errorf with-output-to-string
    ; with-input-from-string
    )
  (import (chezscheme))

  ; the base language
  (define-syntax define-nanopass-record
    (lambda (x)
      (syntax-case x ()
        [(k) (with-implicit (k nanopass-record nanopass-record? nanopass-record-tag)
               #'(define-record-type (nanopass-record make-nanopass-record nanopass-record?)
                   (nongenerative #{nanopass-record d47f8omgluol6otrw1yvu5-0})
                   (fields (immutable tag nanopass-record-tag))))])))
 
  ;; another gensym listed into this library
  (define np-parse-fail-token '#{np-parse-fail-token dlkcd4b37swscag1dvmuiz-13})

  ;; the following should get moved into Chez Scheme proper (and generally
  ;; cleaned up with appropriate new Chez Scheme primitives for support)
  (define regensym
    (case-lambda
      [(gs extra)
       (unless (gensym? gs) (errorf 'regensym "~s is not a gensym" gs))
       (unless (string? extra) (errorf 'regensym "~s is not a string" extra))
       (let ([pretty-name (parameterize ([print-gensym #f]) (format "~s" gs))]
             [unique-name (gensym->unique-string gs)])
         (with-input-from-string (format "#{~a ~a~a}" pretty-name unique-name extra) read))]
      [(gs extra0 extra1)
       (unless (gensym? gs) (errorf 'regensym "~s is not a gensym" gs))
       (unless (string? extra0) (errorf 'regensym "~s is not a string" extra0))
       (unless (string? extra1) (errorf 'regensym "~s is not a string" extra1))
       (with-output-to-string (lambda () (format "~s" gs)))
       (let ([pretty-name (parameterize ([print-gensym #f]) (format "~s" gs))]
             [unique-name (gensym->unique-string gs)])
         (with-input-from-string (format "#{~a~a ~a~a}" pretty-name extra0 unique-name extra1) read))]))

  (define-syntax define-scheme-version-relop
    (lambda (x)
      (syntax-case x ()
        [(_ name relop strict-inequality?)
          #`(define name
              (lambda (ls)
                (let-values ([(a1 b1 c1) (scheme-version-number)]
                             [(a2 b2 c2)
                              (cond
                                [(fx= (length ls) 1) (values (car ls) 0 0)]
                                [(fx= (length ls) 2) (values (car ls) (cadr ls) 0)]
                                [(fx= (length ls) 3) (values (car ls) (cadr ls) (caddr ls))])])
                  #,(if (datum strict-inequality?)
                        #'(or (relop a1 a2)
                              (and (fx= a1 a2)
                                   (or (relop b1 b2)
                                       (and (fx= b1 b2)
                                            (relop c1 c2)))))
                        #'(and (relop a1 a2) (relop b1 b2) (relop c1 c2))))))])))

  (define-scheme-version-relop scheme-version= fx= #f)
  (define-scheme-version-relop scheme-version< fx< #t)
  (define-scheme-version-relop scheme-version> fx> #t)
  (define-scheme-version-relop scheme-version<= fx<= #f)
  (define-scheme-version-relop scheme-version>= fx>= #f)

  (define-syntax with-scheme-version
    (lambda (x)
      (define-scheme-version-relop scheme-version= fx= #f)
      (define-scheme-version-relop scheme-version< fx< #t)
      (define-scheme-version-relop scheme-version> fx> #t)
      (define-scheme-version-relop scheme-version<= fx<= #f)
      (define-scheme-version-relop scheme-version>= fx>= #f)
      (define finish
        (lambda (pat* e** elsee*)
          (if (null? pat*)
              #`(begin #,@elsee*)
              (or (and (syntax-case (car pat*) (< <= = >= >)
                         [(< v ...) (scheme-version< (datum (v ...)))]
                         [(<= v ...) (scheme-version<= (datum (v ...)))]
                         [(= v ...) (scheme-version= (datum (v ...)))]
                         [(>= v ...) (scheme-version>= (datum (v ...)))]
                         [(> v ...) (scheme-version> (datum (v ...)))]
                         [else #f])
                       #`(begin #,@(car e**)))
                  (finish (cdr pat*) (cdr e**) elsee*)))))
      (syntax-case x (else)
        [(_ [pat e1 e2 ...] ... [else ee1 ee2 ...])
         (finish #'(pat ...) #'((e1 e2 ...) ...) #'(ee1 ee2 ...))]
        [(_ [pat e1 e2 ...] ...)
         (finish #'(pat ...) #'((e1 e2 ...) ...) #'())])))

  (define provide-full-source-information
    (make-parameter #f (lambda (n) (and n #t))))

  (define-record-type source-information
    (nongenerative)
    (sealed #t)
    (fields source-file byte-offset-start char-offset-start byte-offset-end
      char-offset-end position-line position-column type)
    (protocol
      (lambda (new)
        (lambda (a type)
          (let ([so (annotation-source a)])
            (let ([sfd (source-object-sfd so)]
                  [bfp (source-object-bfp so)]
                  [efp (source-object-efp so)])
              (if (provide-full-source-information)
                  (let ([ip (open-source-file sfd)])
                    (let loop ([n bfp] [line 1] [col 1])
                      (if (= n 0)
                          (let ([byte-offset-start (port-position ip)])
                            (let loop ([n (- efp bfp)])
                              (if (= n 0)
                                  (let ([byte-offset-end (port-position ip)])
                                    (close-input-port ip)
                                    (new (source-file-descriptor-path sfd)
                                      byte-offset-start bfp
                                      byte-offset-end efp
                                      line col type))
                                  (let ([c (read-char ip)]) (loop (- n 1))))))
                          (let ([c (read-char ip)])
                            (if (char=? c #\newline)
                                (loop (- n 1) (fx+ line 1) 1)
                                (loop (- n 1) line (fx+ col 1)))))))
                  (new (source-file-descriptor-path sfd)
                    #f bfp #f efp #f #f type))))))))

  (define syntax->source-information
    (lambda (stx)
      (let loop ([stx stx] [type 'at])
        (cond
          [(syntax->annotation stx) =>
           (lambda (a) (make-source-information a type))]
          [(pair? stx) (or (loop (car stx) 'near) (loop (cdr stx) 'near))]
          [else #f]))))

  (define-syntax with-compile-time-environment
    (syntax-rules ()
      [(_ (arg) body* ... body) (lambda (arg) body* ... body)]))

  (with-scheme-version
    [(< 8 3 1)
     (define syntax->annotation (lambda (x) #f))
     (define annotation-source (lambda (x) (errorf 'annotation-source "unsupported before version 8.4")))
     (define source-object-bfp (lambda (x) (errorf 'source-object-bfp "unsupported before version 8.4")))
     (define source-object-sfd (lambda (x) (errorf 'source-object-sfd "unsupported before version 8.4")))
     (define source-file-descriptor-path (lambda (x) (errorf 'source-file-descriptor-path "unsupported before version 8.4")))])
  (with-scheme-version
    [(< 8 1) (define-syntax indirect-export (syntax-rules () [(_ id indirect-id ...) (define t (if #f #f))]))]))
