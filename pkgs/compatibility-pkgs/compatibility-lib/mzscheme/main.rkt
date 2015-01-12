;;----------------------------------------------------------------------
;; mzscheme: provide everything

(module mzscheme '#%kernel
  (#%require racket/private/more-scheme
             (all-except racket/private/misc
			 collection-path
			 collection-file-path)
             racket/private/letstx-scheme
             racket/private/stxcase-scheme
             racket/private/stx
             racket/private/qqstx
             racket/private/define
             mzscheme/private/stxmz-body
             mzscheme/private/old-ds
             mzscheme/private/old-rp
             mzscheme/private/old-if
             mzscheme/private/old-procs
             racket/private/map ; shadows #%kernel bindings
             racket/private/kernstruct
             racket/private/promise
             racket/private/cert
             (only racket/private/cond old-cond)
             ;; shadows #%kernel bindings:
             (only racket/private/list 
                   assq assv assoc reverse)
             racket/private/member
             racket/tcp
             racket/udp
             '#%builtin ; so it's attached
             (for-syntax racket/base))
  
  ;; Mostly from Dybvig:
  (define-syntax (old-case x)
    (syntax-case* x (else) (let ([else-stx (datum->syntax #f 'else)])
                             (lambda (a b) (free-identifier=? a else-stx)))
      ((_ v)
       (syntax (#%expression (begin v (void)))))
      ((_ v (else e1 e2 ...))
       (syntax/loc x (#%expression (begin v (let-values () e1 e2 ...)))))
      ((_ v ((k ...) e1 e2 ...))
       (syntax/loc x (if (case-test v (k ...)) (let-values () e1 e2 ...) (void))))
      ((self v ((k ...) e1 e2 ...) c1 c2 ...)
       (syntax/loc x (let ((x v))
                       (if (case-test x (k ...))
                           (let-values () e1 e2 ...)
                           (self x c1 c2 ...)))))
      ((_ v (bad e1 e2 ...) . rest)
       (raise-syntax-error 
        #f
        "bad syntax (not a datum sequence)"
        x
        (syntax bad)))
      ((_ v clause . rest)
       (raise-syntax-error 
        #f
        "bad syntax (missing expression after datum sequence)"
        x
        (syntax clause)))
      ((_ . v)
       (not (null? (syntax-e (syntax v))))
       (raise-syntax-error 
        #f
        "bad syntax (illegal use of `.')"
        x))))
  
  (define-syntax case-test
    (lambda (x)
      (syntax-case x ()
        ;; For up to 3 elements, inline `eqv?' tests:
	[(_ x (k))
         (syntax (eqv? x 'k))]
	[(_ x (k1 k2))
         (syntax (let ([tmp x]) (if (eqv? tmp 'k1) #t (eqv? tmp 'k2))))]
	[(_ x (k1 k2 k3))
         (syntax (let ([tmp x]) (if (eqv? tmp 'k1) #t (if (eqv? tmp 'k2) #t (eqv? tmp 'k3)))))]
	[(_ x (k ...))
	 (syntax (memv x '(k ...)))])))

  (#%provide require require-for-syntax require-for-template require-for-label
             provide provide-for-syntax provide-for-label
             (all-from-except racket/private/more-scheme case
                              log-fatal log-error log-warning log-info log-debug
                              hash-update hash-update!)
             (rename old-case case)
             (all-from racket/private/misc)
             collection-path
             collection-file-path
             (all-from-except racket/private/stxcase-scheme _ datum datum-case with-datum)
             (all-from-except racket/private/letstx-scheme 
                              -define -define-syntax -define-struct
                              cond old-cond else =>)
             (rename old-cond cond)
             define-struct let-struct
             identifier? ;; from racket/private/stx
             (all-from racket/private/cert)
             (all-from-except racket/private/qqstx quasidatum undatum undatum-splicing)
             (all-from racket/private/define)
             (all-from racket/private/kernstruct)
             force delay promise?
             (all-from-except '#%kernel #%module-begin #%datum 
                              if make-empty-namespace
                              syntax->datum datum->syntax
                              free-identifier=?
                              free-transformer-identifier=?
                              free-template-identifier=?
                              free-label-identifier=?
                              vector-copy!
                              thread-send
                              thread-receive
                              thread-try-receive
                              thread-receive-evt
                              make-hash make-immutable-hash make-weak-hash
                              make-hasheq make-immutable-hasheq make-weak-hasheq
                              hash? hash-eq? hash-weak?
                              hash-ref hash-set! hash-set
                              hash-remove! hash-remove 
                              hash-copy hash-count
                              hash-map hash-for-each 
                              hash-iterate-first hash-iterate-next
                              hash-iterate-value hash-iterate-key
                              log-message log-level? make-logger logger? current-logger logger-name
                              make-log-receiver log-receiver?
                              prop:incomplete-arity)
             (rename syntax->datum syntax-object->datum)
             (rename datum->syntax datum->syntax-object)
             (rename free-identifier=? module-identifier=?)
             (rename free-transformer-identifier=? module-transformer-identifier=?)
             (rename free-template-identifier=? module-template-identifier=?)
             (rename free-label-identifier=? module-label-identifier=?)
             (rename free-identifier=?* free-identifier=?)
             make-hash-table hash-table? make-immutable-hash-table
             (rename hash-ref hash-table-get)
             (rename hash-set! hash-table-put!)
             (rename hash-remove! hash-table-remove!)
             (rename hash-count hash-table-count)
             (rename hash-copy hash-table-copy)
             (rename hash-map hash-table-map)
             (rename hash-for-each hash-table-for-each)
             (rename hash-iterate-first hash-table-iterate-first)
             (rename hash-iterate-next hash-table-iterate-next)
             (rename hash-iterate-value hash-table-iterate-value)
             (rename hash-iterate-key hash-table-iterate-key)
             namespace-transformer-require
             transcript-on transcript-off
             (rename cleanse-path expand-path)
             (rename if* if)
             (rename list list-immutable)
             make-namespace
             #%top-interaction
             map for-each andmap ormap
             assq assv assoc reverse memq memv member
             (rename old-datum #%datum)
             (rename mzscheme-in-stx-module-begin #%module-begin)
             (rename #%module-begin #%plain-module-begin)
             (rename lambda #%plain-lambda)
             (rename #%app #%plain-app)
	     (all-from racket/tcp)
             (all-from racket/udp)))
