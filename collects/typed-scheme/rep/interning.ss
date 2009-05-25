#lang scheme/base

(require syntax/boundmap (for-syntax scheme/base stxclass)
         #;macro-debugger/stepper)

(provide defintern hash-id)

(define-syntax (defintern stx)
  (syntax-parse stx
    [(_ name+args make-name key (~or [#:extra-arg e:expr] #:opt) ...)
     (if #'e
         #'(defintern name+args (lambda () (make-hash #;'weak)) make-name key #:extra-arg e)
         #'(defintern name+args (lambda () (make-hash #;'weak)) make-name key))]
    [(_ (*name:id arg:id ...) make-ht make-name key-expr (~or [#:extra-arg e:expr]) ...)
     #'(define *name
	 (let ([table (make-ht)])
	   (lambda (arg ...)
	     #;(all-count!)
	     (let ([key key-expr])
	       (hash-ref table key
			 (lambda ()
			   (let ([new (make-name (count!) e ... arg ...)])
			     (hash-set! table key new)
			     new)))))))]))

(define (make-count!)
  
  (let ([state 0])
    (lambda () (begin0 state (set! state (add1 state)))))   
  #;
  (let ([ch (make-channel)])
    (thread (lambda () (let loop ([n 0]) (channel-put ch n) (loop (add1 n)))))
    (lambda () (channel-get ch))))

(provide #;count! #;all-count! union-count!)

(define count! (make-count!))
(define union-count! (make-count!))
(define all-count! (make-count!))
(define id-count! (make-count!))



(define identifier-table (make-module-identifier-mapping))

(define (hash-id id)
  (module-identifier-mapping-get 
   identifier-table 
   id 
   (lambda () (let ([c (id-count!)])
                (module-identifier-mapping-put! identifier-table id c)
                c))))
