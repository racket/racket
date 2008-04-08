#lang scheme/base

(require syntax/boundmap)

(provide defintern hash-id)


(define-syntax defintern
  (syntax-rules ()
    [(_ name+args make-name key) 
     (defintern name+args (lambda () (make-hash #;'weak)) make-name key)]
    [(_ (*name arg ...) make-ht make-name key-expr)
     (define *name
       (let ([table (make-ht)])
         (lambda (arg ...)
           #;(all-count!)
           (let ([key key-expr])
             (hash-ref table key
                       (lambda ()
                         (let ([new (make-name (count!) arg ...)])
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
