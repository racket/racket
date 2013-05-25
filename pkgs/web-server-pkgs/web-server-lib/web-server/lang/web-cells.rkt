#lang racket/base
(require racket/contract
         racket/list
         racket/serialize
         web-server/lang/serial-lambda)
;; Implementation: Have a distinguished frame variable that is read and captured by send/suspend, 
;; installed on invocations of continuations by the server (and NOT from other continuation invocations)

;; Data types
(define-serializable-struct primitive-wc (id))
(define-serializable-struct frame (env))

;; Frames  
(define *wc-frame* (make-thread-cell (make-frame (make-immutable-hasheq empty)) #t))
(define (current-frame) (thread-cell-ref *wc-frame*))
(define (update-frame! nf) (thread-cell-set! *wc-frame* nf))

;; Web Cell Sets
(define web-cell-set? frame?)
(define (capture-web-cell-set) (current-frame))
(define (restore-web-cell-set! wcs) (update-frame! wcs))

(provide/contract
 [web-cell-set? (any/c . -> . boolean?)]
 [capture-web-cell-set (-> web-cell-set?)]
 [restore-web-cell-set! (web-cell-set? . -> . void)])

;; Web Cells
(define next-web-cell-id
  (let ([i (box 0)])
    (lambda ()
      (begin0 (unbox i)
              (set-box! i (add1 (unbox i)))))))

(define web-cell? primitive-wc?)

(define-syntax make-web-cell
  (syntax-rules ()
    [(_ default)
     (make-web-cell* (closure->deserialize-name (lambda () 'web-cell))
                     default)]))
(define (make-web-cell* label default)
  (define id (next-web-cell-id))
  (define key (string->symbol (format "~a-~a" label id)))
  (define wc (make-primitive-wc key))
  (web-cell-shadow wc default)
  wc)

(define (web-cell-ref pwc)
  (define i (primitive-wc-id pwc))
  (hash-ref
   (frame-env (current-frame)) i
   (lambda ()
     (error 'web-cell "Undefined web-cell: ~.s" i))))

(define (web-cell-shadow wc nv)
  (update-frame!
   (make-frame
    (hash-set (frame-env (current-frame))
              (primitive-wc-id wc) nv))))

(provide make-web-cell)
(provide/contract
 [web-cell? (any/c . -> . boolean?)]
 [web-cell-ref (web-cell? . -> . any/c)]
 [web-cell-shadow (web-cell? any/c . -> . void)])
