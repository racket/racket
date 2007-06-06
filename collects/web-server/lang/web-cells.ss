(module web-cells mzscheme
  (require (lib "serialize.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "contract.ss")
           "../private/closure.ss")
  ;; Implementation: Have a distinguished frame variable that is read and captured by send/suspend, 
  ;; installed on invocations of continuations by the server (and NOT from other continuation invocations)
  
  ;; Data types
  (define-serializable-struct primitive-wc (id))
  (define-serializable-struct frame (env))
  
  ;; Environment
  (define empty-env empty)
  (define env-lookup
    (match-lambda*
      [(list id (list))
       (error 'web-cell "Undefined web-cell: ~e" id)]
      [(list id (list-rest (list-rest a-id a-val) env))
       (if (eq? id a-id)
           a-val
           (env-lookup id env))]))  
  (define env-replace
    (match-lambda*
      [(list id val (list))
       (list (cons id val))]
      [(list id val (list-rest (list-rest a-id a-val) env))
       (if (eq? id a-id)
           (list* (cons id val) env)
           (list* (cons a-id a-val)
                  (env-replace id val env)))]))
  
  ;; Frames  
  (define *wc-frame* (make-thread-cell (make-frame empty-env) #t))
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
    (env-lookup (primitive-wc-id pwc)
                (frame-env (current-frame))))
  
  (define (web-cell-shadow wc nv)
    (update-frame!
     (make-frame
      (env-replace (primitive-wc-id wc) nv
                   (frame-env (current-frame))))))
  
  (provide make-web-cell)
  (provide/contract
   [web-cell? (any/c . -> . boolean?)]
   [web-cell-ref (web-cell? . -> . any/c)]
   [web-cell-shadow (web-cell? any/c . -> . void)]))