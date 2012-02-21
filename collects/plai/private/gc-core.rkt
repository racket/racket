#lang scheme
(require
 (for-syntax scheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Locations
#|
(provide wrapped-location?)
(define-struct wrapped-location (location))

(provide/contract (wrap-location (location? . -> . wrapped-location?)))
(define (wrap-location loc)
  (make-wrapped-location loc))

(provide/contract (unwrap-location (wrapped-location? . -> . location?)))
(define (unwrap-location wloc)
  (wrapped-location-location wloc))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heap management
(provide current-heap)
(define current-heap (make-parameter false))

(define (format-cell cell)
  (let* ([str (format "~s" cell)]
         [len (string-length str)])
    (if (<= len 10)
        (string-append str (build-string (- 10 len) (λ (_) #\space)))
        (substring str 0 10))))

;;; Textual representation of the heap
(provide heap-as-string)
(define (heap-as-string)
  (let ([step 0])
    (apply string-append
           (for/list ([elt (in-vector (current-heap))])
             (cond
               [(= step 0) 
                (begin
                  (set! step (add1 step))
                  (format-cell elt))]
               [(= step 9) 
                (begin
                  (set! step 0)
                  (string-append (format-cell elt) "\n"))]
               [else
                (begin
                  (set! step (add1 step))
                  (string-append " " (format-cell elt)))])))))

;;; Predicate determines values that may be stored on the heap.  Limit this to "small" values that
;;; conceptually occupy a small, fixed amount of space.  Closures are an exception.
(provide/contract [heap-value? (any/c . -> . boolean?)])
(define (heap-value? v)
  (or (number? v) (symbol? v) (boolean? v) (empty? v) (procedure? v)))

(provide location?)
(define (location? v)
  (if (vector? (current-heap))
      (and (exact-nonnegative-integer? v) (< v (vector-length (current-heap))))
      (error "Heap is uninitialized")))

(provide/contract (init-heap! (exact-nonnegative-integer? . -> . void?)))
(define (init-heap! size)
  (current-heap (build-vector size (λ (ix) false))))

(provide/contract (heap-set! (location? heap-value? . -> . void?)))
(define (heap-set! location value)
  (vector-set! (current-heap) location value)
  (when gui
    (send gui update-view #:location location)))

(provide/contract (heap-ref (location? . -> . heap-value?)))
(define (heap-ref location)
  (vector-ref (current-heap) location))

(provide/contract (heap-size (-> (or/c false/c exact-nonnegative-integer?))))
(define (heap-size)
  (and (vector? (current-heap)) (vector-length (current-heap))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Root set management

(provide gc-roots-key)
(define gc-roots-key (gensym 'gc-roots-key))

;;; Roots are defined with custom getters and setters as they can be created in various ways.
(provide root? root-name make-root)
(define-struct root (name get set!)
  #:property prop:custom-write (λ (v port write?)
                                 (display (format "#<root:~a>" (root-name v)) port)))

(provide make-env-root)
(define-syntax (make-env-root stx)
  (syntax-case stx ()
    [(_ id) (identifier? #'id)
            #`(make-root 'id (λ () id) (λ (loc) (set! id loc)))]))

;;; Roots on the stack.
(provide/contract (stack-roots (-> (listof root?))))
(define (stack-roots)
  (filter is-mutable-root?
          (apply append (continuation-mark-set->list (current-continuation-marks) gc-roots-key))))

; An immutable root is a reference to a value or procedure in the Scheme heap.
(define (is-mutable-root? root)
  (location? ((root-get root))))

(provide/contract (make-stack-root (symbol? location? . -> . root?)))
(define (make-stack-root id location)
  (make-root id (λ () location) (λ (new-location) (set! location new-location))))

(provide/contract (read-root (root? . -> . location?)))
(define (read-root root)
  ((root-get root)))

(provide/contract (set-root! (root? location? . -> . any)))
(define (set-root! root loc)
  ((root-set! root) loc))

(provide/contract (get-global-roots (-> (listof root?))))
(define (get-global-roots)
  (filter is-mutable-root? global-roots))

(define global-roots empty)

(provide/contract (add-global-root! (root? . -> . void?)))
(define (add-global-root! root)
  (set! global-roots (cons root global-roots)))

(provide get-root-set)
(define-syntax (get-root-set stx)
  (syntax-case stx ()
    [(_ root-id ...)
     (andmap identifier? (syntax->list #'(root-id ...)))
     #`(begin
         (append
          (list (make-root 'root-id (λ () root-id) 
                           (λ (loc) 
                             (set! root-id loc)))
                ...)
          (get-global-roots)
          (stack-roots)))]
    [(_ e ...)
     (let ([err (ormap (λ (x) (and (not (identifier? x)) x)) (syntax->list #'(e ...)))])
       (raise-syntax-error false
                           "expected an identifier to treat as a root"
                           stx
                           err))]
    [_ (raise-syntax-error false
                           "missing open parenthesis"
                           stx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environments of closures

; Once the closure is garbage collected, its environment is only reachable by a weak reference to 
; the closure.
(define closure-envs (make-weak-hash))

(provide/contract (add-closure-env! (procedure? (listof root?) . -> . any)))
(define (add-closure-env! proc roots)
  (hash-set! closure-envs proc roots))

(provide/contract (get-closure-env (procedure? . -> . (or/c false/c (listof root?)))))
(define (get-closure-env proc)
  (hash-ref closure-envs proc false))

(provide/contract (procedure-roots (procedure? . -> . (listof root?))))
(define (procedure-roots proc)
  (filter is-mutable-root? (hash-ref closure-envs proc empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Optional UI

(provide set-ui!)
(define (set-ui! ui%)
  (set! gui (new ui% [heap-vec (current-heap)])))

(define gui false)
