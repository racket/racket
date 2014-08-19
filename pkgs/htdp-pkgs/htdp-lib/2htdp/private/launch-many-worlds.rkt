#lang racket/base

(provide
 ;; (launch-many-worlds e1 ... e2)
 ;; run expressions e1 through e2 in parallel,
 ;; produce all values
 launch-many-worlds
 ;; launch-many-worlds/proc : (-> Any) *-> [Listof Any]
 launch-many-worlds/proc)

;; ---------------------------------------------------------------------------------------------------
(require racket/list racket/function racket/gui mzlib/etc htdp/error)

(define-syntax-rule 
  (launch-many-worlds e ...) 
  (launch-many-worlds* (lambda () e) ...))

(define (launch-many-worlds/proc . loth)
  ;; check arguments:
  (for ([th loth][i (in-naturals 1)])
    (check-proc 'launch-many-worlds/proc th 0 i "no arguments"))
  ;; -- IN -- 
  (apply launch-many-worlds* loth))

;; [Listof (-> X)] ->* X ...
;; run the thunks as parallel threads and produce 'values' of their results
;; effect: propagate exn when one of them raises one 
(define (launch-many-worlds* . th*)
  ;; allocate the program's resources in the active custodian 
  (define cc (current-custodian))
  ;; c* is used to shut down launch-many-worlds when one of them raises an exn
  [define c* (make-custodian)]
  [define ch (make-channel)]
  (parameterize ([current-custodian c*]) 
    (for/list ((th th*) (i (in-naturals)))
      (parameterize ([current-eventspace (make-eventspace)])
        (queue-callback
         (lambda ()
           (with-handlers ((exn? (lambda (x) (channel-put ch x))))
             (channel-put ch (list i (parameterize ([current-custodian cc]) (th))))))))))
  ;; for all X: (U Exn X) -> X
  (define (handle x)
    (when (exn? x) (custodian-shutdown-all c*) (raise x))
    x)
  ;; [Listof X]
  (define results
     (for/list ((n (in-range (length th*))))
      (sync (handle-evt ch handle))))
  ;; -- IN -- 
  (apply values (map second (sort results < #:key first))))
