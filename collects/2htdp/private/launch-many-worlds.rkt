#lang scheme

(require mred/mred mzlib/etc)

(provide
  launch-many-worlds
  ;; (launch-many-worlds e1 ... e2)
  ;; run expressions e1 through e2 in parallel,
  ;; produce all values
  )

(define-syntax-rule 
  (launch-many-worlds e ...) 
  (launch-many-worlds* (lambda () e) ...))

;; [Listof (-> X)] ->* X ...
(define (launch-many-worlds* . loth)
  (let* ([c* (make-custodian)]
         [ch (make-channel)]
         [pu (curry channel-put ch)]
         [th (map (lambda (th i)
                    (parameterize ([current-custodian c*])
                      (rec t
                        (thread
                         (lambda ()
                           (with-handlers ((exn? pu)) (pu (list i t (th)))))))))
                  loth
                  (build-list (length loth) (lambda (i) i)))])
    (letrec ([L (lambda (th r*)
                  (if (null? th)
                      (apply values (map third (sort r* < #:key first)))
                      (sync 
                       (handle-evt 
                        ch 
                        (lambda (x)
                          (if (exn? x)
                              (begin
                                (custodian-shutdown-all c*)
                                (raise x))
                              (L (remq (second x) th) (cons x r*))))))))])
      (with-handlers ((exn:break? (lambda (xn) (custodian-shutdown-all c*))))
        (L th '())))))

;; some silly tests

; (launch-many-worlds 1 2 3)

; (launch-many-worlds 1 (let loop ([n 100]) (printf "~s\n" n) (sleep 1) (loop n)) 3)
