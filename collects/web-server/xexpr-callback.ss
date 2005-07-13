;; Mike Burns 2004

;; Used for send/suspend/callback.

(module xexpr-callback mzscheme
  (require (lib "xml.ss" "xml"))
  (provide xexpr/callback?)

  ;; Is it a Xexpr, or an Xexpr with procedures?
  (define (xexpr/callback? x)
    (correct-xexpr? x 
                    (lambda () #t)
                    (lambda (exn)
                      (procedure? (exn:invalid-xexpr-code exn))))))
