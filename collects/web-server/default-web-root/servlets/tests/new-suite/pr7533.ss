(module pr7533 mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start initial-request)
    (define start-time (current-seconds))
    (let loop ((last-time start-time))
      (let ((time (current-seconds)))
        (send/suspend
         (lambda (k)
           `(html
             (form ((action ,k) (method "post"))
                   (p ,(format "It has been ~a seconds since starting (~a seconds since last iteration)."
                               (- time start-time)
                               (- time last-time)))
                   (input ((type "submit")))))))
        (loop time)))))
