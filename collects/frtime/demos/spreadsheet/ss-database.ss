(module ss-database (lib "frtime-big.ss" "frtime")
  (require (rename (lib "frp-core.ss" "frtime") current-custs current-custs))
  (require (rename (lib "frp-core.ss" "frtime") do-in-manager do-in-manager))
  (require (as-is:unchecked mzscheme make-hash-table hash-table-get hash-table-put!))
  ;(require (lib "string.ss"))
    
  (define-struct rcvXbeh (rcv beh))

  (define put-text-at!
    (lambda (ht txt key)
      (lambda ()
        (parameterize ([current-custs '()])
          (let* ([rcv (event-receiver)]
                 [hld (hold rcv txt)]
                 [both (make-rcvXbeh rcv hld)])
            (hash-table-put! ht key both)
            both)))))
  
  (define update-value
    (lambda (ht k v)
      (send-event
       (rcvXbeh-rcv
        (hash-table-get
         ht
         k
         (put-text-at! ht v k)))
       v)))
  
  (define retreive-value
    (lambda (ht k)
      (rcvXbeh-beh
       (hash-table-get ht k (put-text-at! ht "" k)))))
      
  
  ;; put-text-at! is used in both the setter and
  ;; getter, so that things will be in sync
  (define (split-through-list-b evt fn)
    (let* ([ht-text (make-hash-table)]
           [sig (map-e (lambda (val-e)
                         (map (lambda (key)
                                (update-value ht-text key val-e))
                              (fn val-e)))
                       evt)])
      (lambda (x)
        sig
        (retreive-value ht-text x))))
  
  (define (split-through-list-b/init evt fn bindings)
    (let* ([ht-text (make-hash-table)]
           [sig (map-e (lambda (val-e)
                         (map (lambda (key)
                                (update-value ht-text key val-e))
                              (fn val-e)))
                       evt)])
      (for-each ; bindings are of the form ((key val) ...)
       (lambda (lst)
         (update-value ht-text (car lst) (cadr lst))
         (printf "~a~n" lst))
       bindings)
      (lambda (x)
        sig
        (retreive-value ht-text x))))
  
  (define (make-accessor formula commit-e currently-selected-cells)
     (split-through-list-b (commit-e . -=> . (value-now formula))
                           (lambda (_) (value-now currently-selected-cells))))
  
  (define (make-accessor/initial-bindings formula commit-e currently-selected-cells bindings)
    (split-through-list-b/init (commit-e . -=> . (value-now formula))
                               (lambda (_) (value-now currently-selected-cells))
                               bindings))
  
  (provide make-accessor
           make-accessor/initial-bindings))