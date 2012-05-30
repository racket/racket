(module wx racket/base
  (require racket/class
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "helper.rkt")

  (provide (protect-out wx<%>
                        wx/proxy<%>
                        make-glue%
                        wx->mred
                        wx->proxy
                        mred%
                        mred->wx
                        mred->wx-container))

  ;; The windowing wx classes are not exposed directly.
  ;; Instead, we expose wrapper classes that have wx instances.
  ;; The `make-glue%' mixin adds fields and methods to map 
  ;; wx (internal) objects to mred (external) objects.
  ;; Sometimes, multiple wx instances have one mred instance;
  ;; hance proxies.

  (define wx<%> (interface () get-mred))
  (define wx/proxy<%> (interface (wx<%>) get-proxy))
  
  (define (make-glue% %)
    (class* % (wx/proxy<%>)
      (init mr prxy)
      (init-rest args)
      (define mred mr)
      (define proxy prxy)
      (public*
       [get-mred (lambda () mred)]
       [get-proxy (lambda () proxy)])
      (apply super-make-object args)))

  (define wx-get-mred/gen (make-generic wx<%> 'get-mred))
  (define wx-get-proxy/gen (make-generic wx/proxy<%> 'get-proxy))

  (define (wx->mred w) (send-generic w wx-get-mred/gen))
  (define (wx->proxy w) (send-generic w wx-get-proxy/gen))


  (define-local-member-name private-wx)
  
  (define mred%
    (class object%
      (init-field private-wx)
      (super-make-object)))
  
  (define mred->wx (class-field-accessor mred% private-wx))
  
  (define (mred->wx-container w) (send (mred->wx w) get-container)))
