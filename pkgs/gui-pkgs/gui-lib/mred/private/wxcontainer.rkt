(module wxcontainer racket/base
  (require racket/class
           racket/list
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "helper.rkt"
           "wx.rkt")

  (provide (protect-out make-container-glue%
                        wx-make-container%))

  (define (make-container-glue% %)
    (class %
      (init mr prxy)
      (init-rest args)
      (inherit do-place-children do-get-graphical-min-size get-children-info get-hidden-child)
      (define mred mr)
      (define proxy prxy)
      (override*
       [get-graphical-min-size (lambda () 
                                 (cond
                                  [mred (let ([info
                                               (map (lambda (i)
                                                      (list (child-info-x-min i) (child-info-y-min i)
                                                            (child-info-x-stretch i) (child-info-y-stretch i)))
                                                    (get-children-info))])
                                          (let-values ([(w h) (as-exit (lambda () (send mred container-size 
                                                                                        (if (get-hidden-child)
                                                                                            (cdr info) ; hidden child is first
                                                                                            info))))])
                                            (list w h)))]
                                  [else (do-get-graphical-min-size)]))]
       [place-children (lambda (l w h) 
                         (cond
                          [(null? l) null]
                          [mred (as-exit (lambda () (send mred place-children l w h)))]
                          [else (do-place-children l w h)]))])
      (apply super-make-object mred proxy args)))

  ;; make-container% - for panels and top-level windows
  (define (wx-make-container% %) %))
