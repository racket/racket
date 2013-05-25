(module pict-box-lib mzscheme
  (require mred
           mzlib/class
           texpict/mrpict
           "image-snipr.rkt")
  
  (provide get-snp/poss
           build-lib-pict-stx
           snip-location
           (struct snp/pos (snp x y))
           generate-ids)
  
  (define-struct snp/pos (snp x y))
  
  ;; get-snip/poss : editor-snip -> (listof snp/pos)
  ;; called on drscheme's thread
  (define (get-snp/poss es)
    (let ([pb (send es get-editor)])
      (let loop ([snip (send pb find-first-snip)])
        (cond
          [(not snip) null]
          [(is-a? snip image-snip/r%)
           (let ([real-snip (send snip get-orig-snip)])
             (let-values ([(x y) (snip-location pb snip)])
               (cons (make-snp/pos real-snip x y)
                     (loop (send snip next)))))]
          [(is-a? snip readable-snip<%>)
           (let-values ([(x y) (snip-location pb snip)])
             (cons (make-snp/pos snip x y)
                   (loop (send snip next))))]
          [else (loop (send snip next))]))))
  
  ;; build-lib-pict-stx : syntax (listof snp/pos) -> syntax
  ;; called on the user's thread
  (define (build-lib-pict-stx send-back snp/poss)
    (with-syntax ([(subpicts ...) (map (lambda (snp/pos) (send (snp/pos-snp snp/pos) read-special #f 0 0 0))
                                       snp/poss)]
                  [(ids ...) (generate-ids "snip-id" (map snp/pos-snp snp/poss))]
                  [(x ...) (map snp/pos-x snp/poss)]
                  [(y ...) (map snp/pos-y snp/poss)])
      (with-syntax ([send-back (send-back (syntax (ids ...)))])
        (syntax
         (let ([ids subpicts] ...)
           send-back
           (let ([max-h (max 0 (+ y (pict-height ids)) ...)])
             (panorama (picture 0 0 `((place ,(- x (/ (pict-height ids) 2))
                                             ,(- max-h y (/ (pict-height ids) 2))
                                             ,ids)
                                      ...)))))))))
  
  (define (generate-ids pre lst)
    (let loop ([i 0]
               [l lst])
      (cond
        [(null? l) null]
        [else (cons (datum->syntax-object #'here (string->symbol (format "~a~a" pre i)))
                    (loop (+ i 1)
                          (cdr l)))])))
  
  (define (snip-location pb snip)
    (let ([x (box 0)]
          [y (box 0)])
      (send pb get-snip-location snip x y)
      (values (unbox x) (unbox y)))))
