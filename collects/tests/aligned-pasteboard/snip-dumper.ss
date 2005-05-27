(module snip-dumper mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred"))
  
  (provide
   dump-children
   (struct snip-dump (left top right bottom children))
   dump=?)
  
  ;;dump=?: ((union snip-dump? (listof snip-dump?)) . -> . boolean?)
  (define (dump=? dump1 dump2)
    (cond
      [(and (list? dump1) (list? dump2)
            (eq? (length dump1) (length dump2)))
       (andmap dump=? dump1 dump2)]
      [(and (snip-dump? dump1) (snip-dump? dump2))
       (and
        (dump=? (snip-dump-left dump1)
                (snip-dump-left dump2))
        (dump=? (snip-dump-top dump1)
                (snip-dump-top dump2))
        (dump=? (snip-dump-right dump1)
                (snip-dump-right dump2))
        (dump=? (snip-dump-bottom dump1)
                (snip-dump-bottom dump2))
        (dump=? (snip-dump-children dump1)
                (snip-dump-children dump2)))]
      [else (equal? dump1 dump2)]))
  
  ;; type snip-dump =
  ;;   (make-single number number number number (union #f (listof snip-dump)))
  ;; if children is #f, this indicates that the snip was not an
  ;; editor-snip. In contrast, if it is null, this indicates that
  ;; the snip is an editor-snip, but has no children.
  (define-struct snip-dump (left top right bottom children))
  
  ;; dump-pb : snip -> snip-dump
  (define (dump-snip snip)
    (let ([outer-pb (send (send snip get-admin) get-editor)]
          [bl (box 0)]
          [bt (box 0)]
          [br (box 0)]
          [bb (box 0)])
      (send outer-pb get-snip-location snip bl bt #t)
      (send outer-pb get-snip-location snip br bb #f)
      (make-snip-dump
       (unbox bl)
       (unbox bt)
       (unbox br)
       (unbox bb)
       (dump-snips snip))))
  
  ;; dump-snips : snip -> (union #f (listof snip-dump))
  (define (dump-snips snip)
    (cond
      [(is-a? snip editor-snip%)
       (dump-children (send snip get-editor))]
      [else #f]))
  
  ;; dump-children : editor<%> -> (listof snip-dump)
  (define (dump-children editor)
    (let loop ([snip (send editor find-first-snip)])
      (cond
        [snip
         (cons (dump-snip snip)
               (loop (send snip next)))]
        [else null])))
  )