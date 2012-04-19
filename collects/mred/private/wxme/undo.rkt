#lang racket/base
(require racket/class
         "private.rkt"
         racket/snip/private/private
         racket/snip/private/snip
         racket/snip/private/snip-flags)

(provide change-record%
         proc-record%
         unmodify-record%
         insert-record%
         insert-snip-record%
         delete-record%
         delete-snip-record%
         style-change-record%
         style-change-snip-record%
         move-snip-record%
         resize-snip-record%
         composite-record%)

(define (disown snip)
  (when (has-flag? (snip->flags snip) OWNED)
    (send snip set-s-flags (remove-flag (snip->flags snip) OWNED))))

(define change-record%
  (class object%
    (super-new)
    (define/public (cancel) (void))
    (define/public (undo editor) #f)
    (define/public (drop-set-unmodified) (void))
    (define/public (is-composite?) #f)
    (define/public (get-id) #f)
    (define/public (get-parity) 0)
    (define/public (inverse) #f)))

(define proc-record%
  (class change-record%
    (init-field proc)
    (super-new)
    
    (define/override (undo editor)
      (proc))))

(define unmodify-record%
  (class change-record%
    (init-field cont?)
    (define ok? #t)
    (super-new)

    (define/override (undo editor)
      (when ok?
        (send editor set-modified #f))
      cont?)

    (define/override (drop-set-unmodified)
      (set! ok? #f))))

(define insert-record%
  (class change-record%
    (init-field start)
    (init       length)
    (init-field cont?
                startsel
                endsel)
    (define end (+ start length))
    (super-new)

    (define/override (undo editor)
      (send editor delete start end)
      (send editor set-position startsel endsel)
      cont?)))

(define insert-snip-record%
  (class change-record%
    (init-field snip 
                cont?)
    (super-new)
    
    (define/override (undo editor)
      (send editor delete snip)
      (unless cont?
        (send editor set-selected snip))
      cont?)))

(define-struct delete-snip-item (snip before x y))

(define delete-snip-record%
  (class change-record%
    (init-field cont?)
    (define deletions null)
    (define undid? #f)
    (super-new)

    (define/public (insert-snip snip before x y)
      (set! deletions (cons (make-delete-snip-item snip before x y)
                            deletions)))

    (define/override (cancel)
      (unless undid?
        (for-each (lambda (i)
                    (let ([snip (delete-snip-item-snip i)])
                      (disown snip)
                      (send snip set-admin #f)))
                  deletions)))

    (define/override (undo editor)
      (unless cont?
        (send editor no-selected))

      (for-each 
       (lambda (del)
         (let ([snip (delete-snip-item-snip del)])
           ;; have to turn off the owned flag; we know that it's really ours
           (disown snip)
           
           (send editor insert snip 
                 (delete-snip-item-before del)
                 (delete-snip-item-x del)
                 (delete-snip-item-y del))

           (unless cont?
             (send editor add-selected snip))))
       deletions)
      
      (set! undid? #t)

      cont?)))

(define delete-record% 
  (class change-record% 
    (init-field start
                end
                cont?
                startsel
                endsel)
    (define deletions null)
    (define clickbacks null)
    (define undid? #f)
    (super-new)

    (define/public (insert-snip snip)
      (set! deletions (cons snip deletions)))

    (define/public (add-clickback click)
      (set! clickbacks (cons click clickbacks)))

    (define/override (cancel)
      (unless undid?
        (for-each (lambda (snip)
                    (disown snip)
                    (send snip set-admin #f))
                  deletions)))

    (define/override (undo editor)
      ;; have to turn off the owned flag; we know that it's really ours
      (for-each disown deletions)
      (send editor do-insert-snips deletions start)
      (for-each (lambda (cb)
                  (send editor add-back-clickback cb))
                clickbacks)

      (send editor set-position startsel endsel)

      (set! undid? #t)

      cont?)))

(define style-change-record%
  (class change-record% 
    (init-field start
                end
                cont?
                startsel
                endsel
                restore-selection?)
    (define changes null)
    (super-new)

    (define/public (add-style-change start end style)
      (set! changes (cons (vector start end style)
                          changes)))

    (define/override (undo editor)
      (for-each (lambda (c)
                  (send editor change-style
                        (vector-ref c 2)
                        (vector-ref c 0)
                        (vector-ref c 1)))
                (reverse changes))
      
      (when restore-selection?
        (send editor set-position startsel endsel))
      
      cont?)))

(define style-change-snip-record%
  (class change-record%
    (init-field cont?)
    (define changes null)
    (super-new)

    (define/public (add-style-change snip style)
      (set! changes (cons (cons snip style) changes)))

    (define/override (undo editor)
      (unless cont?
        (send editor no-selected))
      
      (for-each (lambda (s)
                  (send editor change-style (cdr s) (cdr s))
                  (unless cont?
                    (send editor add-selected (car s))))
                (reverse changes))

      cont?)))

(define move-snip-record%
  (class change-record%
    (init-field snip
                x
                y
                delta?
                cont?)
    (super-new)

    (define/override (undo editor)
      (if delta?
          (send editor move snip x y)
          (send editor move-to snip x y))
      cont?)))

(define resize-snip-record%
  (class change-record%
    (init-field snip
                x
                y
                cont?)
    (super-new)

    (define/override (undo editor)
      (send editor resize snip x y)
      cont?)))

(define composite-record%
  (class change-record%
    (init       count)
    (init-field id
                parity?)
    (unless id (set! id (mcons #f #f)))
    ((if parity? set-mcar! set-mcdr!) id this)
    (define seq (make-vector count))
    (super-new)

    (define/override (cancel)
      (for ([c (in-vector seq)])
        (send c cancel)))

    (define/override (undo editor)
      (for ([c (in-vector seq)])
        (send c undo editor))
      #f)

    (define/override (drop-set-unmodified)
      (for ([c (in-vector seq)])
        (send c drop-set-unmodified)))

    (define/public (add-undo pos c)
      (vector-set! seq (- (vector-length seq) pos 1) c))

    (define/override (is-composite?) #t)

    (define/override (get-id) id)

    (define/override (get-parity) parity?)

    (define/override (inverse)
      (make-object inverse-record% id (not parity?)))))


(define inverse-record%
  (class change-record%
    (init-field id
                parity?)

    (super-new)

    (define/private (get)
      (if parity?
          (mcar id)
          (mcdr id)))

    (define/override (cancel)
      ;; Avoid double-frees by not doing anything
      (void))

    (define/override (undo editor)
      (send (get) undo editor))

    (define/override (drop-set-unmodified)
      (let ([c (get)])
        (when c
          (send c drop-set-unmodified))))

    (define/override (get-id) id)

    (define/override (get-parity) parity?)

    (define/override (inverse)
      (send (get) inverse))))
