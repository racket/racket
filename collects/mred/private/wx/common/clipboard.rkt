#lang racket/base
(require racket/class
         (only-in racket/draw bitmap%)
         "../../syntax.rkt"
         "../platform.rkt"
         "local.rkt"
         "queue.rkt")

(provide 
 (protect-out clipboard<%>
              clipboard-client%
              get-the-clipboard
              get-the-x-selection))

(define pre-client%
  (class object%
    (super-new)

    (def/pubment (get-data [string? format])
      (let ([d (inner #f get-data format)])
        (when d
          (unless (or (string? d) (bytes? d))
            (raise-mismatch-error 
             '|get-data method of clipboard-client%|
             "result is not #f, a string, or byte string: "
             d)))
        d))))

(defclass clipboard-client% pre-client%
  (define types null)
  (define es (current-eventspace))
  (define/public (get-client-eventspace) es)
  (define/public (set-client-eventspace e) (set! es e))
  (def/public (same-eventspace? [eventspace? e])
    (eq? e es))
  (def/public (get-types)
    types)
  (def/public (add-type [string? str])
    (set! types (cons (string->immutable-string str) types)))
  (define/augride (get-data format)
    #f)
  (def/public (on-replaced)
    (void))
  (super-new))

(define string-clipboard-client%
  (class clipboard-client%
    (init-field the-bytes)
    (super-new)
    (define/override (get-types) (list "TEXT"))
    (define/override (get-data s)
      (and (equal? s "TEXT") the-bytes))))

(defclass clipboard% object%
  (init x-selection?)

  (define driver (new clipboard-driver%
                      [x-selection? x-selection?]))

  (def/public (same-clipboard-client? [clipboard-client% c])
    (eq? c (send driver get-client)))

  (def/public (get-clipboard-bitmap [exact-integer? timestamp])
    (send driver get-bitmap-data))
  (def/public (set-clipboard-bitmap [bitmap% bm] [exact-integer? timestamp])
    (send driver set-bitmap-data bm timestamp))
  (def/public (get-clipboard-data [string? type]
                                  [exact-integer? timestamp])
    (send driver get-data type))
  (def/public (get-clipboard-string [exact-integer? timestamp])
    (send driver get-text-data))
  (def/public (set-clipboard-client [clipboard-client% c]
                                    [exact-integer? timestamp])
    (send c set-client-eventspace (current-eventspace))
    (send driver set-client c (send c get-types)))
  (def/public (set-clipboard-string [string? str]
                                    [exact-integer? timestamp])
    (set-clipboard-client (make-object string-clipboard-client% 
                                       (string->bytes/utf-8 str))
                          timestamp))

  (super-new))

(define clipboard<%> (class->interface clipboard%))

(define the-clipboard (new clipboard% [x-selection? #f]))
(define the-x-selection
  (if has-x-selection?
      (new clipboard% [x-selection? #t])
      the-clipboard))

(define (get-the-clipboard)
  the-clipboard)
(define (get-the-x-selection)
  the-x-selection)
