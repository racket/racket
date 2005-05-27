;; Mike Burns 2004

;; Used for send/suspend/callback.

;; A xexpr/callback is one of:
;; - string
;; - symbol
;; - number
;; - comment
;; - processing instruction
;; - procedure
;; - (symbol xexpr/callback ...)
;; - (symbol ((symbol attrib-value) ...) xexpr/callback ...)

;; An attrib-value is one of:
;; - string
;; - procedure

(module xexpr-callback mzscheme
  (require (lib "xml.ss" "xml"))
  (provide xexpr/callback?)

  ;; Is it a Xexpr, or an Xexpr with procedures?
  (define (xexpr/callback? x)
    (correct-xexpr-callback? x #t #f))

  ;;; Copied and pasted from xml/private/xexpr.ss, then modified to include
  ;;; procedures.

  ;; correct-xexpr? : any any any -> any
  (define (correct-xexpr-callback? x true false)
    (cond
      ((string? x) true)
      ((symbol? x) true)
      ((number? x) true)
      ((comment? x) true)
      ((pi? x) true)
      ;; Modified here
      ((procedure? x) true)
      ;;
      ((list? x)
       (or (null? x)
           (if (symbol? (car x))
             (if (has-attribute? x)
               (and (attribute-pairs? (cadr x) true false)
                    (andmap (lambda (part)
                              (correct-xexpr-callback? part true false))
                            (cddr x))
                    true)
               (andmap (lambda (part)
                         (correct-xexpr-callback? part true false))
                       (cdr x)))
             false)))
      (else false)))

  ;; has-attribute? : List -> Boolean
  ;; True if the Xexpr provided has an attribute list.
  (define (has-attribute? x)
    (and (> (length x) 1)
         (list? (cadr x))
         (andmap (lambda (attr)
                   (pair? attr))
                 (cadr x))))

  ;; attribute-pairs? : List any any -> any
  ;; True if the list is a list of pairs.
  (define (attribute-pairs? attrs true false)
    (or (and (null? attrs) true)
        (let ((attr (car attrs)))
          (if (pair? attr)
            (and (attribute-symbol-string? attr true false)
                 (attribute-pairs? (cdr attrs) true false)
                 true)
            false))))

  ;; attribute-symbol-string? : List any any
  ;;                            -> any
  ;; True if the list is a list of String,Symbol pairs.
  (define (attribute-symbol-string? attr true false)
    (if (symbol? (car attr))
      (or (and (or (string? (cadr attr))
                   ;; Modified here
                   (procedure? (cadr attr)))
                   ;;
               true)
          false)
      false))

  )
