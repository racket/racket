#lang racket/base
(require ffi/com
         ffi/com-registry)

;; Methods and Properties:
(provide com-all-coclasses
         com-all-controls

         cocreate-instance-from-coclass
         cci/coclass

         cocreate-instance-from-progid
         cci/progid

         get-active-object-from-coclass
         gao/coclass
         
         coclass
         progid
         
         set-coclass!
         set-coclass-from-progid!

         com-methods
         (rename-out [mx:com-method-type com-method-type])
         com-invoke

         com-get-properties
         (rename-out [mx:com-get-property-type com-get-property-type])
         com-get-property

         com-set-properties
         (rename-out [mx:com-set-property-type com-set-property-type])
         com-set-property!

         com-help

         mx-version)

;; COM types:
(provide com-object?
         com-object-type
         com-is-a?
         com-object-eq?
         
         com-currency?
         com-currency->number
         number->com-currency

         com-date?
         com-date->date
         date->com-date

         com-scode?
         com-scode->number
         number->com-scode

         com-iunknown?
         
         com-omit)

;; Events:
(provide com-events
         (rename-out [mx:com-event-type com-event-type])
         com-register-event-handler
         com-unregister-event-handler)

(define (coclass->clsid* who coclass)
  (or (coclass->clsid coclass)
      (error who "coclass not found: ~e" coclass)))

(define (cocreate-instance-from-coclass coclass [where 'local])
  (com-create-instance (coclass->clsid* 'cocreate-instance-from-coclass coclass) where))

(define (cci/coclass coclass [where 'local])
  (cocreate-instance-from-coclass coclass where))
         
(define (cocreate-instance-from-progid progid [where 'local])
  (com-create-instance progid where))

(define (cci/progid progid [where 'local])
  (cocreate-instance-from-progid progid where))

(define (get-active-object-from-coclass coclass)
  (com-get-active-object (coclass->clsid* 'get-active-object-from-coclass coclass)))
(define (gao/coclass coclass)
  (get-active-object-from-coclass coclass))

(define (coclass obj)
  (clsid->coclass (com-object-clsid obj)))

(define (progid obj)
  (clsid->progid (com-object-clsid obj)))

(define (set-coclass! obj coclass)
  (com-object-set-clsid! obj (coclass->clsid* 'set-coclass! coclass)))

(define (set-coclass-from-progid! obj progid)
  (com-object-set-clsid! obj (progid->clsid progid)))

(define (com-help obj [topic ""])
  (void))


(define (com-is-a? obj type)
  (com-type=? (com-object-type obj) type))

(define (com-currency? obj)
  (and (number? obj)
       (exact? obj)
       (integer? (* 10000 obj))))

(define (com-currency->number c) c)

(define (number->com-currency n)
  (define nn (if (real? n)
                 (* (round (* 10000 (inexact->exact n))) 1/10000)
                 n))
  (if (com-currency? nn)
      nn
      (error 'number->com-currency "cannot convert: ~e" nn)))

(define (com-date? d) (date? d))
(define (com-date->date d) d)
(define (date->com-date d) d)

(define (com-scode? v) (exact-integer? v))
(define (com-scode->number sc) sc)
(define (number->com-scode n) n)

(define (com-register-event-handler obj ev f)
  (define exec (com-make-event-executor))
  (thread (lambda () ((sync exec))))
  (com-register-event-callback obj
                               ev
                               f
                               exec))

(define (com-unregister-event-handler obj ev)
  (com-unregister-event-callback obj ev))

(define (mx-version) (version))

(define (reorder t)
  (if (and (pair? t)
           (eq? (car t) '->))
      (append (cadr t) (list '-> (caddr t)))
      t))

(define (mx:com-method-type obj name)
  (reorder (com-method-type obj name)))

(define (mx:com-get-property-type obj name)
  (reorder (com-get-property-type obj name)))

(define (mx:com-set-property-type obj name)
  (reorder (com-set-property-type obj name)))

(define (mx:com-event-type obj name)
  (reorder (com-event-type obj name)))
