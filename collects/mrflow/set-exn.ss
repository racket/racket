; exceptions for sets
; This file is required by both set-hash.ss and set-list.ss
; so we can't use contracts here because the sets are not always the same.

(module set-exn mzscheme
  (provide
   (struct exn:set:value-not-found (set value))
   (struct exn:set:duplicate-value (set value))
   exn:set
   exn:set?
   raise-value-not-found-exn
   raise-duplicate-value-exn
   )
  
  (define-struct (exn:set exn) ())
  (define-struct (exn:set:value-not-found exn:set) (set value))
  (define-struct (exn:set:duplicate-value exn:set) (set value))
  
  ; string set value -> void
  (define (raise-value-not-found-exn fct-name set value)
    (raise (make-exn:set:value-not-found
            (format "~a: value ~a not found in set ~a" fct-name value set)
            (current-continuation-marks)
            set value)))
  
  ; string set value -> void
  (define (raise-duplicate-value-exn fct-name set value)
    (raise (make-exn:set:duplicate-value
            (format "~a: value ~a already in set ~a" fct-name value set)
            (current-continuation-marks)
            set value)))
  
  )
