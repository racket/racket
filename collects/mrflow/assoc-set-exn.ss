; Exceptions for associative sets
; This file is required by both assoc-set-hash.ss and assoc-set-list.ss
; so we can't use contracts here because the assoc-sets are not always the same.

(module assoc-set-exn mzscheme
  (provide
   (struct exn:assoc-set:key-not-found (assoc-set key))
   (struct exn:assoc-set:duplicate-key (assoc-set key))
   exn:assoc-set
   exn:assoc-set?
   raise-key-not-found-exn
   raise-duplicate-key-exn
   )
  
  (define-struct (exn:assoc-set exn) ())
  (define-struct (exn:assoc-set:key-not-found exn:assoc-set) (assoc-set key))
  (define-struct (exn:assoc-set:duplicate-key exn:assoc-set) (assoc-set key))
  
  ; string assoc-set value -> void
  (define (raise-key-not-found-exn fct-name assoc-set key)
    (raise (make-exn:assoc-set:key-not-found
            (format "~a: key ~a not found in associative set ~a" fct-name key assoc-set)
            (current-continuation-marks)
            assoc-set key)))
  
  ; string assoc-set value -> void
  (define (raise-duplicate-key-exn fct-name assoc-set key)
    (raise (make-exn:assoc-set:duplicate-key
            (format "~a: key ~a already in associative set ~a" fct-name key assoc-set)
            (current-continuation-marks)
            assoc-set key)))
  
  )
