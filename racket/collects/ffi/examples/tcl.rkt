#lang racket/base

(require ffi/unsafe)

(define libtcl (ffi-lib "libtcl"))

(provide current-interp create-interp eval-tcl)

(define current-interp
  (make-parameter
   #f (lambda (x)
        (if (and x (cpointer? x))
          x
          (error 'tcl:current-interp
                 "expecting a non-void C pointer, got ~s" x)))))

;; This creates _interp as a type to be used for functions that return an
;; interpreter that should be destroyed with delete-interp.
(define _interp
  (make-ctype _pointer #f ; no op when going to C
    (lambda (interp)
      (when interp (register-finalizer interp delete-interp))
      interp)))

;; This is for arguments that always use the value of current-interp
(define-fun-syntax _interp*
  (syntax-id-rules ()
    [_ (type: _interp expr: (current-interp))]))

(define create-interp
  (get-ffi-obj "Tcl_CreateInterp" libtcl (_fun -> _interp)))
(define delete-interp
  (let ([f (get-ffi-obj "Tcl_DeleteInterp" libtcl (_fun _interp -> _void))])
    (lambda (i) (f i))))

(current-interp (create-interp))

(define get-string-result
  (get-ffi-obj "Tcl_GetStringResult" libtcl (_fun _interp -> _string)))

(define _tclret
  (make-ctype (_enum '(ok error return break continue))
    (lambda (x) (error "tclret is only for return values"))
    (lambda (x)
      (when (eq? x 'error) (error 'tcl (get-string-result (current-interp))))
      x)))

(define eval-tcl
  (get-ffi-obj "Tcl_Eval" libtcl (_fun _interp* (expr : _string) -> _tclret)))
