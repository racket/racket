; Reference implementation of SRFI-71 using PLT 208's modules
; Sebastian.Egner@philips.com, 29-Apr-2005

#lang scheme/base

(provide srfi-let
         srfi-let*
         srfi-letrec
         srfi-letrec*
         uncons unlist unvector
         values->list values->vector
         uncons-2 uncons-3 uncons-4
         uncons-cons)

; --- textual copy of 'letvalues.scm' starts here ---

; Reference implementation of SRFI-71 (generic part)
; Sebastian.Egner@philips.com, 20-May-2005, PLT 208
;
; In order to avoid conflicts with the existing let etc.
; the macros defined here are called srfi-let etc.,
; and they are defined in terms of r5rs-let etc.
; It is up to the actual implementation to save let/*/rec
; in r5rs-let/*/rec first and redefine let/*/rec
; by srfi-let/*/rec then.
;
; There is also a srfi-letrec* being defined (in view of R6RS.)
;
; Macros used internally are named i:<something>.
;
; Abbreviations for macro arguments:
;   bs  - <binding spec>
;   b   - component of a binding spec (values, <variable>, or <expression>)
;   v   - <variable>
;   vr  - <variable> for rest list
;   x   - <expression>
;   t   - newly introduced temporary variable
;   vx  - (<variable> <expression>)
;   rec - flag if letrec is produced (and not let)
;   cwv - call-with-value skeleton of the form (x formals)
;         (call-with-values (lambda () x) (lambda formals /payload/))
;         where /payload/ is of the form (let (vx ...) body1 body ...).
;
; Remark (*):
;   We bind the variables of a letrec to i:undefined since there is
;   no portable (R5RS) way of binding a variable to a values that
;   raises an error when read uninitialized.

(define i:undefined 'undefined)

(define-syntax srfi-letrec* ; -> srfi-letrec
  (syntax-rules ()
    ((srfi-letrec* () body1 body ...)
     (srfi-letrec () body1 body ...))
    ((srfi-letrec* (bs) body1 body ...)
     (srfi-letrec (bs) body1 body ...))
    ((srfi-letrec* (bs1 bs2 bs ...) body1 body ...)
     (srfi-letrec (bs1) (srfi-letrec* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-letrec ; -> i:let
  (syntax-rules ()
    ((srfi-letrec ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #t () () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax srfi-let* ; -> srfi-let
  (syntax-rules ()
    ((srfi-let* () body1 body ...)
     (srfi-let () body1 body ...))
    ((srfi-let* (bs) body1 body ...)
     (srfi-let (bs) body1 body ...))
    ((srfi-let* (bs1 bs2 bs ...) body1 body ...)
     (srfi-let (bs1) (srfi-let* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-let ; -> i:let or i:named-let
  (syntax-rules ()
    ((srfi-let ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #f () () (body1 body ...) ((b1 b2 b ...) ...)))
    ((srfi-let tag ((b1 b2 b ...) ...) body1 body ...)
     (i:named-let tag () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax i:let
  (syntax-rules (values)

    ; (i:let "bs" rec (cwv ...) (vx ...) body (bs ...))
    ;   processes the binding specs bs ... by adding call-with-values
    ;   skeletons to cwv ... and bindings to vx ..., and afterwards
    ;   wrapping the skeletons around the payload (let (vx ...) . body).

    ; no more bs to process -> wrap call-with-values skeletons
    ((i:let "bs" rec (cwv ...) vxs body ())
     (i:let "wrap" rec vxs body cwv ...))

    ; recognize form1 without variable -> dummy binding for side-effects
    ((i:let "bs" rec cwvs (vx ...) body (((values) x) bs ...))
     (i:let "bs" rec cwvs (vx ... (dummy (begin x #f))) body (bs ...)))

    ; recognize form1 with single variable -> just extend vx ...
    ((i:let "bs" rec cwvs (vx ...) body (((values v) x) bs ...))
     (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))

    ; recognize form1 without rest arg -> generate cwv
    ((i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...))
     (i:let "form1" rec cwvs vxs body (bs ...) (x ()) (values v ...)))

    ; recognize form1 with rest arg -> generate cwv
    ((i:let "bs" rec cwvs vxs body (((values . vs) x) bs ...))
     (i:let "form1+" rec cwvs vxs body (bs ...) (x ()) (values . vs)))

    ; recognize form2 with single variable -> just extend vx ...
    ((i:let "bs" rec cwvs (vx ...) body ((v x) bs ...))
     (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))

    ; recognize form2 with >=2 variables -> transform to form1
    ((i:let "bs" rec cwvs vxs body ((b1 b2 b3 b ...) bs ...))
     (i:let "form2" rec cwvs vxs body (bs ...) (b1 b2) (b3 b ...)))

    ; (i:let "form1" rec cwvs vxs body bss (x (t ...)) (values v1 v2 v ...))
    ;   processes the variables in v1 v2 v ... adding them to (t ...)
    ;   and producing a cwv when finished. There is not rest argument.

    ((i:let "form1" rec (cwv ...) vxs body bss (x ts) (values))
     (i:let "bs" rec (cwv ... (x ts)) vxs body bss))
    ((i:let "form1" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v ...))
     (i:let "form1" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v ...)))

    ; (i:let "form1+" rec cwvs vxs body bss (x (t ...)) (values v ... . vr))
    ;   processes the variables in v ... . vr adding them to (t ...)
    ;   and producing a cwv when finished. The rest arg is vr.

    ((i:let "form1+" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v2 . vs))
     (i:let "form1+" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v2 . vs)))
    ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x (t ...)) (values v1 . vr))
     (i:let "bs" rec (cwv ... (x (t ... t1 . tr))) (vx ... (v1 t1) (vr tr)) body bss))
    ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x ()) (values . vr))
     (i:let "bs" rec (cwv ... (x tr)) (vx ... (vr tr)) body bss))

    ; (i:let "form2" rec cwvs vxs body bss (v ...) (b ... x))
    ;   processes the binding items (b ... x) from form2 as in
    ;   (v ... b ... x) into ((values v ... b ...) x), i.e. form1.
    ;   Then call "bs" recursively.

    ((i:let "form2" rec cwvs vxs body (bs ...) (v ...) (x))
     (i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...)))
    ((i:let "form2" rec cwvs vxs body bss (v ...) (b1 b2 b ...))
     (i:let "form2" rec cwvs vxs body bss (v ... b1) (b2 b ...)))

    ; (i:let "wrap" rec ((v x) ...) (body ...) cwv ...)
    ;   wraps cwv ... around the payload generating the actual code.
    ;   For letrec this is of course different than for let.

    ((i:let "wrap" #f vxs body)
     (let vxs . body))
    ((i:let "wrap" #f vxs body (x formals) cwv ...)
     (call-with-values
      (lambda () x)
      (lambda formals (i:let "wrap" #f vxs body cwv ...))))

    ((i:let "wrap" #t vxs body)
     (letrec vxs . body))
    ((i:let "wrap" #t ((v t) ...) body cwv ...)
     (let ((v i:undefined) ...) ; (*)
       (i:let "wraprec" ((v t) ...) body cwv ...)))

    ; (i:let "wraprec" ((v t) ...) body cwv ...)
    ;   generate the inner code for a letrec. The variables v ...
    ;   are the user-visible variables (bound outside), and t ...
    ;   are the temporary variables bound by the cwv consumers.

    ((i:let "wraprec" ((v t) ...) (body ...))
     (begin (set! v t) ... (let () body ...)))
    ((i:let "wraprec" vxs body (x formals) cwv ...)
     (call-with-values
      (lambda () x)
      (lambda formals (i:let "wraprec" vxs body cwv ...))))

    ))

(define-syntax i:named-let
  (syntax-rules (values)

    ; (i:named-let tag (vx ...) body (bs ...))
    ;   processes the binding specs bs ... by extracting the variable
    ;   and expression, adding them to vx and turning the result into
    ;   an ordinary named let.

    ((i:named-let tag vxs body ())
     (let tag vxs . body))
    ((i:named-let tag (vx ...) body (((values v) x) bs ...))
     (i:named-let tag (vx ... (v x)) body (bs ...)))
    ((i:named-let tag (vx ...) body ((v x) bs ...))
     (i:named-let tag (vx ... (v x)) body (bs ...)))))

; --- standard procedures ---

(define (uncons pair)
  (values (car pair) (cdr pair)))

(define (uncons-2 list)
  (values (car list) (cadr list) (cddr list)))

(define (uncons-3 list)
  (values (car list) (cadr list) (caddr list) (cdddr list)))

(define (uncons-4 list)
  (values (car list) (cadr list) (caddr list) (cadddr list) (cddddr list)))

(define (uncons-cons alist)
  (values (caar alist) (cdar alist) (cdr alist)))

(define (unlist list)
  (apply values list))

(define (unvector vector)
  (apply values (vector->list vector)))

; --- standard macros ---

(define-syntax values->list
  (syntax-rules ()
    ((values->list x)
     (call-with-values (lambda () x) list))))

(define-syntax values->vector
  (syntax-rules ()
    ((values->vector x)
     (call-with-values (lambda () x) vector))))

; --- textual copy of 'letvalues.scm' ends here ---
