#lang racket/base

(require ffi/unsafe)

(define libxosd (ffi-lib "libxosd"))

;; Use this type to properly destroy an xosd object
(define _xosd (make-ctype (_cpointer "xosd") #f
                (lambda (p)
                  (if p
                    (register-finalizer p xosd-destroy)
                    (error '_xosd "got a NULL pointer"))
                  p)))

(define-syntax defxosd
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_") (#rx"[*?]$" "")))
                    libxosd (_fun type ...)))]))

(define-syntax defxosd*
  (syntax-rules ()
    [(_ name x ...) (begin (provide name) (defxosd name x ...))]))

(define _status
  (make-ctype _int #f
    (lambda (x)
      (if (eq? -1 x)
        (error 'xosd "~a"
               (or (get-ffi-obj "xosd_error" libxosd _string)
                   "unknown xosd error"))
        x))))

(define _sbool
  (make-ctype _status #f
    (lambda (x)
      (case x [(1) #t] [(0) #f] [else (error "bad boolean value: ~e" x)]))))

;; ===== Initializing =========================================================

(defxosd* xosd-create : ; [num-lines = 1] -> xosd-obj
  args :: (num-lines : _int = (if (pair? args) (car args) 1)) -> _xosd)
(defxosd xosd-destroy : _xosd -> _int)

(defxosd* xosd-is-onscreen? : _xosd -> _sbool)

;; ===== Displaying & Hiding ==================================================

(defxosd xosd-show* : _xosd -> _status)
(provide xosd-show)
(define (xosd-show xosd) (unless (xosd-is-onscreen? xosd) (xosd-show* xosd)))
(defxosd xosd-hide* : _xosd -> _status)
(provide xosd-hide)
(define (xosd-hide xosd) (when   (xosd-is-onscreen? xosd) (xosd-hide* xosd)))

(defxosd* xosd-set-timeout : _xosd _int -> _status)
(defxosd* xosd-wait-until-no-display : _xosd -> _status)

;; ===== Attributed ===========================================================

(define _xosd-pos (_enum '(top bottom middle)))
(define _xosd-align (_enum '(left center right)))

(defxosd* xosd-set-pos               : _xosd _xosd-pos   -> _status)
(defxosd* xosd-set-align             : _xosd _xosd-align -> _status)
(defxosd* xosd-set-horizontal-offset : _xosd _int        -> _status)
(defxosd* xosd-set-vertical-offset   : _xosd _int        -> _status)
(defxosd* xosd-set-shadow-offset     : _xosd _int        -> _status)
(defxosd* xosd-set-outline-offset    : _xosd _int        -> _status)
(defxosd* xosd-set-colour            : _xosd _string     -> _status)
(defxosd* xosd-set-shadow-colour     : _xosd _string     -> _status)
(defxosd* xosd-set-outline-colour    : _xosd _string     -> _status)
(defxosd* xosd-set-font              : _xosd _string     -> _status)

(defxosd* xosd-get-colour :
  _xosd (r : (_ptr o _int)) (g : (_ptr o _int)) (b : (_ptr o _int)) -> _status
  -> (list r g b))
(defxosd* xosd-get-number-lines : _xosd -> _status)

;; ===== Content ==============================================================

(define _xosd-command (_enum '(percentage string printf slider)))

(define disp-int*
  (get-ffi-obj "xosd_display" libxosd
               (_fun _xosd _int _xosd-command _int -> _status)))
(define disp-string*
  (get-ffi-obj "xosd_display" libxosd
               (_fun _xosd _int _xosd-command _string -> _status)))

(provide xosd-display-percentage xosd-display-string xosd-display-slider)
;; xosd-obj percent [line-num] -> int
(define (xosd-display-percentage xosd percent . line)
  (disp-int* xosd (if (pair? line) (car line) 0) 'percentage percent))
;; xosd-obj string [line-num] -> int
(define (xosd-display-string xosd str . line)
  (disp-string* xosd (if (pair? line) (car line) 0) 'string str))
;; xosd-obj percent [line-num] -> int
(define (xosd-display-slider xosd int . line)
  (disp-int* xosd (if (pair? line) (car line) 0) 'slider int))

(defxosd* xosd-set-bar-length : _xosd _int -> _status)
(defxosd* xosd-scroll : _xosd _int -> _status)
