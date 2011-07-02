;;;
;;; <let.rkt> ---- SRFI 5 A compatible let form with signatures and rest arguments
;;; Time-stamp: <03/04/08 09:56:06 solsona>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  Nevertheless, I only did the
;;; port to PLT Scheme v200, and here is the copyright notice,
;;; comments, and licence from the original source:
;;;
;;; Copyright (C) Andy Gaynor (1999-2003)
;;;
;;; The version of my-let here was cleaned up by: Paul Schlie <schlie@attbi.com>.
;;; Renamed to s:let by Eli Barzilay

#lang scheme/base
(provide s:let)

(define-syntax s:let
  (syntax-rules ()
    ;; standard
    ((s:let () body ...)
     (let () body ...))
    ((s:let ((var val) ...) body ...)
     (let ((var val) ...) body ...))

    ;; rest style
    ((s:let ((var val) . bindings) body ...)
     (let-loop #f bindings (var) (val) (body ...)))

    ;; signature style
    ((s:let (name bindings ...) body ...)
     (let-loop name (bindings ...) () () (body ...)))

    ;; standard named style
    ((s:let name (bindings ...) body ...)
     (let-loop name (bindings ...) () () (body ...)))

    ))

;; A loop to walk down the list of bindings.

(define-syntax let-loop
  (syntax-rules ()

    ;; No more bindings - make a LETREC.
    ((let-loop name () (vars ...) (vals ...) body)
     ((letrec ((name (lambda (vars ...) . body)))
        name)
      vals ...))

    ;; Rest binding, no name
    ((let-loop #f (rest-var rest-val ...) (var ...) (val ...) body)
     (let ((var val) ... (rest-var (list rest-val ...))) . body))

    ;; Process a (var val) pair.
    ((let-loop name ((var val) more ...) (vars ...)     (vals ...)     body)
     (let-loop name (more ...)           (vars ... var) (vals ... val) body))

    ;; End with a rest variable - make a LETREC.
    ((let-loop name (rest-var rest-vals ...) (vars ...) (vals ...) body)
     ((letrec ((name (lambda (vars ... . rest-var) . body)))
        name)
      vals ... rest-vals ...))))

;; Four loops - normal and `signature-style', each with and without a rest
;; binding.
;;
;;(let fibonacci ((n 10) (i 0) (f0 0) (f1 1))
;;  (if (= i n)
;;      f0
;;      (fibonacci n (+ i 1) f1 (+ f0 f1))))
;;
;;(let (fibonacci (n 10) (i 0) (f0 0) (f1 1))
;;  (if (= i n)
;;      f0
;;      (fibonacci n (+ i 1) f1 (+ f0 f1))))
;;
;;(let fibonacci ((n 10) (i 0) . (f 0 1))
;;  (if (= i n)
;;      (car f)
;;      (fibonacci n (+ i 1) (cadr f) (+ (car f) (cadr f)))))
;;
;;(let (fibonacci (n 10) (i 0) . (f 0 1))
;;  (if (= i n)
;;      (car f)
;;      (fibonacci n (+ i 1) (cadr f) (+ (car f) (cadr f)))))
