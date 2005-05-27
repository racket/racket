;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> This module combines the `base', `setf', and `misc', modules to create a
;;> new language module.  Use this module to get most of Swindle's
;;> functionality which is unrelated to the object system.

(module turbo (lib "base.ss" "swindle")
  (require (lib "setf.ss" "swindle")
           (lib "misc.ss" "swindle"))
  (provide (all-from-except (lib "base.ss" "swindle")
                            set! set!-values #%module-begin)
           (rename module-begin~ #%module-begin)
           (all-from-except (lib "setf.ss" "swindle") setf! psetf!)
;;>> (set! place value ...)  [*syntax*]
;;>> (pset! place value ...) [*syntax*]
;;>> (set!-values (place ...) expr) [*syntax*]
;;>   This module renames `setf!', `psetf!', and `setf!-values' from the
;;>   `setf' module as `set!', `pset!' and `set!-values' so the built-in
;;>   `set!' and `set!-values' syntaxes are overridden.
           (rename setf! set!) (rename psetf! pset!)
           (rename setf!-values set!-values)
           (all-from (lib "misc.ss" "swindle")))
;;>> #%module-begin
;;>   `turbo' is a language module -- it redefines `#%module-begin' to load
;;>   itself for syntax definitions.
  (defsyntax (module-begin~ stx)
    (let ([e (if (syntax? stx) (syntax-e stx) stx)])
      (if (pair? e)
        (datum->syntax-object
         (quote-syntax here)
         (list* (quote-syntax #%plain-module-begin)
                (datum->syntax-object stx
                                      (list (quote-syntax require-for-syntax)
                                            '(lib "turbo.ss" "swindle")))
                (cdr e))
         stx)
        (raise-syntax-error #f "bad syntax" stx)))
    ;; This doesn't work anymore (from 203.4)
    ;; (syntax-rules ()
    ;;   [(_ . body)
    ;;    (#%plain-module-begin
    ;;     (require-for-syntax (lib "turbo.ss" "swindle")) . body)])
    )
  )
