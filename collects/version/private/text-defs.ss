(module text-defs mzscheme
  (require (lib "unitsig.ss") "checksigs.ss")

  (provide text-defs@)

  (define text-defs@
    (unit/sig defs^ (import)

      (define (run-thunk th) (th))

      ;; string (list string (listof string)) (union (listof string) #f) -> void
      (define (show-ok title captions details ok-thunk) ; ok-thunk for gui, ignore here
        (printf "~a\n" (car captions))
        (for-each (lambda (c) (printf " ~a\n" c)) (cdr captions))
        (when (and details (not (null? details)))
          (printf "Details:\n")
          (for-each (lambda (d) (printf " ~a\n" d)) details)))

      (define (show-error-ok title caption)
        (show-ok title (list (format "Error: ~a" caption)) #f void))

      (define (make-wait-dialog parent title caption close-fun)
        (list title caption))

      (define (show-wait-dialog dialog)
        ;; dialog is the pair returned by make-wait-dialog
        (printf "~a\n" (cadr dialog)))

      (define (hide-wait-dialog dialog)
        (void)))))
