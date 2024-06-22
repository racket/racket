(define (check-compile-args who import-keys get-import orig-options)
  (unless (or (not import-keys) (vector? import-keys))
    (raise-argument-error who "(or/c #f vector?)" import-keys))
  (unless (or (not get-import)
              (and (procedure? get-import) (procedure-arity-includes? get-import 1)))
    (raise-argument-error who "(or/c (procedure-arity-includes/c 1) #f)" get-import))
  (when (and get-import (not import-keys))
    (raise-arguments-error who
                           (string-append
                            "no vector supplied for import keys, but import-getting function provided;\n"
                            " the function argument must be `#f` when the vector argument is `#f`")
                           "import-getting function" get-import))
  (let loop ([options orig-options]
             [redundant #f]
             [serializable #f]
             [unsafe #f]
             [static #f]
             [use-prompt #f]
             [unlimited-compile #f]
             [uninterned-literal #f])
    (cond
     [(null? options)
      (when redundant
        (raise-arguments-error who "redundant option"
                               "redundant option" redundant
                               "supplied options" orig-options))]
     [(pair? options)
      (case (car options)
        [(serializable)
         (loop (cdr options) (or redundant serializable) 'serializable unsafe static use-prompt unlimited-compile uninterned-literal)]
        [(unsafe)
         (loop (cdr options) (or redundant unsafe) serializable 'unsafe static use-prompt unlimited-compile uninterned-literal)]
        [(static)
         (loop (cdr options) (or redundant static) serializable unsafe 'static use-prompt unlimited-compile uninterned-literal)]
        [(use-prompt)
         (loop (cdr options) (or redundant use-prompt) serializable unsafe static 'use-prompt unlimited-compile uninterned-literal)]
        [(unlimited-compile)
         (loop (cdr options) (or redundant unlimited-compile) serializable unsafe static use-prompt 'unlimited-compile uninterned-literal)]
        [(uninterned-literal)
         (loop (cdr options) (or redundant uninterned-literal) serializable unsafe static use-prompt unlimited-compile 'uninterned-literal)]
        [(quick)
         (loop (cdr options) redundant serializable unsafe static use-prompt unlimited-compile uninterned-literal)]
        [else
         (loop #f redundant serializable unsafe static use-prompt unlimited-compile uninterned-literal)])]
     [else
      (raise-argument-error who "(listof/c 'serializable 'unsafe 'static 'quick 'use-prompt 'unlimited-compile 'uninterned-literal)"
                            orig-options)])))
