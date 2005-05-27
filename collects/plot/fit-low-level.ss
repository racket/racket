(module fit-low-level mzscheme
  (require (lib "foreign.ss") (lib "etc.ss"))
  (unsafe!)

  (define libfit
    (ffi-lib (build-path (this-expression-source-directory)
                         "compiled" "native" (system-library-subpath)
                         "libfit")))

  (define do-fit-int
    (get-ffi-obj "do_fit" libfit
      (_fun (func      : (_fun _int _pointer -> _double))
            (val-num   : _int = (length x-values))
            (x-values  : (_list i _double*))
            (y-values  : (_list i _double*))
            (z-values  : (_list i _double*))
            (errors    : (_list i _double*))
            (param-num : _int = (length params))
            (params    : (_list i _double*))
            -> (_list o _double* param-num))))

  (define (do-fit callback x-vals y-vals z-vals errors params)
    (do-fit-int (lambda (argc argv)
                  (let ([args (cblock->list argv _double argc)])
                    (apply callback args)))
                x-vals y-vals z-vals errors params))

  (define get-asym-error
    (get-ffi-obj "get_asym_error" libfit
      (_fun (len : _?) ; len is only used for list conversion
            -> (_list o _double* len))))

  (define get-asym-error-percent
    (get-ffi-obj "get_asym_error_percent" libfit
      (_fun (len : _?) ; len is only used for list conversion
            -> (_list o _double* len))))

  (define get-rms
    (get-ffi-obj "get_rms" libfit
      (_fun -> _double*)))

  (define get-varience
    (get-ffi-obj "get_varience" libfit
      (_fun -> _double*)))

  (define (fit-internal f-of-x-y x-vals y-vals z-vals err-vals params)

    (let* ([len (length params)]
           [fit-result (do-fit f-of-x-y x-vals y-vals z-vals err-vals params)]
           [asym-error (get-asym-error len)]
           [asym-error-percent (get-asym-error-percent len)]
           [rms (get-rms)]
           [varience (get-varience)])
      (list fit-result asym-error asym-error-percent rms varience)))

  (provide fit-internal))
