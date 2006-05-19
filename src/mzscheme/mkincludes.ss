;; This is used to create the include directory.

;; Should be called with three command-line arguments:
;; 1. The include directory that should be created,
;; 2. The location of the src/mzscheme directory,
;; 3. The location of mzconfig.

(define-values (incdir mzsrcdir mzconfdir)
  (let ([args (vector->list argv)])
    (define (dir path) (normal-case-path (simplify-path (expand-path path))))
    (unless (= 3 (length args)) (error 'mkincludes "bad arguments"))
    (apply values (map dir args))))

(printf "Making ~a\n" incdir)

(define ((change-regexp from to) src dst)
  (call-with-input-file src
    (lambda (src)
      (call-with-output-file dst
        (lambda (dst)
          (when (regexp-match from src 0 #f dst)
            (display to dst)
            (regexp-match "$" src 0 #f dst)))))))

(define (copy-if-newer basedir source-path . args)
  (define (arg!) (and (pair? args) (begin0 (car args) (set! args (cdr args)))))
  (define source (build-path basedir source-path))
  (define target
    (build-path incdir
                (or (arg!)
                    (let-values ([(_1 name _2) (split-path source)]) name))))
  (define source-t (file-or-directory-modify-seconds source))
  (define target-t (and (file-exists? target)
                        (file-or-directory-modify-seconds target)))
  (define copy (or (arg!) copy-file))
  (cond
   [(not target-t) (copy source target)]
   [(< target-t source-t) (delete-file target) (copy source target)]))

(unless (directory-exists? incdir) (make-directory incdir))
(copy-if-newer mzconfdir "mzconfig.h")
(copy-if-newer mzsrcdir "sconfig.h")
(copy-if-newer mzsrcdir "uconfig.h")
(copy-if-newer mzsrcdir "include/escheme.h")
(copy-if-newer mzsrcdir "include/scheme.h" #f
               (change-regexp "/[*]III[*]/"
                              "#define INCLUDE_WITHOUT_PATHS"))
(copy-if-newer mzsrcdir "src/schemef.h")
(copy-if-newer mzsrcdir "src/schvers.h")
(copy-if-newer mzsrcdir "src/stypes.h")
(copy-if-newer mzsrcdir "src/schemex.h")
(copy-if-newer mzsrcdir "src/schemexm.h")
(copy-if-newer mzsrcdir "src/schexn.h")
(copy-if-newer mzsrcdir "include/ext.exp")
(copy-if-newer mzsrcdir "include/mzscheme.exp")
(copy-if-newer mzsrcdir "include/mzscheme3m.exp")
(copy-if-newer mzsrcdir "gc2/gc2.h" "schemegc2.h")
(copy-if-newer mzsrcdir "gc2/gc2.h" "schemegc2.h")

(printf "Done.\n")
