;; This is used to create the include directory.

;; Should be called with three command-line arguments:
;; 1. The include directory that should be created,
;; 2. The location of the src/racket directory,
;; 3. The location of mzconfig.

;; written in #%kernel because it's loaded with -c (ie, no compiled files)

(module mkincludes '#%kernel
  (#%require '#%utils)
  
  (define-values (incdir mzsrcdir mzconfdir)
    (let-values ([(args) (vector->list (current-command-line-arguments))])
      (define-values (dir) (lambda (path) (normal-case-path (simplify-path (cleanse-path path)))))
      (if (= 3 (length args)) (void) (error 'mkincludes "bad arguments"))
      (apply values (map dir args))))
  
  (printf "Making ~a\n" incdir)
  
  (define-values (change-regexp)
    (lambda (from to)
      (lambda (src dst)
        (call-with-input-file src
          (lambda (src)
            (call-with-output-file dst
              (lambda (dst)
                (if (regexp-match from src 0 #f dst)
                    (begin (display to dst)
                           (regexp-match "$" src 0 #f dst))
                    (void))))))
        (void))))
  
  (define-values (copy-if-newer) 
    (case-lambda 
      [(basedir source-path) (copy-if-newer basedir source-path #f copy-file)]
      [(basedir source-path base) (copy-if-newer basedir source-path base copy-file)]
      [(basedir source-path base copy)
       (define-values (source) (build-path basedir source-path))
       (define-values (target)
         (build-path incdir
                     (if base
                         base
                         (let-values ([(_1 name _2) (split-path source)]) name))))
       (define-values (source-t) (file-or-directory-modify-seconds source))
       (define-values (target-t) (if (file-exists? target)
                                     (file-or-directory-modify-seconds target)
                                     #f))
       (if (not target-t)
           (copy source target)
           (if (< target-t source-t)
               (begin (delete-file target) (copy source target))
               (void)))]))
  
  (if (directory-exists? incdir) (void) (make-directory incdir))
  (copy-if-newer mzconfdir "mzconfig.h")
  (copy-if-newer mzsrcdir "sconfig.h")
  (copy-if-newer mzsrcdir "uconfig.h")
  (copy-if-newer mzsrcdir "include/escheme.h")
  (copy-if-newer mzsrcdir "include/scheme.h" #f
                 (change-regexp "/[*]III[*]/"
                                "#define INCLUDE_WITHOUT_PATHS"))
  (copy-if-newer mzsrcdir "include/schthread.h")
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
  (copy-if-newer mzsrcdir "gc2/gc2_obj.h" "schgc2obj.h")
  
  (printf "Done.\n"))
