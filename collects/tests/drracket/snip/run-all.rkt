#lang racket/base
(require racket/gui/base)
(require racket/runtime-path)
(define-runtime-path here ".")

(define known-wxme-failures '("collapsed.rkt"))

(define ((record-failure f gui?) exn)
  (eprintf "failed to load ~a in ~a mode\n" f (if gui? "gui" "wxme"))
  ((error-display-handler) (exn-message exn) exn))

(define tried 0)

(define gui-namespace (make-gui-namespace))
(define base-namespace (make-base-namespace))

(parameterize ([use-compiled-file-paths '()])
  ;; setting the use-compiled-file-paths here is important
  ;; so we don't "cheat" by using the wxme version to compile
  ;; the file and then just avoid using the GUI version at all.
  
  (for ([f (in-list (directory-list here))])
    (define f-str (path->string f))
    (unless (member f-str '("info.rkt" "run-all.rkt"))
      (when (regexp-match #rx"[.]rkt$" f-str)
        
        (parameterize ([current-namespace gui-namespace])
          (set! tried (+ tried 1))
          (with-handlers ((exn:fail? (record-failure f #t)))
            (dynamic-require (build-path here f) #f)))
        
        (unless (member f-str known-wxme-failures)
          (parameterize ([current-namespace base-namespace])
            (set! tried (+ tried 1))
            (with-handlers ((exn:fail? (record-failure f #f)))
              (dynamic-require (build-path here f) #f))))))))

(printf "tried ~a files\n" tried)
