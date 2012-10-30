#lang racket/base
(require racket/gui/base)
(require racket/runtime-path)
(define-runtime-path here ".")

(define known-wxme-failures '("collapsed.rkt"))

(define (record-failure exn)
  (parameterize ([current-error-port (current-output-port)])
    (set! failures (+ failures 1))
    ((error-display-handler) (exn-message exn) exn)))

(define failures 0)
(define tried 0)

(define on (current-namespace))

(parameterize ([use-compiled-file-paths '()])
  ;; setting the use-compiled-file-paths here is important
  ;; so we don't "cheat" by using the wxme version to compile
  ;; the file and then just avoid using the GUI version at all.
  
  (for ([f (in-list (sort (directory-list here) string<=? 
                          #:key path->string))])
    
    (define gui-namespace (make-gui-namespace))
    (define base-namespace (make-base-namespace))
    
    (define f-str (path->string f))
    (unless (member f-str '("info.rkt" "run-all.rkt"))
      (when (regexp-match #rx"[.]rkt$" f-str)
        (parameterize ([current-namespace gui-namespace])
          (set! tried (+ tried 1))
          (printf "=== trying ~a with gui-namespace\n" f)
          (with-handlers ((exn:fail? record-failure))
            (dynamic-require (build-path here f) #f)))
        
        (unless (member f-str known-wxme-failures)
          (parameterize ([current-namespace base-namespace])
            (set! tried (+ tried 1))
            (printf "=== trying ~a with base-namespace\n" f)
            (with-handlers ((exn:fail? record-failure))
              (dynamic-require (build-path here f) #f))))))))

(printf "tried ~a files\n" tried)
(unless (zero? failures)
  (eprintf "~a attempt~a failed\n"
           failures
           (if (= failures 1) "" "s")))
