#lang racket/base

(require setup/getinfo
         racket/list)

(provide all-tools)


(define (log-exn-message-handler exn)
  (log-error (exn-message exn))
  #f)

(define (get-info/full/skip dir)
  (with-handlers ([exn:fail? log-exn-message-handler])
    (get-info/full dir)))

(define (ensure-list v)
  (if (list? v) v (list v)))

(define (check-tool-not-registered-twice tools entry dir)
  (define tool-name (car entry))
  (define previous-tool (hash-ref tools tool-name #f))
  (when previous-tool
    (eprintf "warning: tool ~s registered twice: ~e and ~e\n"
             tool-name
             (car previous-tool)
             dir)))

(define (valid-raco-commands-spec? entry)
  (and (list? entry)
       (= (length entry) 4)
       (string? (car entry))
       (module-path? (cadr entry))
       (string? (caddr entry))
       (or (not (list-ref entry 3))
           (real? (list-ref entry 3)))))

(define (check-valid-raco-commands-spec entry dir)
  (unless (valid-raco-commands-spec? entry)
    (eprintf "warning: ~s provided bad `raco-commands' spec: ~e\n"
             dir
             entry)))


(define (get-info-raco-commands full-info-proc)
  (ensure-list (full-info-proc 'raco-commands (lambda () null))))

(define (all-tools)
  (define tools (make-hash))
  (define dirs (find-relevant-directories '(raco-commands) 'all-available))
  (define infos (filter-map get-info/full/skip dirs))
  (for ([i (in-list infos)]
        [d (in-list dirs)])
    (define entries (get-info-raco-commands i))
    (for ([entry (in-list entries)])
      (check-valid-raco-commands-spec entry d)
      (check-tool-not-registered-twice tools entry d)
      (add-tool! tools (convert-entry entry d))))
  tools)

(define (convert-entry tool-entry dir)
  (define tool-mod-path (cadr tool-entry))
  (if (non-module-tool-path? tool-mod-path)
      (convert-module-tool-path tool-entry tool-mod-path dir)
      tool-entry)) ;; module path is absolute already:

(define (non-module-tool-path? tool-mod-path)
  (or (string? tool-mod-path)
      (and (pair? tool-mod-path)
           (eq? (car tool-mod-path) 'file)
           (relative-path? (cadr tool-mod-path)))))  

;; convert absolute path to relative to "info.rkt":
(define (convert-module-tool-path tool-entry tool-mod-path dir)
  (define new-path
    (build-path dir (if (pair? tool-mod-path)
                        (cadr tool-mod-path)
                        tool-mod-path)))
  (list* (car tool-entry)
         new-path
         (cddr tool-entry)))

(define (add-tool! tools tool-entry)
  (hash-set! tools (car tool-entry) tool-entry))
