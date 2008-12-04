#lang scheme/base

(require scribble/xref
         scheme/fasl
         scheme/path
         "getinfo.ss"
         "private/path-utils.ss")

(provide load-collections-xref)

(define cached-xref #f)

(define (get-dests)
  (for*/list ([dir (find-relevant-directories '(scribblings) 'all-available)]
              [d ((get-info/full dir) 'scribblings)])
    (unless (and (list? d) (pair? d))
      (error 'xref "bad scribblings entry: ~e" d))
    (let* ([len   (length d)]
           [flags (if (len . >= . 2) (cadr d) '())]
           [name  (if (len . >= . 4)
                    (cadddr d)
                    (path->string
                     (path-replace-suffix (file-name-from-path (car d))
                                          #"")))])
      (and (not (and (len . >= . 3) (memq 'omit (caddr d))))
           (let* ([d (doc-path dir name flags 'false-if-missing)]
                  [p (and d (build-path d "out.sxref"))])
             (and p (file-exists? p) p))))))

(define (get-reader-thunks)
  (map (lambda (dest)
         (lambda ()
           (with-handlers ([exn:fail? (lambda (exn)
                                        (fprintf (current-error-port)
                                                 "WARNING: ~a\n"
                                                 (if (exn? exn)
                                                   (exn-message exn)
                                                   (format "~e" exn)))
                                        #f)])
             (cadr (call-with-input-file* dest fasl->s-exp)))))
       (filter values (get-dests))))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin (report-loading)
             (set! cached-xref (load-xref (get-reader-thunks)))
             cached-xref)))
