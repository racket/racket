(module mime-types mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "string.ss"))
  (require "util.ss")
  (provide/contract
   [make-get-mime-type (path? . -> . (path? . -> . bytes?))])
  
  ;; make-get-mime-type : path? -> path? -> bytes?
  ;; determine the mime type based on the filename's suffix
  ;;
  ;; Notes (GregP):
  ;; 1. Can we determine the mime type based on file contents?
  ;; 2. Assuming that 7-bit ASCII is correct for mime-type
  (define (make-get-mime-type a-path)
    (let ([MIME-TYPE-TABLE (make-hash-table)]
          [DEFAULT-MIME-TYPE #"text/plain"]
          [file-suffix-regexp (byte-regexp #".*\\.([^\\.]*$)")])
      (with-input-from-file a-path
        (lambda ()
          (let loop ()
            (match (read-line)
              [(? eof-object?)
               (void)]
              [(regexp #"^([^\t ]+)[\t ]+(.+)$"
                       (list s type exts))
               (for-each (lambda (ext)
                           (hash-table-put! MIME-TYPE-TABLE
                                            (lowercase-symbol! ext)
                                            type))
                         (regexp-split #" " exts))
               (loop)]
              [_
               (loop)]))))
      (lambda (path)
        (match (regexp-match file-suffix-regexp (path->bytes path))
          [(list path-bytes sffx)
           (hash-table-get MIME-TYPE-TABLE
                           (lowercase-symbol! sffx)
                           (lambda () DEFAULT-MIME-TYPE))]
          [_ DEFAULT-MIME-TYPE])))))