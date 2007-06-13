(module mime-types mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "string.ss"))
  (require "util.ss"
           "response-structs.ss")
  (provide/contract
   [read-mime-types (path? . -> . hash-table?)]
   [make-path->mime-type (path? . -> . (path? . -> . bytes?))])
  
  ; read-mime-types : path? -> hash-table?
  (define (read-mime-types a-path)
    (define MIME-TYPE-TABLE (make-hash-table))
    (with-input-from-file a-path
      (lambda ()
        (let loop ()
          (match (read-line (current-input-port) 'any)
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
    MIME-TYPE-TABLE)
  
  ;; make-get-mime-type : path? -> path? -> bytes?
  ;; determine the mime type based on the filename's suffix
  ;;
  ;; Notes (GregP):
  ;; 1. Can we determine the mime type based on file contents?
  ;; 2. Assuming that 7-bit ASCII is correct for mime-type
  (define (make-path->mime-type a-path)
    (define MIME-TYPE-TABLE (read-mime-types a-path))
    (define file-suffix-regexp (byte-regexp #".*\\.([^\\.]*$)"))
    (lambda (path)
      (match (regexp-match file-suffix-regexp (path->bytes path))
        [(list path-bytes sffx)
         (hash-table-get MIME-TYPE-TABLE
                         (lowercase-symbol! sffx)
                         (lambda () TEXT/HTML-MIME-TYPE))]
        [_ TEXT/HTML-MIME-TYPE]))))