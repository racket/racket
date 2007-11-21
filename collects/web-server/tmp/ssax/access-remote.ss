; Module header is generated automatically
#cs(module access-remote mzscheme
(require "common.ss")
(require "myenv.ss")
(require "http.ss")
(require "srfi-12.ss")
(require "util.ss")
(require (lib "string.ss" "srfi/13"))

;; Uniform access to local and remote resources
;; Resolution for relative URIs in accordance with RFC 2396
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; Accessing (remote) resources

; Whether the resource exists (generalization of FILE-EXISTS? predicate)
;  REQ-URI - a string representing a URI of the resource
; This predicate doesn't have any side effects
(define (resource-exists? req-uri)
  (cond
    ((string-prefix? "http://" req-uri)  ; HTTP scheme is used in REQ-URI
     (with-exception-handler
      (lambda (x) #f)  ; an uncaught exception occured during http transaction
      (lambda ()
        (http-transaction
         "HEAD"
         req-uri
         (list (cons 'logger (lambda (port message . other-messages) #t)))
         (lambda (resp-code resp-headers resp-port)
           (close-input-port resp-port)
           (and (>= resp-code 200) (< resp-code 400)))))))
    (else  ; a local file
     (file-exists? req-uri))))
                 
; Opens an input port for a resource
;  REQ-URI - a string representing a URI of the resource
; An input port is returned if there were no errors. In case of an error,
; the function returns #f and displays an error message as a side effect.
; Doesn't raise any exceptions.
(define (open-input-resource req-uri)
  (with-exception-handler
   (lambda (x)
     (cerr nl req-uri ": " ((condition-property-accessor 'exn 'message) x) nl)
     #f)
   (lambda ()
     (cond
       ((string-prefix? "http://" req-uri)  ; HTTP scheme is used in REQ-URI
        (http-transaction
         "GET"
         req-uri
         (list (cons 'logger (lambda (port message . other-messages) #t)))
         (lambda (resp-code resp-headers resp-port)
           (cond
             ((and (>= resp-code 200) (< resp-code 400)) resp-port)
             (else
              (close-input-port resp-port)
              (cerr nl req-uri ": resource not available: " resp-code nl)
              #f)))))
       (else  ; a local file     
        (open-input-file req-uri))))))


;=========================================================================
; Determining resource type

; Returns a file extenstion
;  filename - a string
; File extension is returned in the form of a string
(define (ar:file-extension filename)
  (let loop ((src (reverse (string->list filename)))
             (res '()))
    (cond
      ((null? src)  ; no dot encountered => no extension
       "")
      ((char=? (car src) #\.)
       (list->string res))
      (else
       (loop (cdr src) (cons (car src) res))))))

; Determines the type of a resource
;  REQ-URI - a string representing a URI of the resource
; For a local resource, its type is determined by its file extension
; One of the following is returned:
;  #f - if the requested resource doesn't exist
;  'xml - for a resource that is an XML document
;  'html - for a resource that is an HTML document
;  'unknown - for any other resource type
(define (ar:resource-type req-uri)
  (cond
    ((string-prefix? "http://" req-uri)  ; HTTP scheme is used in REQ-URI
     (with-exception-handler
      (lambda (x) #f)  ; an uncaught exception occured during http transaction
      (lambda ()
        (http-transaction
         "HEAD"
         req-uri
         (list (cons 'logger (lambda (port message . other-messages) #t)))
         (lambda (resp-code resp-headers resp-port)
           (close-input-port resp-port)
           (if
            (or (< resp-code 200) (>= resp-code 400))
            #f  ; Resource doesn't exist              
            (let ((content-type (assq 'CONTENT-TYPE resp-headers)))
              (cond
                ((not content-type)  ; no content type specified
                 'unknown)
                ((string-prefix? "text/xml" (cdr content-type))
                 'xml)
                ((string-prefix? "text/html" (cdr content-type))
                 'html)
                ((string-prefix? "text/plain" (cdr content-type))
                 'plain)
                (else
                 'unknown)))))))))
    (else  ; a local file
     (cond
       ((not (file-exists? req-uri))  ; file doesn't exist
        #f)
       ((assoc (ar:file-extension req-uri)
               '(("xml" . xml) ("html" . html) ("htm" . html)))
        => cdr)
       (else 'unknown)))))


;=========================================================================
; Working on absolute/relative URIs
; This section is based on RFC 2396

;-------------------------------------------------
; The URI and its components
;  URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
;  genericURI = <scheme>://<authority><path>?<query>
; For a sertain subset of URI schemes, absoluteURI = genericURI
; We will suppose this condition valid in this implementation

; Returns: (values scheme authority path query fragment)
; If some component is not presented in the given URI, #f is returned for this
; component. Note that the path component is always presented in the URI
(define (ar:uri->components uri)
  (call-with-values
   (lambda () (cond
                ((string-rindex uri #\#)
                 => (lambda (pos)
                      (values
                       (substring uri (+ pos 1) (string-length uri))
                       (substring uri 0 pos))))
                (else
                 (values #f uri))))
   (lambda (fragment uri)
     (call-with-values
      (lambda () (cond
                   ((string-rindex uri #\?)
                    => (lambda (pos)
                         (values
                          (substring uri (+ pos 1) (string-length uri))
                          (substring uri 0 pos))))
                   (else
                    (values #f uri))))
      (lambda (query uri)
        (call-with-values
         (lambda ()
           (cond
             ((substring? "://" uri)
              => (lambda (pos)
                   (values
                    (substring uri 0 (+ pos 3))
                    (substring uri (+ pos 3) (string-length uri)))))
             ((string-index uri #\:)
              => (lambda (pos)
                   (values
                    (substring uri 0 (+ pos 1))
                    (substring uri (+ pos 1) (string-length uri)))))
             (else
              (values #f uri))))
         (lambda (scheme uri)
           (call-with-values
            (lambda ()
              (cond
                ((not scheme)
                 (values #f uri))
                ((string-index uri #\/)
                 => (lambda (pos)
                      (values
                       (substring uri 0 pos)
                       (substring uri pos (string-length uri)))))
                (else
                 (values #f uri))))
            (lambda (authority path)
              (values scheme authority path query fragment))))))))))

; Combines components into the URI
(define (ar:components->uri scheme authority path query fragment)
  (apply string-append
         (append
          (if scheme (list scheme) '())
          (if authority (list authority) '())
          (list path)
          (if query (list "?" query) '())
          (if fragment (list "#" fragment) '()))))

;-------------------------------------------------
; Path and its path_segments
;  abs_path = "/" path_segments
;  path_segments = segment *( "/" segment ) 

; Splits the given path into segments
; Returns: (values root dir-lst filename)
;  dir-lst ::= (listof directory-name)
;  root - either an empty string, or "/" or drive-name (for Windows filesystems)
(define (ar:path->segments path)
  (call-with-values
   (lambda ()
     (let ((lng (string-length path)))
       (cond
         ((and (> lng 0) (char=? (string-ref path 0) #\/))
           (values "/" (substring path 1 lng)))
       ((and (> lng 1)
             (char=? (string-ref path 1) #\:)
             (member (string-ref path 2) (list #\/ #\\)))
        (values (substring path 0 3)
                (substring path 3 lng)))
       (else (values "" path)))))
   (lambda (root rel-path)
     (let ((lst (string-split rel-path (list #\/ #\\))))
       (if (null? lst)  ; the relative path is empty
           (values root '() "")
           (let ((lst (reverse lst)))
             (values root (reverse (cdr lst)) (car lst))))))))
     
; Combines path_segments into the path
;  backslash? - a boolean value: whether the backslach shall be used as a
; delimiter between path_segments. If #f, straight slash is used
(define (ar:segments->path root dir-lst filename backslash?)
  (let ((delim (if backslash? "\\" "/")))
    (apply string-append
           (append
            (list root)
            (apply append
                   (map
                    (lambda (directory-name)
                      (list directory-name delim))
                    dir-lst))
            (list filename)))))

; Removes redundant segment combinations from the dir-lst
;  '("smth" "..") --> removed
;  '(".") --> removed
; The algorithm is formally specified in RFC 2396, 5.2, step 6)
(define (ar:normalize-dir-lst dir-lst)
  (cond
    ((null? dir-lst) dir-lst)
    ((string=? (car dir-lst) ".")
     (ar:normalize-dir-lst (cdr dir-lst)))
    ((string=? (car dir-lst) "..")
     (cons (car dir-lst) (ar:normalize-dir-lst (cdr dir-lst))))
    (else
     (let ((processed (ar:normalize-dir-lst (cdr dir-lst))))
       (cond
         ((null? processed)
          (list (car dir-lst)))
         ((string=? (car processed) "..")
          (cdr processed))
         (else
          (cons (car dir-lst) processed)))))))
          
;-------------------------------------------------
; Resolves a relative URI with respect to the base URI

;  base-uri - base URI for the requiested one
; Returns the resolved URI
(define (ar:resolve-uri-according-base base-uri req-uri)
  (call-with-values
   (lambda () (ar:uri->components req-uri))
   (lambda (req-scheme req-authority req-path req-query req-fragment)
     (if
      (or req-scheme req-authority)  ; it is the absolute URI
      req-uri
      (call-with-values
       (lambda () (ar:path->segments req-path))
       (lambda (req-root req-dir-lst req-filename)
         (if
          (> (string-length req-root) 1)  ; absolute path from the disc drive
          req-uri
          (call-with-values
           (lambda () (ar:uri->components base-uri))
           (lambda 
               (base-scheme base-authority base-path base-query base-fragment)
             (if
              (string=? req-root "/")  ; absolute path from server
              (ar:components->uri base-scheme base-authority
                                  req-path req-query req-fragment)
              ; else the requested URI is the relative URI
              (call-with-values
               (lambda () (ar:path->segments base-path))
               (lambda (base-root base-dir-lst base-filename)
                 (ar:components->uri
                  base-scheme
                  base-authority
                  (ar:segments->path
                   base-root
                   (ar:normalize-dir-lst (append base-dir-lst req-dir-lst))
                   req-filename
                   (and (not (string-index base-path #\/))
                        (string-index req-path #\\)))
                  req-query
                  req-fragment)))))))))))))

(provide (all-defined)))
