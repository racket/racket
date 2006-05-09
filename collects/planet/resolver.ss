#| resolver.ss -- PLaneT client

1. Introduction

The PLaneT system is a method for automatically sharing code packages, both as libraries and
as full applications, that gives every user of a PLaneT client the illusion of having a local copy
of every code package on the server, but is parsimonious in its transmission. It consists of a 
centralized server that holds all packages and individual clients that hold some portion of the 
archive locally. Maintenance of that archive should be transparent, and is the complete 
responsibility of the PLaneT client.

2. Client behavior

The PLaneT client receives user requests (i.e., the "(require (planet ...))" forms) and loads the
appropriate modules in response. In the course of handling these requests it may download new code
packages from the PLaneT server.

2.1 User interface

The structure of user PLaneT invocations is listed below.

PLANET-REQUEST ::= (planet FILE-NAME PKG-SPEC [PATH ...]?) 
FILE-NAME      ::= string
PKG-SPEC       ::= string | (FILE-PATH ... PKG-NAME) | (FILE-PATH ... PKG-NAME VER-SPEC)
VER-SPEC       ::= Nat | (Nat MINOR) 
MINOR          ::= Nat | (Nat Nat) | (= Nat) | (+ Nat) | (- Nat)
FILE-PATH      ::= string
PKG-NAME       ::= string
OWNER-NAME     ::= string
PATH           ::= string

All strings must be legal filename strings.

When encountered, a planet-request is interpreted as requiring the given file name from the given
logical package, specified by the package spec and the collection specification, if given. If no
VER-SPEC is provided, the most recent version is assumed. If no owner-name/path ... clause is
provided, the default package is assumed.

2. PLaneT protocol

PLaneT clients support two protocols for communicating with the PLaneT server: the standard HTTP
GET/response system (currently the default) and a specialized TCP-based protocol that may become
more important if PLaneT becomes smarter about downloading packages behind the scenes.

In the following sections we describe the specialized protocol only.

2.1 Overview

1. PLaneT client establishes TCP connection to PLaneT server.
2. Client transmits a version specifier.
3. Server either refuses that version and closes connection or accepts.
4. Client transmits a sequence of requests terminated by a special end-of-request marker.
    Simultaneously, server transmits responses to those requests.
5. Once the server has handled every request, it closes the connection.


I am concerned about the overhead of opening and closing TCP connections for a large 
program with many requires, so I want to allow many requests and responses over the same
connection. Unfortunately there's a wrinkle: the standard client, implemented the obvious
way, would be unable to send more than one request at a time because it gets invoked purely 
as a response to a require form and must load an appropriate file before it returns. This
means I can't batch up multiple requires, at least not with an obvious implementation.
  
A possible solution would be to implement an install program that walks over the syntax
tree of a program and gathers all requires, then communicates with the server and learns
what additional packages would be necessary due to those requires, and then downloads all
of them at once. We would have to implement both methods simultaneously, though, to allow
for REPL-based PLaneT use and dynamic-require (unless we want it to be a runtime exception
to use PLaneT from the REPL or via dynamic-require, something I'd rather not do), so I
want a protocol that will allow both forms of access easily. This protocol does that, and
doesn't require too much additional overhead in the case that the client only takes one
package at a time.

2.2 Communication Details

After a TCP connection is established, the client transmits a VERSION-SPECIFIER:

VERSION-SPECIFIER ::= "PLaneT/1.0\n"

The server responds with a VERSION-RESPONSE:

VERSION-RESPONSE ::=
  'ok "\n"
| ('invalid string) "\n" 

where the string in the invalid case is descriptive text intended for display to the user
that may indicate some specific message about the nature of the error.

If the server sends 'invalid, the server closes the connection. Otherwise, the client may 
send any number of requests, followed by an end-of-request marker:

REQUESTS ::= { REQUEST "\n"}* 'end "\n"
REQUEST  ::= (SEQ-NO 'get PKG-LANG PKG-NAME (Nat | #f) (Nat | #f) (Nat | #f) [OWNER-NAME PATH ...]?)
PKG-LANG ::= String
SEQ-NO   ::= Nat
 
The fields in a request are a uniquely identifying sequence number, the literal symbol 'get,
the name of the package to receive, the required major version and the lowest and highest
acceptable version (with #f meaning that there is no constraint for that field, and a #f in
major-version field implying that both other fields must also be #f), and the package path.

As the client is transmitting a REQUESTS sequence, the server begins responding to it with
RESPONSE structures, each with a sequence number indicating to which request it is 
responding (except in the case of input too garbled to extract a sequence number):

RESPONSE ::=
| ('error 'malformed-input string) "\n"
| (SEQ-NO 'error 'malformed-request string) "\n" 
| (SEQ-NO 'bad-language string) "\n"
| (SEQ-NO 'get 'ok Nat Nat Nat) "\n" BYTE-DATA
| (SEQ-NO 'get 'error ERROR-CODE string) "\n"

ERROR-CODE ::= 'not-found

If the server receives a malformed request, it may close connection after sending a
malformed-request response without processing any other requests. Otherwise it must
process all requests even in the event of an error. On a successful get, the three numbers
the server returns are the matched package's major version, the matched package's minor version,
and the number of bytes in the package.

3 Client Download Policies

Mzscheme invokes the PLaneT client once for each instance of a require-planet form in a program
being run (i.e., the transitive closure of the "requires" relation starting from some specified
root module; this closure is calculable statically). At each of these invocations, the client
examines its internal cache to see if an appropriate module exists that matches the specification
given by the user (for details see the next section). If one does, the client loads that module
and returns. If none does, it initiates a transaction with the server using the PLaneT protocol
described in the previous subsection and sends a single request consisting of the user's request. 
It installs the resulting .plt file and then loads the appropriate file.

The client keeps a cache of downloaded packages locally. It does so in the
$PLTCOLLECTS/planet/cache/ directory and subdirectories, in an intuitive manner: each item in
the package's path in the PLaneT require line correspond to a subdirectory in the cache 
directory, starting with the owner name. (They should be unpacked relative to some user-specific
rather than installation-specific place, possibly, but that's difficult to do so we won't do it
yet).

To check whether a package is installed when attempting to satisfy a requirement, the client 
checks its cache to see if an appropriate entry exists in its link-table for that require line.
If one exists, it uses the named package directly. If none exists, it checks to see if there is
an appropriate subdirectory.

||#
(module resolver mzscheme
  
  (require (lib "match.ss")
           (lib "file.ss")
           (lib "port.ss")
           (lib "list.ss")
           
           (lib "date.ss")
           
           (lib "url.ss" "net")
           (lib "head.ss" "net")
           (lib "struct.ss")
           
           "config.ss"
           "private/planet-shared.ss"
           "private/linkage.ss")
  
  (provide (rename resolver planet-module-name-resolver)
           pkg-spec->full-pkg-spec
           get-package-from-cache
           get-package-from-server
           download-package
           install-pkg
           get-planet-module-path/pkg)
  
  (define install? (make-parameter #t)) ;; if #f, will not install packages and instead give an error
  
  (define (resolver spec module-path stx)
    ;; ensure these directories exist
    (make-directory* (PLANET-DIR))
    (make-directory* (CACHE-DIR))
    (establish-diamond-property-monitor)
    (cond
      [(or spec stx) (planet-resolve spec module-path stx)]
      [else module-path]))
  
  ; ==========================================================================================
  ; DIAMOND PROPERTY STUFF
  ; make sure a module isn't loaded twice with two different versions
  ; ==========================================================================================
  (define VER-CACHE-NAME #f)
  
  (define (establish-diamond-property-monitor)
    (unless VER-CACHE-NAME (set! VER-CACHE-NAME (gensym)))
    (unless (namespace-variable-value VER-CACHE-NAME #t (lambda () #f))
      (namespace-set-variable-value! VER-CACHE-NAME (make-hash-table 'equal))))
  
  (define (the-version-cache)    (namespace-variable-value VER-CACHE-NAME))
  (define (pkg->diamond-key pkg) (cons (pkg-name pkg) (pkg-route pkg)))
  
  (define (pkg-matches-bounds? pkg bound-info)
    (match-let ([(maj lo hi) bound-info])
      (and (= maj (pkg-maj pkg))
           (or (not lo) (>= (pkg-min pkg) lo))
           (or (not hi) (<= (pkg-min pkg) hi)))))
  
  ;; COMPAT ::= 'none | 'all | `(all-except ,VER-SPEC ...) | `(only ,VER-SPEC ...)
  ;; build-compatibility-fn : COMPAT -> PKG -> bool
  (define (build-compatibility-fn compat-data)
    (define pre-fn
      (match compat-data
        [`none (lambda (_) #f)]
        [`all (lambda (_) #t)]
        [`(all-except ,vspec ...)
          (let ((bounders (map version->bounds vspec)))
            (if (andmap (lambda (x) x) bounders)
                (lambda (v)
                  (not 
                   (ormap
                    (lambda (bounder)
                      (pkg-matches-bounds? v bounder))
                    bounders)))
                #f))]
        [`(only ,vspec ...)
          (let ((bounders (map version->bounds vspec)))
            (if (andmap (lambda (x) x) bounders)
                (lambda (v)
                  (andmap
                   (lambda (bounder)
                     (pkg-matches-bounds? v bounder))
                   bounders)))
            #f)]
        [_ #f]))
    (or pre-fn (lambda (x) #f)))
  
  ;; can-be-loaded-together? : pkg pkg -> boolean
  ;; side constraint: pkg1 and pkg2 are versions of the same package
  ;; assumption: pkg1 and pkg2 are versions of the same package
  ;; determines if the two versions are side-by-side compatible
  (define (can-be-loaded-together? pkg1 pkg2)
    (cond
      [(pkg> pkg1 pkg2) (can-be-loaded-together? pkg2 pkg1)]
      [(pkg= pkg1 pkg2) #t]
      [(pkg< pkg1 pkg2)
       (let* ([info (pkg->info pkg2)]
              [compat? (build-compatibility-fn (info 'can-be-loaded-with (lambda () 'none)))])
         (compat? pkg1))]))
  
  
  (define (add-pkg-to-diamond-registry! pkg)
    (let ((loaded-packages (hash-table-get (the-version-cache)
                                           (pkg->diamond-key pkg)
                                           (lambda () '()))))
      (begin
        (for-each
         (lambda (already-loaded-pkg)
           (unless (can-be-loaded-together? pkg already-loaded-pkg)
             (raise (make-exn:fail (string->immutable-string
                                    (format 
                                     "Package ~a loaded twice with multiple incompatible versions: 
attempted to load version ~a.~a while version ~a.~a was already loaded" 
                                     (pkg-name pkg) 
                                     (pkg-maj pkg)
                                     (pkg-min pkg)
                                     (pkg-maj already-loaded-pkg)
                                     (pkg-min already-loaded-pkg)))
                                   (current-continuation-marks)))))
         loaded-packages)
        (hash-table-put! (the-version-cache) (pkg->diamond-key pkg) (cons pkg loaded-packages)))))
  
  ; ==========================================================================================
  ; MAIN LOGIC
  ; Handles the overall functioning of the resolver
  ; ==========================================================================================
  
  ; planet-resolve : PLANET-REQUEST symbol syntax[PLANET-REQUEST] -> symbol
  ; resolves the given request. Returns a name corresponding to the module in the correct
  ; environment
  (define (planet-resolve spec module-path stx)
    (let-values ([(path pkg) (get-planet-module-path/pkg spec module-path stx)])
      (add-pkg-to-diamond-registry! pkg)
      (do-require path (pkg-path pkg) module-path stx)))
  
  ;; get-planet-module-path/pkg :PLANET-REQUEST symbol syntax[PLANET-REQUEST] -> path PKG
  ;; returns the matching package and the file path to the specific request
  (define (get-planet-module-path/pkg spec module-path stx)
    (match (cdr spec)
      [(file-name pkg-spec path ...)
       (unless (string? file-name)
         (raise-syntax-error #f (format "File name: expected a string, received: ~s" file-name) stx))
       (unless (andmap string? path)
         ;; special-case to catch a possibly common error:
         (if (ormap number? path)
             (raise-syntax-error #f (format "Module path must consist of strings only, received a number (maybe you intended to specify a package version number?): ~s" path) stx)
             (raise-syntax-error #f (format "Module path must consist of strings only, received: ~s" path) stx)))
       
       (match-let*
           ([pspec (pkg-spec->full-pkg-spec pkg-spec stx)]
            [pkg (or (get-linkage module-path pspec)
                     (add-linkage! module-path pspec
                                   (or
                                    (get-package-from-cache pspec)
                                    (get-package-from-server pspec)
                                    (raise-syntax-error #f (format "Could not find package matching ~s" 
                                                                   (list (pkg-spec-name pspec)
                                                                         (pkg-spec-maj pspec)
                                                                         (list (pkg-spec-minor-lo pspec)
                                                                               (pkg-spec-minor-hi pspec))
                                                                         (pkg-spec-path pspec)))
                                                        stx))))])
         (values (apply build-path (pkg-path pkg) (append path (list file-name))) pkg))]
      [_ (raise-syntax-error 'require (format "Illegal PLaneT invocation: ~e" (cdr spec)) stx)]))
  
  ; pkg-spec->full-pkg-spec : PKG-SPEC syntax -> FULL-PKG-SPEC
  (define (pkg-spec->full-pkg-spec spec stx)
    (define (pkg name maj lo hi path) (make-pkg-spec name maj lo hi path stx (version)))
    (define (fail)
      (raise-syntax-error 'require (format "Invalid PLaneT package specifier: ~e" spec) stx))
    
    (match spec
      [((? string? path) ... ver-spec ...)
       (match (version->bounds ver-spec)
         [(maj min-lo min-hi)
          (pkg (last path) maj min-lo min-hi (drop-last path))]
         [#f (fail)])]
      [_ (fail)]))
  
  ;; version->bounds : VER-SPEC -> (list (number | #f) number (number | #f)) | #f
  ;; determines the bounds for a given version-specifier
  ;; [technically this handles a slightly extended version of VER-SPEC where MAJ may
  ;;  be in a list by itself, because that's slightly more convenient for the above fn]
  (define (version->bounds spec-list)
    (match spec-list
      [() (list #f 0 #f)]
      [(? number? maj) (version->bounds (list maj))]
      [((? number? maj)) (list maj 0 #f)]
      [((? number? maj) min-spec)
       (let ((pkg (lambda (min max) (list maj min max))))
         (match min-spec
           [(? number? min)                 (pkg min #f)]
           [((? number? lo) (? number? hi)) (pkg lo  hi)]
           [('= (? number? min))            (pkg min min)]
           [('+ (? number? min))            (pkg min #f)]
           [('- (? number? min))            (pkg 0   min)]))]
      [_ #f]))
  
  
  ; ==========================================================================================
  ; PHASE 2: CACHE SEARCH
  ; If there's no linkage, there might still be an appropriate cached module.
  ; ==========================================================================================
  
  ; get-package-from-cache : FULL-PKG-SPEC -> PKG | #f
  (define (get-package-from-cache pkg-spec) 
    (lookup-package pkg-spec))
  
  ; ==========================================================================================
  ; PHASE 3: SERVER RETRIEVAL
  ; Ask the PLaneT server for an appropriate package if we don't have one locally.
  ; ==========================================================================================
  
  ; get-package-from-server : FULL-PKG-SPEC -> PKG | #f
  ; downloads and installs the given package from the PLaneT server and installs it in the cache,
  ; then returns a path to it 
  (define (get-package-from-server pkg)
    (with-handlers
        ([exn:fail? (lambda (e) 
                      (raise (make-exn:fail
                              (string->immutable-string
                               (format 
                                "Error downloading module from PLaneT server: ~a"
                                (exn-message e)))
                              (exn-continuation-marks e))))])
      (match (download-package pkg)
        [(#t path maj min) (install-pkg pkg path maj min)]
        [(#f str) #f])))
  
  (define (download-package pkg) 
    ((if (USE-HTTP-DOWNLOADS?) 
         download-package/http
         download-package/planet)
     pkg))
  
  (define (current-time) 
    (let ((date (seconds->date (current-seconds))))
      (parameterize ((date-display-format 'rfc2822))
        (format "~a ~a:~a:~a" 
                (date->string date)
                (date-hour date)
                (date-minute date)
                (date-second date)))))
  
  ; install-pkg : FULL-PKG-SPEC path[file] Nat Nat -> PKG
  ; install the given pkg to the planet cache and return a PKG representing the installed file
  (define (install-pkg pkg path maj min)
    (let* ((owner (car (pkg-spec-path pkg)))
           (extra-path (cdr (pkg-spec-path pkg)))
           (the-dir 
            (apply 
             build-path 
             (CACHE-DIR) 
             (append (pkg-spec-path pkg) 
                     (list (pkg-spec-name pkg) (number->string maj) (number->string min))))))
      (if (directory-exists? the-dir)
          (raise (make-exn:fail 
                  "Internal PLaneT error: trying to install already-installed package" 
                  (current-continuation-marks)))
          (begin
            (with-logging
             (LOG-FILE)
             (lambda ()
               (printf "\n============= Installing ~a on ~a =============\n" 
                       (pkg-spec-name pkg)
                       (current-time))
               ;; oh man is this a bad hack!
               (parameterize ((current-namespace (make-namespace)))
                 ((dynamic-require '(lib "plt-single-installer.ss" "setup") 'install-planet-package)
                  path the-dir (list owner (pkg-spec-name pkg) extra-path maj min)))))
            (make-pkg (pkg-spec-name pkg) (pkg-spec-path pkg) maj min the-dir)))))
  
  ; download-package : FULL-PKG-SPEC -> RESPONSE
  ; RESPONSE ::= (list #f string) | (list #t path[file] Nat Nat)
  ; downloads the given package and returns (list bool string): if bool is #t,
  ; the path is to a file that contains the package. If bool is #f, the package
  ; didn't exist and the string is the server's informative message.
  ; raises an exception if some protocol failure occurs in the download process
  (define (download-package/planet pkg)
    
    (define-values (ip op) (tcp-connect (PLANET-SERVER-NAME) (PLANET-SERVER-PORT)))
    
    (define (close-ports)
      (close-input-port ip)
      (close-output-port op))
    
    (define (request-pkg-list pkgs)
      (for-each/n (lambda (pkg seqno) 
                    (write-line (list* seqno 'get 
                                       (DEFAULT-PACKAGE-LANGUAGE)    
                                       (pkg-spec-name pkg) 
                                       (pkg-spec-maj pkg) 
                                       (pkg-spec-minor-lo pkg)
                                       (pkg-spec-minor-hi pkg)
                                       (pkg-spec-path pkg))
                                op))
                  pkgs)
      (write-line 'end op)
      (flush-output op))
    
    (define (state:initialize)
      (fprintf op "PLaneT/1.0\n")
      (flush-output op)
      (match (read ip)
        ['ok                        (state:send-pkg-request)]
        [('invalid (? string? msg)) (state:abort (string-append "protocol version error: " msg))]
        [bad-msg                    (state:abort (format "server protocol error (received invalid response): ~a" bad-msg))]))
    
    (define (state:send-pkg-request)
      (request-pkg-list (list pkg))
      (state:receive-package))
    
    (define (state:receive-package)
      (match (read ip)
        [(_ 'get 'ok (? nat? maj) (? nat? min) (? nat? bytes))
         (let ((filename (make-temporary-file "planettmp~a.plt")))
           (read-char ip) ; throw away newline that must be present
           (read-n-chars-to-file bytes ip filename)
           (list #t filename maj min))]
        [(_ 'error 'malformed-request (? string? msg)) 
         (state:abort (format "Internal error (malformed request): ~a" msg))]
        [(_ 'get 'error 'not-found (? string? msg)) 
         (state:failure (format "Server had no matching package: ~a" msg))]
        [(_ 'get 'error (? symbol? code) (? string? msg))
         (state:abort (format "Unknown error ~a receiving package: ~a" code msg))]
        [bad-response  (state:abort (format "Server returned malformed message: ~e" bad-response))]))
    
    (define (state:abort msg) 
      (raise (make-exn:i/o:protocol (string->immutable-string msg)
                                    (current-continuation-marks))))
    (define (state:failure msg) (list #f msg))
    
    (with-handlers ([void (lambda (e) (close-ports) (raise e))])
      (begin0
        (state:initialize)
        (close-ports))))
  
  ;; ------------------------------------------------------------
  ;; HTTP VERSION OF THE PROTOCOL
  
  ;; pkg->servlet-args : FULL-PKG-SPEC -> environment[from (lib "url.ss" "net")]
  ;; gets the appropriate query arguments to request the given package from the
  ;; PLaneT HTTP download servlet
  (define (pkg->servlet-args pkg)
    (let ((get (lambda (access) (format "~s" (access pkg)))))
      `((lang   . ,(format "~s" (DEFAULT-PACKAGE-LANGUAGE)))
        (name   . ,(get pkg-spec-name))
        (maj    . ,(get pkg-spec-maj))
        (min-lo . ,(get pkg-spec-minor-lo))
        (min-hi . ,(get pkg-spec-minor-hi))
        (path   . ,(get pkg-spec-path)))))
  
  ;; get-http-response-code : header[from (lib "head.ss" "net")] -> string
  ;; gets the HTTP response code in the given header
  (define (get-http-response-code header)
    (let ((parsed (regexp-match #rx"^HTTP/[^ ]* ([^ ]*)" header)))
      (and parsed (cadr parsed))))
  
  ;; download-package/http : FULL-PKG-SPEC -> RESPONSE
  ;; a drop-in replacement for download-package that uses HTTP rather than the planet protocol.
  ;; The HTTP protocol does not allow any kind of complicated negotiation, but it appears that
  ;; many more users can make HTTP requests than requests from nonstandard protocols.
  (define (download-package/http pkg)
    
    (let* ((args              (pkg->servlet-args pkg))
           (target            (copy-struct url (string->url (HTTP-DOWNLOAD-SERVLET-URL)) (url-query args)))
           (ip                (get-impure-port target))
           (head              (purify-port ip))
           (response-code/str (get-http-response-code head))
           (response-code     (string->number response-code/str)))
      
      (define (abort msg)
        (close-input-port ip)
        (raise (make-exn:i/o:protocol (string->immutable-string msg)
                                      (current-continuation-marks))))
      
      (case response-code
        [(#f)  (abort (format "Server returned invalid HTTP response code ~s" response-code/str))]
        [(200)
         (let ((maj/str (extract-field "Package-Major-Version" head))
               (min/str (extract-field "Package-Minor-Version" head))
               (content-length (extract-field "Content-Length" head)))
           (unless (and maj/str min/str
                        (nat? (string->number maj/str))
                        (nat? (string->number min/str)))
             (printf "~a" head)
             (abort "Server did not include valid major and minor version information"))
           (let* ((filename (make-temporary-file "planettmp~a.plt"))
                  (op       (open-output-file filename 'truncate))
                  (maj      (string->number maj/str))
                  (min      (string->number min/str)))
             (copy-port ip op)
             (close-input-port ip)
             (close-output-port op)
             (list #t filename maj min)))]
        [(404)
         (begin0
           (list #f (format "Server had no matching package: ~a" (read-line ip)))
           (close-input-port ip))]
        [(400)
         (abort (format "Internal error (malformed request): ~a" (read-line ip)))]
        [(500)
         (abort (format "Server internal error: ~a"
                        (apply string-append
                               (let loop ()
                                 (let ((line (read-line ip)))
                                   (cond
                                     [(eof-object? line) '()]
                                     [else (list* line "\n" (loop))]))))))]
        [else
         (abort (format "Internal error (unknown HTTP response code ~a)" response-code))])))
  
  ; ==========================================================================================
  ; MODULE MANAGEMENT
  ; Handles interaction with the module system
  ; ==========================================================================================
  
  ; do-require : path path symbol syntax -> symbol
  ; requires the given filename, which must be a module, in the given path.
  (define (do-require file-path package-path module-path stx)
    (parameterize ((current-load-relative-directory package-path))    
      ((current-module-name-resolver)
       file-path
       module-path
       stx)))
  
  ; ============================================================
  ; UTILITY
  ; A few small utility functions
  
  (define (last l) (car (last-pair l)))
  
  ;; make-directory*/paths : path -> (listof path)
  ;; like make-directory*, but returns what directories it actually created
  (define (make-directory*/paths dir)
    (let ((dir (if (string? dir) (string->path dir) dir)))
      (let-values ([(base name dir?) (split-path dir)])
        (cond
          [(directory-exists? dir)
           '()]
          [(directory-exists? base)
           (make-directory dir)
           (list dir)]
          [else
           (let ((dirs (make-directory*/paths base)))
             (make-directory dir)
             (cons dir dirs))]))))
  
  )
