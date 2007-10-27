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
           resolve-planet-path
           pkg-spec->full-pkg-spec
           get-package-from-cache
           get-package-from-server
           download-package
           pkg->download-url
           pkg-promise->pkg
           install-pkg
           get-planet-module-path/pkg
           install?)
  
  (define install? (make-parameter #t)) ;; if #f, will not install packages and instead give an error
  
  (define resolver 
    (case-lambda
     [(name) (void)]
     [(spec module-path stx load?)
      ;; ensure these directories exist
      (make-directory* (PLANET-DIR))
      (make-directory* (CACHE-DIR))
      (establish-diamond-property-monitor)
      (planet-resolve spec module-path stx load?)]
     [(spec module-path stx) (resolver spec module-path stx #t)]))
  
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
          (let ((bounders (map (λ (x) (version->bounds x (λ (_) #f))) vspec)))
            (if (andmap (lambda (x) x) bounders)
                (lambda (v)
                  (not 
                   (ormap
                    (lambda (bounder)
                      (pkg-matches-bounds? v bounder))
                    bounders)))
                #f))]
        [`(only ,vspec ...)
          (let ((bounders (map (λ (x) (version->bounds x (λ (_) #f))) vspec)))
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
  
  ;; stx->origin-string : stx option -> string
  ;; returns a description [e.g. file name, line#] of the given syntax
  (define (stx->origin-string stx)
    (if stx 
        (format "~a" (syntax-source stx))
        "[unknown]"))
  
  (define (add-pkg-to-diamond-registry! pkg stx)
    (let ((loaded-packages (hash-table-get (the-version-cache)
                                           (pkg->diamond-key pkg)
                                           (lambda () '()))))
      (begin
        (unless (list? loaded-packages)
          (error 'PLaneT "Inconsistent state: expected loaded-packages to be a list, received: ~s" loaded-packages))
        (let* ([all-violations '()]
               [_
                (for-each
                 (lambda (already-loaded-pkg-record)
                   (let* ([already-loaded-pkg (car already-loaded-pkg-record)]
                          [prior-stx (cadr already-loaded-pkg-record)]
                          [prior-stx-origin-string (stx->origin-string prior-stx)])
                     (unless (can-be-loaded-together? pkg already-loaded-pkg)
                       (set!
                        all-violations
                        (cons
                         (list
                          stx
                          (make-exn:fail 
                           (format
                           "Package ~a loaded twice with multiple incompatible versions:
~a attempted to load version ~a.~a while version ~a.~a was already loaded by ~a"
                           (pkg-name pkg)
                           (stx->origin-string stx)
                           (pkg-maj pkg)
                           (pkg-min pkg)
                           (pkg-maj already-loaded-pkg)
                           (pkg-min already-loaded-pkg)
                           prior-stx-origin-string)
                          (current-continuation-marks)))
                         all-violations)))))
                 loaded-packages)])
          (unless (null? all-violations)
            (let ([worst (or (assq values all-violations) (car all-violations))])
              (raise (cadr worst)))))
        (hash-table-put! (the-version-cache)
                         (pkg->diamond-key pkg) 
                         (cons (list pkg stx) loaded-packages)))))
  
  ; ==========================================================================================
  ; MAIN LOGIC
  ; Handles the overall functioning of the resolver
  ; ==========================================================================================
  
  ; planet-resolve : PLANET-REQUEST symbol syntax[PLANET-REQUEST] -> symbol
  ; resolves the given request. Returns a name corresponding to the module in the correct
  ; environment
  (define (planet-resolve spec module-path stx load?)
    (let-values ([(path pkg) (get-planet-module-path/pkg spec module-path stx)])
      (when load? (add-pkg-to-diamond-registry! pkg stx))
      (do-require path (pkg-path pkg) module-path stx load?)))
  
  ;; resolve-planet-path : planet-require-spec -> path
  ;; retrieves the path to the given file in the planet package. downloads and installs
  ;; the package if necessary
  (define (resolve-planet-path spec)
    (let-values ([(path pkg) (get-planet-module-path/pkg spec #f #f)])
      path))
  
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

       (match-let* ([pspec (pkg-spec->full-pkg-spec pkg-spec stx)]
                    [result (get-package module-path pspec)])
         (cond [(string? result)
                (raise-syntax-error 'require result stx)]
               [(pkg? result)
                (values (apply build-path (pkg-path result)
                               (append path (list file-name)))
                        result)]))]
      [_ (raise-syntax-error 'require (format "Illegal PLaneT invocation: ~e" (cdr spec)) stx)]))

  ;; PKG-GETTER ::= module-path pspec 
  ;;                (pkg -> A)
  ;;                ((uninstalled-pkg -> void) (pkg -> void) ((string | #f) -> string | #f) -> A) 
  ;;            -> A
  ;;
  ;; a pkg-getter is a function that tries to fetch a package; it is written in a quasi-cps style;
  ;; the first argument is what it calls to succeed, and the second argument is what it calls when it
  ;; fails. In the second case, it must provide two things: a function to take action if a match
  ;; is found eventually, and a function that gets to mess with the error message if the entire message 
  ;; eventually fails.
  
  
  
  ;; get-package : module-path FULL-PKG-SPEC -> (PKG | string)
  ;; gets the package specified by pspec requested by the module in the given module path,
  ;; or returns a descriptive error message string if that's not possible
  (define (get-package module-path pspec)
    (let loop ([getters (*package-search-chain*)]
               [pre-install-updaters '()]
               [post-install-updaters '()]
               [error-reporters '()])
      (cond
        [(null? getters)
         ; we have failed to fetch the package, generate an appropriate error message and bail
         (let ([msg (foldl (λ (f str) (f str)) #f error-reporters)])
           (or msg 
               (format "Could not find package matching ~s" 
                       (list (pkg-spec-name pspec)
                             (pkg-spec-maj pspec)
                             (list (pkg-spec-minor-lo pspec)
                                   (pkg-spec-minor-hi pspec))
                             (pkg-spec-path pspec)))))]
        [else
         ; try the next error reporter. recursive call is in the failure continuation
         ((car getters)
          module-path 
          pspec
          (λ (pkg) 
            (when (uninstalled-pkg? pkg)
              (for-each (λ (u) (u pkg)) pre-install-updaters))
            (let ([installed-pkg (pkg-promise->pkg pkg)])
              (for-each (λ (u) (u installed-pkg)) post-install-updaters)
              installed-pkg))
          (λ (pre-updater post-updater error-reporter)
            (loop (cdr getters)
                  (cons pre-updater pre-install-updaters)
                  (cons post-updater post-install-updaters)
                  (cons error-reporter error-reporters))))])))
  
  (define (o f g) (λ (x) (f (g x))))
  (define (o/and . es) (λ (x) (andmap (λ (f) (f x)) es)))
  (define (o/or . es) (λ (x) (ormap (λ (f) (f x)) es)))
  
  ; pkg-spec->full-pkg-spec : PKG-SPEC syntax -> FULL-PKG-SPEC
  (define (pkg-spec->full-pkg-spec spec stx)
    (define (pkg name maj lo hi path) (make-pkg-spec name maj lo hi path stx (version)))
    (define (fail* msg)
      (raise-syntax-error 'require (string->immutable-string msg) stx))
    (define (fail)
      (fail* (format "Invalid PLaneT package specifier: ~e" spec)))
    
    
    (match spec
      [((? string? owner) (? string? package) ver-spec ...)
       (match-let ([(maj min-lo min-hi) (version->bounds ver-spec fail*)])
         (pkg package maj min-lo min-hi (list owner)))]
      [((? (o not string?) owner) _ ...)
       (fail* (format "Expected string [package owner] in first position, received: ~e" owner))]
      [(_ (? (o not string?) pkg) _ ...)
       (fail* (format "Expected string [package name] in second position, received: ~e" pkg))]
      [_ (fail)]))
  
  ;; version->bounds : VER-SPEC -> (list (number | #f) number (number | #f)) | #f
  ;; determines the bounds for a given version-specifier
  ;; [technically this handles a slightly extended version of VER-SPEC where MAJ may
  ;;  be in a list by itself, because that's slightly more convenient for the above fn]
  (define (version->bounds spec-list fail)
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
           [('- (? number? min))            (pkg 0   min)]
           
           ;; failure cases
           [(? (o/and (o not number?) 
                      (o/or (o not list?)
                            (λ (x) (not (= (length x) 2))))))
            (fail (format "Expected number or version range specifier for minor version specification, received: ~e" min-spec))]
           [((? (λ (x) 
                  (and (not (number? x))
                       (not (memq x '(= + -)))))
                range)
             _)
            (fail (format "Illegal range specifier in minor version specification. Legal range specifiers are numbers, =, +, -; given: ~e" range))]
           [(_ (? (o not number?) bnd))
            (fail (format "Expected number as range bound in minor version specification, given: ~e" bnd))]
           [_ (fail (format "Illegal minor version specification: ~e" min-spec))]))]

      ;; failure cases
      [(? (o/and (o not number?) (o not list?)) v)
       (fail (format "Version specification expected number or sequence, received: ~e" v))]
      [((? (o not number?) maj) _ ...)
       (fail (format "Version specification expected number for major version, received: ~e" maj))]
      [_  (fail "Invalid version specification")]))
  
  ; ==========================================================================================
  ; PHASE 2: CACHE SEARCH
  ; If there's no linkage, there might still be an appropriate cached module
  ; (either installed or uninstalled)
  ; ==========================================================================================
  
  ; get/installed-cache : pkg-getter
  (define (get/installed-cache module-spec pkg-spec success-k failure-k)
    (let ([p (lookup-package pkg-spec)])
      (if p (success-k p) (failure-k void void (λ (x) x)))))
  
  ; get-package-from-cache : FULL-PKG-SPEC -> PKG | #f
  (define (get-package-from-cache pkg-spec) 
    (lookup-package pkg-spec))
  
  ;; get/uninstalled-cache-dummy : pkg-getter
  ;; always fails, but records the package to the uninstalled package cache
  ;; upon the success of some other getter later in the chain.
  (define (get/uninstalled-cache-dummy module-spec pkg-spec success-k failure-k)
    (failure-k save-to-uninstalled-pkg-cache! void (λ (x) x)))
  
  ; get/uninstalled-cache : pkg-getter
  ; note: this does not yet work with minimum-required-version specifiers
  ; if you install a package and then use an older mzscheme
  (define (get/uninstalled-cache module-spec pkg-spec success-k failure-k)
    (let ([p (lookup-package pkg-spec (UNINSTALLED-PACKAGE-CACHE))])
      (if (and p (file-exists? (build-path (pkg-path p) (pkg-spec-name pkg-spec))))
          (success-k 
           ; note: it's a little sloppy that lookup-pkg returns PKG structures, since
           ; it doesn't actually know whether or not the package is installed. hence
           ; I have to convert what appears to be an installed package into an
           ; uninstalled package
           (make-uninstalled-pkg 
            (build-path (pkg-path p) (pkg-spec-name pkg-spec))
            pkg-spec
            (pkg-maj p)
            (pkg-min p)))
          (failure-k void void (λ (x) x)))))
  
  ;; save-to-uninstalled-pkg-cache! : uninstalled-pkg -> path[file]
  ;; copies the given uninstalled package into the uninstalled-package cache,
  ;; replacing any old file that might be there. Returns the path it copied
  ;; the file into.
  (define (save-to-uninstalled-pkg-cache! uninst-p)
    (let* ([pspec (uninstalled-pkg-spec uninst-p)]
           [owner (car (pkg-spec-path pspec))]
           [name (pkg-spec-name pspec)]
           [maj (uninstalled-pkg-maj uninst-p)]
           [min (uninstalled-pkg-min uninst-p)]
           [dir (build-path (UNINSTALLED-PACKAGE-CACHE) 
                            owner 
                            name
                            (number->string maj)
                            (number->string min))]
           [full-pkg-path (build-path dir name)])
      (make-directory* dir)
      (unless (equal? (normalize-path (uninstalled-pkg-path uninst-p)) (normalize-path full-pkg-path))
        (when (file-exists? full-pkg-path) (delete-file full-pkg-path))
        (copy-file (uninstalled-pkg-path uninst-p) full-pkg-path))
      full-pkg-path))

  ; ==========================================================================================
  ; PHASE 3: SERVER RETRIEVAL
  ; Ask the PLaneT server for an appropriate package if we don't have one locally.
  ; ==========================================================================================
  
  (define (get/server module-spec pkg-spec success-k failure-k)
    (let ([p (get-package-from-server pkg-spec)])
      (cond
        [(pkg-promise? p) (success-k p)]
        [(string? p)
         ; replace any existing error message with the server download error message
         (failure-k void void (λ (_) p))])))
  
  ; get-package-from-server : FULL-PKG-SPEC -> PKG-PROMISE | #f | string[error message]
  ; downloads the given package file from the PLaneT server and installs it in the 
  ; uninstalled-packages cache, then returns a promise for it   
  (define (get-package-from-server pkg)
    (match (download-package pkg)
      [(#t tmpfile-path maj min) 
       (let* ([upkg (make-uninstalled-pkg tmpfile-path pkg maj min)]
              [cached-path (save-to-uninstalled-pkg-cache! upkg)]
              [final (make-uninstalled-pkg cached-path pkg maj min)])
         (unless (equal? (normalize-path tmpfile-path) (normalize-path cached-path))
           (delete-file tmpfile-path)) ;; remove the tmp file, we're done with it
         final)]
      [(#f str) (string-append "PLaneT could not find the requested package: " str)]
      [(? string? s) (string-append "PLaneT could not download the requested package: " s)]))
  
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
  
  
  ; pkg-promise->pkg : pkg-promise -> pkg
  ; "forces" the given pkg-promise (i.e., installs the package if it isn't installed yet)
  (define (pkg-promise->pkg p)
    (cond
      [(pkg? p) p]
      [(uninstalled-pkg? p)
       (install-pkg (uninstalled-pkg-spec p)
                    (uninstalled-pkg-path p)
                    (uninstalled-pkg-maj p)
                    (uninstalled-pkg-min p))]))
                
  
  ; install-pkg : FULL-PKG-SPEC path[file] Nat Nat -> PKG
  ; install the given pkg to the planet cache and return a PKG representing the installed file
  (define (install-pkg pkg path maj min)
    (unless (install?)
      (raise (make-exn:fail
              (format
               "PLaneT error: cannot install package ~s since the install? parameter is set to #f"
               (list (car (pkg-spec-path pkg)) (pkg-spec-name pkg) maj min))
              (current-continuation-marks))))
    
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
                 (let ([ipp (dynamic-require '(lib "plt-single-installer.ss" "setup") 'install-planet-package)])
                   (ipp path the-dir (list owner (pkg-spec-name pkg) extra-path maj min))))))
            (make-pkg (pkg-spec-name pkg) (pkg-spec-path pkg) maj min the-dir 'normal)))))
  
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
      (raise (make-exn:i/o:protocol msg (current-continuation-marks))))
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
  
  ;; pkg->download-url : FULL-PKG-SPEC -> url
  ;; gets the download url for the given package
  (define (pkg->download-url pkg)
    (copy-struct url (string->url (HTTP-DOWNLOAD-SERVLET-URL)) (url-query (pkg->servlet-args pkg))))

  ;; download-package/http : FULL-PKG-SPEC -> RESPONSE
  ;; a drop-in replacement for download-package that uses HTTP rather than the planet protocol.
  ;; The HTTP protocol does not allow any kind of complicated negotiation, but it appears that
  ;; many more users can make HTTP requests than requests from nonstandard protocols.
  (define (download-package/http pkg)
    (let/ec return
      (let loop ([attempts 1])
        (when (> attempts 5)
          (return "Download failed too many times (possibly due to an unreliable network connection)"))
        
        (with-handlers ([exn:fail:network? (λ (e) (return (exn-message e)))])
          (let* ((target            (pkg->download-url pkg))
                 (ip                (get-impure-port target))
                 (head              (purify-port ip))
                 (response-code/str (get-http-response-code head))
                 (response-code     (string->number response-code/str)))
            
            (define (abort msg)
              (close-input-port ip)
              (return msg))
            
            (case response-code
              [(#f)  (abort (format "Server returned invalid HTTP response code ~s" response-code/str))]
              [(200)
               (let ((maj/str (extract-field "Package-Major-Version" head))
                     (min/str (extract-field "Package-Minor-Version" head))
                     (content-length/str (extract-field "Content-Length" head)))
                 (unless (and maj/str min/str content-length/str
                              (nat? (string->number maj/str))
                              (nat? (string->number min/str))
                              (nat? (string->number content-length/str)))
                   (abort "Server did not include valid major and minor version information"))
                 (let* ((filename (make-temporary-file "planettmp~a.plt"))
                        (maj      (string->number maj/str))
                        (min      (string->number min/str))
                        (content-length (string->number content-length/str)))
                   (let ([op (open-output-file filename 'truncate/replace)])
                     (copy-port ip op)
                     (close-input-port ip)
                     (close-output-port op)
                     (if (= (file-size filename) content-length)
                         (list #t filename maj min)
                         (loop (add1 attempts))))))]
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
               (abort (format "Internal error (unknown HTTP response code ~a)" response-code))]))))))
  
  ; ==========================================================================================
  ; MODULE MANAGEMENT
  ; Handles interaction with the module system
  ; ==========================================================================================
  
  ; do-require : path path symbol syntax -> symbol
  ; requires the given filename, which must be a module, in the given path.
  (define (do-require file-path package-path module-path stx load?)
    (parameterize ((current-load-relative-directory package-path))    
      ((current-module-name-resolver)
       file-path
       module-path
       stx
       load?)))
  
  (define *package-search-chain* 
    (make-parameter 
     (list
      get/linkage
      get/installed-cache
      get/uninstalled-cache-dummy
      get/server
      get/uninstalled-cache)))
  
  
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
             (cons dir dirs))])))))
