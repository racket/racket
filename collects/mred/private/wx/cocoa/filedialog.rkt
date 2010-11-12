#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/path
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "queue.rkt"
         "frame.rkt")

(provide 
 (protect-out file-selector))

(import-class NSOpenPanel NSSavePanel NSURL NSArray)

(define (nsurl->string url)
  (string->path (tell #:type _NSString url path)))

(define (file-selector message directory filename 
                       extension
                       filters style parent)
  (let ([ns (as-objc-allocation-with-retain
             (if (memq 'put style)
                (tell NSSavePanel savePanel)
                (tell NSOpenPanel openPanel)))]
        [parent (and parent
                     (not (send parent get-sheet))
                     parent)])

    (let ([extensions (append
                       (if extension (list extension) null)
                       (if (memq 'packages style) (list "app") null)
                       (for/list ([e (in-list filters)]
                                  #:when (and (regexp-match #rx"[*][.][^.]+$" (cadr e))
                                              (not (equal? (cadr e) "*.*"))))
                         (car (regexp-match #rx"[^.]+$" (cadr e)))))])
      (unless (null? extensions)
        (when (memq 'put style)
          (tellv ns setCanSelectHiddenExtension: #:type _BOOL #t))
        (let ([a (tell NSArray 
                       arrayWithObjects: #:type (_list i _NSString) extensions
                       count: #:type _NSUInteger (length extensions))])
          (tellv ns setAllowedFileTypes: a))))
    (let ([others? (ormap (lambda (e)
                            (equal? (cadr e) "*.*"))
                          filters)])
      (tellv ns setAllowsOtherFileTypes: #:type _BOOL others?))

    (cond
     [(memq 'multi style)
      (tellv ns setAllowsMultipleSelection: #:type _BOOL #t)]
     [(memq 'dir style)
      (tellv ns setCanChooseDirectories: #:type _BOOL #t)
      (tellv ns setCanChooseFiles: #:type _BOOL #f)])

    (when message
      (tellv ns setMessage: #:type _NSString message))
    (when directory
      (tellv ns setDirectoryURL: (tell NSURL 
                                       fileURLWithPath: #:type _NSString (if (string? directory)
                                                                             directory
                                                                             (path->string directory))
                                       isDirectory: #:type _BOOL #t)))
    (when filename
      (tellv ns setNameFieldStringValue: #:type _NSString (path->string
                                                           (file-name-from-path filename))))

    (when (memq 'enter-packages style)
      (tellv ns setTreatsFilePackagesAsDirectories: #:type _BOOL #t))

    (let ([result 
           ;; We run the file dialog completely modally --- shutting out
           ;; all other eventspaces and threads. It would be nice to improve
           ;; on this, but it's good enough.
           (atomically
            (let ([front (get-front)])
              (when parent
                (tellv ns beginSheetModalForWindow: (send parent get-cocoa-window)
                       completionHandler: #f))
              (begin0
               (tell #:type _NSInteger ns runModal)
               (when parent (tell app endSheet: ns))
               (when front (tellv (send front get-cocoa-window)
                                  makeKeyAndOrderFront: #f)))))])
      (begin0
       (if (zero? result)
           #f
           (if (memq 'multi style)
               (let ([urls (tell ns URLs)])
                 (for/list ([i (in-range (tell #:type _NSUInteger urls count))])
                   (nsurl->string (tell urls objectAtIndex: #:type _NSUInteger i))))
               (let ([url (tell ns URL)])
                 (nsurl->string url))))
       (release ns)))))

