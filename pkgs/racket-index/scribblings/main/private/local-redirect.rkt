#lang at-exp racket/base
(require scribble/core
         racket/serialize
         racket/class
         racket/match
         racket/set
         setup/dirs
         net/url
         scribble/html-properties
         setup/xref
         "index-scope.rkt")

(provide make-local-redirect)

(define (rewrite-code prefix here-url num-bins)
  @string-append|{
    function |@|prefix|bsearch(str, a, start, end) {
       if (start >= end)
         return false;
       else {
         var mid = Math.floor((start + end) / 2);
         if (a[mid][0] == str)
           return mid;
         else if (a[mid][0] < str)
           return |@|prefix|bsearch(str, a, mid+1, end);
         else
           return |@|prefix|bsearch(str, a, start, mid);
       }
    }

    var |@|prefix|link_target_prefix = false;

    |@(make-convert-all-links prefix
                              "" ""
                              #t
                              #f
                              here-url
                              num-bins)

    AddOnLoad(|@|prefix|convert_all_links);
  }|)

(define (indent n . strs)
  (define i (make-string n #\space))
  (apply
   string-append
   (let loop ([l strs])
     (cond
      [(null? l) null]
      [(equal? (car l) "\n") (list* "\n" i (loop (cdr l)))]
      [else (cons (car l) (loop (cdr l)))]))))

(define (make-convert-all-links prefix suffix lt-suffix by-doc? by-link?
                                here-url num-bins)
  @string-append|{
    |@(if by-link?
          ""
          @string-append|{
             function hash_string(s) {
                var v = 0;
                for (var i = 0; i < s.length; i++) {
                  v = (((v << 5) - v) + s.charCodeAt(i)) & 0xFFFFFF;
                }
                return v;
             }

             function demand_load(p, callback) {
                // Based on a StackOverflow answer, which cites:
                // JavaScript Patterns, by Stoyan Stefanov (O’Reilly). Copyright 2010 Yahoo!, Inc., 9780596806750.
                var script = document.getElementsByTagName('script')[0];
                var newjs = document.createElement('script');
                newjs.src = p;
                if (callback) {
                   // IE
                   newjs.onreadystatechange = function () {
                       if (newjs.readyState === 'loaded' || newjs.readyState === 'complete') {
                         newjs.onreadystatechange = null;
                         callback();
                       }
                     };
                   // others
                   newjs.onload = callback;
                }
                script.parentNode.appendChild(newjs);
             }

             var |@|prefix|loaded_link_targets = [];
             var |@|prefix|link_targets = [];
             var |@|prefix|num_link_target_bins = |@(format "~a" num-bins);|@"\n"
          }|)
    function |@|prefix|convert_all_links|@|suffix|() {
       var elements = document.getElementsByClassName("Sq");
       for (var i = 0; i < elements.length; i++) {
         var elem = elements[i];
         var tag = elem.href.match(/tag=[^&]*/);
         |@(if by-doc?
               @indent[5]|{
                 var doc = elem.href.match(/doc=[^&]*/);
                 var rel = elem.href.match(/rel=[^&]*/);
                 if (doc && rel) {
                     var pos = |@|prefix|bsearch(decodeURIComponent(doc[0].substring(4)),
                                                 |@|prefix|link_dirs,
                                                 0,
                                                 |@|prefix|link_dirs.length);
                     if (pos) {
                       var p = |@|prefix|link_dirs[pos][1];
                       if (|@|prefix|link_target_prefix) {
                         p = |@|prefix|link_target_prefix + p;
                       }
                       elem.href = p + "/" + decodeURIComponent(rel[0].substring(4));
                       tag = false;
                     }
                 }
               }|
              "")
         if (tag) {
           |@(if by-link?
                 @indent[7]|{
                   var pos = |@|prefix|bsearch(decodeURIComponent(tag[0].substring(4)),
                                               |@|prefix|link_targets|@|lt-suffix|,
                                               0,
                                               |@|prefix|link_targets|@|lt-suffix|.length);
                   if (pos) {
                     var p = |@|prefix|link_targets|@|lt-suffix|[pos][1];
                     if (|@|prefix|link_target_prefix) {
                       p = |@|prefix|link_target_prefix + p;
                     }
                     elem.href = p;
                   }
                 }|
                 @indent[7]|{
                   var v = hash_string(decodeURIComponent(tag[0].substring(4))) % |@(format "~a" num-bins);
                   if (!|@|prefix|loaded_link_targets[v]) {
                     |@|prefix|loaded_link_targets[v] = true;
                     var p = "|@|here-url|/local-redirect_" + v + ".js";
                     if (|@|prefix|link_target_prefix) {
                       p = |@|prefix|link_target_prefix + p;
                     }
                     demand_load(p, false);
                   }
                 }|)
         }
      }
    }
  }|)

(define search-code
  @string-append|{
    /* http://stackoverflow.com/questions/901115/how-can-i-get-query-string-values */
    function getParameterByName(name) {
        name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec(location.search);
        return results == null ? false : decodeURIComponent(results[1].replace(/\+/g, " "));
    }

    var tag = getParameterByName("tag");
    var doc = getParameterByName("doc");
    var rel = getParameterByName("rel");
    if (doc && rel) {
       var pos = bsearch(doc, link_dirs, 0, link_dirs.length);
       if (pos) {
          window.onload = function() {
            window.location = link_dirs[pos][1] + "/" + rel;
          }
          tag = false;
       }
    }
    if (tag) {
       var v = hash_string(tag) % num_link_target_bins;
       demand_load("local-redirect_" + v + ".js",
                   function() {
                     var r = bsearch(tag, link_targets[v], 0, link_targets[v].length);
                     if (r) {
                       window.location = link_targets[v][r][1];
                     }
                   });
    }
 }|)

(define (js-hash-string s)
  ;; Needs to be the same as a hash function used for keys in JavaScript;
  ;; the JavaScript implementation hash_string() above is based on 
  ;;  http://stackoverflow.com/questions/7616461/generate-a-hash-from-string-in-javascript-jquery
  (for/fold ([v 0]) ([c (in-string s)])
    (bitwise-and (+ (- (arithmetic-shift v 5) v) (char->integer c))
                 #xFFFFFF)))

(define (make-local-redirect user?)
  (define main-at-user? (index-at-user?))
  (list
   (make-render-element
    #f
    null
    (lambda (renderer p ri)
      (define doc-dirs (get-rendered-doc-directories (not user?) user?))
      (define keys (if (and main-at-user? (not user?))
                       ;; If there's no installation-scope "doc", then
                       ;; the "main" redirection table is useless.
                       null
                       (resolve-get-keys #f ri
                                         (lambda (v)
                                           ;; Support key-based indirect only on sections
                                           ;; and module names:
                                           (define t (car v))
                                           (or (eq? t 'part)
                                               (eq? t 'mod-path))))))
      (define (target? v) (and (vector? v) (= 5 (vector-length v))))
      (define dest-dir (send renderer get-dest-directory #t))
      (define (make-dest user? [suffix ""])
        (build-path dest-dir
                    (format (if user?
                                "local-user-redirect~a.js"
                                "local-redirect~a.js")
                            suffix)))
     (define dest (make-dest user?))
     (define alt-dest (make-dest (not user?)))
      ;; Whether references include user and/or main docs is determined
      ;; by 'depends-all-main, 'depends-all-user, or 'depends-all flag
      ;; in "info.rkt".
      (define all-db-content
        (for/list ([k (in-list keys)]
                   #:when (tag? k)
                   #:when (target? (resolve-get p ri k)))
          (list (send renderer tag->query-string k)
                (send renderer tag->url-string ri k #:absolute? user?))))
      (define (write-db o unsorted-db prefix suffix lt-suffix)
        (define db (sort unsorted-db string<? #:key car))
        (fprintf o "~alink_targets~a = [" prefix lt-suffix)
        (for ([e (in-list db)]
              [i (in-naturals)])
          (fprintf o (if (zero? i) "\n" ",\n"))
          (fprintf o " [~s, ~s]" (car e) (cadr e)))
        (fprintf o "];\n\n"))

      (define prefix (if user? "user_" ""))
      (define here-url (if (or user? main-at-user?)
                           (url->string (path->url (find-user-doc-dir)))
                           "../local-redirect"))

      ;; Break all-db-content into 1000-entry chunks that are loaded
      ;; on demand, so that we don't have to load a file proportional
      ;; to the size of all documentation to resolve a small number
      ;; of indirect links.
      (define num-bins (add1 (quotient (length all-db-content) 1000)))
      (define bins (make-vector num-bins null))
      (for ([c (in-list all-db-content)])
        (define h (modulo (js-hash-string (car c)) num-bins))
        (vector-set! bins h (cons c (vector-ref bins h))))
      (for ([unsorted-db (in-vector bins)]
            [i (in-naturals)])
        (define suffix (format "_~a" i))
        (define lt-suffix (format "[~a]" i))
        (call-with-output-file*
         (make-dest #f suffix)
         #:exists 'truncate/replace
         (lambda (o)
           (write-db o unsorted-db prefix suffix lt-suffix)
           (display (make-convert-all-links prefix suffix lt-suffix #f #t here-url 0) o)
           (fprintf o "\n~aconvert_all_links~a();\n" prefix suffix))))
            
      (call-with-output-file*
       dest
       #:exists 'truncate/replace
       (lambda (o)
         (fprintf o "// Autogenerated by `scribblings/main/private/local-redirect'\n")
         (fprintf o "//  This script is included by generated documentation to rewrite\n")
         (fprintf o "//  links expressed as tag queries into local-filesystem links.\n")
         (newline o)
         (when user?
           ;; If not user, link_target_prefix is declared  in the output
           ;; of `rewrite-code` (and we don't want to include a build-time path
           ;; that would end up in a distribution)
           (fprintf o "link_target_prefix = ~s;\n" (url->string
                                                    (path->url
                                                     (path->directory-path
                                                      (build-path (find-doc-dir) "local-redirect")))))
           (newline o))
         (fprintf o "var ~alink_dirs = [" (if user? "user_" ""))
         (define (extract-name e)
           (let-values ([(base name dir?) (split-path e)])
             (path->string name)))
         (for ([e (in-list (sort doc-dirs string<? #:key extract-name))]
               [i (in-naturals)])
           (define name (extract-name e))
           (fprintf o (if (zero? i) "\n" ",\n"))
           (fprintf o " [~s, ~s]" name (if user?
                                           (url->string (path->url e))
                                           (format "../~a" name))))
         (fprintf o "];\n\n")
         (fprintf o (rewrite-code prefix here-url num-bins))
         (newline o)))
      (unless (file-exists? alt-dest)
        ;; make empty alternate file; in `user?` mode, this
        ;; file will get used only when "racket-index" is not
        ;; in installation scope
        (call-with-output-file* alt-dest void))))
   (element
    (style #f (list
               (js-addition (if (and user? (not main-at-user?))
                                (path->url (build-path (find-doc-dir)
                                                       "local-redirect" 
                                                       "local-redirect.js"))
                                (string->url "local-redirect.js")))
               (js-addition (string->url "local-user-redirect.js"))
               (js-addition
                (string->bytes/utf-8 search-code))))
    null)))
