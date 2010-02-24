#lang at-exp scheme
(require scheme/date
         scheme/runtime-path
         xml
         "config.ss"
         "diff.ss"
         "list-count.ss"
         "svn.ss"
         "cache.ss"
         (except-in "dirstruct.ss"
                    revision-trunk-dir)
         "run-collect.ss"
         "monitor-svn.ss"
         "metadata.ss"
         "formats.ss"
         "path-utils.ss"
         "analyze.ss")

(define (base-path pth)
  (define rev (current-rev))
  (define log-dir (revision-log-dir rev))
  ((rebase-path log-dir "/") pth))

(define-runtime-path static "static")

(define (snoc l x) (append l (list x)))
(define (list-head l n)
  (if (zero? n)
      empty
      (list* (first l)
             (list-head (rest l) (sub1 n)))))
(define (all-but-last l) (list-head l (sub1 (length l))))

(define (to-index i)
  (cond
    [(<= i 0) "."]
    [else
     (apply string-append (snoc (add-between (make-list i "..") "/") "/"))]))

(define (current-depth log-pth directory?)
  (define new-pth ((rebase-path (revision-log-dir (current-rev)) "/") log-pth))
  (define depth (sub1 (length (explode-path new-pth))))
  (if directory? 
      depth
      (sub1 depth)))

(define (path->breadcrumb pth directory?)
  (define the-rev (current-rev))
  (define new-pth ((rebase-path (revision-log-dir the-rev) "/") pth))
  (define parts (rest (explode-path new-pth)))
  (define string-parts (list* (format "R~a" the-rev) (map path->string parts)))
  (define (parent-a href sp)
    `(a ([class "parent"] [href ,href]) ,sp))
  (define the-base-path*
    (format "~a~a"
            (base-path pth)
            (if directory? "/" "")))
  (define the-base-path
    (if (string=? the-base-path* "//")
        "/"
        the-base-path*))
  (define prev-rev-url (format "/~a~a" (previous-rev) the-base-path))
  (define cur-rev-url (format "/~a~a" "current" the-base-path))
  ; XXX Don't special case top level
  (values (apply string-append (add-between (list* "DrDr" string-parts) " / "))
          `(span
            (span ([class "breadcrumb"])
                 ,(parent-a "/" "DrDr") " / "
                 ,@(add-between
                    (snoc
                     (for/list ([sp (in-list (all-but-last string-parts))]
                                [from-root (in-naturals)])
                       (define the-depth (current-depth pth directory?))
                       (parent-a (to-index (- the-depth from-root)) sp))
                     `(span ([class "this"]) 
                            ,(last string-parts)))
                    " / "))
            (span ([class "revnav"])
                  (a ([href ,prev-rev-url]) "<-")
                  nbsp
                  (a ([href ,cur-rev-url]) "->|")))))

(define (looks-like-directory? pth)
  (and (regexp-match #rx"/$" pth) #t))

(define (svn-date->nice-date date)
  (regexp-replace "^(....-..-..)T(..:..:..).*Z$" date "\\1 \\2"))

(define (format-commit-msg)
  (define pth (revision-commit-msg (current-rev)))
  (define msg-v (read-cache* pth))
  (match msg-v
    [(struct svn-rev-log (num author date msg changes))
     (define url (format "http://svn.plt-scheme.org/view?view=rev&revision=~a" num))
     (define (timestamp pth)
       (with-handlers ([exn:fail? (lambda (x) "")])
         (date->string (seconds->date (read-cache (build-path (revision-dir (current-rev)) pth))) #t)))
     (define bdate/s (timestamp "checkout-done"))
     (define bdate/e (timestamp "integrated"))
     `(table ([class "data"])
             (tr ([class "author"]) (td "Author:") (td ,author))
             (tr ([class "date"]) (td "Commit Date:") (td ,(svn-date->nice-date date)))
             (tr ([class "date"]) (td "Build Start:") (td ,bdate/s))
             (tr ([class "date"]) (td "Build End:") (td ,bdate/e))
             (tr ([class "msg"]) (td "Log:") (td (pre ,msg)))
             (tr ([class "changes"]) (td "Changes:")
                 (td
                  ,@(map (match-lambda
                           [(struct svn-change (action path))
                            `(p ([class "output"])
                                ,(symbol->string action) " " 
                                ,(if (regexp-match #rx"^/trunk/collects" path)
                                     (local [(define path-w/o-trunk
                                               (apply build-path (list-tail (explode-path path) 2)))
                                             (define html-path
                                               (if (looks-like-directory? path)
                                                   (format "~a/" path-w/o-trunk)
                                                   path-w/o-trunk))
                                             (define path-url
                                               (path->string* html-path))
                                             (define path-tested?
                                               #t)]
                                       (if path-tested?
                                           `(a ([href ,path-url]) ,path)
                                           path))
                                     path))])
                         changes)))
             (tr (td nbsp) (td (a ([href ,url]) "View Commit"))))]
    [else
     'nbsp]))

(define (footer)
  `(div ([id "footer"])
        "Powered by " (a ([href "http://plt-scheme.org/"]) "PLT Scheme") ". "
        "Written by " (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "
        (a ([href "/help"])
           "Need help?")
        (br)
        "Current time: " ,(date->string (seconds->date (current-seconds)) #t)))

(define (revision-svn-url rev)
  (format "http://svn.plt-scheme.org/view?view=rev&revision=~a"
          rev))

(define (render-event e)
  (with-handlers ([exn:fail?
                   (lambda (x)
                     `(pre ([class "unprintable"]) "UNPRINTABLE"))])
    (match e
      [(struct stdout (bs))
       `(pre ([class "stdout"]) ,(bytes->string/utf-8 bs))]
      [(struct stderr (bs))
       `(pre ([class "stderr"]) ,(bytes->string/utf-8 bs))])))

(define (render-log log-pth)
  (match (log-rendering log-pth)
    [#f
     (file-not-found log-pth)]
    [(and the-log-rendering (struct rendering (_ _ dur _ _ _ responsible changed)))
     (match (read-cache log-pth)
       [(and log (struct status (_ _ command-line output-log)))
        (define-values (title breadcrumb) (path->breadcrumb log-pth #f))
        (define the-base-path
          (base-path log-pth))
        (define svn-url
          (format "http://svn.plt-scheme.org/view/trunk/~a?view=markup&pathrev=~a"
                  the-base-path
                  (current-rev)))
        (define prev-rev-url (format "/~a~a" (previous-rev) the-base-path))
        (define cur-rev-url (format "/~a~a" "current" the-base-path))
        (define output (map render-event output-log))
        `(html (head (title ,title)
                     (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
               (body 
                (div ([class "log, content"])
                     ,breadcrumb
                     (table ([class "data"])
                            (tr (td "Responsible:")
                                (td ,responsible))
                            (tr (td "Command-line:") 
                                (td ,@(add-between
                                       (map (lambda (s)
                                              `(span ([class "commandline"]) ,s))
                                            command-line)
                                       " ")))
                            (tr (td "Duration:") (td ,(format-duration-ms dur)))
                            (tr (td "Timeout:") (td ,(if (timeout? log) checkmark-entity "")))
                            (tr (td "Exit Code:") (td ,(if (exit? log) (number->string (exit-code log)) "")))
                            (tr (td nbsp) (td (a ([href ,svn-url]) "View File"))))
                     ,(if (lc-zero? changed)
                          ""
                          `(div ([class "error"])
                                  "This result of executing this file has changed since the previous revision."
                                  " "
                                  (a ([href ,(format "/diff/~a/~a~a" (current-rev) (previous-rev) the-base-path)])
                                     "See the difference")))
                     ,@(if (empty? output)
                           '()
                           `((div ([class "output"]) " "
                                  ,@output)))
                     ,(with-handlers ([exn:fail?
                                       ; XXX Remove this eventually
                                       (lambda (x)
                                         ; XXX use dirstruct functions
                                         (define png-path
                                           (format "/data~a" (path-add-suffix (path-add-suffix the-base-path #".timing") #".png")))
                                         `(div ([class "timing"])
                                               (a ([href ,png-path])
                                                  (img ([src ,png-path])))))])
                        (make-cdata
                         #f #f
                         (file->string
                          (path-timing-html (substring (path->string* the-base-path) 1)))))
                     ,(footer))))])]))

(define (number->string/zero v)
  (cond 
    [(zero? v)
     'nbsp]
    [else
     (number->string v)]))

(define checkmark-entity
  10004)

(define (render-logs/dir dir-pth #:show-commit-msg? [show-commit-msg? #f])
  (match (dir-rendering dir-pth)
    [#f
     (dir-not-found dir-pth)]
    [(and pth-rendering (struct rendering (tot-start tot-end tot-dur tot-timeout tot-unclean tot-stderr tot-responsible tot-changes)))
     (define files
       (foldl (lambda (sub-pth files)
                (define pth (build-path dir-pth sub-pth))
                (define directory? (cached-directory-exists? pth))
                (define pth-rendering
                  (if directory?
                      (dir-rendering pth)
                      (log-rendering pth)))
                (list* (list directory? sub-pth pth-rendering) files))
              empty
              (cached-directory-list* dir-pth)))
     (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
     `(html (head (title ,title)
                  (script ([src "/sorttable.js"]) " ")
                  (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
            (body
             (div ([class "dirlog, content"])
                  ,breadcrumb
                  ,(if show-commit-msg?
                       (format-commit-msg)
                       "")
                  ,(local [(define log-dir (revision-log-dir (current-rev)))
                           (define base-path 
                             (rebase-path log-dir "/"))
                           (define (path->url pth)
                             (format "http://drdr.plt-scheme.org/~a~a" (current-rev) (base-path pth)))
                           (define id->str
                             #hasheq([timeout . "Timeout"]
                                     [unclean . "Unclean Exit"]
                                     [stderr . "STDERR Output"]
                                     [changes . "Changes"]))
                           
                           (define responsible->problems
                             (local [(define ht (make-hash))]
                               (for ([lc (in-list (list tot-timeout tot-unclean tot-stderr tot-changes))]
                                     [id (in-list (list 'timeout 'unclean 'stderr 'changes))])
                                 (for ([pp (in-list (lc->list lc))])
                                   (define p (bytes->string/utf-8 pp))
                                   (for ([responsible (in-list (rendering-responsibles (log-rendering p)))])
                                     (hash-update! (hash-ref! ht responsible make-hasheq)
                                                   id
                                                   (curry list* p)
                                                   empty))))
                               ht))]
                     (if (zero? (hash-count responsible->problems))
                         ""
                         `(div ([class "status"])
                               ,@(for/list ([(responsible ht) (in-hash responsible->problems)])
                                   (define rcss-id (symbol->string (gensym)))
                                   (define rg-id (symbol->string (gensym 'glyph)))
                                   (define summary
                                     (for/fold ([s ""])
                                               ([id (in-list (list 'timeout 'unclean 'stderr 'changes))])
                                       (define llc (hash-ref ht id empty))
                                       (if (empty? llc)
                                           s
                                           (format "~a [~a: ~a]" s id (length llc)))))                                  
                                   `(div (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" rg-id rcss-id)])
                                             (span ([id ,rg-id]) 9658) " "
                                             ,responsible
                                             " " ,summary)
                                         (blockquote 
                                          ([id ,rcss-id]
                                           [style "display: none;"])
                                          ,@(local [(define i 0)]
                                              (for/list ([id (in-list (list 'timeout 'unclean 'stderr 'changes))])
                                                (define llc (hash-ref ht id empty))
                                                (if (empty? llc)
                                                    ""
                                                    (local [(define display? (< i 2))
                                                            (define css-id (symbol->string (gensym 'ul)))
                                                            (define glyph-id (symbol->string (gensym 'glyph)))]
                                                      (set! i (add1 i))
                                                      `(div (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" glyph-id css-id)])
                                                              (span ([id ,glyph-id]) ,(if display? 9660 9658)) " "
                                                              ,(hash-ref id->str id))
                                                          (ul ([id ,css-id] 
                                                               [style ,(format "display: ~a"
                                                                               (if display? "block" "none"))])
                                                              ,@(for/list ([p llc])
                                                                  `(li (a ([href ,(path->url p)]) ,(path->string (base-path p)))))))))))))))))
                  (table ([class "sortable, dirlist"])
                         (thead
                          (tr (td "Path")
                              (td "Duration (Abs)")
                              (td "Duration (Sum)")
                              (td "Timeout?")
                              (td "Unclean Exit?")
                              (td "STDERR Output")
                              (td "Changes")
                              (td "Responsible")))
                         (tbody
                          ,@(map (match-lambda
                                   [(list directory? sub-pth (struct rendering (start end dur timeout unclean stderr responsible-party changes)))
                                    (define name (path->string sub-pth))
                                    (define abs-dur (- end start))
                                    (define url 
                                      (if directory?
                                          (format "~a/" name)
                                          name))
                                    `(tr ([class ,(if directory? "dir" "file")]
                                          [onclick ,(format "document.location = ~S" url)])
                                         (td ([sorttable_customkey 
                                               ,(format "~a:~a"
                                                        (if directory? "dir" "file")
                                                        name)])
                                             (a ([href ,url]) ,name ,(if directory? "/" "")))
                                         (td ([sorttable_customkey ,(number->string abs-dur)])
                                             ,(format-duration-ms abs-dur))
                                         (td ([sorttable_customkey ,(number->string dur)])
                                             ,(format-duration-ms dur))
                                         ,@(map (lambda (vv)
                                                  (define v (lc->number vv))
                                                  `(td ([sorttable_customkey ,(number->string v)])
                                                       ,(if directory?
                                                            (number->string/zero v)
                                                            (if (zero? v)
                                                                'nbsp
                                                                checkmark-entity))))
                                                (list timeout unclean stderr changes))
                                         (td ,responsible-party))])
                                 (sort files
                                       (match-lambda*
                                         [(list (list dir?1 name1 _)
                                                (list dir?2 name2 _))
                                          (cond
                                            [(and dir?1 dir?2)
                                             (string<=? (path->string name1)
                                                        (path->string name2))]
                                            [dir?1 #t]
                                            [dir?2 #f])]))))
                         (tfoot
                          (tr ([class "total"])
                              (td "Total")
                              (td ,(format-duration-ms (- tot-end tot-start)))
                              (td ,(format-duration-ms tot-dur))
                              (td ,(number->string/zero (lc->number tot-timeout)))
                              (td ,(number->string/zero (lc->number tot-unclean)))
                              (td ,(number->string/zero (lc->number tot-stderr)))
                              (td ,(number->string/zero (lc->number tot-changes)))
                              (td nbsp))))
                  ,(footer))))]))

(define (show-help req)
  `(html
    (head (title "DrDr > Help")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "dirlog, content"])
          ; XXX Use same function as above
          (span ([class "breadcrumb"])
                (a ([class "parent"] [href "/"]) "DrDr") " / "
                (span ([class "this"]) 
                      "Help"))
          @div[[(class "help")]]{
            @h1{What is DrDr?}
            @p{DrDr is a server at @a[[(href "http://www.byu.edu")]]{Brigham Young University} that builds
               and "tests" every revision of the PLT Scheme code base.}
            
            @h1{What kind of server?}
            @p{A 64-bit Linux 2.6.28-15 server running Ubuntu 9.04 with @,(number->string (number-of-cpus)) cores.}
            
            @h1{How is the build run?}
            @p{Every revision is built from a clean checkout with the standard separate build directory command sequence, except that @code{make}
               is passed @code{-j} with the number of cores. Each revision also has a fresh home directory and PLaneT cache.}
               
            @h1{How long does it take for a build to start after a check-in?}
            @p{Only one build runs at a time and when none is running the SVN repository is polled every @,(number->string (current-monitoring-interval-seconds)) seconds.}
            
            @h1{How is the revision "tested"?}
            @p{Each file's @code{@,SVN-PROP:command-line} SVN property is consulted. If it is the empty string, the file is ignored. If it is a string, then @code{$path} is replaced with the file's path, @code{mzscheme} and @code{mzc} with their path (for the current revision), and @code{mred} and @code{mred-text} with @code{mred-text}'s path (for the current revision); then the resulting command-line is executed. 
               (Currently no other executables are allowed, so you can't @code{rm -fr /}.)
               If there is no property value, the default (@code{mzscheme -t $path}) is used if the file's suffix is @code{.ss}, @code{.scm}, or @code{.scrbl}.}
                    
            @p{The command-line is always executed with a fresh empty current directory which is removed after the run. But all the files share the same home directory and X server, which are both removed after each revision's testing is complete.}
            
            @h1{How many files are "tested" concurrently?}
            @p{One per core, or @,(number->string (number-of-cpus)).}
            
            @h1{How long may a file run?}
            @p{The execution timeout is @,(number->string (current-subprocess-timeout-seconds)) seconds by default, but the @code{@,SVN-PROP:timeout} property is used if @code{string->number} returns a number on it.}
            
            @h1{May these settings be set on a per-directory basis?}
            @p{Yes; if the SVN property is set on any ancestor directory, then its value is used for its descendents when theirs is not set.
               }
            
            @h1{What data is gathered during these runs?}
            @p{When each file is run the following is recorded: the start time, the command-line, the STDERR and STDOUT output, the exit code (unless there is a timeout), and the end time. All this information is presented in the per-file DrDr report page.}
            
            @h1{How is the data analyzed?}
            @p{From the data collected from the run, DrDr computes the total test time and whether output has "changed" since the last time the file was tested.}
            
            @h1{What output patterns constitute a "change"?}
            @p{At the most basic level, if the bytes are different. However, there are two subtleties. First, DrDr knows to ignore the result of @code{time}. Second, the standard output and standard error streams are compared independently. The difference display pages present changed lines with a @span[([class "difference"])]{unique background}.}
            
            @h1{How is this site organized?}
            @p{Each file's test results are displayed on a separate page, with a link to the previous revision on changes. All the files in a directory are collated and indexed recursively. On these pages each column is sortable and each row is clickable. The root of a revision also includes the SVN commit message with links to the test results of the modified files. The top DrDr page displays the summary information for all the tested revisions.}
            
            @h1{What is the difference between @code{Duration (Abs)} and @code{Duration (Sum)}?}
            @p{@code{Duration (Abs)} is the difference between the earliest start time and the latest end time in the collection.}
            @p{@code{Duration (Sum)} is the sum of each file's difference between the start time and end time.}
            @p{The two are often different because of parallelism in the testing process. (Long absolute durations indicate DrDr bugs waiting to get fixed.)}
            
            @h1{What do the graphs mean?}
            @p{There is a single graph for each file, i.e., graphs are not kept for old revisions.}
            @p{The X-axis is the revision tested. The Y-axis is the percentage of the time of the slowest revision.}
            @p{The gray, horizontal lines show where 0%, 25%, 50%, 75%, and 100% are in the graph.}
            @p{The black line shows the times for overall running of the file. The colored lines show the results from @code{time}. For each color, the "real" time is the darkest version of it and the "cpu" and "gc" time are 50% and 25% of the darkness, respectively.}
            @p{If the number of calls to @code{time} change from one revision to the next, then there is a gray, vertical bar at that point. Also, the scaling to the slowest time is specific to each horizontal chunk.}
            @p{The graph is split up into panes that each contain approximately 300 revisions. The green arrowheads to the left
               and right of the image move between panes.}
            @p{The legend at the bottom of the graph shows the current pane, as well as the revision number and any timing information from that revision.}
            @p{Click on the graph to jump to the DrDrs page for a specific revision.}

            @h1{Why are some revisions missing?}
            @p{Some revisions are missing because they only modify branches. Only revisions that change @code{/trunk} are tested.}
            
            @h1{How do I make the most use of DrDr?}
            @p{So DrDr can be effective with all testing packages and untested code, it only pays attention to error output and non-zero exit codes. You can make the most of this strategy by ensuring that when your tests are run successfully they have no STDERR output and exit cleanly, but have both when they fail.}
            
            @h1{How do I fix the reporting of an error in my code?}
            @p{If you know you code does not have a bug, but DrDr thinks it does, you can probably fix it by setting its SVN properties: allow it to run longer with @code{@,SVN-PROP:timeout} (but be kind and perhaps change the program to support work load selection on the command-line) or make sure it is run with the right command-line using @code{@,SVN-PROP:command-line}.}
            
            @h1{How can I do the most for DrDr?}
            @p{The most important thing you can do is eliminate false positives by configuring DrDr for your code and removing spurious error output.}
            @p{The next thing is to structure your code so DrDr does not do the same work many times. For example, because DrDr will load every file if your test suite is broken up into different parts that execute when loaded @em{and} they are all loaded by some other file, then DrDr will load and run them twice. The recommended solution is to have DrDr ignore the combining file or change it so a command-line argument is needed to run everything but is not provided by DrDr, that way the combining code is compiled but the tests are run once.}
            
            }                                           
          ,(footer)))))

(define (list-limit len offset l)
  (take (drop l offset) len))

(define (string-first-line s)
  (define v
    (with-input-from-string s read-line))
  (if (eof-object? v)
      "" v))

(require web-server/servlet-env
         web-server/http
         web-server/dispatch)
(define how-many-revs 45)
(define (show-revisions req)
  (define builds-pth (plt-build-directory))
  (define offset
    (match (bindings-assq #"offset" (request-bindings/raw req))
      [(struct binding:form (_ val))
       (string->number (bytes->string/utf-8 val))]
      [_
       0]))
  (define future-revs
    (map (curry cons 'future)
         (sort (directory-list* (plt-future-build-directory))
               >
               #:key (compose string->number path->string))))
  (define how-many-future-revs
    (length future-revs))
  (define built-or-building-revs
    (map (curry cons 'past)
         (sort (directory-list* builds-pth)
               >
               #:key (compose string->number path->string))))
  (define all-revs
    (append future-revs built-or-building-revs))
  (define how-many-total-revs
    (length all-revs))
  `(html
    (head (title "DrDr")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "dirlog, content"])
          ; XXX Use same function as above
          (span ([class "breadcrumb"])
                (span ([class "this"]) 
                      "DrDr"))
          (table ([class "dirlist"])
                 (thead
                  (tr (td "Revision")
                      (td "Duration (Abs)")
                      (td "Duration (Sum)")
                      (td "Timeout?")
                      (td "Unclean Exit?")
                      (td "STDERR Output")
                      (td "Changes")
                      (td "Committer")))
                 (tbody
                  ,@(map (match-lambda
                           [(cons 'future rev-pth)
                            (define name (path->string rev-pth))
                            (define rev (string->number name))
                            (define log (read-cache (future-record-path rev)))
                            (define committer (svn-rev-log-author log))
                            (define commit-msg (string-first-line (svn-rev-log-msg log)))
                            (define title 
                              (format "~a - ~a"
                                      (svn-date->nice-date (svn-rev-log-date log))
                                      commit-msg))
                            
                            `(tr ([class "dir"]
                                  [title ,title])
                                 (td (a ([href ,(revision-svn-url name)]) ,name))
                                 (td ([class "building"] [colspan "6"])
                                     "")
                                 (td ([class "author"]) ,committer))]
                           [(cons 'past rev-pth)
                            (define name (path->string rev-pth))
                            (define url (format "~a/" name))
                            (define rev (string->number name))
                            (define log (read-cache (revision-commit-msg rev)))
                            (define committer (svn-rev-log-author log))
                            (define commit-msg (string-first-line (svn-rev-log-msg log)))
                            (define title 
                              (format "~a - ~a"
                                      (svn-date->nice-date (svn-rev-log-date log))
                                      commit-msg))
                            (define (no-rendering-row)
                              (define mtime 
                                (file-or-directory-modify-seconds (build-path builds-pth rev-pth)))
                              
                              `(tr ([class "dir"]
                                    [title ,title])
                                   (td (a ([href "#"]) ,name))
                                   (td ([class "building"] [colspan "6"])
                                       "Build in progress. Started "
                                       ,(format-duration-m
                                         (/ (- (current-seconds) mtime) 60))
                                       " ago.")
                                   (td ([class "author"]) ,committer)))
                            (parameterize ([current-rev rev])
                              (with-handlers 
                                  ([(lambda (x)
                                      (regexp-match #rx"No cache available" (exn-message x)))
                                    (lambda (x)
                                      (no-rendering-row))])
                                ; XXX One function to generate
                                (match (dir-rendering (revision-log-dir rev))
                                  [#f
                                   (no-rendering-row)]
                                  [(struct rendering 
                                           (start end dur timeout unclean
                                                  stderr responsible-party changes))
                                   (define abs-dur (- end start))
                                   
                                   `(tr ([class "dir"]
                                         [title ,title]
                                         [onclick ,(format "document.location = ~S" url)])
                                        (td (a ([href ,url]) ,name))
                                        (td ([sorttable_customkey ,(number->string abs-dur)])
                                            ,(format-duration-ms abs-dur))
                                        (td ([sorttable_customkey ,(number->string dur)])
                                            ,(format-duration-ms dur))
                                        ,@(map (lambda (vv)
                                                 (define v (lc->number vv))
                                                 `(td ([sorttable_customkey ,(number->string v)])
                                                      ,(number->string/zero v)))
                                               (list timeout unclean stderr changes))
                                        (td ,responsible-party))])))])
                                (list-limit
                                 how-many-revs offset
                                 all-revs))))
          (table ([width "100%"])
                 (tr (td ([align "left"])
                         (a ([href ,(format "~a?offset=~a"
                                            (top-url show-revisions)
                                            (max 0 (- offset how-many-revs)))])
                            "Newer Revisions"))
                     (td ([align "right"])
                         (a ([href ,(format "~a?offset=~a"
                                            (top-url show-revisions)
                                            (min (- how-many-total-revs how-many-revs)
                                                 (+ offset how-many-revs)))])
                            "Older Revisions"))))
          ,(footer)))))

(define (show-revision req rev)
  (define log-dir (revision-log-dir rev))
  (parameterize ([current-rev rev]
                 [previous-rev (find-previous-rev rev)])
    (with-handlers ([(lambda (x)
                       (regexp-match #rx"No cache available" (exn-message x)))
                     (lambda (x)
                       (rev-not-found log-dir rev))])
      (render-logs/dir log-dir #:show-commit-msg? #t))))

(define (file-not-found file-pth)
  (define-values (title breadcrumb) (path->breadcrumb file-pth #f))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "This file does not exist in revision " ,(number->string (current-rev)) " or has not been tested.")
          ,(footer)))))
(define (dir-not-found dir-pth)
  (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "This directory does not exist in revision " ,(number->string (current-rev)) " or has not been tested.")
          ,(footer)))))
(define (rev-not-found dir-pth path-to-file)
  (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "The revision " ,(number->string (current-rev)) " does not exist or has not been tested.")
          ,(footer)))))

(define (find-previous-rev this-rev)
  (if (zero? this-rev)
      #f
      (local [(define maybe (sub1 this-rev))]
        (if (cached-directory-exists? (revision-log-dir maybe))
            maybe
            (find-previous-rev maybe)))))

(define (show-file req rev path-to-file)
  (define log-dir (revision-log-dir rev))
  (parameterize ([current-rev rev]
                 [previous-rev (find-previous-rev rev)])
    (if (member "" path-to-file)
        (local [(define dir-pth
                  (apply build-path log-dir (all-but-last path-to-file)))]
          (with-handlers ([(lambda (x)
                             (regexp-match #rx"No cache available" (exn-message x)))
                           (lambda (x)
                             (dir-not-found dir-pth))])
            (render-logs/dir dir-pth)))
        (local [(define file-pth
                  (apply build-path log-dir path-to-file))]
          (with-handlers ([(lambda (x)
                             (regexp-match #rx"No cache available" (exn-message x)))
                           (lambda (x)
                             (file-not-found file-pth))])
            (render-log file-pth))))))

(define (show-revision/current req)
  (init-revisions!)
  (redirect-to
   (top-url show-revision (newest-completed-revision))))
(define (show-file/current req . args)
  (init-revisions!)
  (redirect-to
   (apply top-url show-file (newest-completed-revision) args)))

(define (show-diff req r1 r2 f)
  (define f1 (apply build-path (revision-log-dir r1) f))
  (with-handlers ([(lambda (x)
                     (regexp-match #rx"File is not cached" (exn-message x)))
                   (lambda (x)
                     ; XXX Make a little nicer
                     (parameterize ([current-rev r1])
                       (file-not-found f1)))])
    (define l1 (status-output-log (read-cache f1)))
    (define f2 (apply build-path (revision-log-dir r2) f))
    (with-handlers ([(lambda (x)
                       (regexp-match #rx"File is not cached" (exn-message x)))
                     (lambda (x)
                       ; XXX Make a little nicer
                       (parameterize ([current-rev r2])
                         (file-not-found f2)))])
      (define l2 (status-output-log (read-cache f2)))
      (define f-str (path->string (apply build-path f)))
      (define title 
        (format "DrDr / File Difference / ~a (~a:~a)"
                f-str r1 r2))
      
      `(html (head (title ,title)
                   (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
             (body 
              (div ([class "log, content"])
                   (span ([class "breadcrumb"])
                         (a ([class "parent"] [href "/"])
                            "DrDr")
                         " / "
                         (span ([class "this"]) 
                               "File Difference"))
                   (table ([class "data"])
                          (tr (td "First Revision:") (td (a ([href ,(format "/~a/~a" r1 f-str)]) ,(number->string r1))))
                          (tr (td "Second Revision:") (td (a ([href ,(format "/~a/~a" r2 f-str)]) ,(number->string r2))))
                          (tr (td "File:") (td "/" ,f-str)))
                   (div ([class "output"])
                        (table ([class "diff"])
                               ,@(for/list ([d (in-list (render-log-difference l1 l2))])
                                   (match d
                                     [(struct difference (old new))
                                      `(tr ([class "difference"])
                                           (td ,(render-event old))
                                           (td ,(render-event new)))]
                                     [(struct same-itude (e))
                                      `(tr (td ([colspan "2"]) ,(render-event e)))]))))
                   ,(footer)))))))

(define-values (top-dispatch top-url)
  (dispatch-rules
   [("help") show-help]
   [("") show-revisions]
   [("diff" (integer-arg) (integer-arg) (string-arg) ...) show-diff]
   [("current" "") show-revision/current]
   [("current" (string-arg) ...) show-file/current]
   [((integer-arg) "") show-revision]
   [((integer-arg) (string-arg) ...) show-file]))

(date-display-format 'iso-8601)
(cache/file-mode 'no-cache)
(serve/servlet top-dispatch
               #:port 9000
               #:listen-ip #f
               #:quit? #f
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:extra-files-paths (list static))
