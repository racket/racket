#reader scribble/reader
#lang scheme/base

(require scribble/decode
         scribble/struct
         scribble/manual-struct
         scheme/list
         scheme/string
         scheme/match
         net/url
         (only-in scheme/class send)
         (only-in xml xexpr->string)
         (only-in setup/dirs find-doc-dir)
         "utils.ss")

(provide make-search)

(define (cadr-string-lists<? a b)
  (let loop ([a (cadr a)] [b (cadr b)])
    (cond [(null? b) #f]
          [(null? a) #t]
          [(string-ci=? (car a) (car b))
           (or (loop (cdr a) (cdr b))
               ;; Try string<? so "Foo" still precedes "foo"
               (string<? (car a) (car b)))]
          [else (string-ci<? (car a) (car b))])))

(define (make-script renderer sec ri)
  (define l null)
  (define span-classes null)
  ;; To make the index smaller, html contents is represented as one of these:
  ;; - a string
  ;; - an array of contents to be concatenated
  ;; - a two-item array [idx, content], where idx is an index into the
  ;;   span-classes table holding a class name.
  ;; In addition, a "file:/main-doc.../path..." url is saved as ">path..."
  ;; This function does the url compacting.
  (define main-url ; (make sure that it teminates with a slash)
    (regexp-replace #rx"/*$" (url->string (path->url (find-doc-dir))) "/"))
  (define compact-url
    (let ([rx (regexp (string-append "^" (regexp-quote main-url)))])
      (lambda (url) (regexp-replace rx url ">"))))
  ;; This function does the html compacting.
  (define (compact-body xexprs)
    (define (compact xexprs)
      (match xexprs
        [`() xexprs]
        [`("" . ,r) (compact r)]
        [`(,(? string? s1) ,(? string? s2) . ,r)
         (compact `(,(string-append s1 s2) . ,r))]
        [`((span ([class ,c]) . ,b1) (span ([class ,c]) . ,b2) . ,r)
         (compact `((span ([class ,c]) ,@b1 ,@b2) . ,r))]
        [`((span ([class ,c]) . ,b) . ,r)
         (let ([c (cond [(assoc c span-classes) => cdr]
                        [else (let ([n (length span-classes)])
                                (set! span-classes
                                      (cons (cons c n) span-classes))
                                n)])])
           (cons `(,c . ,(compact-body b)) (compact r)))]
        [`(,x . ,r) (cons (xexpr->string x) (compact r))]))
    ;; generate javascript array code
    (let loop ([body (compact xexprs)])
      (if (andmap string? body)
        (format "~s" (string-append* body))
        (let ([body (map (lambda (x)
                           (if (string? x)
                             (format "~s" x)
                             (format "[~a,~a]" (car x) (cdr x))))
                         body)])
          (if (= 1 (length body))
            (car body)
            (string-append* `("[" ,@(add-between body ",") "]")))))))
  (hash-for-each
   (let ([parent (collected-info-parent (part-collected-info sec ri))])
     (if parent
       (collected-info-info (part-collected-info parent ri))
       (collect-info-ext-ht (resolve-info-ci ri))))
   (lambda (k v)
     (when (and (pair? k) (eq? 'index-entry (car k)))
       (set! l (cons (cons (cadr k) v) l)))))
  (set! l (sort l cadr-string-lists<?))
  (set! l
    (for/list ([i l])
      ;; i is (list tag (text ...) (element ...) index-desc)
      (define-values (tag texts elts desc) (apply values i))
      (define text (string-downcase (string-join texts " ")))
      (define-values (href html)
        (let* ([e (add-between elts ", ")]
               [e (make-link-element "indexlink" e tag)]
               [e (send renderer render-element e sec ri)])
          (match e ; should always render to a single `a'
            [`((a ([href ,href] [class "indexlink"]) . ,body))
             (let (;; throw away tooltips, we don't need them
                   [body (match body
                           [`((span ((title ,label)) . ,body))
                            (if (regexp-match? #rx"^Provided from: " label)
                              body
                              ;; if this happens, this code should be updated
                              (error "internal error: unexpected tooltip"))]
                           [else body])])
               (values (compact-url href) (compact-body body)))]
            [else (error "something bad happened")])))
      (define from-libs
        (if (exported-index-desc? desc)
          (string-append*
           `("["
             ,@(add-between
                (map (lambda (x)
                       (format "~s" (match x
                                      [(? symbol?) (symbol->string x)]
                                      [`',(? symbol? x)
                                       (string-append "'" (symbol->string x))])))
                     (exported-index-desc-from-libs desc))
                ",")
             "]"))
          "false"))
      ;; Note: using ~s to have javascript-quoted strings
      (format "[~s,~s,~a,~a]" text href html from-libs)))
  (set! l (add-between l ",\n"))

  @script[#:noscript @list{Sorry, you must have JavaScript to use this page.}]{
    // the url of the main doc tree, for compact url
    // representation (see also the UncompactUrl function)
    plt_main_url = @(format "~s" main-url);
    // classes to be used for compact representation of html strings in
    // plt_search_data below (see also the UncompactHtml function)
    plt_span_classes = [
      @(add-between (map (lambda (x) (format "~s" (car x)))
                         (reverse span-classes))
                    ",\n  ")];
    // this array has an entry for each index link: [text, url, html, from-lib]
    // - text is a string holding the indexed text
    // - url holds the link (">" prefix means relative to plt_main_url)
    // - html holds either a string, or [idx, html] where idx is an
    //   index into plt_span_classes (note: this is recursive)
    plt_search_data = [
    @l];

    // Globally visible bindings
    var key_handler;

    (function(){

    // Configuration options
    var results_num = 20;

    var query, status, results_container, result_links;

    function InitializeSearch() {
      var n;
      n = document.getElementById("plt_search_container").parentNode;
      // hack the table in
      n.innerHTML = ''
        +'<table width="100%">'
        +'<tr><td align="center" colspan="3">'
          +'<input type="text" id="search_box" style="width: 100%;"'
                 +'onchange="key_handler(event);"'
                 +'onkeypress="return key_handler(event);" />'
        +'</td></tr>'
        +'<tr><td align="left">'
          +'<a href="#" title="Previous Page"'
             +'onclick="key_handler(\"PgUp\"); return false;"'
             +'><tt><b>&lt;&lt;</b></tt></a>'
        +'</td><td align="center">'
          +'<span id="search_status" style="color: #601515; font-weight: bold;">'
            +'&nbsp;'
          +'</span>'
        +'</td><td align="right">'
          +'<a href="#" title="Next Page"'
             +'onclick="key_handler(\"PgDn\"); return false;"'
             +'><tt><b>&gt;&gt;</b></tt></a>'
        +'</td></tr>'
        +'<tr><td colspan="3">'
          +'<span id="search_result"'
                +'style="display: none; margin: 0.5em 1em;"></span>'
        +'</td></tr>'
        +'</table>';
      // get the query box
      query = document.getElementById("search_box");
      // status should point to the text object
      status = document.getElementById("search_status");
      if (status.childNodes.length == 1) status = status.firstChild;
      // result_links is the array of result link <container,link> pairs
      result_links = new Array();
      n = document.getElementById("search_result");
      results_container = n.parentNode;
      results_container.normalize();
      result_links.push(n);
      AdjustResultsNum();
      // get search string
      if (location.search.length > 0) {
        var paramstrs = location.search.substring(1).split(/[@";"&]/);
        for (var i=0@";" i<paramstrs.length@";" i++) {
          var param = paramstrs[i].split(/=/);
          if (param.length == 2 && param[0] == "q") {
            query.value = unescape(param[1]).replace(/\+/g," ");
            break;
          }
        }
      }
      if (query.value != "") DoSearch();
      query.focus();
      query.select();
    }

    function AdjustResultsNum() {
      if (result_links.length == results_num) return;
      if (results_num <= 0) results_num = 1; // should have at least one template
      while (result_links.length > results_num)
        results_container.removeChild(result_links.pop());
      while (result_links.length < results_num) {
        var n = result_links[0].cloneNode(true);
        result_links.push(n);
        results_container.appendChild(n);
      }
    }

    var last_search_term_raw, last_search_terms;
    var search_results, first_search_result;
    function DoSearch() {
      var terms = query.value;
      if (terms == last_search_term_raw) return;
      last_search_term_raw = terms;
      terms= terms.toLowerCase()
               .replace(/\s\s*/g," ")                  // single spaces
               .replace(/^\s/g,"").replace(/\s$/g,""); // trim edge spaces
      if (terms == last_search_terms) return;
      last_search_terms = terms;
      status.nodeValue = "Searching " + plt_search_data.length + " entries";
      terms = (terms=="") ? [] : terms.split(/ /);
      if (terms.length == 0) {
        search_results = plt_search_data;
      } else {
        search_results = new Array();
        for (var i=0@";" i<plt_search_data.length@";" i++) {
          var show = true, curtext = plt_search_data[i][0];
          for (var j=0@";" j<terms.length@";" j++) {
            if (curtext.indexOf(terms[j]) < 0) {
              show = false;
              break;
            }
          }
          if (show) search_results.push(plt_search_data[i]);
        }
      }
      first_search_result = 0;
      status.nodeValue = "" + search_results.length + " entries found";
      query.style.backgroundColor =
        (search_results.length == 0) ? "#ffe0e0" : "white";
      UpdateResults();
    }

    function UncompactUrl(url) {
      return url.replace(/^>/, plt_main_url);
    }

    function UncompactHtml(x) {
      if (typeof x == "string") {
        return x;
      } else if (! (x instanceof Array)) {
        alert("Internal error in PLT docs");
      } else if ((x.length == 2) && (typeof(x[0]) == "number")) {
        return '<span class="' + plt_span_classes[x[0]]
               + '">' + UncompactHtml(x[1]) + '</span>';
      } else {
        var s = "";
        for (var i=0@";" i<x.length@";" i++) s = s.concat(UncompactHtml(x[i]));
        return s;
      }
    }

    function UpdateResults() {
      if (first_search_result < 0 ||
          first_search_result >= search_results.length)
        first_search_result = 0;
      for (var i=0@";" i<result_links.length@";" i++) {
        var n = i + first_search_result;
        if (n < search_results.length) {
          var desc = "";
          if (search_results[n][3]) {
            for (var j=0@";" j<search_results[n][3].length@";" j++)
              desc += (j==0 ? "" : ", " )
                      + '<span class="schememod">'
                      + search_results[n][3][j]
                      + '</span>';
            desc = '&nbsp;&nbsp;'
                   + '<span style="font-size: 80%;">'
                   + '<span style="font-size: 80%;">provided from</span> '
                   + desc + '</span>';
          }
          result_links[i].innerHTML =
            '<a href="'
            + UncompactUrl(search_results[n][1]) + '" class="indexlink">'
            + UncompactHtml(search_results[n][2]) + '</a>' + desc;
          result_links[i].style.display = "block";
        } else {
          result_links[i].style.display = "none";
        }
      }
      if (search_results.length == 0)
        status.nodeValue = "No matches found";
      else if (search_results.length <= results_num)
        status.nodeValue = "Showing all " + search_results.length + " matches";
      else
        status.nodeValue =
          "Showing "
          + (first_search_result+1) + "-"
          + Math.min(first_search_result+results_num,search_results.length)
          + " of " + search_results.length
          + ((search_results.length==plt_search_data.length) ? "" : " matches");
    }

    var search_timer = null;
    function HandleKeyEvent(event) {
      if (search_timer != null) {
        var t = search_timer;
        search_timer = null;
        clearTimeout(t);
      }
      var key = event && event.keyCode;
      if (key == 13) {
        DoSearch();
        return false;
      } else if (key == 33) {
        DoSearch(); // in case we didn't update it yet
        first_search_result -= results_num;
        UpdateResults();
        return false;
      } else if (key == 34) {
        DoSearch(); // in case we didn't update it yet
        if (first_search_result + results_num < search_results.length) {
          first_search_result += results_num;
          UpdateResults();
        }
        return false;
      }
      search_timer = setTimeout(DoSearch, 400);
      return true;
    }

    key_handler = HandleKeyEvent;

    window.onload = InitializeSearch;

    })();
  })

(define (make-search)
  (make-splice
   (list
    (make-delayed-block
     (lambda (r s i) (make-paragraph (list (make-script r s i)))))
    (make-element (make-with-attributes #f '((id . "plt_search_container")))
                  null))))
