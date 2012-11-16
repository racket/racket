// Globally visible bindings
var key_handler, toggle_panel, hide_prefs, new_query, refine_query,
    set_ctx_query, set_context_query, set_show_manuals, set_show_manual_titles,
    set_results_num, set_type_delay, set_highlight_color, status_line,
    saved_status = false, ctx_query_label_line;

(function(){

// Configuration options (use || in case a cookie exists but is empty)
var ctx_query          = GetCookie("PLT_ContextQuery","");
var ctx_query_label    = GetCookie("PLT_ContextQueryLabel",""); // no prefs UI
var manual_settings    = parseInt(GetCookie("PLT_ManualSettings",1));
var show_manuals       = manual_settings % 10;
var show_manual_titles = ((manual_settings - show_manuals) / 10) > 0;
var results_num        = (parseInt(GetCookie("PLT_ResultsNum",false)) || 20);
var type_delay         = (parseInt(GetCookie("PLT_TypeDelay",false)) || 150);
var highlight_color    = (GetCookie("PLT_HighlightColor",false) || "#ffd");
var background_color   = "#f8f8f8";

var query, results_container, result_links;
var prev_page_link1, prev_page_link2, next_page_link1, next_page_link2;

// tabIndex fields are set:
//   1 query
//   2 index links
//   3 help/pref toggle
//   4 pref widgets
//   5 clear current context-query
//  -1 prev/next page (un-tab-able)

function MakePref(label, input) {
  return '<tr><td align="right">' + label + ':&nbsp;&nbsp;</td>'
            +'<td>' + input + '</td></tr>';
}
var descriptions = new Array();
function PrefInputArgs(name, desc) {
  // don't plant `desc' directly in the text -- it might have quotes
  descriptions[name] = desc;
  return 'tabIndex="4" id="'+name+'_pref"'
       +' onkeypress="hide_prefs(event);"'
       +' onchange="set_'+name+'(this); return true;"'
       +' onfocus="saved_status=status_line.innerHTML;'
                 +'status_line.innerHTML=descriptions[\''+name+'\'];"'
       +' onblur="status_line.innerHTML=(saved_status || \'\');"';
}

function MakeChevrons(num, middle) {
  return '<div style="text-align: center;">'
          +'<a href="#" title="Previous Page" id="prev_page_link'+num+'"'
            +' tabIndex="-1"'
            +' style="float: left; text-decoration: none; font-weight: bold;'
                   +' font-family: monospace;"'
            +' onclick="key_handler(\'PgUp\'); return false;"'
            +'>&lt;&lt;</a>'
          +'<a href="#" title="Next Page" id="next_page_link'+num+'" '
            +'tabIndex="-1"'
            +' style="float: right; text-decoration: none; font-weight: bold;'
                   +' font-family: monospace;"'
            +' onclick="key_handler(\'PgDn\'); return false;"'
            +'>&gt;&gt;</a>'
          +middle
        +'</div>';
}

function MakeContextQueryItem(qry, desc) {
  return '<li><a href="?hq='+encodeURIComponent(qry)+'" tabIndex="4"'
              +' title="set this as your context-query"'
              +' style="text-decoration: none;"'
              +' onclick="return new_query(this,\''
                                   +encodeURIComponent(desc)
                                   +'\');">'
              + desc.replace(/{{/g, "<tt><b>").replace(/}}/g, "</b></tt>")
           + '</a></li>';
}

function MakeIcon(label,title,action) {
  return '<a href="#" title="'+title+'" tabIndex="3"'
          +' style="text-decoration: none; color: black;'
                 +' font-weight: bold; font-family: monospace;"'
          +' onclick="'+action+'; return false;"'
          +'>'+label+'</a>';
}

function InitializeSearch() {
  var n;
  n = document.getElementById("plt_search_container");
  // hack the dom widgets in
  var panelbgcolor = "background-color: #f0f0f0;";
  var panelstyle =
    'style="display: none; border: 1px solid #222; border-top: 0px;'
         +' font-family: arial, sans-serif; margin: 0em 0em 1em 0em;'
         +' padding: 0.5em; '+panelbgcolor+'"';
  n.innerHTML =
    '<div style="width: 100%; margin: 0em; padding: 0em;'
              +' background-color: '+background_color+';">'
      +'<div style="'+panelbgcolor+'">'
        +'<div style="float: right; border-color: #222; border-style: solid;'
                   +' border-width: 1px 1px 0px 0px;">'
          +MakeIcon("[?]", "help",        "toggle_panel(\'help\')")
          +MakeIcon("[!]", "preferences", "toggle_panel(\'prefs\')")
        +'</div>'
        +'<input type="text" id="search_box" style="width: 90%;"'
              +' tabIndex="1" onkeydown="return key_handler(event);"'
              +' onkeydown="return key_handler(null);" />'
      +'</div>'
      +'<div id="close_panel"'
          +' style="display: none; float: right;'
                 +' margin: 0.2em 0.5em; '+panelbgcolor+'">'
        +MakeIcon("&#10005;", "close", "toggle_panel(false)")
      +'</div>'
      +'<div id="help_panel" '+panelstyle+'>'
        +'<ul style="padding: 0em; margin: 0.5em 1.5em;">'
        +'<li>Hit <tt>PageUp</tt>/<tt>PageDown</tt> or'
           +' <tt>C+Enter</tt>/<tt>S+C+Enter</tt> to scroll through the'
           +' results.</li>'
        +'<li>Search terms are all required, use'
           +' &ldquo;<tt>N:<i>str</i></tt>&rdquo; to negate a term.'
        +'<li>Use &ldquo;<tt>M:<i>str</i></tt>&rdquo; to match only'
           +' identifiers from modules that (partially) match'
           +' &ldquo;<tt><i>str</i></tt>&rdquo;; &ldquo;<tt>M:</tt>&rdquo; by'
           +' itself will restrict results to bound names only.</li>'
        +'<li>&ldquo;<tt>L:<i>str</i></tt>&rdquo; is similar to'
           +' &ldquo;<tt>M:<i>str</i></tt>&rdquo;, but'
           +' &ldquo;<tt><i>str</i></tt>&rdquo; should match the module name'
           +' exactly.</li>'
        +'<li>&ldquo;<tt>T:<i>str</i></tt>&rdquo; restricts results to ones in'
           +' the &ldquo;<tt><i>str</i></tt>&rdquo; manual (naming the'
           +' directory where the manual is found).</li>'
        +'<li>Entries that correspond to bindings have module links that'
           +' create a query restricted to bindings in that module (using'
           +' &ldquo;<tt>L:</tt>&rdquo;), other entries have similar links for'
           +' restricting results to a specific manual (using'
           +' &ldquo;<tt>T:</tt>&rdquo;); you can control whether manual links'
           +' appear (and how) in the preferences.</li>'
        +'<li>Right-clicking these links refines the current query instead of'
           +' changing it (but some browsers don\'t support this).</li>'
        +'</ul>'
      +'</div>'
      +'<div id="prefs_panel" '+panelstyle+'>'
        +'<table align="center" style="margin: 0em 2em;">'
        + MakePref('Show manuals',
           '<select '
                   +PrefInputArgs("show_manuals",
                                  "Controls when manual links are shown")
                   +'>'
             +'<option>never</option>'
             +'<option>except identifiers</option>'
             +'<option>always</option>'
           +'</select>'
           +'<input type="checkbox" '
                   +PrefInputArgs("show_manual_titles",
                                  "Controls how manual links are shown")
                   +'>'
           +' use titles')
        + MakePref('Results per page',
           '<input type="text" '
                   +PrefInputArgs("results_num",
                                  "How many results to show on the page")
                   +'>')
        + MakePref('Type delay',
           '<input type="text" '
                   +PrefInputArgs("type_delay",
                                  "The delay to wait (in msec) before search"
                                  +" results are updated")
                   +'>')
        + MakePref('Exact matches color',
           '<input type="text" '
                   +PrefInputArgs("highlight_color",
                                  "The color to use for highlighting exact"
                                  +" matches (a known color name or #RGB)")
                   +'>')
        + MakePref('Context-Query',
           '<input type="text" '
                   +PrefInputArgs("ctx_query",
                     "A &ldquo;context&rdquo; query that is implicitly added"
                     +" to all searches")
                   +'>&nbsp;'
           +'<a style="font-size: 82%; color: #444; text-decoration: none;"'
             +' href="#" onclick="toggle_panel(\'contexts\'); return false;">'
            +'[more]</a>')
        +'</table>'
      +'</div>'
      +'<div id="contexts_panel" '+panelstyle+'>'
        +'<table align="center" style="margin: 0em 2em;">'
          + MakePref('Context-Query',
             '<input type="text" '
                     +PrefInputArgs("context_query",
                       "A &ldquo;context&rdquo; query that is implicitly added"
                       +" to all searches")
                     +'>')
        +'</table>'
        +'Clicking the following links will set your context-query to a'
        +' few common choices:'
        +'<ul style="padding: 0em; margin: 0.5em 1.5em;">'
        +MakeContextQueryItem("M:", "Bindings")
        +MakeContextQueryItem("T:reference", "Reference manual")
        +MakeContextQueryItem("M:racket", "{{racket}} bindings")
        +MakeContextQueryItem("M:racket/base", "{{racket/base}} bindings")
        +'</ul>'
      +'</div>'
      +MakeChevrons(1,
        '<span id="search_status"'
            +' style="color: #601515; font-weight: bold;">&nbsp;</span>')
      +'<div>'
        +'<div id="search_result"'
             +' style="display: none;'
             +' margin: 0.1em 0em; padding: 0.25em 1em;"></div>'
      +'</div>'
      +'<br />'
      +MakeChevrons(2,
        '<span id="ctx_query_label" style="color: #444;">&nbsp;</span>')
    +'</div>';
  // get the widgets we use
  query = document.getElementById("search_box");
  status_line = document.getElementById("search_status");
  ctx_query_label_line = document.getElementById("ctx_query_label");
  prev_page_link1 = document.getElementById("prev_page_link1");
  prev_page_link2 = document.getElementById("prev_page_link2");
  next_page_link1 = document.getElementById("next_page_link1");
  next_page_link2 = document.getElementById("next_page_link2");
  // result_links is the array of result link <container,link> pairs
  result_links = new Array();
  n = document.getElementById("search_result");
  results_container = n.parentNode;
  results_container.normalize();
  result_links.push(n);
  AdjustResultsNum();
  // get search string
  var init_q = GetPageArg("q",false);
  if (init_q && init_q != "") query.value = init_q;
  ContextFilter();
  DoSearch();
  query.focus();
  query.select();
}

function AdjustResultsNum() {
  if (result_links.length == results_num) return;
  if (results_num <= 0) results_num = 1; // expects at least one template
  while (result_links.length > results_num)
    results_container.removeChild(result_links.pop());
  while (result_links.length < results_num) {
    var n = result_links[0].cloneNode(true);
    result_links.push(n);
    results_container.appendChild(n);
  }
}

// Terms are compared using Compare(pat,str), which returns one of several
// constants, in increasing order of success:
// - C_fail: no match
// - C_words1: all of the "subwords" in pat matched, where a word is an
//   alphanumeric or a punctuation substring (for example, "foo-bar!!" has
//   these words: "foo", "-", "bar", "!!"), and a match means a prefix of a
//   subword in str matched one of pat's subwords
// - C_words2: same, but all of the subwords in str were matched (so both str
//   and pat have the same number of subwords)
// - C_words3: same, but all matches were complete (so str is a permutation of
//   pat)
// - C_match: pat matched somewhere in str
// - C_prefix: pat is a prefix of str
// - C_exact: pat is equal to str
// - C_rexact: there was a ("real") exact match, see below
// Note that the value is searched from the last one and going back, so, for
// example, if pat is a prefix of str the result is C_prefix, and no subword
// matching is attempted.  A result of C_exact can be returned by some of the
// `X:' operators: its purpose is to be able to return a result that doesn't
// affect the exactness of the search (for example L:foo searches for a source
// module that is exactly `foo', but it will return C_exact which means that it
// doesn't change whether the match is considered exact or not).
var C_fail   = 0, C_min = 0,
    C_words1 = 1,
    C_words2 = 2,
    C_words3 = 3,
    C_match  = 4,
    C_prefix = 5,
    C_exact  = 6,
    C_rexact = 7, C_max = 7;

function Compare(pat, str) {
  var i = str.indexOf(pat);
  if (i < 0) return C_fail;
  if (i > 0) return C_match;
  if (pat.length == str.length) return C_rexact;
  return C_prefix;
}
function MaxCompares(pat, strs) {
  var r = C_min, c;
  for (var i=0; i<strs.length; i++) {
    c = Compare(pat,strs[i]);
    if (c > r) { if (c >= C_max) return c; else r = c; }
  }
  return r;
}

// Utilities for dealing with "subword" searches
function SortByLength(x, y) {
  // use this to sort with descending length and otherwise lexicographically
  var xlen = x.length, ylen = y.length;
  if (xlen != ylen) return ylen - xlen;
  else if (x < y) return -1;
  else if (x > y) return +1;
  else return 0;
}
function CompileWordCompare(term) {
  var words = term.split(/\b/).sort(SortByLength);
  var word_num = words.length;
  var is_word = new Array(word_num);
  for (var i=0; i<word_num; i++) is_word[i] = (words[i].search(/\w/) >= 0);
  return function (str) {
    var str_words = str.split(/\b/).sort(SortByLength);
    var str_word_num = str_words.length;
    if (str_word_num < word_num) return C_fail; // can't work
    var r = C_words3;
    for (var i=0; i<word_num; i++) {
      var word = words[i];
      var found = -1;
      for (var j=0; j<str_word_num; j++) {
        var str_word = str_words[j];
        if (str_word != null) {
          if (word == str_word) {
            found = j;
            break;
          } else {
            var k = str_word.indexOf(word);
            if (k == 0 || (k > 0 && !is_word[i])) {
              found = j;
              r = C_words2;
              break;
            }
          }
        }
      }
      if (found >= 0) str_words[found] = null;
      else return C_fail;
    }
    // either C_words3 for a complete permutation, or C_words2 when all of the
    // subwords in str were matched
    if (word_num == str_word_num) return r;
    // otherwise return C_words1 regardless of whether all of the words in term
    // matched exactly words in str
    else return C_words1;
  };
}
// Tests:
// console.log("testing...");
// function t(x,y,e) {
//   var r = CompileWordCompare(x)(y);
//   if (e != r) {
//     console.log('CompileWordCompare("'+x+'")("'+y+'")'+
//                 " => "+r+" but expected "+e);
//   }
// }
// t("x-y","x-y",C_words3);
// t("x-y","y-x",C_words3);
// t("x-y","y--x",C_words2);
// t("x-y","y=-x",C_words2);
// t("xx-yy","x-y",C_fail);
// t("xx-yy","xxx-yyy",C_words2);
// t("x-y-x","xxx-yyy",C_fail);
// t("x-y-x","xxx-yyy-xxx",C_words2);
// t("x-y-x","yyy-xxx-xxx",C_words2);
// t("x-y-xx","xx-y-xxx",C_words2);
// console.log("done");

function NormalizeSpaces(str) {
  // use single spaces first, then trim edge spaces
  return str.replace(/\s\s*/g," ").replace(/^ /,"").replace(/ $/,"");
}

function SanitizeHTML(str) {
  // Minimal protection against bad html strings
  // HACK: we don't want to actually parse these things, but we do want a way
  // to have tt text, so use a "{{...}}" markup for that.
  return str.replace(/[&]/g, "&amp;")
            .replace(/>/g,   "&gt;")
            .replace(/</g,   "&lt;")
            .replace(/\"/g,  "&quot;")
            .replace(/{{/g,  "<tt><b>")
            .replace(/}}/g,  "</b></tt>");
}

function UrlToManual(url) {
  return url.replace(/#.*$/, "")       // remove fragment,
            .replace(/\?.*$/, "")      // query,
            .replace(/\/[^\/]*$/, "")  // filename,
            .replace(/^(.*\/|>)/, ""); // and directory.
}

// Tests for matches and highlights:
//   "append"
//   "L:racket append"
//   "L:racket" (no exact matches except for the `racket' module)
//   "L:racke" (only module names that match `racke')
//   "edi" and "edit" (the top few should be the same)

// Additional "hidden" operators:
//   "A:{ foo bar }" -- an `and' query
//   "O:{ foo bar }" -- an `or' query
//   "Q:foo" -- stands for just "foo", useful for quoting Q:} inside the above
// Note: they're "hidden" because the syntax might change, and it's intended
// mostly for context queries.

function CompileTerm(term) {
  var op = ((term.search(/^[NLMTQ]:/) == 0) && term.substring(0,1));
  if (op) term = term.substring(2);
  term = term.toLowerCase();
  switch (op) {
  case "N":
    op = CompileTerm(term);
    // return C_exact if it's not found, so it doesn't disqualify exact matches
    return function(x) { return (op(x) >= C_match) ? C_fail : C_exact; };
  case "L":
    return function(x) {
      if (!x[3]) return C_fail;
      if (x[3] == "module") // rexact allowed, show partial module matches
        return Compare(term,x[0]);
      return (MaxCompares(term,x[3]) >= C_exact) ? C_exact : C_fail;
    };
  case "M":
    return function(x) {
      if (!x[3]) return C_fail;
      if (x[3] == "module") return Compare(term,x[0]); // rexact allowed
      return (MaxCompares(term,x[3]) >= C_match) ? C_exact : C_fail;
    };
  case "T":
    return function(x) {
      if (Compare(term,UrlToManual(x[1])) < C_exact) return C_fail;
      else if (x[1].search(/\/index\.html$/) > 0) return C_rexact;
      else return C_exact;
    };
  /* a case for "Q" is not needed -- same as the default case below */
  default:
    var compare_words = CompileWordCompare(term);
    return function(x) {
      var r = Compare(term,x[0]);
      // only bindings can be used for rexact matches
      if (r >= C_rexact) return (x[3] ? r : C_exact);
      if (r > C_words3) return r;
      else return compare_words(x[0]);
    };
  }
}

function CompileAndTerms(preds) {
  return function(x) {
    var r = C_max, c;
    for (var i=0; i<preds.length; i++) {
      c = preds[i](x);
      if (c < r) { if (c <= C_min) return c; else r = c; }
    }
    return r;
  };
}

function CompileOrTerms(preds) {
  return function(x) {
    var r = C_min, c;
    for (var i=0; i<preds.length; i++) {
      c = preds[i](x);
      if (c > r) { if (c >= C_max) return c; else r = c; }
    }
    return r;
  };
}

function CompileTermsR(terms, nested) {
  var term, result = new Array();
  while (terms.length > 0) {
    term = terms.pop();
    switch (term) {
    case "A:{": result.push(CompileTermsR(terms, CompileAndTerms)); break;
    case "O:{": result.push(CompileTermsR(terms, CompileOrTerms));  break;
    default:
      // "}" has terminates a compound, otherwise it's an ordinary search term
      if (nested && (term == "}")) return nested(result);
      else result.push(CompileTerm(term));
    }
  }
  // all compound operators are implicitly terminated at the end
  if (nested) return nested(result);
  else return result;
}

function CompileTerms(terms, nested) {
  terms.reverse();
  return CompileTermsR(terms, nested);
}

function Id(x) {
  return x;
}

var indicators =
  (function() {
    // construct indicator lines that look like: "--->..."
    var i, j, s, a = new Array();
    for (i=0; i<11; i++) {
      s = "";
      for (j=0; j<10; j++) {
        s += (j==9 && i==10) ? "&#9632;"
           : (j<i)  ? "&mdash;"
           : (j==i) ? "&#9658;"
           : "&middot;";
      }
      a.push("&nbsp;<tt>"+s+"</tt>");
    }
    return a;
   }());
function MakeShowProgress() {
  var orig = status_line.innerHTML;
  return function(n) {
    status_line.innerHTML =
      orig + indicators[Math.round(10*n/search_data.length)];
  };
}

function Search(data, term, is_pre, K) {
  // `K' is a continuation if this run is supposed to happen in a "thread"
  // false otherwise
  var t = false;
  function Killer() { if (t) clearTimeout(t); };
  // term comes with normalized spaces (trimmed, and no double spaces)
  var preds = (term=="") ? [] : CompileTerms(term.split(/ /), false);
  if (preds.length == 0) {
    var ret = is_pre ? [0,data] : [0,[]];
    if (K) { K(ret); return Killer; }
    else return ret;
  }
  var i = 0;
  var matches = new Array(C_max-C_min);
  for (i=0; i<matches.length; i++) matches[i] = new Array();
  var chunk_fuel = K ? Math.round(data.length/10) : data.length;
  var progress = K ? MakeShowProgress() : Id;
  i = 0;
  function DoChunk() {
    var fuel = Math.min(chunk_fuel, data.length-i);
    progress(i+fuel);
    while (fuel > 0) {
      var r, min = C_max, max = C_min;
      for (var j=0; j<preds.length; j++) {
        r = preds[j](data[i]);
        if (r < min) {
          min = r;
          if (r <= C_min) break; // get out if it's hopeless
        }
        if (r > max) max = r;
      }
      if (max >= C_rexact && min >= C_exact) min = C_rexact;
      if (min > C_min) matches[C_max - min].push(data[i]);
      fuel--; i++;
    }
    if (i<data.length) t = setTimeout(DoChunk,5);
    else {
      r = [matches[0].length, [].concat.apply([],matches)];
      if (K) K(r); else return r;
    }
  };
  if (!K) return DoChunk();
  else { progress(0); t = setTimeout(DoChunk,5); return Killer; }
}

function GetContextHTML() {
  // useful only when a context is set
  return (((ctx_query_label != "") && ctx_query_label)
          ? SanitizeHTML(ctx_query_label)
          : ('&ldquo;' + SanitizeHTML(ctx_query) + '&rdquo;'));
}
function GetContextClearerHTML(text) {
  return ('<a href="?hq=" tabIndex="5"'
           +' title="Clear context-query"'
           +' style="text-decoration: none; color: #444;'
                  +' font-size: 82%; font-weight: bold;"'
           +' onclick="return new_query(this,\'\');">'
          + text
          + '</a>');
}

var search_data; // pre-filtered searchable index data
function ContextFilter() {
  ctx_query = NormalizeSpaces(ctx_query);
  search_data = Search(plt_search_data, ctx_query, true, false)[1];
  if (ctx_query == "") {
    ctx_query_label_line.innerHTML =
      '<a href="#" tabIndex="5"'
       +' title="Edit context-query"'
       +' style="text-decoration: none; color: #444;'
              +' font-size: 82%; font-weight: bold;"'
       +' onclick="toggle_panel(\'contexts\'); return false;">'
       +'[set context]</a>';
  } else {
    ctx_query_label_line.innerHTML =
      'Context:&nbsp;' + GetContextHTML()
      + '&nbsp;'
      + GetContextClearerHTML('[clear')
      + '/<a href="#" tabIndex="5"'
          +' title="Edit context-query"'
          +' style="text-decoration: none; color: #444;'
                 +' font-size: 82%; font-weight: bold;"'
          +' onclick="toggle_panel(\'contexts\'); return false;">'
          +'modify]</a>';
  }
  last_search_term = null;
  last_search_term_raw = null;
}

var last_search_term, last_search_term_raw;
var search_results = [], first_search_result, exact_results_num;
var kill_bg_search = function(){ return; };
var search_timer = false;
function DoDelayedSearch() {
  // the whole delayed search thing was done to avoid redundant searching that
  // get the UI stuck, but now the search happens on a "thread" anyway, so it
  // might not be needed
  if (search_timer) clearTimeout(search_timer);
  search_timer = setTimeout(DoSearch, type_delay);
}
function DoSearch() {
  var term = query.value;
  if (term == last_search_term_raw) return;
  last_search_term_raw = term;
  term = NormalizeSpaces(term);
  if (term == last_search_term) return;
  last_search_term = term;
  status_line.innerHTML = "Searching " + search_data.length + " entries";
  kill_bg_search();
  kill_bg_search = Search(search_data, term, false,
    // use a continuation to run this in the background
    function(res) {
      search_results = res[1];
      exact_results_num = res[0];
      first_search_result = 0;
      status_line.innerHTML = "" + search_results.length + " entries found";
      query.style.backgroundColor =
        ((search_results.length == 0) && (term != "")) ? "#ffe0e0" : "white";
      UpdateResults();
    });
}

function UncompactUrl(url) {
  return url.replace(/^>/, plt_main_url);
}

function UncompactHtml(x) {
  if (typeof x == "string") {
    return x;
  } else if (!(x instanceof Array)) {
    return alert("Internal error in PLT docs");
  } else if ((x.length == 2) && (typeof(x[0]) == "number")) {
    return '<span class="' + plt_span_classes[x[0]]
           + '">' + UncompactHtml(x[1]) + '</span>';
  } else {
    var s = "";
    for (var i=0; i<x.length; i++) s = s.concat(UncompactHtml(x[i]));
    return s;
  }
}

function UpdateResults() {
  if (first_search_result < 0 ||
      first_search_result >= search_results.length)
    first_search_result = 0;
  var link_args = (page_query_string && ("?"+page_query_string));
  for (var i=0; i<result_links.length; i++) {
    var n = i + first_search_result;
    if (n < search_results.length) {
      var note = false, res = search_results[n], desc = res[3];
      if ((desc instanceof Array) && (desc.length > 0)) {
        note = '<span class="smaller">provided from</span> ';
        for (var j=0; j<desc.length; j++)
          note +=
            (j==0 ? "" : ", ")
            + '<a href="?q=' + encodeURIComponent("L:"+desc[j]) + '"'
               +' class="RktMod" tabIndex="2"'
               +' title="show bindings from the '+desc[j]+' module'
                       +' (right-click to refine current query)"'
               +' style="text-decoration: none; color: #006;"'
               +' onclick="return new_query(this,\'\');"'
               +' oncontextmenu="return refine_query(this);">'
            + desc[j] + '</a>';
      } else if (desc == "module") {
        note = '<span class="smaller">module</span>';
      }
      if (show_manuals == 2 || (show_manuals == 1 && !desc)) {
        var manual = UrlToManual(res[1]),
            idx = (show_manual_titles && plt_manual_ptrs[manual]);
        note = (note ? (note + " ") : "");
        note += '<span class="smaller">in</span> '
                + '<a href="?q=T:' + manual + '" tabIndex="2"'
                   +' title="show entries from the '+manual+' manual'
                           +' (right-click to refine current query)"'
                   +' style="text-decoration: none; color: #006;"'
                   +' onclick="return new_query(this,\'\');"'
                   +' oncontextmenu="return refine_query(this);">'
                + ((typeof idx == "number")
                   ? ('<i>'+UncompactHtml(search_data[idx][2])+'</i>')
                   : manual)
                + '</a>';
      }
      if (note)
        note = '&nbsp;&nbsp;<span class="smaller">' + note + '</span>';
      var href = UncompactUrl(res[1]);
      if (link_args) {
        var hash = href.indexOf("#");
        if (hash >= 0)
          href = href.substring(0,hash) + link_args + href.substring(hash);
        else
          href = href + link_args;
      }
      result_links[i].innerHTML =
        '<a href="' + href + '" class="indexlink" tabIndex="2">'
        + UncompactHtml(res[2]) + '</a>' + (note || "");
      result_links[i].style.backgroundColor =
        (n < exact_results_num) ? highlight_color : background_color;
      result_links[i].style.display = "block";
    } else {
      result_links[i].style.display = "none";
    }
  }
  var exact = Math.min((exact_results_num - first_search_result),
                       results_num);
  exact = (exact <= 0) ? ''
          : ' (<span style="background-color: '+highlight_color+';">'
            + ((exact == results_num) ? 'all' : exact)
            + ' exact</span>)';
  if (search_results.length == 0) {
    if (last_search_term == "") status_line.innerHTML = "";
    else status_line.innerHTML = 'No matches found'
           + ((ctx_query != "")
              ? (' in '+GetContextHTML()
                 +' '+GetContextClearerHTML('<small>[clear]</small>'))
              : '')
           + '<br><div style="color: black; font-size: 82%;">'
           + '(Make sure your spelling is correct'
           + (last_search_term.search(/ /)>=0 ? ', or try fewer keywords' : '')
           + ((ctx_query != "") ? ', or clear the search context' : '')
           + ')</div>';
  } else if (search_results.length <= results_num)
    status_line.innerHTML = "Showing all matches" + exact;
  else
    status_line.innerHTML =
      "Showing "
      + (first_search_result+1) + "-"
      + Math.min(first_search_result+results_num,search_results.length)
      + exact
      + " of " + search_results.length
      + ((search_results.length==search_data.length) ? "" : " matches");
  prev_page_link1.style.color = prev_page_link2.style.color =
    (first_search_result-results_num >= 0) ? "black" : "#e0e0e0";
  next_page_link1.style.color = next_page_link2.style.color =
    (first_search_result+results_num < search_results.length)
    ? "black" : "#e0e0e0";
  saved_status = false;
}

function HandleKeyEvent(event) {
  var key = null;
  if (typeof event == "string") key = event;
  else if (event) {
    switch (event.which || event.keyCode) {
      case 13: key = (event.ctrlKey ? "C-Enter" : "Enter"); break;
      case 33: key = "PgUp";  break;
      case 34: key = "PgDn";  break;
    }
  }
  // note: uses of DoSearch() below starts a background search, which
  // means that the operation can still be done on the previously
  // displayed results.
  switch (key) {
    case "Enter": // starts a search immediately
      DoSearch();
      return false;
    case "C-Enter": // C-enter with no change scrolls down (S -> up)
      if (query.value == last_search_term_raw) {
        if (!event.shiftKey) first_search_result += results_num;
        else if (first_search_result > 0) first_search_result -= results_num;
        else first_search_result = search_results.length - results_num;
        UpdateResults();
      } else {
        DoSearch();
      }
      return false;
    case "PgUp":
      DoSearch(); // in case we didn't update it yet
      first_search_result -= results_num;
      UpdateResults();
      return false;
    case "PgDn":
      DoSearch(); // in case we didn't update it yet
      if (first_search_result + results_num < search_results.length) {
        first_search_result += results_num;
        UpdateResults();
      }
      return false;
  }
  // this function is called via onkeydown, which is happens before the change
  // is visible; so use a timer to call this after the input field is updated
  setTimeout(DoDelayedSearch, 0);
  return true;
}
key_handler = HandleKeyEvent;

// use this one to set the query field without jumping to the current url
// again, since some browsers will reload the whole page for that (it would be
// nice if there was an easy way to add it to the history too)
function NewQuery(node,label) {
  var m, href = node.href;
  if ((m = href.search(/[?]q=[^?&;]+$/)) >= 0) { // `q' cannot be empty
    query.value = decodeURIComponent(href.substring(m+3));
    query.focus();
    DoSearch();
    return false;
  } else if ((m = href.search(/[?]hq=[^?&;]*$/)) >= 0) { // `hq' can
    SetContextQuery(decodeURIComponent(href.substring(m+4)),
                    decodeURIComponent(label));
    return false;
  } else {
    return true;
  }
}
new_query = NewQuery;

// and this appends the query to the current value (it's hooked on the
// oncontextmenu handler that doesn't work everywhere, but at least in
// FF and IE)
function RefineQuery(node) {
  var m = node.href.search(/[?]q=[^?&;]+$/);
  if (m < 0) return true;
  m = decodeURIComponent(node.href.substring(m+3));
  if (query.value.indexOf(m) >= 0) return true;
  else {
    query.value = query.value + " " + m;
    query.focus();
    DoSearch();
    return false;
  }
}
refine_query = RefineQuery;

var panel_shown = false;
function TogglePanel(name) {
  if (panel_shown) {
    document.getElementById(panel_shown+"_panel").style.display = "none";
    document.getElementById("close_panel").style.display = "none";
  }
  panel_shown = ((panel_shown != name) && name);
  if (panel_shown == "prefs") {
    document.getElementById("ctx_query_pref").value = ctx_query;
    document.getElementById("show_manuals_pref").selectedIndex = show_manuals;
    document.getElementById("show_manual_titles_pref").checked
                                                          = show_manual_titles;
    document.getElementById("results_num_pref").value = results_num;
    document.getElementById("type_delay_pref").value = type_delay;
    document.getElementById("highlight_color_pref").value = highlight_color;
  } else if (panel_shown == "contexts") {
    document.getElementById("context_query_pref").value = ctx_query;
  }
  if (panel_shown) {
    document.getElementById(panel_shown+"_panel").style.display = "block";
    document.getElementById("close_panel").style.display = "block";
    // This is annoying -- would be nicer to have a way to make sure
    // that it's in view without scrolling if not necessary.
    // document.getElementById(panel_shown+"_panel").scrollIntoView();
  }
}
toggle_panel = TogglePanel;

function HidePrefs(event) {
  if ((event.which || event.keyCode) == 27) {
    query.focus();
    TogglePanel("prefs"); // this function is called only when it's shown
  }
}
hide_prefs = HidePrefs;

function SetContextQuery(inp,label) {
  // can be called with the input object, or with a string
  if (typeof inp != "string") inp = inp.value;
  if (inp != ctx_query) {
    ctx_query = inp;
    SetCookie("PLT_ContextQuery", ctx_query);
    label = label || "";
    ctx_query_label = label;
    SetCookie("PLT_ContextQueryLabel",label);
    ContextFilter();
    document.getElementById("ctx_query_pref").value = ctx_query;
    document.getElementById("context_query_pref").value   = ctx_query;
    DoSearch();
  }
}
set_ctx_query     = SetContextQuery;
set_context_query = SetContextQuery; // a different widget, same effect

function SetShowManuals(inp) {
  if (inp.selectedIndex != show_manuals) {
    show_manuals = inp.selectedIndex;
    SetCookie("PLT_ManualSettings", show_manuals+(show_manual_titles?10:0));
    UpdateResults();
  }
}
set_show_manuals = SetShowManuals;

function SetShowManualTitles(inp) {
  if (inp.checked != show_manual_titles) {
    show_manual_titles = inp.checked;
    SetCookie("PLT_ManualSettings", show_manuals+(show_manual_titles?10:0));
    UpdateResults();
  }
}
set_show_manual_titles = SetShowManualTitles;

function SetResultsNum(inp) {
  var n = (parseInt(inp.value.replace(/[^0-9]+/g,"")) || results_num);
  inp.value = n;
  if (n != results_num) {
    results_num = n;
    SetCookie("PLT_ResultsNum", results_num);
    AdjustResultsNum();
    UpdateResults();
  }
}
set_results_num = SetResultsNum;

function SetTypeDelay(inp) {
  var n = (parseInt(inp.value.replace(/[^0-9]+/g,"")) || type_delay);
  inp.value = n;
  if (n != type_delay) {
    type_delay = n;
    SetCookie("PLT_TypeDelay", type_delay);
  }
}
set_type_delay = SetTypeDelay;

function SetHighlightColor(inp) {
  var c = (inp.value.replace(/[^a-zA-Z0-9#]/g,"") || highlight_color);
  inp.value = c;
  if (c != highlight_color) {
    highlight_color = c;
    SetCookie("PLT_HighlightColor", highlight_color);
    UpdateResults();
  }
}
set_highlight_color = SetHighlightColor;

AddOnLoad(InitializeSearch);

})();
