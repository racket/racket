// Globally visible bindings
var key_handler, toggle_panel, hide_prefs, new_query, refine_query,
    set_pre_query, set_show_manuals, set_show_manual_titles, set_results_num,
    set_type_delay, set_highlight_color, status_line, saved_status = false;

(function(){

// Configuration options (use || in case a cookie exists but is empty)
var pre_query          = GetCookie("PLT_PreQuery","");
var manual_settings    = parseInt(GetCookie("PLT_ManualSettings",1));
var show_manuals       = manual_settings % 10;
var show_manual_titles = ((manual_settings - show_manuals) / 10) > 0;
var results_num        = (parseInt(GetCookie("PLT_ResultsNum",false)) || 20);
var type_delay         = (parseInt(GetCookie("PLT_TypeDelay",false)) || 300);
var highlight_color    = (GetCookie("PLT_HighlightColor",false) || "#ffd");
var background_color   = "#f8f8f8";

var query, results_container, result_links, prev_page_link, next_page_link;

// tabIndex fields are set:
//   1 query
//   2 index links
//   3 help/pref toggle
//   4 pref widgets
//  -1 prev/next page (un-tab-able)

function MakePref(label, input) {
  return '<tr><td align="right">' + label + ':&nbsp;&nbsp;</td>'
            +'<td>' + input + '</td></tr>';
}
descriptions = new Array();
function PrefInputArgs(name, desc) {
  descriptions[name] = desc;
  return 'tabIndex="4" id="'+name+'_pref"'
       +' onkeypress="hide_prefs(event);"'
       +' onchange="set_'+name+'(this); return true;"'
       +' onfocus="saved_status=status_line.innerHTML;'
                 +'status_line.innerHTML=descriptions[\''+name+'\'];"'
       +' onblur="if (saved_status) status_line.innerHTML=saved_status;"';
}

function InitializeSearch() {
  var n;
  n = document.getElementById("plt_search_container").parentNode;
  // hack the table in
  n.innerHTML = ''
    +'<table width="100%" cellspacing="0" cellpadding="0"'
          +' bgcolor='+background_color+' style="margin: 0em; padding: 0em;">'
    +'<tr><td align="center" colspan="2">'
      +'<input type="text" id="search_box" style="width: 100%;"'
            +' tabIndex="1" onkeydown="return key_handler(event);"'
            +' onkeydown="return key_handler(null);" />'
    +'</td><td class="nobreak">'
      +'<a href="#" title="help" tabIndex="3"'
        +' style="text-decoration: none; font-weight: bold; color: black;"'
        +' onclick="toggle_panel(\'help\'); return false;"'
        +'><tt><b>[?]</b></tt></a>'
      +'<a href="#" title="preferences" tabIndex="3"'
        +' style="text-decoration: none; font-weight: bold; color: black;"'
        +' onclick="toggle_panel(\'prefs\'); return false;"'
        +'><tt><b>[!]</b></tt></a>'
    +'</td></tr>'
    +'<tr><td colspan="3" class="smaller" style="padding: 0em;">'
    +'<div id="help_panel"'
        +' style="display: none; border: 1px solid #222; border-top: 0px;'
               +' font-family: arial, sans-serif; margin: 0em 0em 1em 0em;'
               +' padding: 0.5em; background-color: #f0f0f0;">'
      +'<ul style="padding: 0em; margin: 0.5em 1.5em;">'
      +'<li>Hit <tt>PageUp</tt>/<tt>PageDown</tt> and'
         +' <tt>C+Enter</tt>/<tt>S+C+Enter</tt> to scroll through the'
         +' results.</li>'
      +'<li>Use &ldquo;<tt>M:<i>str</i></tt>&rdquo; to match only identifiers'
         +' from modules that (partially) match'
         +' &ldquo;<tt><i>str</i></tt>&rdquo;; &ldquo;<tt>M:</tt>&rdquo; by'
         +' itself will restrict results to bound names only.</li>'
      +'<li>&ldquo;<tt>L:<i>str</i></tt>&rdquo; is similar to'
         +' &ldquo;<tt>M:<i>str</i></tt>&rdquo;, but'
         +' &ldquo;<tt><i>str</i></tt>&rdquo; should match the module name'
         +' exactly.</li>'
      +'<li>&ldquo;<tt>T:<i>str</i></tt>&rdquo; restricts results to ones in'
         +' the &ldquo;<tt><i>str</i></tt>&rdquo; manual (naming the directory'
         +' where the manual is found).</li>'
      +'<li>Entries that correspond to bindings have module links that create'
         +' a query restricted to bindings in that module (using'
         +' &ldquo;<tt>L:</tt>&rdquo;), other entries have similar links for'
         +' restricting results to a specific manual (using'
         +' &ldquo;<tt>T:</tt>&rdquo;); you can control whether manual links'
         +' appear (and how) in the preferences.</li>'
      +'<li>Right-clicking these links refines the current query instead of'
         +' changing it (but some browsers don\'t support this).</li>'
      +'</ul>'
    +'</div></td></tr>'
    +'<tr><td colspan="3" class="smaller" style="padding: 0em;">'
    +'<div id="prefs_panel"'
        +' style="display: none; border: 1px solid #222; border-top: 0px;'
               +' font-family: arial, sans-serif; margin: 0em 0em 1em 0em;'
               +' padding: 0.5em; background-color: #f0f0f0;">'
    +'<table width="100%" align="center" style="margin: 0em 2em;">'
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
      + MakePref('Pre-Query',
         '<input type="text" '
                 +PrefInputArgs("pre_query",
                   "A &ldquo;context&rdquo; query that is implicitly added to"
                   +" all searches, for example, &ldquo;<tt>M:</tt>&rdquo; to"
                   +" search only bindings, &ldquo;<tt>T:reference</tt>&rdquo;"
                   +" to search only the reference manual")
                 +'>')
    +'</table></div></td></tr>'
    +'<tr><td align="left">'
      +'<a href="#" title="Previous Page" id="prev_page_link" tabIndex="-1"'
        +' style="text-decoration: none; font-weight: bold;"'
        +' onclick="key_handler(\'PgUp\'); return false;"'
        +'><tt><b>&lt;&lt;</b></tt></a>'
    +'</td><td align="center" width="100%">'
      +'<span id="search_status"'
           +' style="color: #601515; font-weight: bold;">&nbsp;</span>'
    +'</td><td align="right">'
      +'<a href="#" title="Next Page" id="next_page_link" tabIndex="-1"'
        +' style="text-decoration: none; font-weight: bold;"'
        +' onclick="key_handler(\'PgDn\'); return false;"'
        +'><tt><b>&gt;&gt;</b></tt></a>'
    +'</td></tr>'
    +'<tr><td colspan="3">'
      +'<span id="search_result"'
           +' style="display: none;'
           +' margin: 0.1em 0em; padding: 0.25em 1em;"></span>'
    +'</td></tr>'
    +'</table>';
  // get the widgets we use
  query = document.getElementById("search_box");
  status_line = document.getElementById("search_status");
  prev_page_link = document.getElementById("prev_page_link");
  next_page_link = document.getElementById("next_page_link");
  // result_links is the array of result link <container,link> pairs
  result_links = new Array();
  n = document.getElementById("search_result");
  results_container = n.parentNode;
  results_container.normalize();
  result_links.push(n);
  AdjustResultsNum();
  PreFilter();
  // get search string
  if (location.search.length > 0) {
    var paramstrs = location.search.substring(1).split(/[;&]/);
    for (var i=0; i<paramstrs.length; i++) {
      var param = paramstrs[i].split(/=/);
      if (param.length == 2 && param[0] == "q") {
        query.value = unescape(param[1]);
        break;
      }
    }
  }
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

// constants for Compare(() results;
// `rexact' is for an actual exact match, so we know that we matched
// *something* and can show exact matches as such, in other words:
//   - < exact => this match is inexact
//   - = exact => does not affect the exactness of this match
//   - > exact => this is an exact match as far as this predicate goes
var C_fail = 0, C_match = 1, C_prefix = 2, C_exact = 3, C_rexact = 4;

function Compare(pat, str) {
  var i = str.indexOf(pat);
  if (i < 0) return C_fail;
  else if (i > 0) return C_match;
  else if (pat.length == str.length) return C_rexact;
  else return C_prefix;
}
function MaxCompares(pat, strs) {
  var r = C_fail;
  for (var i=0; i<strs.length; i++)
    r = Math.max(r, Compare(pat,strs[i]));
  return r;
}

function NormalizeSpaces(str) {
  return str.replace(/\s\s*/g," ")                  // single spaces
             replace(/^\s/g,"").replace(/\s$/g,""); // trim edge spaces
}

function UrlToManual(url) {
  return url.replace(/#.*$/, "")       // remove fragment,
            .replace(/\?.*$/, "")      // query,
            .replace(/\/[^\/]*$/, "")  // filename,
            .replace(/^(.*\/|>)/, ""); // and directory.
}

// Tests for matches and highlights:
//   "append"
//   "L:scheme append"
//   "L:scheme" (no exact matches except for the `scheme' module)
//   "L:schem" (only module names that match `schem')

function CompileTerm(term) {
  var flag = ((term.search(/^[LMT]:/)==0) && term.substring(0,1));
  if (flag) term = term.substring(2);
  term = term.toLowerCase();
  switch(flag) {
    case "L": return function(x) {
      if (!x[3]) return C_fail;
      if (x[3] == "module") // rexact allowed, show partial module matches
        return Compare(term,x[0]);
      return (MaxCompares(term,x[3]) >= C_exact) ? C_exact : C_fail;
    }
    case "M": return function(x) {
      if (!x[3]) return C_fail;
      if (x[3] == "module") return Compare(term,x[0]); // rexact allowed
      return (MaxCompares(term,x[3]) >= C_match) ? C_exact : C_fail;
    }
    case "T": return function(x) {
      if (Compare(term,UrlToManual(x[1])) < C_exact) return C_fail;
      else if (x[1].search(/\/index\.html$/) > 0) return C_rexact;
      else return C_exact;
    }
    default: return function(x) {
      switch (Compare(term,x[0])) {
        case C_fail: return C_fail;
        case C_match: case C_prefix: return C_match;
        case C_exact: case C_rexact: return (x[3] ? C_rexact : C_match);
      }
    }
  }
}

function Search(data, term, is_pre) {
  var preds = (term=="") ? [] : term.split(/ /);
  for (var i=0; i<preds.length; i++) preds[i] = CompileTerm(preds[i]);
  if (preds.length == 0) return (is_pre ? data : []);
  var matches = new Array(), exacts = new Array();
  for (var i=0; i<data.length; i++) {
    var r, min = C_rexact, max = C_fail;
    for (var j=0; j<preds.length; j++) {
      r = preds[j](data[i]); min = Math.min(r, min); max = Math.max(r, max);
    }
    if (max >= C_rexact && min >= C_exact) exacts.push(data[i]);
    else if (min > C_fail) matches.push(data[i]);
  }
  exact_results_num = exacts.length;
  if (exacts.length > 0) return exacts.concat(matches);
  else return matches;
}

var search_data; // pre-filtered searchable index data
function PreFilter() {
  pre_query = NormalizeSpaces(pre_query);
  search_data = Search(plt_search_data, pre_query, true);
  last_search_term = null;
  last_search_term_raw = null;
}

var last_search_term, last_search_term_raw;
var search_results = [], first_search_result, exact_results_num;
function DoSearch() {
  var term = query.value;
  if (term == last_search_term_raw) return;
  last_search_term_raw = term;
  term = NormalizeSpaces(term);
  if (term == last_search_term) return;
  last_search_term = term;
  status_line.innerHTML = "Searching " + search_data.length + " entries";
  search_results = Search(search_data, term, false);
  first_search_result = 0;
  status_line.innerHTML = "" + search_results.length + " entries found";
  query.style.backgroundColor =
    ((search_results.length == 0) && (term != "")) ? "#ffe0e0" : "white";
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
    for (var i=0; i<x.length; i++) s = s.concat(UncompactHtml(x[i]));
    return s;
  }
}

function UpdateResults() {
  if (first_search_result < 0 ||
      first_search_result >= search_results.length)
    first_search_result = 0;
  for (var i=0; i<result_links.length; i++) {
    var n = i + first_search_result;
    if (n < search_results.length) {
      var note = false, res = search_results[n], desc = res[3];
      if ((desc instanceof Array) && (desc.length > 0)) {
        note = '<span class="smaller">provided from</span> ';
        for (var j=0; j<desc.length; j++)
          note +=
            (j==0 ? "" : ", ")
            + '<a href="?q=L:' + encodeURIComponent(desc[j]) + '"'
               +' class="schememod" tabIndex="2"'
               +' style="text-decoration: none; color: #006;"'
               +' onclick="return new_query(this);"'
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
                   +' style="text-decoration: none; color: #006;"'
                   +' onclick="return new_query(this);"'
                   +' oncontextmenu="return refine_query(this);">'
                + ((typeof idx == "number")
                   ? ('<i>'+UncompactHtml(search_data[idx][2])+'</i>')
                   : manual)
                + '</a>';
      }
      if (note)
        note = '&nbsp;&nbsp;<span class="smaller">' + note + '</span>';
      result_links[i].innerHTML =
        '<a href="' + UncompactUrl(res[1]) + '"'
         +' class="indexlink" tabIndex="2">'
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
  if (search_results.length == 0)
    status_line.innerHTML = ((last_search_term=="") ? "" : "No matches found");
  else if (search_results.length <= results_num)
    status_line.innerHTML = "Showing all matches" + exact;
  else
    status_line.innerHTML =
      "Showing "
      + (first_search_result+1) + "-"
      + Math.min(first_search_result+results_num,search_results.length)
      + exact
      + " of " + search_results.length
      + ((search_results.length==search_data.length) ? "" : " matches");
  prev_page_link.style.color =
    (first_search_result-results_num >= 0) ? "black" : "#e0e0e0";
  next_page_link.style.color =
    (first_search_result+results_num < search_results.length)
    ? "black" : "#e0e0e0";
  saved_status = false;
}

var search_timer = null;
function HandleKeyEvent(event) {
  if (search_timer != null) {
    var t = search_timer;
    search_timer = null;
    clearTimeout(t);
  }
  var key = null;
  if (typeof event == "string") key = event;
  else if (event) {
    switch (event.which || event.keyCode) {
      case 13: if (event.ctrlKey) key = "C-Enter";
               break;
      case 33: key = "PgUp";  break;
      case 34: key = "PgDn";  break;
    }
  }
  switch (key) {
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
  search_timer = setTimeout(DoSearch, type_delay);
  return true;
}
key_handler = HandleKeyEvent;

// use this one to set the query field without jumping to the current
// url again, since some browsers will reload the whole page for that
// (it would be nice if there was a way to add it to the history too)
function NewQuery(node) {
  var m = node.href.search(/[?]q=[^?&;]+$/);
  if (m < 0) return true;
  else {
    query.value = decodeURIComponent(node.href.substring(m+3));
    query.focus();
    DoSearch();
    return false;
  }
}
new_query = NewQuery;

// and this appends the the query to the current value (it's hooked
// on the oncontextmenu handler that doesn't work everywhere, but at
// least in FF and IE)
function RefineQuery(node) {
  var m = node.href.search(/[?]q=[^?&;]+$/);
  if (m < 0) return true;
  m = decodeURIComponent(node.href.substring(m+3));
  if (query.value.indexOf(m) >= 0) return true;
  else {
    query.value = m + " " + query.value;
    query.focus();
    DoSearch();
    return false;
  }
}
refine_query = RefineQuery;

var panel_shown = false;
function TogglePanel(name) {
  if (panel_shown)
    document.getElementById(panel_shown+"_panel").style.display = "none";
  panel_shown = ((panel_shown != name) && name);
  if (panel_shown == "prefs") {
    document.getElementById("pre_query_pref").value = pre_query;
    document.getElementById("show_manuals_pref").selectedIndex = show_manuals;
    document.getElementById("show_manual_titles_pref").checked
                                                          = show_manual_titles;
    document.getElementById("results_num_pref").value = results_num;
    document.getElementById("type_delay_pref").value = type_delay;
    document.getElementById("highlight_color_pref").value = highlight_color;
  }
  if (panel_shown)
    document.getElementById(panel_shown+"_panel").style.display = "block";
}
toggle_panel = TogglePanel;

function HidePrefs(event) {
  if ((event.which || event.keyCode) == 27) {
    query.focus();
    TogglePanel("prefs"); // this function is called only when it's shown
  }
}
hide_prefs = HidePrefs;

function SetShowManuals(inp) {
  if (inp.selectedIndex != show_manuals) {
    show_manuals = inp.selectedIndex;
    SetCookie("PLT_ManualSettings", show_manuals+(show_manual_titles?10:0));
    UpdateResults();
  }
}
set_show_manuals = SetShowManuals;

function SetPreQuery(inp) {
  if (inp.value != pre_query) {
    pre_query = inp.value;
    SetCookie("PLT_PreQuery", pre_query);
    PreFilter();
    DoSearch();
  }
}
set_pre_query = SetPreQuery;


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

window.onload = InitializeSearch;

})();
