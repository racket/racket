// Common functionality for PLT documentation pages

// Page Parameters ------------------------------------------------------------

var page_query_string =
  (location.href.search(/\?([^#]+)(?:#|$)/) >= 0) && RegExp.$1;

var page_args =
  ((function(){
      if (!page_query_string) return [];
      var args = page_query_string.split(/[&;]/);
      for (var i=0; i<args.length; i++) {
        var a = args[i];
        var p = a.indexOf('=');
        if (p >= 0) args[i] = [a.substring(0,p), a.substring(p+1)];
        else args[i] = [a, false];
      }
      return args;
    })());

function GetPageArg(key, def) {
  for (var i=0; i<page_args.length; i++)
    if (page_args[i][0] == key) return unescape(page_args[i][1]);
  return def;
}

function MergePageArgsIntoLink(a) {
  if (page_args.length == 0 ||
      (!a.attributes["data-pltdoc"]) || (a.attributes["data-pltdoc"].value == ""))
    return;
  a.href.search(/^([^?#]*)(?:\?([^#]*))?(#.*)?$/);
  if (RegExp.$2.length == 0) {
    a.href = RegExp.$1 + "?" + page_query_string + RegExp.$3;
  } else {
    // need to merge here, precedence to arguments that exist in `a'
    var i, j;
    var prefix = RegExp.$1, str = RegExp.$2, suffix = RegExp.$3;
    var args = str.split(/[&;]/);
    for (i=0; i<args.length; i++) {
      j = args[i].indexOf('=');
      if (j) args[i] = args[i].substring(0,j);
    }
    var additions = "";
    for (i=0; i<page_args.length; i++) {
      var exists = false;
      for (j=0; j<args.length; j++)
        if (args[j] == page_args[i][0]) { exists = true; break; }
      if (!exists) str += "&" + page_args[i][0] + "=" + page_args[i][1];
    }
    a.href = prefix + "?" + str + suffix;
  }
}

// Cookies --------------------------------------------------------------------

function GetCookie(key, def) {
  var i, cookiestrs;
  try {
    if (document.cookie.length <= 0) return def;
    cookiestrs = document.cookie.split(/; */);
  } catch (e) { return def; }
  for (i = 0; i < cookiestrs.length; i++) {
    var cur = cookiestrs[i];
    var eql = cur.indexOf('=');
    if (eql >= 0 && cur.substring(0,eql) == key)
      return unescape(cur.substring(eql+1));
  }
  return def;
}

function SetCookie(key, val) {
  var d = new Date();
  d.setTime(d.getTime()+(365*24*60*60*1000));
  try {
    document.cookie =
      key + "=" + escape(val) + "; expires="+ d.toGMTString() + "; path=/";
  } catch (e) {}
}

// note that this always stores a directory name, ending with a "/"
function SetPLTRoot(ver, relative) {
  var root = location.protocol + "//" + location.host
           + NormalizePath(location.pathname.replace(/[^\/]*$/, relative));
  SetCookie("PLT_Root."+ver, root);
}

// adding index.html works because of the above
function GotoPLTRoot(ver, relative) {
  var u = GetCookie("PLT_Root."+ver, null);
  if (u == null) return true; // no cookie: use plain up link
  // the relative path is optional, default goes to the toplevel start page
  if (!relative) relative = "index.html";
  location = u + relative;
  return false;
}

// Utilities ------------------------------------------------------------------

var normalize_rxs = [/\/\/+/g, /\/\.(\/|$)/, /\/[^\/]*\/\.\.(\/|$)/];
function NormalizePath(path) {
  var tmp, i;
  for (i = 0; i < normalize_rxs.length; i++)
    while ((tmp = path.replace(normalize_rxs[i], "/")) != path) path = tmp;
  return path;
}

// `noscript' is problematic in some browsers (always renders as a
// block), use this hack instead (does not always work!)
// document.write("<style>mynoscript { display:none; }</style>");

// Interactions ---------------------------------------------------------------

function DoSearchKey(event, field, ver, top_path) {
  var val = field.value;
  if (event && event.keyCode == 13) {
    var u = GetCookie("PLT_Root."+ver, null);
    if (u == null) u = top_path; // default: go to the top path
    u += "search/index.html?q=" + escape(val);
    if (page_query_string) u += "&" + page_query_string;
    location = u;
    return false;
  }
  return true;
}

function TocviewToggle(glyph, id) {
  var s = document.getElementById(id).style;
  var expand = s.display == "none";
  s.display = expand ? "block" : "none";
  glyph.innerHTML = expand ? "&#9660;" : "&#9658;";
}

// Page Init ------------------------------------------------------------------

// Note: could make a function that inspects and uses window.onload to chain to
// a previous one, but this file needs to be required first anyway, since it
// contains utilities for all other files.
var on_load_funcs = [];
function AddOnLoad(fun) { on_load_funcs.push(fun); }
window.onload = function() {
  for (var i=0; i<on_load_funcs.length; i++) on_load_funcs[i]();
};

AddOnLoad(function(){
    var links = document.getElementsByTagName("a");
    for (var i=0; i<links.length; i++) MergePageArgsIntoLink(links[i]);
    var label = GetPageArg("ctxtname",false);
    if (!label) return;
    var indicator = document.getElementById("contextindicator");
    if (!indicator) return;
    indicator.innerHTML = label;
    indicator.style.display = "block";
  });
