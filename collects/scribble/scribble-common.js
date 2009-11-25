// Common functionality for PLT documentation pages

// Cookies --------------------------------------------------------------------

function GetCookie(key, def) {
  if (document.cookie.length <= 0) return def;
  var i, cookiestrs = document.cookie.split(/; */);
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
  document.cookie =
    key + "=" + escape(val) + "; expires="+ d.toGMTString() + "; path=/";
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

// URL Parameters -------------------------------------------------------------

// In the following functions, the `name' argument is assumed to be simple in
// that it doesn't contain anything that isn't plain text in a regexp.  (This
// is because JS doesn't have a `regexp-quote', easy to hack but not needed
// here).  Also, the output value from the Get functions and the input value to
// the Set functions is decoded/encoded.  Note that `SetArgInURL' mutates the
// string in the url object.

function GetArgFromString(str, name) {
  var rx = new RegExp("(?:^|[;&])"+name+"=([^&;]*)(?:[;&]|$)");
  return rx.test(str) && unescape(RegExp.$1);
}

function SetArgInString(str, name, val) {
  val = escape(val);
  if (str.length == 0) return name + "=" + val;
  var rx = new RegExp("^((?:|.*[;&])"+name+"=)(?:[^&;]*)([;&].*|)$");
  if (rx.test(str)) return RegExp.$1 + val + RegExp.$2;
  else return name + "=" + val + "&" + str;
}

function GetArgFromURL(url, name) {
  if (url.href.search(/\?([^#]*)(?:#|$)/) < 0) return false;
  return GetArgFromString(RegExp.$1, name);
}

function SetArgInURL(url, name, val) { // note: mutates the string
  url.href.search(/^([^?#]*)(?:\?([^#]*))?(#.*)?$/);
  url.href = RegExp.$1 + "?" + SetArgInString(RegExp.$2,name,val) + RegExp.$3;
}

// Utilities ------------------------------------------------------------------

normalize_rxs = [/\/\/+/g, /\/\.(\/|$)/, /\/[^\/]*\/\.\.(\/|$)/];
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
    var args = "";
    if (u == null) u = top_path; // default: go to the top path
    u += "search/index.html";
    args = SetArgInString(args, "q", val);
    if (cur_plt_lang) args = SetArgInString(args, "lang", cur_plt_lang);
    location = u + "?" + args;
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

var cur_plt_lang = GetArgFromURL(location,"lang");

function PropagateLangInLink(a) {
  // the attribute's value doesn't matter
  if (cur_plt_lang
      && a.attributes["pltdoc"] && a.attributes["pltdoc"].value != ""
      && !GetArgFromURL(a,"lang"))
    SetArgInURL(a, "lang", cur_plt_lang);
}

AddOnLoad(function(){
    if (!cur_plt_lang) return;
    var indicator = document.getElementById("langindicator");
    if (indicator) {
      indicator.innerHTML = cur_plt_lang;
      indicator.style.display = "block";
    }
    var links = document.getElementsByTagName("a");
    for (var i=0; i<links.length; i++) PropagateLangInLink(links[i]);
  });
