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
  if (!url.href.search(/\?([^#]*)(?:#|$)/)) return false;
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

// Interactions ---------------------------------------------------------------

function DoSearchKey(event, field, ver, top_path) {
  var val = field.value;
  if (event && event.keyCode == 13) {
    var u = GetCookie("PLT_Root."+ver, null);
    if (u == null) u = top_path; // default: go to the top path
    location = u + "search/index.html" + "?q=" + escape(val);
    return false;
  }
  return true;
}

function TocviewToggle(glyph,id) {
  var s = document.getElementById(id).style;
  var expand = s.display == "none";
  s.display = expand ? "block" : "none";
  glyph.innerHTML = expand ? "&#9660;" : "&#9658;";
}

// `noscript' is problematic in some browsers (always renders as a
// block), use this hack instead (does not always work!)
// document.write("<style>mynoscript { display:none; }</style>");
