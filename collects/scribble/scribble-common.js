// Common functionality for PLT documentation pages

function GetCookie(key) {
  if (document.cookie.length <= 0) return null;
  var cookiestrs = document.cookie.split(/; */);
  for (var i in cookiestrs) {
    var cur = cookiestrs[i];
    var eql = cur.indexOf('=');
    if (eql >= 0 && cur.substring(0,eql) == key)
      return unescape(cur.substring(eql+1));
  }
  return null;
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
  var u = GetCookie("PLT_Root."+ver);
  if (u == null) return true; // no cookie: use plain up link
  // the relative path is optional, default goes to the toplevel start page
  if (!relative) relative = "index.html";
  location = u + relative;
  return false;
}

normalize_rxs = [/\/\/+/g, /\/\.(\/|$)/, /\/[^\/]*\/\.\.(\/|$)/];
function NormalizePath(path) {
  var tmp, i;
  for (i = 0; i < normalize_rxs.length; i++)
    while ((tmp = path.replace(normalize_rxs[i], "/")) != path) path = tmp;
  return path;
}

function DoSearchKey(event, field) {
  var val = field.value;
  if (event && event.keyCode == 13 && val.indexOf("...search...") < 0) {
    var u = GetCookie("PLT_Root");
    if (u == null) u = "../"; // default: go up
    u = u.replace(/[^\/\\]*$/, "") + "plt-search.html";
    location = u + "?q=" + escape(val);
  }
}

// `noscript' is problematic in some browsers (always renders as a
// block), use this hack instead
document.write("<style>mynoscript { display:none; }</style>");
