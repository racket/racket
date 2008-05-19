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

function GotoPLTRoot() {
  var u = GetCookie("PLT_Root");
  if (u == null) return true; // no cookie: use plain up link
  location = u;
  return false;
}

function SetPLTRoot() {
  SetCookie("PLT_Root", location);
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
