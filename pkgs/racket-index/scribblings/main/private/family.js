
AddOnLoad(function(){
  var es = document.getElementsByClassName("famlink");
  for (var i=0; i < es.length; i++) {
    e = es[i];
    var sp = new URL(location).searchParams
    if (sp.has("qfrom")) {
      var new_u = new URL(sp.get("qfrom"))
      new_u.searchParams.delete("fam");
      new_u.searchParams.delete("famroot");
      for (const [key, val] of (new URL(e.href).searchParams)) {
        if (key[0] == "q") continue;
        if (new_u.searchParams.has(key)) continue;
        new_u.searchParams.append(key, val)
      }
      e.href = new_u;
    }
  }
});
