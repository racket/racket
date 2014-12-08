
AddOnLoad(function(){
    var u = GetCookie("PLT_Root."+racket_root_version, null);
    if (u) {
        var info = document.getElementById("rootPathInfo");
        info.style.display = "block";
    }
})

function GoToRootPath() {
    return GotoPLTRoot(racket_root_version, "index.html");
}

function DisableRootPath() {
    SetCookie("PLT_Root."+racket_root_version, "");

    var info = document.getElementById("rootPathInfo");
    info.style.display = "none";
    
    return false;
}
