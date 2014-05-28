
AddOnLoad(function () {
    /* Lift figure targets to the start of the figure's blockquote,
       so that clicking on a target reference shows the figure
       content, instead of scrolling the figure caption to the
       top of the page. */
    var targets = document.getElementsByTagName("a");
    for (var i = 0; i < targets.length; i++) {
        var a = targets[i];
        var n = a.attributes["x-target-lift"];
        if (n) {
            var s = n.value;
            var p = a.parentNode;
            while (p && (p.className != s)) {
                p = p.parentNode;
            }
            if (p) {
                var cs = p.children;
                a.parentNode.removeChild(a);
                if (cs.length > 0)
                    p.insertBefore(a, cs[0]);
                else
                    p.appendChild(a);
            }
        }
    }
});
