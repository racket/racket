/* For the Racket manual style */

AddOnLoad(function() {
    /* Look for header elements that have x-source-module and x-part tag.
       For those elements, add a hidden element that explains how to
       link to the section, and set the element's onclick() to display
       the explanation. */
    var tag_names = ["h1", "h2", "h3", "h4", "h5"];
    for (var j = 0; j < tag_names.length; j++) {
        elems = document.getElementsByTagName(tag_names[j]);
        for (var i = 0; i < elems.length; i++) {
            var elem = elems.item(i);
            AddPartTitleOnClick(elem);
        }
    }
})

function AddPartTitleOnClick(elem) {
    var mod_path = elem.getAttribute("x-source-module");
    var tag = elem.getAttribute("x-part-tag");
    if (mod_path && tag) {
        var info = document.createElement("div");
        info.className = "RPartExplain";

        /* The "top" tag refers to a whole document: */
        var is_top = (tag == "\"top\"");
        info.appendChild(document.createTextNode("Link to this "
                                                 + (is_top ? "document" : "section")
                                                 + " with "));

        /* Break `secref` into two lines if the module path and tag
           are long enough: */
        var is_long = (is_top ? false : (mod_path.length + tag.length > 60));

        var line1 = document.createElement("div");
        var line2 = (is_long ? document.createElement("div") : line1);

        function add(dest, str, cn) {
            var s = document.createElement("span");
            s.className = cn;
            s.style.whiteSpace = "nowrap";
            s.appendChild(document.createTextNode(str));
            dest.appendChild(s);
        }
        /* Construct a `secref` call with suitable syntax coloring: */
        add(line1, "\xA0@", "RktRdr");
        add(line1, (is_top ? "other-doc" : "secref"), "RktSym");
        add(line1, "[", "RktPn");
        if (!is_top)
            add(line1, tag, "RktVal");
        if (is_long) {
            /* indent second line: */
            add(line2, "\xA0\xA0\xA0\xA0\xA0\xA0\xA0\xA0", "RktPn");
        }
        if (!is_top)
            add(line2, " #:doc ", "RktPn");
        add(line2, "'", "RktVal");
        add(line2, mod_path, "RktVal");
        add(line2, "]", "RktPn");

        info.appendChild(line1);
        if (is_long)
            info.appendChild(line2);

        info.style.display = "none";

        /* Add the new element afterthe header: */
        var n = elem.nextSibling;
        if (n)
            elem.parentNode.insertBefore(info, n);
        else
            elem.parentNode.appendChild(info);

        /* Clicking the header shows the explanation element: */
        elem.onclick = function () {
            if (info.style.display == "none")
                info.style.display = "block";
            else
                info.style.display = "none";
        }
    }
}
