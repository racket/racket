onload = function() {
    var elems = $(".codesnip");

    console.log("in slideshow")

    var current = 0;
    var last = elems.length;
    var cur_elem = $("#codesnip"+current);

    var question = $(".question_button")
    window.question = question;
    function prev() {
        console.log("in prev");
        console.log(question);
        if (current === 0) return;
        current -= 1;
        cur_elem.removeClass('active')
        var next_elem = $("#codesnip"+current);
        next_elem.addClass('active')
        cur_elem = next_elem;
        question.attr("gumby-trigger","#modal"+current)
    }

    function next() {

        if (current+1 === last) return;
        current += 1;
        console.log("in next " + current)
        
        cur_elem.removeClass('active')
        var next_elem = $("#codesnip"+current);
        next_elem.addClass('active')
        cur_elem = next_elem;
        question.attr("gumby-trigger","#modal"+current)
    }

    $(".prev_toggle").on("click",prev);
    $(".next_toggle").on("click",next);
};
