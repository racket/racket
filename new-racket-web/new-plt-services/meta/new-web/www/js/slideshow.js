onload = function() {
    var current = 0;
    var last = $(".codesnip").length - 1;
    function clamp(lo,n,hi) {
        return Math.max(lo, (Math.min(n, hi)));
    }

    function move(inc) {
        return function() {
            move_to(clamp(0,current + inc,last));
        };
    }

    function move_to(n) {
        $("#codesnip"+current).removeClass('active')
        $("#codesnip"+n).addClass('active');
        current = n;
    }

    $("#question_button").removeClass('hide');
    $("#question_button").on("gumby.onTrigger",
                             function(e) {$("#code-modal"+current).addClass('active');});
    $(".prev_toggle").on("click",move(-1));
    $(".next_toggle").on("click",move(+1));
};
