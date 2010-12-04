var ws;

$(document).ready(function() {
    if (!window.console) window.console = {};
    if (!window.console.log) window.console.log = function() {};

    ws = new WebSocket("ws://localhost:8080/");

    ws.onopen = function() {
        console.log("websocket connected");
        ws.onmessage = function(event) {
            showCommand(eval("\"" + event.data + "\""));
        };

        $(window).bind('beforeunload', function() {
            ws.close();
        });
    };

    ws.onclose = function() {
        console.log("websocket disconnected");
    };

    $("#commandForm").bind("submit", function(e) {
        e.preventDefault();
        e.stopPropagation();

        newCommand($(this));
        return false;
    });

    $("#commandForm").bind("keypress", function(e) {
        if (e.keyCode == 13) {
            e.preventDefault();
            e.stopPropagation();

            newCommand($(this));
        }
    });

    $("#commandInput").select();
});

function newCommand(form) {
    var submit = form.find("#commandSubmit");
    var text = form.find("#commandInput");
    submit.disable();
    ws.send(text.val());
    text.val("").select();
    submit.enable();
}

function showCommand(message) {
    var node = $("<p>" + message + "</p>");
    node.hide();
    $("#inbox").append(node);
    node.show();
}

jQuery.fn.enable = function(opt_enable) {
    if (arguments.length && !opt_enable) {
        this.attr("disabled", "disabled");
    } else {
        this.removeAttr("disabled");
    }
    return this;
};

jQuery.fn.disable = function() {
    this.enable(false);
    return this;
};
