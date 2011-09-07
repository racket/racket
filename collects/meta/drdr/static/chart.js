var path = ""
var data = null;
var sub_times = [];
var overall_times = [];
var chart_data = [];
var options = { selection: { mode: "xy" },
                legend: { backgroundOpacity: 0, position: "sw", show: true },
                xaxes: [{label: 'push'}],
                yaxes: [{}, {position: "right"}],
                grid: { clickable: true, hoverable : true }
              };
var placeholder = $("#_chart");
var cur_options = options;
var previousPoint = null;

function showTooltip(x, y, contents) {
    $('<div id="tooltip">' + contents + '</div>').css( {
        position: 'absolute',
        display: 'none',
        top: y + 5,
        left: x + 5,
        border: '1px solid #fdd',
        padding: '2px',
        'background-color': '#fee',
        opacity: 0.80
    }).appendTo("body").fadeIn(200);
}

function makeTooltip(item,path) {
    var x = item.datapoint[0];
    var y = item.datapoint[1].toFixed(2);
    showTooltip(item.pageX, item.pageY,
                item.series.label + ' at <a href="http://drdr.racket-lang.org/'
                + x + path + '">push ' + x + "</a>: "
                + y + " ms");
}
placeholder.bind("plotselected", handle_selection);

// is the tooltip shown b/c of a click?
var tooltip_clicked = false;

function remove_tooltip() {
    tooltip_clicked = false;
    $("#tooltip").remove();
}

function hover(event,pos,item) {
    if (tooltip_clicked) return;
    if (item) {
        // don't re-show the same tool-tip that's already shown
        if (previousPoint != item.dataIndex) {
            previousPoint = item.dataIndex;
            remove_tooltip();
            makeTooltip(item,path);
        }
    }
    else {
        remove_tooltip();
        previousPoint = null;
    }
}

function click(e,pos,item) {
    if (tooltip_clicked) {
        remove_tooltip();
        return;
    }
    if (!item) return;
    tooltip_clicked = true;
    // if we've already got the tooltip, just keep it around
    if (previousPoint != item.dataIndex) {
        $("#tooltip").remove();
        makeTooltip(item,path);
    }
}
placeholder.bind("plothover", hover);
placeholder.bind("plotclick", click);

function load_data(d) {
    chart_data = [];
    overall_times = [];
    sub_times = [];
    pdata = []
    reset_chart();
    data = d;

    pdata = data && JSON.parse(data);

    var max_overall = 0;
    var max_sub = 0;

    // build the timing data arrays
    for (var i = 0; i < pdata.length; i++) {
        overall_times.push([pdata[i][0], pdata[i][1]]);
        max_overall = Math.max(max_overall, pdata[i][1]);
        if (pdata[i][2].length != 0) {
            for (var j = 0; j < pdata[i][2].length; j++) {
                sub_times[j] = sub_times[j] || [];
                sub_times[j].push([pdata[i][0],pdata[i][2][j][0]]);
                max_sub = Math.max(max_sub, pdata[i][2][j][0]);
            }
        }
    };

    // is there a significant difference between the overall times
    // and the internal timings?

    var ya = 1;
    if (max_overall > (5 * max_sub)) { ya = 2; }

    // put the data into the chart format
    chart_data.push({data: overall_times, label: "Overall Time"});
    for(var i = 0; i < sub_times.length; i++) {
        chart_data.push({data: sub_times[i], label: "Timer "+ (i+1), points: { show: true }, yaxis: ya});
    }
}

function get_data(_path) {
    if (_path[0] != '/')
        _path = '/' + _path;
    path = _path;
    console.log("_path",_path);
    console.log("path",path);
    $.ajax({url: 'http://drdr.racket-lang.org/json/timing'+path,
            beforeSend: function(xhr) {
                xhr.overrideMimeType( 'text/plain; charset=x-user-defined' );
            },
            success: function(d) { load_data(d); show(); }});
}


function show() { $.plot(placeholder, chart_data, cur_options); }

function handle_selection(event, ranges) {
    cur_options = $.extend(true, {}, cur_options, {
        yaxes: [ { min: ranges.yaxis.from, max: ranges.yaxis.to },cur_options.yaxes[1]],
        xaxis: { min: ranges.xaxis.from, max: ranges.xaxis.to }});
    show();
}

function set_legend(new_val) {
  cur_options = $.extend(true,{},cur_options, {legend: {show: new_val}});
  show();
  if (new_val)
      $("#setlegend").text("Hide Legend")
  else
      $("#setlegend").text("Show Legend")
}

function reset_chart() { cur_options = options; show(); }
