function moving_avg(arr, i, _acc, _m) {
    var acc = _acc || function(j) { return arr[j]; };
    var m = _m || 5;
    var top = Math.min(i + m, arr.length);
    var bot = Math.max(0, i - m);
    var n = top - bot;
    var sum = 0;
    for (var i = bot; i < top; i++)
        sum += acc(i);
    return sum/n;
}

var data = null;
var sub_times = [];
var overall_times = [];
//var overall_avg = [];
var chart_data = [];
var options = { selection: { mode: "xy" },
                legend: { backgroundOpacity: 0, position: "sw", show: true },
                xaxes: [{label: 'push'}],
                yaxes: [{}, {position: "right"}],
                grid: { hoverable : true }
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

placeholder.bind("plotselected", handle_selection);

placeholder.bind("plothover", function (event, pos, item) {
    if (item) {
        if (previousPoint != item.dataIndex) {
            previousPoint = item.dataIndex;

            $("#tooltip").remove();
            var x = item.datapoint[0],
            y = item.datapoint[1].toFixed(2);

            showTooltip(item.pageX, item.pageY,
                        item.series.label + " at push " + x + ": "
                        + y + " ms");
        }
    }
    else {
        $("#tooltip").remove();
        previousPoint = null;
    }
});

function load_data(d) {
    chart_data = [];
    overall_times = [];
    //overall_avg = [];
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
        // overall_avg.push([pdata[i][0],
        //                   moving_avg(pdata, i,
        //                              function(j) { return pdata[j][1]; })]);
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
    //chart_data.push({data: overall_avg, label: "Overall Moving Avg"});
    for(var i = 0; i < sub_times.length; i++) {
        chart_data.push({data: sub_times[i], label: "Timer "+ (i+1), points: { show: true }, yaxis: ya});
    }
}

function get_data(url) {
    //console.log("URL:", url);
    $.ajax({url: url,
            beforeSend: function(xhr) {
                xhr.overrideMimeType( 'text/plain; charset=x-user-defined' );
            },
            success: function(d) { load_data(d); show(); }});
}


function show() { $.plot(placeholder, chart_data, cur_options); }

function handle_selection(event, ranges) {
    cur_options = $.extend(true, {}, cur_options, {
        yaxis: { min: ranges.yaxis.from, max: ranges.yaxis.to },
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
