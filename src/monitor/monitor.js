/* $Id: monitor.js 919 2010-03-01 19:33:31Z tolpin $ */

/* client code for monitor */

/* Log object:
 *  - constructor gets update url and log name 
 *  - initializiation is delayed until names and types are retrieved
 *  - re-created to reset
 *  - updated by log fragments
 *  - remembers update url, log name and current position
 * 
 * provides:
 *  - column map from names to types
 *  - group map from types to arrays of names
 *  - array abstraction, each element is a map from names to values
 */

var URL = '/tail';
var UPDATE_INTERVAL = 1; /* seconds */

function Log(fname) {
    this.fname = fname;
    this.pos = 0;
    this.data = undefined;      /* log data */
    this.colnames = undefined;  /* column names */
    this.coltypes = undefined;  /* column types */
    this.colgroups = undefined; /* hashtable of column groups by type */
}

/* callbacks: initialize and draw */
Log.prototype.initialize = function () {};
Log.prototype.moredata = function () {};
Log.prototype.newstage = function (stage) {};
Log.prototype.draw = function () {};

Log.prototype.update = function () {
    var that = this; /* for callback */
    var r = new Ajax.Request(URL, {
            method: 'get',
            parameters: {fname: this.fname, pos: this.pos},
            onSuccess: function (response) {
                var tail = response.responseText.evalJSON(true);
                if(tail.lines.length!=0) { /* log updating is non-blocking, may return 0 lines */
                    if(that.pos==0) {
                        that.data = [];
                        that.colnames = tail.lines[0].names;
                        that.coltypes = tail.lines[1].types;
                        that.colgroups = {};
                        for(var i=0; i!=that.coltypes.length; ++i) {
                            var coltype = that.coltypes[i];
                            if(!that.colgroups[coltype])
                                that.colgroups[coltype] = [];
                            that.colgroups[coltype].push(i);
                        }
                        /* initialize the log */
                        that.initialize();
                    } else {
                        /* add lines to the log */
                        tail.lines.each(function (line) {
                                if(line.linetype=='stage') {
                                    that.newstage(line.stage);
                                } else {
                                    that.data.push(line.values);
                                }
                            });
                        that.moredata();
                    }
                    that.pos = tail.pos;
                    that.draw();
                }
            },
            onFailure: function (response) {
                alert(that.fname+': invalid log');
            }
        });
};

/* for observations:
 *   series of normalized distances between
 *      - subsequent measurements
 *      - subsequent alphas
 */

function Obs(fname) {
    Log.call(this, fname);
};
Obs.prototype = new Log();
Obs.prototype.draw = function () {
    var obs = [], opt = [], eu  = [];
    if(this.data.length!=0) {
        var obscols = this.colgroups[1], optcols = this.colgroups[3], eucol = this.colgroups[4][0];

        /* normalize */
        var cols = obscols.concat(optcols, eucol);
        var lim = {};
        cols.each(function (col) {
                lim[col] = {
                    min: Number.MAX_VALUE,
                    max: -Number.MAX_VALUE
                };
            });
        this.data.each(function (entry) {
                cols.each(function (col) {
                        if(entry[col] > lim[col].max)
                            lim[col].max = entry[col];
                        if(entry[col] < lim[col].min)
                            lim[col].min = entry[col];
                    });
            });
        cols.each(function (col) {
                lim[col].range = lim[col].min!=lim[col].max ?
                      lim[col].max-lim[col].min
                    : lim[col].max!=0?
                      lim[col].max
                    : 1;
            });

        /* build series */
        var iprev = 0;
        function dist(cols, cur, prev) {
            var dist = 0;
            for(var j=0; j!=cols.length; ++j) {
                diff = (cur[cols[j]]-prev[cols[j]])/lim[cols[j]].range;
                dist += diff*diff;
            }
            dist = Math.sqrt(dist/cols.length);
            return dist;
        }
        for(var i=0; i!=this.data.length; ++i) {
            obs.push([i, dist(obscols, this.data[i], this.data[iprev])]);
            opt.push([i, dist(optcols, this.data[i], this.data[iprev])]);
            eu.push([i, (-lim[eucol].min+this.data[i][eucol])/lim[eucol].range]);
            iprev = i;
        }
    }
    Flotr.draw($('observations-plot'),
               [ { data: obs, label: '|Δλ|' },
                 { data: opt, label: '|Δα|' },
                 { data: eu, label:  this.colnames[eucol] } ]);
};


/* section table -- object Section:
 *  - generating the table from the log  (DOM manipulation)
 *  - updating axis and section
 */
function Section(log, type) {
    this.columns = log.colgroups[type];
    this.colnames = this.columns.map(function (icol) {return log.colnames[icol];});
    this.xaxis = 0;    /* index of the axis */
    this.values = this.columns.map(function (icol) {return undefined;});
    this.ranges = this.columns.map(function (icol) {return [];});
}

Section.prototype.update_ranges = function(log) {
    var that = this;
    this.ranges = this.columns.map(function(icol) {return [];});
    /* update ranges from the log */
    log.data.each(function (entry) {
        for(var i=0; i!=that.columns.length; ++i)
            that.ranges[i].push(entry[that.columns[i]]);
    });
    this.ranges = this.ranges.map(function (range) {
        return range.sort(function(a,b) {return a-b;}).uniq();
    });
    for(var i=0; i!=this.columns.length; ++i)
        if(this.values[i]==undefined)
            this.values[i] = this.ranges[i][0];
};

/* update section from user choices */
Section.prototype.update_choices = function () {
    this.xaxis = $RF('opinions-section-xaxis');
    for(var i=0; i!=this.columns.length; ++i)
        this.values[i] = $F('opinions-section-value-'+this.colnames[i]);
};

Section.prototype.draw = function() {
    var that = this;
    var table = $('opinions-section-table');
    for(var i=0; i!=3; ++i)
        while(table.rows[i].cells.length)
            table.rows[i].deleteCell(0);
    var row_name = table.rows[0], row_axis = table.rows[1], row_value = table.rows[2];
    row_name.innerHTML = '<th>&nbsp;</th>'
        +this.colnames.map(function (name) {
            return '<th class="colname">'+name+'</th>';
        }).join('');
    row_axis.innerHTML = '<th>axis</th>'
        +$R(0, this.columns.length-1).map(function (icolumn) {
            return '<td><input type="radio" name="opinions-section-xaxis"'
                + ' id="opinions-section-xaxis" value="'+icolumn +'" '
                + (icolumn==that.xaxis?'checked="checked"':'')
                + ' onchange="monitor.opinions_disable(\''+that.colnames[icolumn]+'\');'
                + ' monitor.opinions_changed()"/></td>';
        }).join('');
    row_value.innerHTML = '<th>value</th>'
        +$R(0, this.columns.length-1).map(function (icolumn) {
            var ctlname = 'opinions-section-value-'+that.colnames[icolumn];
            return '<td><select class="opinions-section-range" id="'+ctlname+'" name="'+ctlname
                + (icolumn==that.xaxis?'disabled="disabled"':'')
                + '" onchange="monitor.opinions_changed()"/>'
                + that.ranges[icolumn].map(function (value) {
                    return '<option value="'+value+'">'+value+'</option>';
                  }).join('');
                + '</select></td>';
        }).join('');

};

/* for opinions:
 *   series of EU and its components for given X axis and section
 */

function Opn(fname) {
    Log.call(this, fname);
    this.section = undefined; 
};
Opn.prototype = new Log();
Opn.prototype.initialize = function () {
    this.section = new Section(this, 0);
};
Opn.prototype.moredata = function () {
    this.section.update_ranges(this);
    this.section.draw();
};
Opn.prototype.newstage = function (stage) {
    this.data = [];
};
Opn.prototype.draw = function () {
    var that = this;
    var xcolumn = this.section.columns[that.section.xaxis];
    var ycolumn = this.colgroups[2][0]; 
    var series = { data: [], label: that.colnames[ycolumn] };


    this.data.each(function (entry) {
        for(var i=0; i!=that.section.columns.length; ++i) {
            var icolumn = that.section.columns[i];
            if(xcolumn!=icolumn && entry[icolumn]!=that.section.values[i])
                return;
        }
        series.data.push([entry[xcolumn], entry[ycolumn]]);
    });
                
    Flotr.draw($('opinions-plot'), [series]);
};

/* HTML hooks:
 *  - change log name
 *  - update
 *  - rewind
 *  - recurring update on timer 
 */

function Monitor () {
    var obs = undefined, opn = undefined;
    var watchdog = undefined;

    this.reset = function () { /* on log name change or reset */
        var prefix = $F('logpfx');
        obs = new Obs(prefix+'.obs');
        opn = new Opn(prefix+'.opn');
        monitor.update();
    };
    this.update = function () { /* on update */
        obs.update(); opn.update();
    };
    this.opinions_changed = function () {
        opn.section.update_choices();
        opn.draw();
    };
    this.opinions_disable = function(colname) { /* disable range for the xaxis */
        $$('.opinions-section-range').each(function (el) { el.disabled = false; });
        $('opinions-section-value-'+colname).disabled = true;
    };

    var control_ids = ['logpfx', 'reset', 'update'];
    this.animate = function () { /* recurring update on timer */
        control_ids.each(function (id) {$(id).disabled = true; });
        if(watchdog)
            watchdog.registerCallback();
        else
            watchdog = new PeriodicalExecuter(monitor.update, UPDATE_INTERVAL);
    };
    this.freeze = function () {
        watchdog.stop();
        control_ids.each(function (id) {$(id).disabled = false; });
    };
}
var monitor = new Monitor();

/* hacks */

/**
* Returns the value of the selected radio button in the radio group, null if
* none are selected, and false if the button group doesn't exist
*
* @param {radio Object} or {radio id} el
* OR
* @param {form Object} or {form id} el
* @param {radio group name} radioGroup
*/
function $RF(el, radioGroup) {
    if($(el).type && $(el).type.toLowerCase() == 'radio') {
        var radioGroup = $(el).name;
        var el = $(el).form;
    } else if ($(el).tagName.toLowerCase() != 'form') {
        return false;
    }

    var checked = $(el).getInputs('radio', radioGroup).find(
        function(re) {return re.checked;}
    );
    return (checked) ? $F(checked) : null;
}