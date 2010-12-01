function erase(array, element, pred) {
    return array.filter(function(el) {
        return vertex_equal(el, element);
    });
}

function vertex_equal(a, b) {
    return a[0] === b[0] && a[1] === b[1];
}

function contains(array, el, pred) {
    for (var i = 0; i < array.length; ++i) {
        if (pred(array[i], el)) {
            return true;
        }
    }
    return false;
}

function diff_array(a, b) {
    return a.filter(function(el) {
        return !contains(b, el, vertex_equal);
    });
}

function vertex_diff(current_set, new_set) {
    return {'+': diff_array(new_set, current_set),
            '-': diff_array(current_set, new_set)};
}

function char_to_int(c) {
    var c_code = c.toLowerCase().charCodeAt(0);
    var a_code = 'a'.charCodeAt(0);
    var i_code = 'i'.charCodeAt(0);
    if (c_code >= i_code) {
        return (c_code-a_code);
    } else {
        return (c_code-a_code)+1;
    }
}

function int_to_char(i) {
    var a_code = 'a'.charCodeAt(0);
    if (i >= 9) {
        return String.fromCharCode(i+a_code);
    } else {
        return String.fromCharCode(i+a_code-1);
    }
}

function vertex_to_coords(vertex) {
    var row = char_to_int(vertex[0]);
    var col = parseInt(vertex.slice(1));
    return [row, col];
}

function gtp_request(req, cb, t) {
    new Request.JSON({url: '../go', onSuccess: function(response_json) {
        cb.call(t, response_json);
    }}).get(req);
}

var Goban = new Class({
    Implements: Events,

    initialize: function(el, size) {
        this.el = el;
        this.size = size;
        this.stones = {'w': [], 'b': []};

        // register click callback
        set_stone = (function (ev) {
            ev.stopPropagation();
            ev.preventDefault();
            
            var id = $(ev.target).id;
            if (id.split('-')[0] == 'cell') {
                var row = id.split('-')[1];
                var col = id.split('-')[2];

                // call user provided callback with row and col
                this.fireEvent('click', [parseInt(row), parseInt(col)]);
            }
        }).bind(this);

        el.addEvent('mousedown', (function(ev) {
            set_stone(ev);
        }).bind(this));

        for (var row = 0; row < size; ++row) {
            var row_element = new Element('tr', {'id': 'row-'+row});
            row_element.inject(el);
            for (var col = 0; col < size; ++col) {
                var image;
                if (row === 0 && col === 0) {
                    image = 'top-left';
                } else if (row === 0 && col === size-1) {
                    image = 'top-right';
                } else if (row === size-1 && col === 0) {
                    image = 'bottom-left';
                } else if (row === size-1 && col === size-1) {
                    image = 'bottom-right';
                } else if (row === 0) {
                    image = 'top';
                } else if (row === size-1) {
                    image = 'bottom';
                } else if (col === 0) {
                    image = 'left';
                } else if (col === size-1) {
                    image = 'right';
                } else {
                    image = 'center';
                }

                td = new Element('td');
                td.setProperty('class', image);

                div = new Element('div', {'id': 'cell-'+row+'-'+col, 'class': 'transparent'});
                td.grab(div).inject(row_element);
            }
        }
    },

    put_stone: function(color, row, col) {
        var td = $('cell-'+row+'-'+col);
        td.setProperty('class', color);
    },

    remove_stone: function(row, col) {
        console.log("remove ", row, col);
        var td = $('cell-'+row+'-'+col);
        td.setProperty('class', 'transparent');
    },

    get_stone: function(row, col) {
        var td = $('cell-'+row+'-'+col);
        if (td.getChildren().length > 0) {
            // lol
            return td.getProperty('src').split('/')[1].split('.')[0];
        } else {
            return false;
        }
    },

    clear: function() {
        console.log("Cleared goban");
        for (var row = 0; row < this.size; ++row) {
            for (var col = 0; col < this.size; ++col) {
                this.remove_stone(row, col);
            }
        }
    },

    update: function(color) {
        var json_request = new Request.JSON({
            url: '../go',
            onSuccess: (function(response_json) {
                // console.log("response ", response_json);
                if (response_json.success && response_json.data[0] !== '') {
                    var new_coords = response_json.data.map(function(vertex) {
                        return vertex_to_coords(vertex);
                    });
                    // What stones were removed or added?
                    var diffobj = vertex_diff(this.stones[color], new_coords);
                    console.log(diffobj);
                    // Update board accordingly
                    diffobj['+'].each(function(added) {
                        this.stones[color].push(added);
                        this.put_stone(color, added[0]-1, added[1]-1);
                    }, this);
                    diffobj['-'].each(function(removed) {
                        console.log(removed);
                        erase(this.stones[color], removed, vertex_equal);
                        var coords = [removed[0]-1, removed[1]-1];
                        console.log(color, coords[0], coords[1]);
                        this.remove_stone(color,
                                          coords[0],
                                          coords[1]);
                    }, this);
                } else {
                    console.log("Error while listing stones");
                }
            }).bind(this),
            onFailure: function(xhr) {
                console.log(xhr.status, xhr.statusText, xhr.responseText);
            },
            onException: function() {
                console.log(exception);                
            }
        });
        json_request.get({
            'command-name': 'list_stones',
            'args': color
        });
    }
});

var Game = new Class({
    Implements: [Events, Chain],

    initialize: function(goban, difficulty, komi, handicap, human_color) {
        this.goban = goban;
        this.size = this.goban.size
        this.difficulty = difficulty;
        this.komi = komi;
        this.handicap = handicap;
        this.human_color = human_color;

        // Black always starts
        this.current_player = 'b';

        // this.init_handicap(handicap); TODO

        gtp_request({'command-name': 'boardsize', 'args': this.size}, function (json) {
            gtp_request({'command-name': 'clear_board'}, function (json) {
                if (this.human_color === 'w') {
                    this.computer_turn();
                }
            }, this);
        }, this);

        goban.addEvent('click', (function (row, col) {
            // To gnugo coords
            ++row;
            ++col;

            if (this.human_color == this.current_player) {
                var json_request = new Request.JSON({
                    url: '../go',
                    onSuccess: (function(response_json) {
                        if (response_json.success) {
                            this.fireEvent('finishTurn', [row, col]);
                        } else {
                            this.fireEvent('invalidTurn', [row, col]);
                        }
                    }).bind(this),
                    onFailure: function(xhr) {
                        console.log(xhr.status, xhr.statusText, xhr.responseText);
                    },
                    onException: function() {
                        alert("ex");
                    }
                });
                var row_char = int_to_char(row);
                json_request.get({
                    'command-name': 'play',
                    'args': this.current_player + ' ' + row_char + '' + col
                });
            }
        }).bind(this));
    },

    next_player: function() {
        this.current_player = this.current_player == 'w' ? 'b': 'w';
    },

    other_player: function() {
        return this.current_player == 'w' ? 'b': 'w';
    },

    computer_turn: function() {
        gtp_request({'command-name': 'genmove', 'args': this.current_player}, function (json) {
            var coord = vertex_to_coords(json.data[0])
            this.goban.update('b');
            this.goban.update('w');
            this.next_player();
        }, this);
    }
});

function init() {
    var goban = new Goban($('goban'), 9);
    var game = new Game(goban, 0, 4.5, 0, 'w');
    game.addEvent('finishTurn', function(row, col) {
        game.next_player();
        game.computer_turn();
    });
    game.addEvent('invalidTurn', function(row, col) {
        console.log("Invalid turn: %o", [row, col]);
    }, this);
}

window.addEvent('domready', init);
