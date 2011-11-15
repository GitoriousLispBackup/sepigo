var request_url = 'go';

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

function map_vertices_to_coords(vertices) {
    // FIXXXX
    if (!vertices) {
        return [];
    }

    return vertices.map(function(vertex) {
        return vertex_to_coords(vertex);
    });
}

function gtp_request(req, cb, t) {
    new Request.JSON({url: request_url, onSuccess: function(response_json) {
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

	// Build DOM for game board
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

    update: function(color, stones) {
        stones.each(function (stone) {
            this.put_stone(color, stone[0]-1, stone[1]-1);
        }, this);
    }
});

var Game = new Class({
    Implements: [Events, Options, Chain, Model],

    options: {
        difficulty: 0,
        komi: 4.5,
        handicap: 0,
        human_color: 'b',
        current_player: 'b',
    },

    initialize: function(goban, options) {
        this.goban = goban;

        this.setOptions(options);
        this.options.size = this.goban.size

        // this.human_color = human_color;
        this.click_locked = true;

        // Black always starts
        this.options.current_player = 'b';

        if (this.options.human_color === this.options.current_player) {
            this.click_locked = false;
        }

        // this.init_handicap(handicap); TODO

        gtp_request({'command-name': 'boardsize', 'args': this.options.size}, function (json) {
            gtp_request({'command-name': 'clear_board'}, function (json) {
                console.log("cleared board");
                if (this.options.human_color === 'w') {
                    this.computer_turn();
                }
            }, this);
        }, this);

        goban.addEvent('click', (function (row, col) {
            // To gnugo coords
            ++row;
            ++col;

            console.log("click event at: ", [row, col]);

            if (this.click_locked) {
                console.log("click locked");
                return;
            }
            // Prevent user from issuing more ajax requests
            this.click_locked = true;

            var json_request = new Request.JSON({
                url: request_url,
                onSuccess: (function(response_json) {
                    // Setting stone was successful procede
                    if (response_json[0].success) {
                        var opponent_turn = 
                            vertex_to_coords(response_json[1].data[0]);
                        console.log("opponent played: ", opponent_turn);

                        this.goban.clear();
                        this.goban.update("b", map_vertices_to_coords(response_json[2].data));
                        this.goban.update("w", map_vertices_to_coords(response_json[3].data));
                    } else {
                        this.fireEvent("invalidTurn", row, col);
                    }
                    this.click_locked = false;
                }).bind(this),
                onFailure: function(xhr) {
                    console.log(xhr.status, xhr.statusText, xhr.responseText);
                },
                onException: function() {
                    alert("ex");
                }
            });
            var row_char = int_to_char(row);
            json_request.get({'command-list': JSON.encode([{
                'command-name': 'play',
                'args': this.options.current_player + ' ' + row_char + '' + col
            },
            {
                'command-name': 'genmove',
                'args': this.other_player()
            },
            {
                'command-name': 'list_stones',
                'args': this.options.current_player
            },
            {
                'command-name': 'list_stones',
                'args': this.other_player()
            }])});
        }).bind(this));
    },

    next_player: function() {
        this.options.current_player = this.options.current_player == 'w' ? 'b': 'w';
    },

    other_player: function() {
        return this.options.current_player == 'w' ? 'b': 'w';
    },

    computer_turn: function() {
        new Request.JSON({
            url: request_url,
            onSuccess: (function(response_json) {
                console.log(response_json);
                var opponent_turn = 
                    vertex_to_coords(response_json[0].data[0]);
                console.log("opponent played: ", opponent_turn);

                this.goban.clear();
                if (response_json[1])
                    this.goban.update("b", map_vertices_to_coords(response_json[1].data));
                if (response_json[2])
                    this.goban.update("w", map_vertices_to_coords(response_json[2].data));
                this.click_locked = false;
            }).bind(this),
            onFailure: function(xhr) {
                console.log(xhr.status, xhr.statusText, xhr.responseText);
            },
        }).get({'command-list': JSON.encode([{
	                                         'command-name': 'genmove',
					         'args': this.other_player()
					     },
					     {
						 'command-name': 'list_stones',
						 'args': this.options.current_player
					     },
					     {
						 'command-name': 'list_stones',
						 'args': this.other_player()
					     }])});
    }
});

function init() {
    var goban = new Goban($('goban'), 9);
    var game = new Game(goban);

    game.addEvent('invalidTurn', function(row, col) {
        console.log("Invalid turn: %o", [row, col]);
    }, this);

    var game_inspector = new ObjectInspector(game, {
        id: 'game-inspector',
        items: [
            {
                id: 'current_player',
                title: 'Your color',
                description: 'Your fucking color',
            }
        ]
    });
}

window.addEvent('domready', init);
