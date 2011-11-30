request_url = 'go';

// req: gtp request hash
// response_callback: gets called with the response hash
// tis: this reference the callback will be bound to
function gtp_request(req, response_callback, tis) {
    new Request.JSON(
	{url: request_url,
	 onSuccess: function(response_json) {
	                response_callback.call(tis, response_json);
	            },
	 onFailure: function() {
	                console.error("Error from server");
	            }
	}
    ).get(req);
}

// Transform between js and gnugo data

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

Game = new Class({
    Implements: [Events, Options, Chain, Model],

    options: {
        difficulty: 0,
        komi: 4.5,
        handicap: 0,
        human_color: 'b',
        current_player: 'b',
	state: 'init',
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

	this.command_loop_driver(this.init_state_machine);

        goban.addEvent('click', this.click_handler.bind(this));
    },

    // This method is called when a click on a goban field is registered
    click_handler: function (row, col) {
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
    },

    command: function(cmd) {
	console.log("Issued command: ", cmd);
        gtp_request(cmd,
		    function (response) {
			switch (this.options.state) {
			case 'init':
			    if (response.success) {
				this.options.state = 'size-setup';
				this.command({'command-name': 'clear-board'});
			    }
			case 'size-setup':
			    if (response.success) {
				this.options.state = 'game-started';
			    }
			case 'game-started':
			    return;
			default:
			    console.log("Error setting up goban: state =", this.options.state);
			}
		    }, this);
    },

    init_state_machine: function(current_state, response) {
	console.log("in state: ", current_state);
	switch (current_state) {
	case 'init':
	    return ['boardsize_initialized',
		    {'command-name': 'boardsize', 'args': this.options.size}];
	case 'boardsize_initialized':
	    return ['board_cleared',
		    {'command-name': 'clear_board'}];
	case 'board_cleared':
	    return ['done', false];
	}
    },

    command_loop_driver: function(transition_lambda)  {
	[this.options.state, next_command] = transition_lambda.call(this, 'init', false);
	this.command_loop(next_command, transition_lambda);
    },

    // Executes the state machine transition_lambda. This function
    // takes two arguments:
    //   current_stat: the current state the machine is in
    //   the response from last state transition
    // it returns an array of:
    //   next_state: the state to continue with
    //   command: a gtp command to be sent
    command_loop: function(initial_command, transition_lambda) {
    	gtp_request(initial_command,
		    function(response) {
			if (response.success) {
			    var next_command;
			    [this.options.state, next_command] = 
				transition_lambda.call(this, this.options.state, response);
			    if (this.options.state == "done") {
				console.log("command_loop done");
				return true;
			    } else {
				this.command_loop(next_command, transition_lambda);
			    }
                        }
		    },
		    this);
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
