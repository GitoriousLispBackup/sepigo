var request_url = 'go';

function find_stone(a, el) {
    for (var i = 0; i < a.length; ++i) {
	if (a[i][0] === el[0] &&
	    a[i][1] === el[1]) {
	    return i;
	}
    }
    return -1;
}

Array.prototype.diff = function(a) {
    return this.filter(function(i) {return !(find_stone(a, i) > -1);});
};

// req: gtp request hash
// response_callback: gets called with the response hash
// tis: this reference the callback will be bound to
function gtp_request(req, response_callback, tis) {
    if (req) {
        new Request.JSON(
	    {url: request_url,
	     onSuccess: function(response_json) {
	         response_callback.call(tis, response_json);
	     },
	     onFailure: function(response_json) {
	         console.error("Server error: ", response_json);
	     }
	    }
        ).get(req);
    }
}

// Transform between js and gnugo data
function char_to_int(c) {
    var c_code = c.toLowerCase().charCodeAt(0);
    var a_code = 'a'.charCodeAt(0);
    var i_code = 'i'.charCodeAt(0);
    if (c_code >= i_code) {
        return (c_code-a_code-1);
    } else {
        return (c_code-a_code-1)+1;
    }
}

function int_to_char(i) {
    var a_code = 'a'.charCodeAt(0);
    if (i >= 8) {
        return String.fromCharCode(i+1+a_code);
    } else {
        return String.fromCharCode(i+a_code);
    }
}

function vertex_to_coord(vertex) {
    var row = char_to_int(vertex[0]);
    var col = parseInt(vertex.slice(1));
    return [row, col-1];
}

function coord_to_vertex(coord) {
    // TODO: alarm Hack
    if (coord[0] === 'pass') {
	return 'pass';
    } else {
	var row_char = int_to_char(coord[0]);
	var col = coord[1]+1;
	return row_char+col;
    }
}

function map_vertices_to_coords(vertices) {
    // FIXXXX
    if (!vertices) {
        return [];
    }

    return vertices.map(function(vertex) {
        return vertex_to_coord(vertex);
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
	server_player: 'w',
	client_player: 'b',
	state: 'stopped'
    },

    initialize: function(goban, options) {
        this.goban = goban;

        this.setOptions(options);
        this.options.size = this.goban.size;

        // Black always starts
        this.options.current_player = 'b';

        // this.init_handicap(handicap); TODO
	this.command_loop_run(this.init_init);

        goban.addEvent('touchstart', this.click_handler.bind(this));
        goban.addEvent('click', this.click_handler.bind(this));
    },

    // This method is called when a click on a goban field is registered
    click_handler: function (row, col) {
	if (row === 'pass') {
	    this.row = 'pass';
	} else {
 	    this.row = row;
	    this.col = col;

            console.info("Click event at: ", [row, col]);

	    this.fireEvent('click', [row, col]);
	}
        // Prevent user from issuing more ajax requests
        if (this.click_locked) {
            console.info("Click is locked, no action!");
            return;
        } else {
            this.lock_click();
	    this.command_loop_run(this.play_state_init);
	}
    },
    
    position: function() {
    	return int_to_char(this.row) + this.col;
    },

    update_stones: function(role, player, data) {
        console.info("Update stones: ", role, player, data);
	var old_stones = this.goban.get_stones(player);
	var new_stones = map_vertices_to_coords(data);
	var removed_stones = old_stones.diff(new_stones);
	var added_stones = new_stones.diff(old_stones);
	if (removed_stones.length > 0) {
	    this.fireEvent(role + '_stones_removed', [removed_stones]);
	}
	if (added_stones.length > 0) {
	    this.fireEvent(role + '_stones_added', [added_stones]);
	}
    },

    // Init state machine
    init_init: function(response) {
	return [this.init_boardsize_initialized,
		{'command-name': 'boardsize', 'args': this.options.size}];
    },
    init_boardsize_initialized: function(response) {
        this.fireEvent('init_sepigo');
	return [this.init_board_cleared,
		{'command-name': 'clear_board'}];
    },
    init_board_cleared: function(response) {
	return ['done', false];
    },

    // Play state machine
    play_state_init: function(response) {
	var vertex = coord_to_vertex([this.row, this.col]);

        this.client_played = false;
        this.server_played = false;
        this.half_listed = false;

	return [this.play_state_client_played,
                {'command-name': 'play',
		 'args': this.options.client_player + ' ' + vertex}];
    },
    play_state_client_played: function(response) {
	if (response.success) {
	    console.info("Client played: ", [this.row, this.col]);
	    this.fireEvent('client_played', [[this.row, this.col]]);

            this.client_played = true;
            this.server_played = false;

	    return [this.play_state_stones_listed,
                    {'command-name': 'list_stones',
		     'args': this.options.client_player}];
	} else {
	    console.info("Field taken, play again");
	    this.unlock_click();
	    return ['done', false];
	}        
    },
    play_state_stones_listed: function(response) {
        if (response.success) {
            if (!this.half_listed) { // Client played
                this.update_stones('client',
                                   this.options.client_player,
                                   response.data);
                this.half_listed = true;
	        return [this.play_state_stones_listed,
                        {'command-name': 'list_stones',
			 'args': this.options.server_player}];
            } else if (this.half_listed) {
                this.update_stones('server',
                                   this.options.server_player,
                                   response.data);

                if (this.client_played && !this.server_played) {

	            return [this.play_state_server_played,
                            {'command-name': 'genmove',
	                     'args': this.options.server_player}];
                } else if (this.server_played) {
                    return ['done', false];
                }
            } else {
                return ['done', false];
            }
        } else {
            console.error("Could not list all stones!");
            return ['done', false];
        }
        return ['done', false];
    },
    play_state_server_played: function(response) {
	if (response.success) {
            this.half_listed = false;

            this.client_played = false;
            this.server_played = true;

	    if (response.data[0] == "PASS") {
		console.info("Server passed");
		this.fireEvent('server_passed');
		return [this.play_state_pass, false];
	    } else {
		console.info("Server played: ", vertex_to_coord(response.data[0]));
		this.fireEvent('server_played', [vertex_to_coord(response.data[0])]);
		return [this.play_state_stones_listed,
                        {'command-name': 'list_stones',
			 'args': this.options.client_player}];
	    }
	} else {
	    console.error("ALARM: genmove failed!");
	    return ['done', false];
	}
    },
    play_state_pass: function(response) {
	this.unlock_click();
	return ['done', {'command-name': 'list_stones',
			 'args': this.other_player()}];
    },


    command_loop_run: function(transition_function)  {
        var next_transition_function, next_gtp_command;
        var x = transition_function.call(this, false);
	next_transition_function = x[0];
        next_gtp_command = x[1]; 
	this.command_loop(next_transition_function, next_gtp_command);
    },

    // Executes the state machine transition_lambda. This function
    // takes two arguments:
    //   current_stat: the current state the machine is in
    //   the response from last state transition
    // it returns an array of:
    //   next_state: the state to continue with
    //   command: a gtp command to be sent
    command_loop: function(transition_function, gtp_command) {
    	gtp_request(gtp_command,
		    function(response) {
			var next_transition_function, next_gtp_command;
                        var x = transition_function.call(this, response);
                        next_transition_function = x[0];
                        next_gtp_command = x[1];
                        
			// Statemachine is done or no command given
			if (next_transition_function === 'done') {
			    return true;
			} else {
			    return this.command_loop(next_transition_function, next_gtp_command);
			}
		    },
		    this);
    },

    other_player: function() {
        return this.options.current_player == 'w' ? 'b': 'w';
    },

    lock_click: function() {
	this.click_locked = true;
    },

    unlock_click: function() {
	this.click_locked = false;
    },
    
    pass: function() {
	this.click_handler('pass', 0);
    }
});
