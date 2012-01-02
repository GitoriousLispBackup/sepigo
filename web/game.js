request_url = 'go';

function find_stone(a, el) {
    for (var i = 0; i < a.length; ++i) {
	if (a[i][0] === el[0] &&
	    a[i][1] === el[1]) {
	    return i
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
    var row_char = int_to_char(coord[0]);
    var col = coord[1]+1;
    return row_char+col;
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
	state: 'stopped',
    },

    initialize: function(goban, options) {
        this.goban = goban;

        this.setOptions(options);
        this.options.size = this.goban.size

        // Black always starts
        this.options.current_player = 'b';

        // this.init_handicap(handicap); TODO

	this.command_loop_run(this.init_state_machine);

        goban.addEvent('click', this.click_handler.bind(this));
    },

    // This method is called when a click on a goban field is registered
    click_handler: function (row, col) {
 	this.row = row;
	this.col = col;

        console.log("click event at: ", [row, col]);

        // Prevent user from issuing more ajax requests
        if (this.click_locked) {
            console.log("click is locked, no action!");
            return;
        } else {
            this.lock_click();
	    this.command_loop_run(this.play_state_machine);
	}
    },
    
    position: function() {
    	return int_to_char(this.row) + this.col;
    },

    play_state_machine: function(current_state, response) {
	// console.log("in state: ", current_state);
	switch (current_state) {
	case 'init':
	    var vertex = coord_to_vertex([this.row, this.col]);
	    return ['client_played', {'command-name': 'play',
				      'args': this.options.current_player + ' ' + vertex}];
	case 'client_played':
	    if (response.success) {
		console.log("SUCCESS: client played: ", this.row, this.col);
		this.fireEvent('client_played', [[this.row, this.col]]);
		return ['stones_listed_server', {'command-name': 'list_stones',
						 'args': this.options.server_player}];
	    } else {
		console.log("FAIL: field taken, play again");
		this.unlock_click();
		return ['done', false];
	    }
	case 'server_played':
	    if (response.success) {
		if (response.data[0] == "PASS") {
		    console.log("SUCCESS: server passed");
		    this.fireEvent('server_passed');
		    return ['pass', false];
		} else {
		    console.log("SUCCESS: server played: ", vertex_to_coord(response.data[0]));
		    this.fireEvent('server_played', [vertex_to_coord(response.data[0])]);
		    return ['stones_listed_client', {'command-name': 'list_stones',
						     'args': this.options.client_player}];
		}
	    } else {
		console.error("ALARM: genmove failed!");
		return ['done', false];
	    }
	case 'stones_listed_client':
	    if (response.success) {
		console.log("SUCCESS: client stones received: ", response.data);

		var old_client_stones = this.goban.get_stones(this.options.client_player);
		var new_client_stones = map_vertices_to_coords(response.data);
		var removed_client_stones = old_client_stones.diff(new_client_stones);
		var added_client_stones = new_client_stones.diff(old_client_stones);
		console.log("REM ", removed_client_stones);
		console.log("ADD ", added_client_stones);
		if (removed_client_stones.length > 0) {
		    console.log("ALARM: client_stones_removed");
		    this.fireEvent('client_stones_removed', [removed_client_stones]);
		}
		if (added_client_stones.length > 0) {
		    this.fireEvent('client_stones_added', [added_client_stones]);
		}

		return ['done', {'command-name': 'list_stones',
						 'args': this.other_player()}];
	    } else {
		console.error("ALARM: client list_stones failed!");
		return ['done', false];
	    }
	case 'stones_listed_server':
	    if (response.success) {
		console.log("SUCCESS: server stones received: ", response.data);

		var old_server_stones = this.goban.get_stones(this.options.server_player);
		var new_server_stones = map_vertices_to_coords(response.data);
		var removed_server_stones = old_server_stones.diff(new_server_stones);
		var added_server_stones = new_server_stones.diff(old_server_stones);
		if (removed_server_stones.length > 0) {
		    this.fireEvent('server_stones_removed', [removed_server_stones]);
		}
		if (added_server_stones.length > 0) {
		    this.fireEvent('server_stones_added', [added_server_stones]);
		}

		return ['server_played', {'command-name': 'genmove',
					  'args': this.options.server_player}];
	    } else {
		console.error("ALARM: server list_stones failed!");
		return ['done', false];
	    }
	case 'pass':
	    this.unlock_click();
	    return ['done', false];
	}
    },

    init_state_machine: function(current_state, response) {
	// console.log("in state: ", current_state);
	switch (current_state) {
	case 'init':
	    return ['boardsize_initialized',
		    {'command-name': 'boardsize', 'args': this.options.size}];
	case 'boardsize_initialized':
	    return ['board_cleared',
		    {'command-name': 'clear_board'}];
	case 'board_cleared':
	    // console.log("SUCCESS: Game initialized");
	    return ['done', false];
	}
    },

    command_loop_run: function(transition_lambda)  {
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
			var next_command;
			[this.options.state, next_command] = 
			    transition_lambda.call(this, this.options.state, response);

			// Statemachine is done or no command given
			if (this.options.state == "done" ||
			    next_command == false) {
			    return true;
			} else {
			    this.command_loop(next_command, transition_lambda);
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

    lock_click: function() {
	// console.log("click locked");
	this.click_locked = true;
    },

    unlock_click: function() {
	// console.log("click unlocked");
	this.click_locked = false;
    }
});
