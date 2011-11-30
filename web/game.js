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
        this.lock_click();

        // Black always starts
        this.options.current_player = 'b';

        if (this.options.human_color === this.options.current_player) {
	    this.unlock_click();
        }

        // this.init_handicap(handicap); TODO

	this.command_loop_run(this.init_state_machine);

        goban.addEvent('click', this.click_handler.bind(this));
    },

    // This method is called when a click on a goban field is registered
    click_handler: function (row, col) {
        // To gnugo coords
	this.row = row+1;
	this.col = col+1;

        ++row; ++col;

        console.log("click event at: ", [row, col]);

        // Prevent user from issuing more ajax requests
        if (this.click_locked) {
            console.log("click is locked, no action!");
            return;
        }
        this.lock_click();
	
	this.command_loop_run(this.play_state_machine);
    },
    
    position: function() {
    	return int_to_char(this.row) + this.col;
    },

    play_state_machine: function(current_state, response) {
	// console.log("in state: ", current_state);
	switch (current_state) {
	case 'init':
            var row_char = int_to_char(this.row);
	    return ['played', {'command-name': 'play',
			       'args': this.options.current_player + ' ' + row_char + '' + this.col}];
	case 'played':
	    if (response.success) {
		console.log("SUCCESS: client played: ", this.position());
		return ['genmoved', {'command-name': 'genmove',
				     'args': this.other_player()}];
	    } else {
		console.log("FAIL: field taken, play again");
		this.unlock_click();
		return ['done', false];
	    }
	case 'genmoved':
	    console.log(response);
	    if (response.success) {
		if (response.data[0] == "PASS") {
		    console.log("SUCCESS: server passed");
		    return ['pass', false];
		} else {
		    console.log("SUCCESS: server played: ", vertex_to_coords(response.data[0]));
		    return ['stones_listed_client', {'command-name': 'list_stones',
						     'args': this.options.current_player}];
		}
	    } else {
		console.error("ALARM: genmove failed!");
		return ['done', false];
	    }
	case 'stones_listed_client':
	    if (response.success) {
		console.log("SUCCESS: client stones received: ", response.data);
		this.goban.clear();
		this.goban.update("b", map_vertices_to_coords(response.data));
		return ['stones_listed_server', {'command-name': 'list_stones',
						 'args': this.other_player()}];
	    } else {
		console.error("ALARM: client list_stones failed!");
		return ['done', false];
	    }
	case 'stones_listed_server':
	    if (response.success) {
		console.log("SUCCESS: server stones received: ", response.data);
		this.goban.update("w", map_vertices_to_coords(response.data));
		this.unlock_click();
		return ['done', false];
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
	    console.log("SUCCESS: Game initialized");
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
			if (this.options.state == "done") {
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
