var goban;
var game;

function player_indicator(who) {
    $('#player-slider').val(who);
    $('#player-slider').slider('refresh');
    $.mobile.loadingMessage = "Thinking";
    if (who === 'computer') {
	$.mobile.showPageLoadingMsg();
    } else {
	$.mobile.hidePageLoadingMsg();
    }
}

function score_el(who) {
    el = $('#'+who+'-score');
    return el;
}

function add_score(who, stones) {
    el = score_el(who);
    el.html(parseInt(el.html()) + stones);
    el.effect('highlight', {color: 'red'});
}

function reset_score() {
    el = score_el('w');
    el.html(0);
    el = score_el('b');
    el.html(0);
}

function setup() {
    goban = new Goban(document.id('goban'), 9);
    game = new Game(goban);

//    reset_score();

    game.addEvent('invalidTurn', function(row, col) {
        console.log("Invalid turn: %o", [row, col]);
    }, this);

    game.addEvent('client_played', function(stone) {
	// TODO: alarm hack
	if (stone[0] === 'pass') {
	    alert("passed");
	} else  {
    	    goban.add_stone(game.options.client_player, stone[0], stone[1]);
	    game.lock_click();
	}
	player_indicator('computer');
    }, this);

    game.addEvent('server_played', function(stone) {
    	goban.add_stone(game.options.server_player, stone[0], stone[1]);
	game.unlock_click();
	player_indicator('you');
    }, this);

    game.addEvent('server_stones_removed', function(stones) {
	add_score('score', stones.length);
	goban.remove_stones(stones);
    }, this);
    
    game.addEvent('server_stones_added', function(stones) {
	goban.add_stones(game.options.server_color, stones);
    }, this);

    game.addEvent('client_stones_removed', function(stones) {
	add_score('server', stones.length);
	goban.remove_stones(stones);
    }, this);
    
    game.addEvent('client_stones_added', function(stones) {
	goban.add_stones(game.options.client_color, stones);
    }, this);

    game.addEvent('client_passed', function(stones) {
	alert("client passed!");
    }, this);

    game.addEvent('server_passed', function(stones) {
	game.unlock_click();
	
	setup();
	alert("server passed!");
    }, this);

    game.addEvent('init_sepigo', function() {
        alert("FFO");
        console.error("Init alarm");
        reset_score();
        return false;
    });

    $('#pass-button').click(function(e) {
	e.stopImmediatePropagation();
	e.preventDefault();
	game.pass();
    });

    $('#new-game-button').click(function(e) {
        setup();
    });
}

window.addEvent('domready', setup);
