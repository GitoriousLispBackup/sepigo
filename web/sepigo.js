function init() {
    goban = new Goban(document.id('goban'), 9);
    game = new Game(goban);

    game.addEvent('invalidTurn', function(row, col) {
        console.log("Invalid turn: %o", [row, col]);
    }, this);

    game.addEvent('client_played', function(stone) {
    	goban.add_stone(game.options.client_player, stone[0], stone[1]);
	game.lock_click();
    }, this);

    game.addEvent('server_played', function(stone) {
    	goban.add_stone(game.options.server_player, stone[0], stone[1]);
	game.unlock_click();
    }, this);

    game.addEvent('server_stones_removed', function(stones) {
	goban.remove_stones(stones);
    }, this);
    
    game.addEvent('server_stones_added', function(stones) {
	goban.add_stones(game.options.server_color, stones);
    }, this);

    game.addEvent('client_stones_removed', function(stones) {
	goban.remove_stones(stones);
    }, this);
    
    game.addEvent('client_stones_added', function(stones) {
	goban.add_stones(game.options.client_color, stones);
    }, this);

    game.addEvent('client_passed', function(stones) {
	alert("client passed!");
    }, this);

    game.addEvent('server_passed', function(stones) {
	goban = new Goban(document.id('goban'), 9);
	game = new Game(goban);
	alert("server passed!");
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
