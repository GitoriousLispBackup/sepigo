function init() {
    var goban = new Goban(document.id('goban'), 9);
    var game = new Game(goban);

    game.addEvent('invalidTurn', function(row, col) {
        console.log("Invalid turn: %o", [row, col]);
    }, this);

    // var game_inspector = new ObjectInspector(game, {
    //     id: 'game-inspector',
    //     items: [
    //         {
    //             id: 'current_player',
    //             title: 'Your color',
    //             description: 'Your fucking color',
    //         }
    //     ]
    // });
}

window.addEvent('domready', init);
