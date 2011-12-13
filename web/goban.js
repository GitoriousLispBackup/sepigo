Goban = new Class({
    Implements: Events,

    initialize: function(el, size) {
        this.el = el;
        this.size = size;
        this.stones = {'w': [], 'b': []};

        // register click callback
        set_stone = (function (ev) {
            ev.stopPropagation();
            ev.preventDefault();
            
            var id = document.id(ev.target).id;
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

    add_stone: function(color, row, col) {
        var td = document.id('cell-'+row+'-'+col);
        td.setProperty('class', color);
    },

    remove_stone: function(row, col) {
        var td = document.id('cell-'+row+'-'+col);
        td.setProperty('class', 'transparent');
    },

    get_stone: function(row, col) {
        var td = document.id('cell-'+row+'-'+col);
	var klass = td.getProperty('class');
        if (klass === 'b' || klass === 'w') {
            return klass;
        } else {
            return false;
        }
    },

    get_stones: function(color) {
	var stones = new Array();
	for (var row = 0; row < this.size; ++row) {
	    for (var col = 0; col < this.size; ++col) {
		if (this.get_stone(row, col) === color) {
		    stones.unshift([row, col]);
		}
	    }
	}
	return stones;
    },

    clear: function() {
        console.log("Cleared goban");
        for (var row = 0; row < this.size; ++row) {
            for (var col = 0; col < this.size; ++col) {
                this.remove_stone(row, col);
            }
        }
    },

    add_stones: function(color, stones) {
        stones.each(function (stone) {
            this.add_stone(color, stone[0], stone[1]);
        }, this);
    },

    remove_stones: function(stones) {
        stones.each(function (stone) {
            this.remove_stone(stone[0], stone[1]);
        }, this);
    }
});
