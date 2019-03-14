# Grid
This package defines 2-dimensional Grids and functions to operate them.

For example, you may want to represent a [Go](https://en.wikipedia.org/wiki/Go_(game)) board as a 19x19 grid with `Maybe Piece`s.

Let's first define pieces.

``` elm
Type Piece = White | Black
```

Then we can initialize the board as empty.

``` elm
initialBoard : Grid (Maybe Piece)
initialBoard =
    Grid.repeat 19 19 empty
```

Then we can use `Grid.get` and `Grid.set` to access and modify the values in grid.

## License
This package follows [Apache License Version 2.0](LICENSE).
