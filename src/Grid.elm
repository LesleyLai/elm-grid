module Grid exposing(Grid, foldl, foldr, fromList,
                         get, rows, height, initialize,
                         set, map, indexedMap, repeat, width)

{-| Grids are 2-dimensional arrays. The elements in a grid must have the same type.

# Definition
@docs Grid

# Initializeation
@docs initialize, repeat, fromList

# Queries
@docs get, width, height, rows

# Manipulate
@docs set

# Transform
@docs map, indexedMap, foldl, foldr

-}
import Array exposing (Array)

{-|
 Grid represents immutable 2 dimensional arrays.
-}
type Grid a = Grid {
        w: Int
      , h: Int
      , data: (Array a)
    }

{-|
 Initializes a Grid. `initialize w h f` creates a grid of width `w`,
 height 'h', with the elements at (i j) initialized to the result
 of `(f i j)`.
-}
initialize : Int -> Int -> (Int -> Int -> a) -> Grid a
initialize w h f =
    let indices = List.range 0 (h - 1) |>
                  List.concatMap (\y ->
                                  List.range 0 (w - 1) |>
                                  List.map (\x -> (x, y))) in
    let data = indices
               |> List.map (\(x,y) -> f x y)
               |> Array.fromList in
    Grid {w=w, h=h, data=data}

{-|
 Creates an grid with a given width and height, filled with a default
 element.

    Just (repeat 2 2 'a') == fromList[['a', 'a'], ['a', 'a']]
-}
repeat : Int -> Int -> a -> Grid a
repeat w h elem =
    let data = Array.repeat (w * h) elem in
    Grid {w=w, h=h, data=data}


inRange : Int -> Int -> Int -> Int -> Bool
inRange x y w h =
    x >= 0 && y >= 0 && x < w && y < h

{-|
 Returns Just the element at the index or Nothing if the index is out
 of range.

    let grid = fromList [['a','b','c'],
                         ['d','e','f'],
                         ['g','h','i']]
    in
    Maybe.andThen (get 1 2) grid == Just 'h'
    Maybe.andThen (get -1 2) grid == Nothing
-}
get : (Int, Int) -> Grid a -> Maybe a
get (x, y) grid =
    case grid of
        Grid {w, h, data} ->
            if (inRange x y w h) then
                 Array.get (x + y * w) data
            else
                Nothing

{-|
 Set the element at a particular index. Returns an updated grid.
 If the index is out of range, the grid is unaltered.

    let grid = fromList [['a','b','c'],
                         ['d','e','f'],
                         ['g','h','i']]
        grid2 = fromList [['a','b','c'],
                          ['d','e','f'],
                          ['g','a','i']]
    in
    Maybe.andThen (set 1 2 'a') grid == grid2
-}
set : (Int, Int) -> a -> Grid a -> Grid a
set (x, y) elem grid =
    case grid of
        Grid {w, h, data} ->
            if (inRange x y w h) then
                Grid {w=w, h=h, data=(Array.set (x + y * w) elem data)}
            else
                grid

{-|
 Creates a grid from a List of List. If the input list is not well
 formed, this function will returns Nothing.
    fromList [['e','l','m'],
              ['e','l','m'],
              ['e','l','m']] -- A 3x3 (Maybe grid)

    fromList [['e','l','m'],
              ['e','l','m'],
              ['e','l']] == Nothing
-}
fromList : List (List a) -> Maybe (Grid a)
fromList lst =
    case lst of
        [] -> Just <| Grid {w = 0, h = 0, data = Array.fromList []}
        row :: tails ->
            fromList tails |>
            Maybe.andThen (\remainGrid ->
                    case remainGrid of
                        Grid {w, h, data} ->
                            let rowData = Array.fromList row in
                            let rowWidth = Array.length rowData in
                            let maybeNewWidth = if (w == 0) then
                                                    Just (Array.length rowData)
                                                else if (w == rowWidth) then
                                                         Just w
                                                     else
                                                         Nothing
                            in
                            maybeNewWidth |>
                            Maybe.andThen (\newWidth ->
                                               Just <| Grid {w=newWidth
                                                           , h=h+1
                                                           , data=Array.append rowData data})
                                          )
                                

{-|
 Gets the width of the Grid
-}
width: Grid a -> Int
width (Grid {w, h, data}) =
    w

{-|
 Gets the height of the Grid
-}
height: Grid a -> Int
height (Grid {w, h, data}) =
    h

{- Gets the next index from the current index. No bound checking -}
nextIndex: (Int, Int) -> Grid a -> (Int, Int)
nextIndex (x, y) (Grid {w, h, data}) =
    if (x == w - 1) then
        (0, y + 1)
    else
        (x + 1, y)
    
{-|
Applies a function on every element in an array.

    Maybe.map (map sqrt) (fromList [[1,4], [9,16]]) == fromList [[1,2], [3,4]]
-}
map: (a -> b) -> Grid a -> Grid b
map func (Grid {w, h, data}) =
    Grid {w=w, h=h, data=Array.map func data}

{-|
Applies a function on every element with its index as first and second arguments.

    indexedMap (\x y e -> (x, y, e)) (repeat 2 2 1)
    -- [[(0,0,1),(1,0,1)],
    --  [(0,1,1),(1,1,1)]]
-}
indexedMap: (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap func grid =
    case grid of
        Grid {w, h, data} ->
            let (_, _, newData) = Array.foldl (\e (x, y, acc) ->
                                   let (x1, y1) = nextIndex (x, y) grid in
                                   (x1, y1, Array.push (func x y e) acc))
                                   (0, 0, Array.fromList []) data in
            Grid {w=w, h=h, data=newData}

{-|
 Reduces the grid from the left.
-}
foldl: (a -> b -> b) -> b -> Grid a -> b
foldl func acc (Grid {w, h, data}) =
    Array.foldl func acc data

{-|
 Reduces the grid from the right.
-}
foldr: (a -> b -> b) -> b -> Grid a -> b
foldr func acc (Grid {w, h, data}) =
    Array.foldr func acc data

{-|
 Gets the an array of rows of the grid
-}
rows: Grid a -> Array (Array a)
rows (Grid {w, h, data}) =
    List.range 0 (h-1)
    |> Array.fromList
    |> Array.map (\y -> Array.slice (w*y) (w*y+w) data)
