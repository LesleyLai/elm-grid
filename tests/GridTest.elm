module GridTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array
import Grid exposing (..)

getSuite : Test
getSuite =
    let grid = Grid.repeat 8 8 1 in
    describe "Gets elements from a grid"
        [
         test "Inside range" <| \_ ->
             Grid.get (4, 4) grid
               |> Expect.equal (Just 1)
         , test "get with index x < 0 returns nothing" <| \_ ->
             Grid.get (-1, 4) grid
               |> Expect.equal Nothing
         , test "get with index y < 0 returns nothing" <| \_ ->
             Grid.get (4, -1) grid
               |> Expect.equal Nothing
         , test "get with index x >= (width grid) returns nothing" <| \_ ->
             Grid.get (8, 4) grid
               |> Expect.equal Nothing
         , test "get with index y >= (height grid) returns nothing" <| \_ ->
             Grid.get (4, 8) grid
               |> Expect.equal Nothing
        ]

setSuite : Test
setSuite =
    let grid = Grid.repeat 8 8 1 in
    describe "Sets elements from a grid"
        [
         test "Set (1, 0)" <| \_ ->
             Grid.get (1, 0) (Grid.set (1, 0) 2 grid)
               |> Expect.equal (Just 2)
        ]

fromListSuite : Test
fromListSuite =
    describe "Grid.fromList function create an array from a List of List"
        [
         test "list [[1, 2], [3, 4]] creates a 2x2 Grid" <| \_ ->
             let maybeGrid = Grid.fromList [[1, 2], [3, 4]] in
             case maybeGrid of
                 Nothing -> Expect.fail "Does not get the grid as expected"
                 Just grid ->
                     grid |>
                     Expect.all [
                          \g -> Grid.get (0, 0) g |> Expect.equal (Just 1)
                        , \g -> Grid.get (1, 0) g |> Expect.equal (Just 2)
                        , \g -> Grid.get (0, 1) g |> Expect.equal (Just 3)
                        , \g -> Grid.get (1, 1) g |> Expect.equal (Just 4)
                        , \g -> Grid.width g |> Expect.equal 2
                        , \g -> Grid.height g |> Expect.equal 2
                         ]
         , test "list [[1, 2], [3, 4, 5]] does not create a grid" <| \_ ->
             let maybeGrid = Grid.fromList [[1, 2], [3, 4, 5]] in
             case maybeGrid of
                 Nothing -> Expect.pass
                 Just grid -> Expect.fail "Gets a grid from malformed list"
        ]

mapSuite : Test
mapSuite =
    let maybeGrid = Grid.fromList [[1, 2], [3, 4]] in
    describe "Tests varies map functions"
        [
         test "Grid.map"
             <| (\_ -> (Expect.equal
                            (Maybe.map
                                 (Grid.map (\x -> x * 2)) maybeGrid)
                            (Grid.fromList [[2, 4], [6, 8]]))
                )
        ,test "Grid.indexedMap"
             <| (\_ -> (Expect.equal
                            (Maybe.map
                                 (Grid.indexedMap (\x y e -> x + y * e)) maybeGrid)
                            (Grid.fromList [[0, 1], [3, 5]]))
                )
        ]

foldSuite : Test
foldSuite =
    let maybeGrid = Grid.fromList [[1, 2], [3, 4]] in
    describe "Tests varies fold functions"
        [
         test "Grid.foldl"
             <| (\_ ->
                     case maybeGrid of
                         Nothing -> Expect.fail "Cannot create the list"
                         Just grid -> 
                             (Expect.equal
                                  (Grid.foldl (\x y -> x * y + 1) 0 grid)
                                  41))
        ,test "Grid.foldr"
             <| (\_ ->
                     case maybeGrid of
                         Nothing -> Expect.fail "Cannot create the list"
                         Just grid ->
                             (Expect.equal
                                  (Grid.foldr (\x y -> x * y + 1) 0 grid)
                                  10))
        ]

rowsColumnsSuite : Test
rowsColumnsSuite =
    let maybeGrid = Grid.fromList [[1, 2], [3, 4]] in
    describe "Tests rows and column functions"
        [
         test "Grid.rows"
             <| (\_ ->
                     case maybeGrid of
                         Nothing -> Expect.fail "Cannot create the list"
                         Just grid -> 
                             (Expect.equal
                                  (Grid.rows grid)
                                   (Array.fromList
                                       [(Array.fromList [1, 2])
                                       ,(Array.fromList [3, 4])])
                                  ))
         ]
