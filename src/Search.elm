module Search exposing
    ( breadthFirst, depthFirst, uniformCost
    , greedy, bestFirst
    , treeBreadthFirst, treeDepthFirst, treeUniformCost, treeGreedy, treeBestFirst
    , Model, next, nextN, solve, exhaust, exhaustBoundary
    , Result(..), mapResult, resultWithDefault, resultHasState
    , Node, expand, path, reversePathWithPosition, unestrangedChildren
    )

{-| These search algorithms will solve our `Problem`, if it's soluble and not too hard. They all have different advantages and drawbacks.

[For an introduction to search algorithms, read through this wonderful interactive visualization.](https://www.redblobgames.com/pathfinding/a-star/introduction.html)

(I plan to add some precise information about their runtime complexity and space complexity.)


# Graph Search

Graph search means that the algorithm assumes that the search space looks like a graph. This means that there might be multiple ways to arrive at a state. The algorithm therefore takes care of this and avoids visiting the same state twice.


## Uninformed search

@docs breadthFirst, depthFirst, uniformCost


## Informed search

@docs greedy, bestFirst


# Tree Search

Tree search means that the algorithm assumes that the search space looks like a tree. This means that there will always be just a single way to arrive at a state.

Tree search is usually slightly more efficient than graph search (how much exactly?), but:

  - If we run tree search on a problem where the search space is not a tree, the algorithm might get stuck in cycles. Even if not, it will probably be less efficient than the corresponding graph search algorithm.
  - Unlike "real" tree search algorithms, the tree search algorithms in this library do also track all the explored states, because we want to have them available for visualization. There is still some performance advantage with tree search, but it is smaller than in "real" tree search, and might be asymptotically negligible. (I need to check this.)

@docs treeBreadthFirst, treeDepthFirst, treeUniformCost, treeGreedy, treeBestFirst


# Performing the search

The functions above only initialize a search model. We can use one of the functions in here to run the search model.

@docs Model, next, nextN, solve, exhaust, exhaustBoundary


# Inspecting the Result

@docs Result, mapResult, resultWithDefault, resultHasState


# Internal infrastructure

Use these for building you own visualizations. First, check out the ready-made visualizations in the `Problem.Search.Visual` submodule.

@docs Node, expand, path, reversePathWithPosition, unestrangedChildren

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Problem exposing (Problem)



-- SEARCH RESULT


{-| The current result of any search.

  - `Pending`: No solution has been found, but the search has not yet exhausted the state space and more search steps can be performed.
  - `Solution a`: A solution has been found
  - `Failure`: The complete search space has been searched, but no solution could be found.

-}
type Result a
    = Pending
    | Solution a
    | Failure


{-| -}
mapResult : (a -> b) -> Result a -> Result b
mapResult f result =
    case result of
        Solution a ->
            Solution (f a)

        Pending ->
            Pending

        Failure ->
            Failure


{-| -}
resultWithDefault : a -> Result a -> a
resultWithDefault default result =
    case result of
        Solution a ->
            a

        Pending ->
            default

        Failure ->
            default


{-| -}
resultHasState : a -> Result (Node a) -> Bool
resultHasState state_ result =
    case result of
        Solution { state } ->
            state == state_

        _ ->
            False



-- NODE INFRASTRUCTURE


{-| -}
type alias Node a =
    { state : a
    , parent : Maybe a
    , pathCost : Float
    , children :
        Maybe
            (List
                { pathCost : Float
                , state : a
                }
            )
    }


{-| -}
expand : Problem a -> Node a -> { updatedParent : Node a, children : List (Node a) }
expand problem ({ state, pathCost } as node) =
    let
        actions =
            problem.actions state

        children =
            List.map
                (\{ stepCost, result } ->
                    { state = result
                    , parent = Just state
                    , pathCost = pathCost + stepCost
                    , children = Nothing
                    }
                )
                actions

        updatedParent =
            { node
                | children =
                    children
                        |> List.map (\child -> { pathCost = child.pathCost, state = child.state })
                        |> Just
            }
    in
    { updatedParent = updatedParent, children = children }



-- QUEUES


{-| Performs a pop operation on a list, such that, when `::` is used for insertion, the list resembles a certain type of a queue.
-}
type alias QueuePopper a =
    Dict String (Node a) -> List a -> Maybe ( a, List a )


{-| simulates a First-In-First-Out queue when using `::` for insertion
-}
fifo : QueuePopper a
fifo _ =
    List.unconsLast


{-| simulates a Last-In-First-Out queue when using `::` for insertion
-}
lifo : QueuePopper a
lifo _ =
    List.uncons


{-| simulates a priority queue when using `::` for insertion
-}
priority : (Dict String (Node a) -> a -> comparable) -> QueuePopper a
priority f d l =
    List.minimumBy (f d) l
        |> Maybe.map (\a -> ( a, List.remove a l ))



-- STRATEGIES


{-| -}
type alias Strategy a =
    Problem a
    -> Dict String (Node a)
    -> List (Node a)
    -> a
    -> List (Node a)


{-| -}
treeSearch : Strategy a
treeSearch _ _ childNodes _ =
    childNodes


{-| Ensures states are not explored twice and always at the lowest known path cost.
-}
graphSearch : Strategy a
graphSearch problem explored childNodes h =
    -- only add child node if
    -- a) their state is not the same as their parent's and
    -- b) their state is not in a sibling node with a lower path cost
    -- c) their state is not already explored and
    -- d) their state is not already in the frontier with a lower path cost
    childNodes
        |> List.filter
            (\{ state, pathCost } ->
                -- check parent
                not (state == h)
                    -- check sibling
                    && not
                        (List.any
                            (\sibling ->
                                sibling.state
                                    == state
                                    && pathCost
                                    < sibling.pathCost
                            )
                            childNodes
                        )
                    -- check explored & frontier
                    && not
                        (Dict.member (problem.stateToString state) explored)
            )



-- MODEL


{-| This record represents the inner state of the search algorithm. You can integrate it into the model of your web application.

The type parameter `a` refers to the `State` type of the search problem. For example, if you want to search a sliding puzzle, you can import it with `import Problem.Search.SlidingPuzzle exposing (State)`.

Initialize your model with `searchInit` (see below).

-}
type alias Model a =
    { strategy : Strategy a
    , queue : QueuePopper a
    , problem : Problem a
    , explored : Dict String (Node a)
    , frontier : List a
    , result : Result (Node a)
    , maxPathCost : Float
    }


{-| Initializes your model of the search algorithm. It takes a `Problem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
init :
    Strategy a
    -> QueuePopper a
    -> Problem a
    -> Model a
init strategy queue problem =
    { strategy = strategy
    , queue = queue
    , problem = problem
    , explored =
        Dict.fromList
            [ ( problem.stateToString problem.initialState
              , { state = problem.initialState
                , pathCost = 0
                , parent = Nothing
                , children = Nothing
                }
              )
            ]
    , frontier = [ problem.initialState ]
    , result = Pending
    , maxPathCost = 0
    }


searchStep :
    Strategy a
    -> QueuePopper a
    -> (Node a -> Bool)
    -> Model a
    -> Model a
searchStep strategy pop boundary ({ problem, explored } as model) =
    case pop explored model.frontier of
        Just ( h, t ) ->
            case Dict.get (problem.stateToString h) explored of
                Just node ->
                    let
                        { updatedParent, children } =
                            expand problem node

                        filteredChildren =
                            strategy problem explored children h
                                |> List.filter boundary
                    in
                    { model
                        | result =
                            case List.find (\{ state } -> problem.goalTest state) children of
                                Just newSolution ->
                                    -- always keep the first solution (a questionable design choice, maybe to be revised)
                                    case model.result of
                                        Solution _ ->
                                            model.result

                                        _ ->
                                            Solution newSolution

                                Nothing ->
                                    model.result
                        , frontier =
                            (filteredChildren
                                |> List.map .state
                                |> List.reverse
                            )
                                ++ t
                        , explored =
                            List.foldl
                                (\node_ ->
                                    Dict.insertDedupe
                                        (\original new ->
                                            if new.pathCost > original.pathCost then
                                                original

                                            else
                                                new
                                        )
                                        (problem.stateToString node_.state)
                                        node_
                                )
                                explored
                                (updatedParent :: children)
                        , maxPathCost = List.foldl max model.maxPathCost (List.map (\n -> n.pathCost) children)
                    }

                -- will never occur, since the `h` state *must* be in the `explored` dictionary
                Nothing ->
                    { model | result = Failure }

        Nothing ->
            { model | result = Failure }



-- STEPPERS


noBoundary : Node a -> Bool
noBoundary _ =
    True


{-| Performs a single step of the search algorithm. Returns a new search model.
-}
next : Model a -> Model a
next ({ strategy, queue } as model) =
    searchStep strategy queue noBoundary model


{-| Performs n steps of the search algorithm. Returns a new search model.
-}
nextN : Int -> Model a -> Model a
nextN n ({ strategy, queue } as model) =
    if n > 0 then
        searchStep strategy queue noBoundary model |> nextN (n - 1)

    else
        model


{-| Tries to solve the search. Returns a pair of the result (if it can find one), and the search model.
-}
solve : Model a -> ( Maybe (Node a), Model a )
solve ({ result } as model) =
    case result of
        Solution a ->
            ( Just a, model )

        Failure ->
            ( Nothing, model )

        Pending ->
            solve (next model)


{-| Searches through the complete search space without stopping at solutions. Returns a new search model. Note that the complete search space is usually very large.
-}
exhaust : Model a -> Model a
exhaust model =
    let
        new =
            next model
    in
    if new.frontier == [] then
        new

    else
        exhaust new


{-| Searches through the search space until the boundary condition is no longer fulfilled. Returns a new search model. Could perhaps be used to implement IDA\*, but I have not yet attempted that. **Experimental.**
-}
exhaustBoundary : (Node a -> Bool) -> Model a -> Model a
exhaustBoundary boundary ({ strategy, queue } as model) =
    let
        new =
            searchStep strategy queue boundary model
    in
    if new.frontier == [] then
        new

    else
        exhaust new



-- INTERFACE


{-| -}
breadthFirst : Problem a -> Model a
breadthFirst =
    init graphSearch fifo


{-| -}
treeBreadthFirst : Problem a -> Model a
treeBreadthFirst =
    init treeSearch fifo


{-| -}
depthFirst : Problem a -> Model a
depthFirst =
    init graphSearch lifo


{-| -}
treeDepthFirst : Problem a -> Model a
treeDepthFirst =
    init treeSearch lifo


{-| -}
uniformCost_ : Strategy a -> Problem a -> Model a
uniformCost_ strategy problem =
    init strategy
        (priority
            (\explored state ->
                explored
                    |> Dict.get (problem.stateToString state)
                    |> Maybe.map .pathCost
                    -- default will never be used, since the state *must* be in the `explored` dictionary
                    |> Maybe.withDefault 0
            )
        )
        problem


{-| Dijkstra's algorithm.
-}
uniformCost : Problem a -> Model a
uniformCost =
    uniformCost_ graphSearch


{-| -}
treeUniformCost : Problem a -> Model a
treeUniformCost =
    uniformCost_ treeSearch


{-| -}
greedy_ : Strategy a -> Problem a -> Model a
greedy_ strategy problem =
    init strategy
        (priority (\_ state -> problem.heuristic state))
        problem


{-| -}
greedy : Problem a -> Model a
greedy =
    greedy_ graphSearch


{-| -}
treeGreedy : Problem a -> Model a
treeGreedy =
    greedy_ treeSearch


{-| -}
bestFirst_ : Strategy a -> Problem a -> Model a
bestFirst_ strategy problem =
    init
        strategy
        (priority
            (\explored state ->
                (explored
                    |> Dict.get (problem.stateToString state)
                    |> Maybe.map .pathCost
                    -- default will never be used, since the state *must* be in the `explored` dictionary
                    |> Maybe.withDefault 0
                )
                    + problem.heuristic state
            )
        )
        problem


{-| A\* search.
-}
bestFirst : Problem a -> Model a
bestFirst =
    bestFirst_ graphSearch


{-| -}
treeBestFirst : Problem a -> Model a
treeBestFirst =
    bestFirst_ treeSearch



-- NODE HELPERS


{-| -}
path : Model a -> Node a -> List ( Float, a )
path { problem, explored } node =
    let
        stacksafePath : Node a -> List ( Float, a ) -> List ( Float, a )
        stacksafePath { state, pathCost, parent } stack =
            case parent of
                Just parentState ->
                    case Dict.get (problem.stateToString parentState) explored of
                        Just parentNode ->
                            stacksafePath parentNode (( pathCost, state ) :: stack)

                        -- never occurs, since the state *must* be in the `explored` dictionary
                        Nothing ->
                            ( pathCost, state ) :: stack

                Nothing ->
                    ( pathCost, state ) :: stack
    in
    stacksafePath node [] |> List.reverse


{-| -}
reversePathWithPosition : Model a -> Node a -> List ( Float, a, ( Int, Int ) )
reversePathWithPosition ({ problem, explored } as model) node =
    let
        stacksafePathWithPosition : Node a -> List ( Float, a, ( Int, Int ) ) -> List ( Float, a, ( Int, Int ) )
        stacksafePathWithPosition { state, pathCost, parent } stack =
            case parent of
                Just parentState ->
                    case Dict.get (problem.stateToString parentState) explored of
                        Just parentNode ->
                            let
                                unestrangedSiblings =
                                    unestrangedChildren model parentNode

                                enumeratedUnestrangedSiblings =
                                    unestrangedSiblings
                                        |> Maybe.map (List.indexedMap Tuple.pair)
                            in
                            stacksafePathWithPosition
                                parentNode
                                (( pathCost
                                 , state
                                 , ( enumeratedUnestrangedSiblings
                                        |> Maybe.map (List.findIndex (Tuple.second >> (==) state))
                                        |> Maybe.join
                                        |> Maybe.withDefault 0
                                   , enumeratedUnestrangedSiblings
                                        |> Maybe.map List.length
                                        |> Maybe.withDefault 0
                                   )
                                 )
                                    :: stack
                                )

                        -- never occurs, since the state *must* be in the `explored` dictionary
                        Nothing ->
                            ( pathCost, state, ( 0, 1 ) ) :: stack

                Nothing ->
                    ( pathCost, state, ( 0, 1 ) ) :: stack
    in
    stacksafePathWithPosition node []


{-| -}
unestrangedChildren : Model a -> Node a -> Maybe (List a)
unestrangedChildren { problem, explored } node =
    node.children
        |> Maybe.map
            (\children ->
                children
                    |> List.map
                        (\child ->
                            Dict.get (problem.stateToString child.state) explored
                        )
                    |> Maybe.values
                    |> List.filter (\child -> child.parent == Just node.state)
                    |> List.map .state
            )
