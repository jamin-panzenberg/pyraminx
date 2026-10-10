module Problem exposing (Problem)

{-| Let's formalize the problem!

In order to solve our problem with this library, we need to formalize it using the following three concepts:

  - The **initial state** of the problem.
  - The **actions** that are available from the initial state. These actions lead us to new exciting states. And then from these new states there might be more actions, leading us to even more states. This can go on further and further.
  - A **goal test** that tells us when we have reached a solution.

This all sounds very abstract, so I recommend checking out some examples from the [`Problem.Example`](Problem-Example) module.

@docs Problem

That's it! Once we have specified all these parameters, the algorithms from [`Problem.Search`](Problem-Search) will solve our problem for us (or at least try their best).

-}


{-| We use the `Problem` type to formalize our problem. We can then use the search algorithms in [`Problem.Search`](Problem-Search) to search for a solution to the `Problem`.

First, we need to think of a suitable data structure to represent the states that can occur in our problem. Then, we can think of the parameters that are necessary to formalize our problem:


## `initialState`

Simply the initial state from where we start exploring solutions.


## `actions`

Actions explain which states are reachable from any given state. Each action consists of a`result`, that is the state which is reached by the action, and of a`stepCost` between the current state and the result state.

For example, in some route-finding problem there might exist direct connections from Abuja to Accra (933 kilometers) and from Abuja to Lagos (536 kilometers). We could formalize these facts like so:

    actions =
        \state ->
            [ ( "Abuja"
              , [ { state = "Accra", stepCost = 933 }
                , { state = "Lagos", stepCost = 536 }
                ]
              )
            , -- more connections, starting at other cities
              Debug.todo
            ]
                |> Dict.fromList
                |> Dict.get state
                |> Maybe.withDefault []

For toy problems, the step cost of every step will often be `1`; Any action takes some effort, but the effort is always the same:

    stepCost =
        \_ -> 1

Sometimes, we do not care at all about how many steps are taken to solve a problem:

    stepCost =
        \_ -> 0

You might be worrying about avoiding redundant states. That is very reasonable, but don't worry! The search algorithms will avoid them automatically, in a smart way.


## `goalTest`

Describes under which conditions a state is a solution to the problem.

Sometimes we know exactly which state is a solution. Thus, if our goal state is Abidjan:

    goalTest =
        (==) "Abidjan"

But at other times we only know which conditions to pose, so we will write a more sophisticated function here.


## `heuristic`

A heuristic is an estimate about the path cost (the sum of step costs of all actions involved) between a state and the nearest goal state.

If we can think of such an estimate, this is great, because we can then use faster search algorithms ­— `greedySearch` and `bestFirstSearch`!

Often, however, we do not know a heuristic. In that case:

    heuristic =
        \_ -> 0

(Or choose any other arbitrary fixed value instead of `0`).


## `stateToString`

A function that _uniquely (!)_ converts any state to a string. It will be used for some optimizations.

For prototyping, we can just use:

    stateToString =
        Debug.toString

Otherwise, we will need to come up with a custom function. [JSON encoders](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode) might be helpful for coverting more complex states to strings.

This parameter may appear a bit tedious, but it is necessary for two different kinds of optimizations:

1.  Using [keyed nodes](https://guide.elm-lang.org/optimization/keyed.html) in visualizations. This needs a `String` state representation.

2.  Storing states in dictionaries to access them in logarithmic rather than linear time. This needs a `comparable` state representation. As a `String` happens to be `comparable`, there is no need for a separate `stateToComparable` function.

There are reasons why `Debug.toString` cannot be part of published packages, so the stringification needs to be done by the library user, sorry.

-}
type alias Problem state =
    { -- init
      initialState : state
    , actions :
        state
        ->
            List
                { stepCost : Float
                , result : state
                }
    , goalTest : state -> Bool
    , heuristic : state -> Float
    , stateToString : state -> String
    }
