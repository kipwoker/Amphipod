module Render

open Browser.Dom
open Browser.Types
open Domain
open Fable.Core.JS

type DisplayState = {
    State: State
    Selected: (int*int) option
    Warn: (int*int) option
    History: State[]
}

let saveState displayState = document.getElementById("state").innerHTML <- JSON.stringify(displayState)
let restoreState() : DisplayState = JSON.parse (document.getElementById("state").innerHTML) :?> DisplayState

let addProp (element : HTMLElement) name value i j =
    if element.classList.contains(name) then
        element.classList.remove(name)
    match value with
    | Some (sx,sy) when sx = i && sy = j -> element.classList.add(name)
    | _ -> ignore 0

let printCell displayState i j id =
    let state = displayState.State
    let selected = displayState.Selected
    let warn = displayState.Warn
    let element = document.getElementById(id)
    element.innerHTML <- getLetter state.Rooms.[i].[j]
    addProp element "selected" selected i j
    addProp element "warn" warn i j

let print() =
    let displayState = restoreState()
    let state = displayState.State
    for i = 0 to 3 do
        for j = 0 to 3 do
            printCell displayState i j ("room-" + j.ToString() + "-" + i.ToString())

    for i = 0 to state.Rooms.[4].Length - 1 do
        printCell displayState 4 i ("hallway-" + i.ToString())

    document.getElementById("score").innerHTML <- state.Score.ToString()
    document.getElementById("min-score").innerHTML <- state.MinScore.ToString()

let onClickSquare (_ : MouseEvent) i j =
    let displayState = restoreState()
    let state = displayState.State
    let selected = displayState.Selected
    let hasElement = state.Rooms.[i].[j].IsSome
    match (selected, hasElement) with
    | None, false -> ignore 0
    | None, _ ->
        let displayState1 = { displayState with Selected = Some (i,j) }
        saveState displayState1
    | Some s, _ ->
        let sx,sy = s
        if sx = i && sy = j then
            let displayState1 = { displayState with Selected = None }
            saveState displayState1
        else
            match tryMakeTurn state sx sy i j with
            | None ->
                ignore 0
            | Some state1 ->
                let displayState1 = { displayState with State = state1; Selected = None; History = append [|state|] displayState.History }
                saveState displayState1
    print()

let onClickRevert (_ : MouseEvent) =
    let displayState = restoreState()
    match displayState.History with
    | [||] ->
        console.log("empty")
        ignore 0
    | arr ->
        let head = arr.[0]
        console.log("revert")
        let displayState1 = { displayState with State = head; History = tail arr; Selected = None }
        saveState displayState1
        print()

let addEvent state =
    for i = 0 to 3 do
        for j = 0 to 3 do
            document.getElementById("room-" + j.ToString() + "-" + i.ToString()).onclick <- (fun mouseEvent -> onClickSquare mouseEvent i j)

    for i = 0 to state.Rooms.[4].Length - 1 do
        document.getElementById("hallway-" + i.ToString()).onclick <- (fun mouseEvent -> onClickSquare mouseEvent 4 i)

    document.getElementById("revert").onclick <- onClickRevert

let init () =
    let state = prod()
    saveState { State = state; Selected = None; Warn = None; History = [||] };
    print()
    addEvent state

