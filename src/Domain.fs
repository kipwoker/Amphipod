module Domain

open Common

type State = {
    // rooms first -> hallway last
    Rooms : int option[][]
    Score : int
    MinScore : int
    IterationsCount: int
}

let append a b = Array.append a b

let tail a = Array.tail a

let convert a =
    match a with
    | 'A' -> 1
    | 'B' -> 10
    | 'C' -> 100
    | 'D' -> 1000
    | _ -> 0
    |> Some

let prod() = {
    Rooms = [|
        [|'B'; 'D'; 'D'; 'C'|] |> Array.map convert
        [|'D'; 'C'; 'B'; 'D'|] |> Array.map convert
        [|'C'; 'B'; 'A'; 'B'|] |> Array.map convert
        [|'A'; 'A'; 'C'; 'A'|] |> Array.map convert
        Array.init 11 (fun _ -> None)
    |]
    Score = 0
    MinScore = 900000
    IterationsCount = 0
}

let getLetter a =
    match a with
    | Some a1 ->
        match a1 with
        | 1 -> "A"
        | 10 -> "B"
        | 100 -> "C"
        | 1000 -> "D"
        | _ -> "ERROR"
    | _ -> ""

let sumOrDie lst =
    lst
    |> List.fold (fun sum x ->
        match sum, x() with
        | None, _ -> None
        | _, None -> None
        | Some s, Some x' -> s + x' |> Some
    ) (Some 0)

let isDoor x = x = 2 || x = 4 || x = 6 || x = 8

let getTargetRoom weight =
    match weight with
    | 1 -> 0
    | 10 -> 1
    | 100 -> 2
    | 1000 -> 3
    | _ -> -1

let getHallIndex roomIndex =
    match roomIndex with
    | 0 -> 2
    | 1 -> 4
    | 2 -> 6
    | 3 -> 8
    | _ -> -1

let hasObstacle state offset length = Array.sub state.Rooms.[4] offset length |> Array.exists(fun x -> x.IsSome)

let getCostInHallway state y1 y2 p =
    let min1 = min y1 y2
    let max1 = max y1 y2
    let length = max1 - min1 + 1
    let hasObstacle1 = hasObstacle state min1 length
    if hasObstacle1 then
        print ("min1", min1)
        print ("max1", max1)
        print ("length", length)
        print "hasObstacle"
        None
    else
        p * length |> Some

let getLeaveRoomCost state x1 y1 p =
    let isBusy() = state.Rooms.[x1] |> Array.take y1 |> Array.exists (fun x -> x.IsSome)
    let target = getTargetRoom p
    let isAlreadyThere() = state.Rooms.[x1] |> Array.exists (fun x -> x.IsSome && x.Value <> target) |> not

    if isBusy() || isAlreadyThere() then
        print "busy"
        None
    else
        y1 * p |> Some

let getResideInTheRoomCost state target p =
    let actualTarget = getTargetRoom p
    let emptyPlaces = state.Rooms.[target] |> Array.takeWhile (fun c -> c.IsNone)
    if emptyPlaces.Length = 0 || actualTarget <> target then
        if emptyPlaces.Length = 0 then
            print "emptyPlaces"
        if actualTarget <> target then
            print "wrongTarget"
        None
    else
        emptyPlaces.Length * p |> Some

let getCostBetweenRooms state x1 y1 x2 p =
    let target = getTargetRoom p
    print ("target", target)
    print ("x2", target)
    if x2 <> target then
        None
    else
        let leaveRoomCost() = getLeaveRoomCost state x1 y1 p
        let startHallIndex = getHallIndex x1
        let endHallIndex = getHallIndex x2
        let hallwayWalkCost() = getCostInHallway state startHallIndex endHallIndex p
        let resideInTheRoomCost() = getResideInTheRoomCost state target p
        sumOrDie [hallwayWalkCost; resideInTheRoomCost; leaveRoomCost]

let reduceIndex x y = if x > y then x - 1 else x + 1

let getCost state x1 y1 x2 y2 =
    if (x1 = x2 && x1 <> 4) || (x1 = x2 && y1 = y2) || state.Rooms.[x2].[y2].IsSome then
        None
    else
        let weight = state.Rooms.[x1].[y1]
        match weight with
        | None -> None
        | Some weight1 ->
            match x1 with
            | 4 ->
                match x2 with
                | 4 ->
                    None
//                    if isDoor y2 then
//                        None
//                    else
//                        let y1r = reduceIndex y1 y2
//                        getCostInHallway state y1r y2 weight1
                | _ ->
                    let hallwayIndex = getHallIndex x2
                    let y1r = reduceIndex y1 hallwayIndex
                    let hallwayWalkCost() = getCostInHallway state y1r hallwayIndex weight1
                    let resideInTheRoomCost() = getResideInTheRoomCost state x2 weight1
                    sumOrDie [hallwayWalkCost; resideInTheRoomCost]
            | _ ->
                match x2 with
                | 4 ->
                    let leaveRoomCost() = getLeaveRoomCost state x1 y1 weight1
                    let hallwayIndex = getHallIndex x1
                    if isDoor y2 then
                        None
                    else
                        let hallwayWalkCost() = getCostInHallway state hallwayIndex y2 weight1
                        sumOrDie [leaveRoomCost; hallwayWalkCost]
                | _ -> getCostBetweenRooms state x1 y1 x2 weight1

let copy rooms = rooms |> Array.map Array.copy

let isWin state =
    let full = state.Rooms
                |> Array.take 4
                |> Array.mapi (fun i r -> r |> Array.filter (fun x -> x.IsSome && getTargetRoom x.Value = i) |> Array.length)
                |> Array.filter (fun x -> x = 4)
    full.Length = 4

let tryMakeTurn state x1 y1 x2 y2 =
    let cost = getCost state x1 y1 x2 y2
    match cost with
    | None -> None
    | Some cost1 ->
        let rooms = copy state.Rooms
        let item = rooms.[x1].[y1]
        rooms.[x1].[y1] <- None
        rooms.[x2].[y2] <- item
        let state1 = { state with Rooms = rooms; Score = state.Score + cost1; MinScore = state.MinScore }
        if isWin state1 then
            let minScore = min state1.MinScore state1.Score
            { prod() with MinScore = minScore } |> Some
        else
            state1 |> Some