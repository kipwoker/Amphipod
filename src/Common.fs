module Common

open Fable.Core.JS

let print obj =
    let msg = sprintf "%A" obj
    console.log(msg)