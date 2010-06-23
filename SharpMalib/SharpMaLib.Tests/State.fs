
// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module StateTests

open System
open NUnit.Framework
open NUnitFsCheck
open Monad.State

let equals expected actual = (expected = actual) || (Double.IsNaN(expected) || Double.IsNaN(actual))
                                 
[<TestFixture>]
type StateTests =
    new() = {}
        
    [<Test>]
    member o.NoState() =
        let f x = state { return x }
        quickCheck (fun x -> Execute (f x) () = x)
             
    [<Test>]
    member o.getState() =        
        let f x = state { let! s = getState 
                          return x + s }
        quickCheck (fun x -> Execute (f x) 42 = x + 42) 

    [<Test>]
    member o.setState() =   
        let rec mcount xs = state { let! s = getState
                                    match xs with
                                    | [] -> return s
                                    | x::xs' -> do! setState (s + 1)
                                                return! mcount xs' }
        quickCheck (fun x -> Execute (mcount [1..x]) 0 = List.length [1..x]) 
        
    [<Test>]
    member o.foldr() =
        quickCheck (fun xs (v:float) -> 
            let expected = List.fold (/) v xs
            let actual = Execute (foldr (fun x y -> state { return (/) x y }) v xs) ()
            expected |> equals <| actual)
            
    [<Test>]
    member o.foldl() =
        quickCheck (fun xs (v:float) -> 
            let expected = List.foldBack (/) xs v 
            let actual = Execute (foldl (fun x y -> state { return (/) x y }) xs v) ()
            expected |> equals <| actual)
            
    [<Test>]
    member o.foldProperty() =
        quickCheck(fun xs v -> ((+) v << List.sum) xs = Execute (foldr (fun x y -> state { return x + y }) v xs) ())

    [<Test>]
    member o.map() =        
        let f x = state { let! s = getState
                          return x + s }     
        let gm xs = Execute (mapList f xs) 42
        let g xs = List.map (fun x -> x + 42) xs
        quickCheck (fun x -> g [0..x] = gm [0..x])