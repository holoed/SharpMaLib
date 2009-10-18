
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

namespace SharpMalib.State
[<System.Runtime.CompilerServices.Extension>]
module StateMonad

open Utils
open System
open System.Runtime.CompilerServices

// State Monad
// A pure functional language cannot update values in place because it violates referential transparency. 
// A common idiom to simulate such stateful computations is to "thread" a state parameter through a sequence of functions.
// This approach works, but such code can be error-prone, messy and difficult to maintain. 
// The State monad hides the threading of the state parameter inside the binding operation, 
// simultaneously making the code easier to write, easier to read and easier to modify.

type State<'a, 'state> = State of ('state -> 'a * 'state)

type StateBuilder() =
    // a -> m a
    member this.Return a = State(fun s -> a, s)
    //  m a -> (a -> m b) -> m b
    member this.Bind (m, f) =  State (fun s -> let (v, s') = let (State f) = m in f s
                                               let (State f') = f v in f' s')  
                                               
                                    
let getState = State (fun s -> s, s)
let setState s = State (fun _ -> (), s)  

let Execute m s = let (State f) = m in
                  let (x,_) = f s in x

let state = StateBuilder() 

// (a -> b) -> m a -> m b
let map f m = state.Bind(m, fun x -> x |> f |> state.Return) 

// m (m a) -> m a
let join m = state.Bind(m, id)

// (a -> b -> m a) -> a -> [b] -> m a     
let rec foldr f v xs =
    state { match xs with
            | [] -> return v
            | h::t -> let! v'= (f v h)
                      return! foldr f v' t }
                      
// (a -> b -> m b) -> [a] -> b -> m b
let foldl f xs v = foldr (fun x y -> f y x) v (List.rev xs)

// (a -> m b) -> [a] -> m [b]
let mapList f xs = foldl (fun x y -> state { let! x' = f x
                                             return x' :: y }) xs []
                                         
// C# Support

[<Extension>]
let Select(m, f) = state.Bind(m, fun x -> state.Return (applyFunc f x))      
    
[<Extension>]
let SelectMany(m, f, p)  = 
   state.Bind (m, (fun x -> let (State g) = (applyFunc f x) in
                            State(fun s -> let (y, s') = g s
                                           applyFunc2 p x y, s')))
                                           