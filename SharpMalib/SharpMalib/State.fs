
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
    member this.Return a = State(fun s -> a, s)
    member this.Bind (m, f) =  State (fun s -> let (v, s') = let (State f) = m in f s
                                               let (State f') = f v in f' s')  
   
let getState = State (fun s -> s, s)
let setState s = State (fun _ -> (), s)  

let Execute m s = let (State f) = m in
                  let (x,_) = f s in x
    
let state = StateBuilder() 


let rec foldr f v xs =
    state { match xs with
            | [] -> return v
            | h::t -> let! v'= (f v h)
                      return! foldr f v' t }
                      
let foldl f xs v = foldr (fun x y -> f y x) v (List.rev xs)

let map f xs = foldl (fun x y -> state { let! x' = f x
                                         return x' :: y }) xs []
                                         
// C# Support

[<Extension>]
let Select(m, f : Func<'a,'b>) = state.Bind(m, fun x -> state.Return (f.Invoke(x)))      
    
[<Extension>]
let SelectMany(m, f : Func<'a, State<'b,'s>>, projection : Func<'a, 'b, 'c>)  = 
   state.Bind (m, (fun x -> let (State g) = f.Invoke(x) in
                            State(fun s -> let (y, s') = 
                                                let (State g) = f.Invoke(x) in
                                                g s
                                           projection.Invoke(x, y), s')))