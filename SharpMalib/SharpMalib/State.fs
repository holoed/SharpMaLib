
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

namespace SharpMalib
module StateMonad

// State Monad

type State<'state,'a> = State of ('state -> 'a * 'state)

type StateBuilder() =
    member this.Return a = State(fun s -> a, s)
    member this.Bind (State x, f) = State(fun s -> let (y, s') = x s in (f y) s') 

let get = State(fun s -> (s,s))
let put s = State(fun _ -> ((), s))
    
let state = StateBuilder()                                
