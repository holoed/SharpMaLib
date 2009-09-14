
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