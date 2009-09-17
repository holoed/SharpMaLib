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

namespace SharpMalib.Maybe
[<System.Runtime.CompilerServices.Extension>]
module MaybeMonad

open System
open System.Runtime.CompilerServices

// Maybe Monad
// The Maybe monad embodies the strategy of combining a chain of computations  
// that may each return Nothing by ending the chain early if any step produces Nothing as output. 
// It is useful when a computation entails a sequence of steps that depend on one another, 
// and in which some steps may fail to return a value.

type 'a Maybe = Nothing | Just of 'a

type MaybeBuilder() =
    member this.Return a = Just a
    member this.Bind (m, f) = match m with
                              | Just x -> f x
                              | _ -> Nothing
    
let maybe = MaybeBuilder()   

let map f m = maybe.Bind(m, fun x -> maybe.Return (f x))                                  

// C# Support

[<Extension>]
let Select(m, f : Func<'a,'b>) = map f.Invoke m
    
[<Extension>]
let SelectMany(m:'a Maybe, f : Func<'a, 'b Maybe>, projection : Func<'a, 'b, 'c>) = 
   maybe.Bind (m, (fun x -> match f.Invoke(x) with
                            | Just y -> Just(projection.Invoke(x, y))
                            | Nothing -> Nothing))

[<Extension>]
let Just(x) = Just x

