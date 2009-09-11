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
module MaybeMonad

// Maybe Monad
// The Maybe monad embodies the strategy of combining a chain of computations  
// that may each return Nothing by ending the chain early if any step produces Nothing as output. 
// It is useful when a computation entails a sequence of steps that depend on one another, 
// and in which some steps may fail to return a value.

type Maybe<'a> = Nothing | Just of 'a

type MaybeBuilder() =
    member this.Return a = Just a
    member this.Bind (m, f) = match m with
                              | Just x -> f x
                              | _ -> Nothing
    
let maybe = MaybeBuilder()                                
