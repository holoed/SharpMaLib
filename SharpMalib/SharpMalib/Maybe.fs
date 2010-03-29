﻿// * **********************************************************************************************
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
module MaybeMonad = 

    open SharpMalib.Basic.Combinators
    open SharpMalib.Utils
    open System
    open System.Runtime.CompilerServices

    // Maybe Monad
    // The Maybe monad embodies the strategy of combining a chain of computations  
    // that may each return Nothing by ending the chain early if any step produces Nothing as output. 
    // It is useful when a computation entails a sequence of steps that depend on one another, 
    // and in which some steps may fail to return a value.

    type 'a Maybe = Nothing | Just of 'a

    type MaybeBuilder() =
        // a -> m a
        member this.Return a = Just a
        //  m a -> (a -> m b) -> m b
        member this.Bind (m, f) = match m with
                                  | Just x -> f x
                                  | _ -> Nothing
        
    let maybe = MaybeBuilder()   

    // (a -> b) -> m a -> m b
    let inline map f m = mapM maybe f m

    // m (m a) -> m a
    let inline join z = joinM maybe z                               

    // C# Support

    [<Extension>]
    let inline Select (m, f) = map (applyFunc f) m
        
    [<Extension>]
    let SelectMany(m, f, p) = 
       maybe.Bind (m, (fun x -> match (applyFunc f x) with
                                | Just y -> Just(applyFunc2 p x y)
                                | Nothing -> Nothing))
    [<Extension>]
    let Just x = Just x

    [<Extension>]
    let Join m = join m

    [<Extension>]
    let inline Map (m, f:Converter<'a,'b>) = map (FuncConvert.ToFSharpFunc f) m