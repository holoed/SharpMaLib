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

namespace Monad
module Maybe = 

    open Combinators
    open Utils

    // Maybe Monad
    // The Maybe monad embodies the strategy of combining a chain of computations  
    // that may each return Nothing by ending the chain early if any step produces Nothing as output. 
    // It is useful when a computation entails a sequence of steps that depend on one another, 
    // and in which some steps may fail to return a value.

    type MaybeBuilder() =
        // a -> m a
        member this.Return a = Some a
        //  m a -> (a -> m b) -> m b
        member this.Bind (m, f) = match m with
                                  | Some x -> f x
                                  | _ -> None
        
    let maybe = MaybeBuilder()   

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM maybe f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 maybe maybe f ma mb

    // m (m a) -> m a
    let inline join z = joinM maybe z                               

// C# Support
namespace MonadMaybeLinq
[<System.Runtime.CompilerServices.Extension>]
module Monad =

    open System
    open System.Runtime.CompilerServices    
    open Monad
    open Monad.Utils
    open Monad.Maybe
    open Monad.LinqCombinators

    [<Extension>]
    let inline Select (m, f) = select maybe f m
        
    [<Extension>]
    let inline SelectMany(m, f, p) = selectMany maybe maybe f p m

    [<Extension>]
    let Just x = Some x

    [<Extension>]
    let Join m = join m

    [<Extension>]
    let inline Map (m, f:Converter<'a,'b>) = liftM (FuncConvert.ToFSharpFunc f) m

    [<Extension>]
    let IsSome (m : option<'a>) = m.IsSome

    [<Extension>]
    let IsNone (m : option<'a>) = m.IsNone