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
module Error = 

    open Combinators
    open Utils
    open System
    open System.Runtime.CompilerServices

    // The Error monad (also called the Exception monad) 
    // embodies the strategy of combining computations that can throw exceptions by bypassing 
    // bound functions from the point an exception is thrown to the point that it is handled. 

    type Error<'a, 'b> = Return of 'a | Error of 'b

    type ErrorMonad() =
        // a -> m a
        member o.Return x = Return x
        // m a -> (a -> m b) -> m b
        member o.Bind (m, f) = match m with
                               | Error e -> Error e
                               | Return x -> f x
        // m a -> m a
        member o.ReturnFrom x = x

        member o.Zero () = Return ()

        member o.Combine (e1, e2) = o.Bind (e1, (fun () -> e2))

        member o.Delay f = o.Bind(o.Zero (), f)
                               

    let error = ErrorMonad()

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM error f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 error error f ma mb

    // m (m a) -> m a
    let inline join z = joinM error z       

    let throw e = Error e
    let catch f m = match m with 
                    | Error e -> f e
                    | _ -> m 

    let execute (Return x) = x

