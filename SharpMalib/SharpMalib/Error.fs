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

namespace SharpMalib.Error
[<System.Runtime.CompilerServices.Extension>]
module ErrorMonad = 

    open SharpMalib.Basic.Combinators
    open SharpMalib.Utils
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
    let inline map f m = mapM error f m

    // m (m a) -> m a
    let inline join z = joinM error z       

    let throw e = Error e
    let catch f m = match m with 
                    | Error e -> f e
                    | _ -> m 

    let execute (Return x) = x

