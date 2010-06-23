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
module Continuation = 

    open Combinators
    open Utils

    // Continuation Monad
    // Continuations represent the future of a computation, as a function from an intermediate result to the final result. 
    // In continuation-passing style, computations are built up from sequences of nested continuations, 
    // terminated by a final continuation (often id) which produces the final result. Since continuations are functions which represent the future of a computation, 
    // manipulation of the continuation functions can achieve complex manipulations of the future of the computation, such as interrupting a computation in the middle, 
    // aborting a portion of a computation, restarting a computation and interleaving execution of computations. 
    // The Continuation monad adapts CPS to the structure of a monad.

    type ContinuationMonad() =
        // ma -> (a -> mb) -> mb
        member this.Bind (m, f) = fun c -> m (fun a -> f a c)
        // a -> ma
        member this.Return x = fun k -> k x
        // ma -> ma
        member this.ReturnFrom m = m

    let cont = ContinuationMonad()

   
    

