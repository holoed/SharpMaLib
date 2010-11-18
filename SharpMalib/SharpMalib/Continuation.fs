// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo, Ryan Riley 
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

    type Cont<'a, 'r> = Cont of (('a -> 'r) -> 'r)

    let runCont (Cont c) f = c f
    type ContinuationMonad() =
        // a -> ma
        member this.Return(x) = Cont (fun c -> c x)
    
        // ma -> ma
        member this.ReturnFrom(m: Cont<_,_>) = m
    
        // ma -> (a -> mb) -> mb
        member this.Bind(m, k) = Cont (fun c -> runCont m (fun x -> runCont (k x) c))
    
        member this.Zero() = this.Return()
    
        member this.TryWith(m, h) =
          Cont (fun k -> try runCont m k
                         with e -> runCont (h e) k)
    
        member this.TryFinally(m, compensation) =
          Cont (fun k -> try runCont m k
                         finally compensation())
    
        member this.Using(res:#System.IDisposable, body) =
          this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
    
        member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
    
        member this.Delay(f) = this.Bind(this.Return(), f)
    
        member this.While(guard, m) =
          if not(guard()) then this.Zero() else
            this.Bind(m, (fun () -> this.While(guard, m)))
    
        member this.For(sequence:seq<_>, body) =
          this.Using(sequence.GetEnumerator(),
                     (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    
    let cont = ContinuationMonad()
    
    // Call-with-current-continuation: experimental
    let callCC f = Cont(fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k)
    