
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

type State<'a, 'state> = State of ('state -> 'a * 'state)

module State =

    open Utils
    open Combinators
    open System    

    // State Monad
    // A pure functional language cannot update values in place because it violates referential transparency. 
    // A common idiom to simulate such stateful computations is to "thread" a state parameter through a sequence of functions.
    // This approach works, but such code can be error-prone, messy and difficult to maintain. 
    // The State monad hides the threading of the state parameter inside the binding operation, 
    // simultaneously making the code easier to write, easier to read and easier to modify.

    let runState (State s) initialState = s initialState
    
    type StateBuilder() =
        // a -> m a
        member this.Return a = State(fun s -> (a, s))

        //  m a -> (a -> m b) -> m b
        member this.Bind(m, f) = State (fun s -> let (v, s') = runState m s in runState (f v) s')

        // a -> a
        member this.ReturnFrom a = a

        member this.Zero() = this.Return ()
    
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
    
        member this.TryWith(m, h) =
          State (fun env -> try runState m env
                            with e -> runState (h e) env)
    
        member this.TryFinally(m, compensation) =
          State (fun env -> try runState m env
                            finally compensation())
    
        member this.Using(res:#IDisposable, body) =
          this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
    
        member this.Delay(f) = this.Bind(this.Return (), f)
    
        member this.While(guard, m) =
          if not(guard()) then this.Zero() else
            this.Bind(m, (fun () -> this.While(guard, m)))
    
        member this.For(sequence:seq<_>, body) =
          this.Using(sequence.GetEnumerator(),
                     (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
                                        
    let getState = State (fun s -> (s, s))
    let setState s = State (fun _ -> ((), s))
    let eval m s = runState m s |> fst
    let exec m s = runState m s |> snd

    let Execute m s = eval m s

    let state = StateBuilder() 

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM state f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 state state f ma mb

    // m (m a) -> m a
    let inline join z = joinM state z

    // (a -> b -> m a) -> a -> [b] -> m a     
    let rec foldr f v xs =
        state { match xs with
                | [] -> return v
                | h::t -> let! v'= (f v h)
                          return! foldr f v' t }
                          
    // (a -> b -> m b) -> [a] -> b -> m b
    let foldl f xs v = foldr (fun x y -> f y x) v (List.rev xs)

    // (a -> m b) -> [a] -> m [b]
    let mapList f xs = foldl (fun x y -> state { let! x' = f x
                                                 return x' :: y }) xs []
                                                 

// C# Support
namespace MonadStateLinq
[<System.Runtime.CompilerServices.Extension>]
module Monad =

    open System.Runtime.CompilerServices    
    open Monad
    open Monad.Utils
    open Monad.State
    open Monad.LinqCombinators

    [<Extension>]
    let inline Select (m, f) = select state f m
        
    [<Extension>]
    let inline SelectMany(m, f, p)  = selectMany state state f p m

    [<Extension>]
    let Join m = join m
                                               