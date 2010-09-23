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

module Combinators =

    let inline joinM b m =
        let (>>=) m f = (^x: (member Bind: ^m -> (^n -> ^n) -> ^n) b, m, f)
        m >>= id


    // Monad m => (a -> b) -> m a -> m b
    let inline liftM m f ma =
 
        let unit x = (^m: (member Return: ^b -> ^mb) m, x)
        let (>>=) m_a f = (^m: (member Bind: ^ma -> (^a -> ^mb) -> ^mb) m, ma, f)
 
        ma >>= (fun a -> unit (f a))

    //  Monad m /* m1 m2 */ => (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 m1 m2 f ma mb =
 
        let bind1 ma f = (^m1: (member Bind: ^ma -> (^a -> ^mc) -> ^mc) m1, ma, f)
 
        let unit2 x = (^m2: (member Return: ^c -> ^mc) m2, x)
        let bind2 mb f = (^m2: (member Bind: ^mb -> (^b -> ^mc) -> ^mc) m2, mb, f)
 
        bind1 ma (fun a1 -> bind2 mb (fun a2 -> unit2 (f a1 a2)))

    // msum :: MonadPlus m => [m a] -> m a
    //TODO: Sould it use Delay? Sould it use left or right folding?
    let inline msum m maList = 
        let mplus a b = (^x: (member mplus: ^m -> ^m -> ^m) m, a, b)
        let mzero = (^x: (member mzero: unit -> ^m) m)

        maList |> List.fold mplus mzero

// С# support
module LinqCombinators =
    
    open Combinators
    open Utils

    // Monad m => (a -> b) -> m a -> m b
    let inline select m f ma = liftM m (applyFunc f) ma
 
    // Monad m /* m1 m2 */ => (a -> m b) -> (a -> b -> c) -> m a -> m c
    let inline selectMany m1 m2 selector projector ma =
 
        let bind1 ma f = (^m1: (member Bind: ^ma -> (^a -> ^mc) -> ^mc) m1, ma, f)
 
        let unit2 x = (^m2: (member Return: ^c -> ^mc) m2, x)
        let bind2 mb f = (^m2: (member Bind: ^mb -> (^b -> ^mc) -> ^mc) m2, mb, f)

        bind1 ma (fun a -> let mb = applyFunc selector a in
                           bind2 mb (fun b -> unit2 (applyFunc2 projector a b)))