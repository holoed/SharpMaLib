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
module Identity = 

    open Utils
    open Combinators
    open System    

    // Identity Monad
    // The Identity monad is a monad that does not embody any computational strategy. 
    // It simply applies the bound function to its input without any modification. 
    // Computationally, there is no reason to use the Identity monad instead of the much simpler act of 
    // simply applying functions to their arguments.

    type IdentityBuilder() =
        // a -> m a
        member this.Return a = a
        //  m a -> (a -> m b) -> m b
        member this.Bind (a, f) = f a  
        
    let identity = IdentityBuilder()    

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM identity f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 identity identity f ma mb

    // m (m a) -> m a
    let inline join z = joinM identity z    

    
// C# Support
namespace MonadIdentityLinq
[<System.Runtime.CompilerServices.Extension>]
module Monad =

    open System
    open System.Runtime.CompilerServices    
    open Monad
    open Monad.Utils
    open Monad.Identity
    open Monad.LinqCombinators

    [<Extension>]
    let inline Select(m, f) = select identity f m
        
    [<Extension>]
    let inline SelectMany(m, f, p) = selectMany identity identity f p m
       
    [<Extension>]
    let inline Join m = join m

    [<Extension>]
    let inline Map (m, f:Converter<'a,'b>) = liftM (FuncConvert.ToFSharpFunc f) m