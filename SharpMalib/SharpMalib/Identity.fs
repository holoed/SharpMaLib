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

namespace SharpMalib.Identity
[<System.Runtime.CompilerServices.Extension>]
module IdentityMonad

open Utils
open System
open System.Runtime.CompilerServices

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
let map f m = identity.Bind(m, fun x -> x |> f |> identity.Return)      

// m (m a) -> m a
let join z = identity.Bind(z, id)    

// C# Support

[<Extension>]
let Select(m, f) = map (applyFunc f) m
    
[<Extension>]
let SelectMany(m, f, p) = 
   identity.Bind (m, (fun x -> x |> applyFunc f |> applyFunc2 p x))