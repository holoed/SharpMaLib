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

namespace SharpMalib
module IdentityMonad

// Identity Monad
// The Identity monad is a monad that does not embody any computational strategy. 
// It simply applies the bound function to its input without any modification. 
// Computationally, there is no reason to use the Identity monad instead of the much simpler act of 
// simply applying functions to their arguments.

type IdentityBuilder() =
    member this.Return a = a
    member this.Bind (m, f) = f m   
    
let identity = IdentityBuilder()                                