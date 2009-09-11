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

open NUnit.Framework
open FsCheck
open NUnitFsCheck
open SharpMalib.IdentityMonad

let (>>=) m f = identity.Bind (m, f)
let unit = identity.Return

[<TestFixture>]
type IdentityTests =
    new() = {}
    
    [<Test>]
    member x.MonadLaws() =
        // Left unit  
        quickCheck (fun m f a -> ((unit a) >>= f) = f a) 
        // Left unit
        quickCheck (fun m  -> (m >>= unit) = m)
        // Associative
        quickCheck (fun m f g-> ((m >>= f) >>= g) = (m >>= (fun x -> f x >>= g)))

    [<Test>]
    member x.MonadLawsUsingSugarSyntax() =
        quickCheck (fun x y -> identity  { let! x' = x
                                           let! y' = y
                                           return x' + y' } = x + y)
