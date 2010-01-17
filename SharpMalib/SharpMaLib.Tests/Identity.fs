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

namespace SharpMaLib.Tests
module IdentityTests = 

    open NUnit.Framework
    open SharpMaLib.Tests.NUnitFsCheck
    open SharpMalib.Identity.IdentityMonad

    let (>>=) m f = identity.Bind (m, f)
    let unit = identity.Return

    [<TestFixture>]
    type IdentityTests =
        new() = {}
        
        [<Test>]
        member x.MonadLaws() =
            // Left unit  
            quickCheck (fun m f a -> ((unit a) >>= f) = f a) 
            // Right unit
            quickCheck (fun m  -> (m >>= unit) = m)
            // Associative
            quickCheck (fun m f g-> ((m >>= f) >>= g) = (m >>= (fun x -> f x >>= g)))
            
        [<Test>]
        member x.MonadLawsInTermsOfMapAndJoin() =
            let f x = x / 2
            let g x = x - 2
            quickCheck (fun x -> map id x = id x)
            quickCheck (fun x -> map (f << g) x = ((map f) << (map g)) x)
            quickCheck (fun x -> (map f << unit) x = (unit << f) x)
            quickCheck (fun x -> (map f << join) x = (join << (map (map f))) x)
            quickCheck (fun x -> (join << unit) x = id x)
            quickCheck (fun x -> (join << map unit) x = id x)
            quickCheck (fun x -> (join << map join) x = (join << join) x)
            quickCheck (fun m k -> m >>= k = join(map k m))
            

        [<Test>]
        member x.IdentityDoesntPerformAnyExtraComputation() =
            quickCheck (fun x y -> identity  { let! x' = x
                                               let! y' = y
                                               return x' + y' } = x + y)
