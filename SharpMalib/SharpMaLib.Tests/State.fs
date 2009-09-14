
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
open SharpMalib.StateMonad
                               
[<TestFixture>]
type StateTests =
    new() = {}
    
    [<Test>]
    member x.NoStateChange() =
        let f x = state { return x }
        quickCheck (Execute (f x) () = x)
        
//    [<Test>]
//    member x.StateInc() =
//        let f x = state { return x }
//        quickCheck (Execute (f x) () = x)
        
