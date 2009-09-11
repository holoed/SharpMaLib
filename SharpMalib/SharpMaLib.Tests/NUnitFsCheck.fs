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

open System 
open FsCheck.Runner 
open NUnit.Framework 
 
let NUnitRunner = 
     { new IRunner with
        member x.OnArguments (ntest,args, every) = ()
        member x.OnShrink(args, everyShrink) = ()
        member x.OnFinished(name, result)=
            match result with
                | True data -> Assert.IsTrue(true);printfn "%A" data
                | False (_,args,_,FsCheck.Property.Outcome.False,_) -> Assert.Fail("{0}-Falsifiable: {1}", [|(name :> obj);(sprintf "%A" args :> obj)|])
                | False (_,args,_,FsCheck.Property.Outcome.Exception(exc),_) -> Assert.Fail("{0}-Falsifiable: {1}", [|(name :> obj);(sprintf "%A with exception:%O" args exc :>obj)|])
                | Exhausted data -> Assert.Inconclusive("Exhausted after {0} tests", [|(data.NumberOfTests :> obj)|])
     }
 
let nunitConfig = {quick with Runner = NUnitRunner}
let quickCheck x = check nunitConfig x