

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

module ContinuationTests

open NUnit.Framework
open NUnitFsCheck
open Monad.Continuation
open System

[<TestFixture>]
type ContinuationTests =
    new() = {}
    
    [<Test>]
    member x.ExampleOfUse() =
        
       let facCPS n =
                let rec loop n =
                  cont {
                          match n with
                          | n when n <= 0 -> return 1
                          | _ -> let! x = Cont(fun f -> f n)
                                 let! y = loop (n - 1)
                                 return x * y
                       }
                runCont (loop n) (fun x -> x)

       let product = List.fold (*) 1

       quickCheck (fun n -> facCPS n = product [1..n])
        

