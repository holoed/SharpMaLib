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

module ErrorTests

open NUnit.Framework
open NUnitFsCheck
open Monad.Error

let (>>=) m f = error.Bind (m, f)
let unit = error.Return

[<TestFixture>]
type ErrorTests =
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
        quickCheck (fun x -> liftM id x = id x)
        quickCheck (fun x -> liftM (f << g) x = ((liftM f) << (liftM g)) x)
        quickCheck (fun x -> (liftM f << unit) x = (unit << f) x)
        quickCheck (fun x -> (liftM f << join) x = (join << (liftM (liftM f))) x)
        quickCheck (fun x -> (join << unit) x = id x)
        quickCheck (fun x -> (join << liftM unit) x = id x)
        quickCheck (fun x -> (join << liftM join) x = (join << join) x)
        quickCheck (fun m k -> m >>= k = join(liftM k m))
    
    [<Test>]
    member x.ThrowAndCatch() = 
        let f x = (error {
                                // Statements in which errors might be thrown
                                if (x > 0) then 
                                    return! throw "This is an Error"  
                                else                                                                             
                                    return x
                            }) 
                  |> catch (fun ex -> 
                    error {
                                // Statements that execute in the event of an exception, with 'ex' bound to the exception
                                Assert.AreEqual ("This is an Error", ex)
                                return 42
                           })
        Assert.AreEqual (-1,  execute (f -1))
        Assert.AreEqual (42,  execute (f 5))

        
  
        