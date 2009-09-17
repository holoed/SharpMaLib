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
open SharpMalib.Maybe.MaybeMonad

let (>>=) m f = maybe.Bind (m, f)
let unit = maybe.Return

let f x = if x > 0 then Just x else Nothing 

[<TestFixture>]
type MaybeTests =
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
    member x.OneMaybe() =
        quickCheck (fun x -> maybe  { let! x' = f x 
                                      return x' } = f x)
                                      
    [<Test>]
    member x.CombineTwoMaybesBothSucceedOrFail() =
        quickCheck (fun x -> maybe  { let! x' = f x 
                                      let! y' = f x 
                                      return x' + y' } = match (f x, f x) with
                                                         | (Just x', Just y') -> Just(x' + y')
                                                         | (Nothing, Nothing) -> Nothing
                                                         | _ -> failwith "This should never happen")

    [<Test>]
    member x.CombineTwoMaybesOneFails() =
        quickCheck (fun x -> maybe  { let! x' = (f -x) 
                                      let! y' = f x 
                                      return x' + y' } = match (f x, f x) with
                                                         | (_, _) -> Nothing)
                                                         
