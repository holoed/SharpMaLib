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

module MaybeTests

open NUnit.Framework
open NUnitFsCheck
open Monad.Maybe

let (>>=) m f = maybe.Bind (m, f)
let unit = maybe.Return

let f x = if x > 0 then Some x else None 

[<TestFixture>]
type MaybeTests() =
    
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
    member x.OneMaybe() =
        quickCheck (fun x -> maybe  { let! x' = f x 
                                      return x' } = f x)
                                      
    [<Test>]
    member x.CombineTwoMaybesBothSucceedOrFail() =
        quickCheck (fun x -> maybe  { let! x' = f x 
                                      let! y' = f x 
                                      return x' + y' } = match (f x, f x) with
                                                         | (Some x', Some y') -> Some(x' + y')
                                                         | (None, None) -> None
                                                         | _ -> failwith "This should never happen")

    [<Test>]
    member x.CombineTwoMaybesOneFails() =
        quickCheck (fun x -> maybe  { let! x' = (f -x) 
                                      let! y' = f x 
                                      return x' + y' } = match (f x, f x) with
                                                         | (_, _) -> None)
                                                         

                                                         
