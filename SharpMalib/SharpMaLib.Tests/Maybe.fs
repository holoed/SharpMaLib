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
        quickCheck (fun x -> liftM id x = id x)
        quickCheck (fun x -> liftM (f << g) x = ((liftM f) << (liftM g)) x)
        quickCheck (fun x -> (liftM f << unit) x = (unit << f) x)
        quickCheck (fun x -> (liftM f << join) x = (join << (liftM (liftM f))) x)
        quickCheck (fun x -> (join << unit) x = id x)
        quickCheck (fun x -> (join << liftM unit) x = id x)
        quickCheck (fun x -> (join << liftM join) x = (join << join) x)
        quickCheck (fun m k -> m >>= k = join(liftM k m))

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

    [<Test>]
    member x.MZeroLeft () =
        quickCheck (fun f -> maybe.Bind(maybe.Zero(), f) = maybe.Zero())

    [<Test>]
    member x.MZeroRight () =
        quickCheck (fun f -> maybe.Bind(f, fun () -> maybe.Zero()) = maybe.Zero())

    [<Test>]
    member x.MZeroInIfExpression () =
        quickCheck (fun a -> maybe { if (a = true) then return 1 } = match a with
                                                                     | true  -> Some 1
                                                                     | false -> None )

    [<Test>]
    member x.MPlusOnYield () =
        quickCheck (fun (a, b, c) -> maybe { yield a
                                             yield b
                                             yield c } = Some a)

    [<Test>]
    member x.MPlusOnYieldFrom () =
        quickCheck (fun (a, b, c) -> maybe { yield! a
                                             yield! b
                                             yield! c } = match (a, b, c) with 
                                                          | (Some _, _, _) -> a 
                                                          | (_, Some _, _) -> b 
                                                          | (_, _, Some _) -> c
                                                          | _ -> None)

    [<Test>]
    member x.YieldAndBindToZero () =
        quickCheck (fun (a, b, c) -> maybe { yield! a
                                             yield! b
                                             do! None
                                             yield! c } = match (a, b, c) with 
                                                          | (Some _, _, _) -> a 
                                                          | (_, Some _, _) -> b 
                                                          //| (_, _, Some _) -> c // this should never hit, monad will short-circuit at do! None
                                                          | _ -> None)

    [<Test>]
    member x.MSum () =
        quickCheck (fun (a, b, c) -> msum [a; b; c] = match (a, b, c) with 
                                                        | (Some _, _, _) -> a 
                                                        | (_, Some _, _) -> b 
                                                        | (_, _, Some _) -> c
                                                        | _ -> None)