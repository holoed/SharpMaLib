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

namespace SharpMalib.Tests
module ParserTests =
    open System
    open NUnit.Framework
    open SharpMalib.Parser.ParserMonad
    open SharpMaLib.Tests.NUnitFsCheck

    let (>>=) m f = parser.Bind (m, f)
    let unit = parser.Return
    let (++) p q = parser.Combine(p, q)
                                  
    let f x = Parser(fun s -> [(x, s)])

    let testCharParser p c y   =  let ys = y.ToString()                                       
                                  let result = parse p ys in 
                                  if (c y) then
                                     let [(x, xs)] = result in x = ys.[0]                          
                                  else 
                                     result = []

    [<TestFixture>]
    type ParserTests =
        new() = {}
        
        [<Test>]
        member this.Item() =
            Assert.AreEqual ([('H', "ello")], parse item "Hello")
            Assert.AreEqual ([], parse item "")

        [<Test>]
        member this.Result() = 
            Assert.AreEqual ([('X', "42")], parse (result 'X') "42")

        [<Test>]
        member this.Zero() =
            Assert.AreEqual ([] , parse zero "Hello")

        [<Test>]
        member this.DeterministicChoice() =
            let parserA = result 'X'
            let parserB = zero
            let input = "42"
            Assert.AreEqual (['X', "42"], parse (parserA +++ parserB) input)
            Assert.AreEqual (['X', "42"], parse (parserB +++ parserA) input)

        [<Test>]
        member this.Bind() = 
            let parser = result ['X'] >>=  fun s1 -> 
                         result ['Y'] >>=  fun s2 -> 
                         result ([s1] @ [s2])
            let ret = parse parser "42"
            Assert.AreEqual([([['X'];['Y']], "42")], ret)            

        [<Test>]
        member this.ReturnFrom() =
            let rec f x y  = parser { if (x > 0) then
                                        return! (List.head y) 
                                        return! (f (x - 1) (List.tail y) ) }
            let ret = f 2 ([char '4'; char '2'])
            let actual = parse ret "42"
            Assert.AreEqual( [('4', "2")], actual)

        [<Test>]
        member this.ZeroComputation() =
            Assert.AreEqual([], parse (parser { let! x = char 'H'
                                                if x <> 'H' then
                                                    return 42  }) "Hello")

        [<Test>]
        member this.Combine() =
            Assert.AreEqual([('H', "ello")], parse (parser { let! y = item 
                                                             if true then ()
                                                             return y}) "Hello")

        [<Test>]
        member this.Char() =
            Assert.AreEqual ([], parse (char 'X') "Hello")
            Assert.AreEqual ([('H', "ello")], parse (char 'H') "Hello")
            quickCheck (fun (y:char) -> testCharParser (char y) (fun x -> true) y)

        [<Test>]
        member this.Digit() =
            quickCheck (fun (y:int) -> testCharParser digit (fun x -> x >= 0) y)

        [<Test>]
        member this.Lower() =
            quickCheck (fun (y:char) -> testCharParser lower Char.IsLower y)

        [<Test>]
        member this.Upper() =
            quickCheck (fun (y:char) -> testCharParser upper Char.IsUpper y)

        [<Test>]
        member this.Letter() =
            quickCheck (fun (y:char) -> testCharParser letter Char.IsLetter y)

        [<Test>]
        member this.Word() =
            Assert.AreEqual([("Hello", " World")], parse word "Hello World")
            Assert.AreEqual([], parse word "42 World")

        [<Test>]
        member this.Number() =
            Assert.AreEqual([], parse number "Hello World")
            Assert.AreEqual(["42", " World"], parse number "42 World")

        [<Test>]
        member this.Stringp() =
            Assert.AreEqual([("Hello", " World")], parse word "Hello World")
            Assert.AreEqual([], parse word "42 World")

        [<Test>]
        member this.NonDeterministicChoice() =
            Assert.AreEqual ([('X', "Hello"); ('Y', "Hello")], parse (parser { return 'X' ; return 'Y' }) "Hello")

        [<Test>]
        [<Ignore("Still Working on this one")>]
        member this.BindDistributesOverChice() =
            // (p ++ q) >>= f = (p >>= f) ++ (q >>= f)
            quickCheck (fun p q f s -> parse ((p ++ q) >>= f) s = parse ((p >>= f) ++ (q >>= f)) s)            
