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

    [<TestFixture>]
    type ParserTests =
        new() = {}
        
        [<Test>]
        member this.Item() =
            Assert.AreEqual ([('H', ['e';'l';'l';'o'])], parse item "Hello")
            Assert.AreEqual ([], parse item "")

        [<Test>]
        member this.Ret() = 
            Assert.AreEqual ([('X', ['4';'2'])], parse (ret 'X') "42")

        [<Test>]
        member this.Fail() =
            Assert.AreEqual ([] , parse fail "Hello")

        [<Test>]
        member this.DeterministicChoice() =
            let parserA = ret 'X'
            let parserB = fail
            let input = "42"
            Assert.AreEqual (['X', ['4';'2']], parse (parserA +++ parserB) input)
            Assert.AreEqual (['X', ['4';'2']], parse (parserB +++ parserA) input)

        [<Test>]
        member this.Bind() = 
            let parser = ret ['X'] >>=  fun s1 -> 
                         ret ['Y'] >>=  fun s2 -> 
                         ret ([s1] @ [s2])
            let ret = parse parser "42"
            Assert.AreEqual([([['X'];['Y']], ['4';'2'])], ret)            

        [<Test>]
        member this.ReturnFrom() =
            let rec f x y  = parser { if (x > 0) then
                                        return! (List.head y) 
                                        return! (f (x - 1) (List.tail y) ) }
            let ret = f 2 ([sat '4'; sat '2'])
            let actual = parse ret "42"
            Assert.AreEqual( [('4', ['2'])], actual)

        [<Test>]
        member this.Sat() =
            Assert.AreEqual ([], parse (sat 'X') "Hello")
            Assert.AreEqual ([('H', ['e';'l';'l';'o'])], parse (sat 'H') "Hello")

        [<Test>]
        member this.StringP() =
            Assert.AreEqual([], parse (stringp (Seq.toList "Hello")) "World")
            Assert.AreEqual([(['H';'e';'l';'l';'o'], [' ';'W';'o';'r';'l';'d'])], parse (stringp (Seq.toList "Hello")) "Hello World")

        [<Test>]
        member this.NonDeterministicChoice() =
            Assert.AreEqual ([('X', ['H';'e';'l';'l';'o']); ('Y', ['H';'e';'l';'l';'o'])], parse (parser { return 'X' ; return 'Y' }) "Hello")

        [<Test>]
        [<Ignore("Still Working on this one")>]
        member this.BindDistributesOverChice() =
            // (p ++ q) >>= f = (p >>= f) ++ (q >>= f)
            quickCheck (fun p q f s -> parse ((p ++ q) >>= f) s = parse ((p >>= f) ++ (q >>= f)) s)            
