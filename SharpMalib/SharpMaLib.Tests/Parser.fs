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
    open FsCheck
    open SharpMalib.Parser.ParserMonad

    let (>>=) m f = parser.Bind (m, f)
    let unit = parser.Return
                                  
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
