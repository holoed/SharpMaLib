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
    open NUnit.Framework
    open FsCheck
    open SharpMalib.Parser.ParserMonad

    let (>>=) m f = parser.Bind (m, f)
    let unit = parser.Return

    let f x = Parser(fun s -> Some (x, s))

    type Helper =
     static member Some (a, s:string) = Some (a, Seq.toList s)
     static member Some (x:string, y:string) = Some (Seq.toList x, Seq.toList y)

    [<TestFixture>]
    type ParserTests =
        new() = {}
        
        [<Test>]
        member this.Item() =
            Assert.AreEqual (Helper.Some ('H', "ello"), parse item "Hello")
            Assert.AreEqual (None, parse item "")

        [<Test>]
        member this.Ret() = 
            Assert.AreEqual (Helper.Some ('X', "42"), parse (ret 'X') "42")

        [<Test>]
        member this.Fail() =
            Assert.AreEqual (None , parse fail "Hello")

        [<Test>]
        member this.Or() =
            let parserA = ret 'X'
            let parserB = fail
            let input = "42"
            let assertValue = fun actual -> Assert.AreEqual (Helper.Some ('X', input), actual)
            assertValue (parse (parserA +++ parserB) input)
            assertValue (parse (parserB +++ parserA) input)

        [<Test>]
        member this.Sat() =
            Assert.AreEqual (None, parse (sat 'X') "Hello")
            Assert.AreEqual (Helper.Some ('H', "ello"), parse (sat 'H') "Hello")

        [<Test>]
        member this.StringP() =
            Assert.AreEqual(None, parse (stringp (Seq.toList "Hello")) "World")
            Assert.AreEqual(Helper.Some ("Hello", " World"), parse (stringp (Seq.toList "Hello")) "Hello World")