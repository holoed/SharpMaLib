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

namespace SharpMalib.Parser
module ParserMonad = 

    type Parser<'a, 'b> = Parser of ('a list -> ('b * ('a list)) list)

    let item = Parser (fun s -> match s with
                                | x::xs -> [(x, xs)]
                                | [] -> [])
    
    let ret x = Parser (fun s -> [(x, s)])

    let fail = Parser (fun _ -> [])

    let parse (Parser p) s = p (Seq.toList s)

    // Deterministic choice operator
    let (+++) p q = Parser (fun s -> match parse p s with
                                     | []-> parse q s
                                     | result -> result)

    type ParserMonad() =
        // m a -> (a -> m b) -> m b
        member this.Bind (m, f) = Parser (fun s -> match parse m s with
                                                   | [] -> []
                                                   | [(x, xs)] -> parse (f x) xs)
        // a -> m a
        member thid.Return x = ret x

        // m a -> m a
        member this.ReturnFrom (x:Parser<'a, 'b>) = x

        // () -> m a
        member this.Zero () = fail

        // m a -> m a -> m a
        member this.Combine (p, q) = Parser (fun s -> (parse p s) @ (parse q s))

        // (() -> M a>) -> M a 
        member this.Delay (f : (unit -> Parser<'a,'b>)) = f()

    let parser = ParserMonad()

    let sat ch = parser { let! x = item
                          if ch = x then
                             return x
                          else
                             return! fail }

    let rec stringp s = match s with
                        | x::xs -> parser { let! y = sat x
                                            let! ys = stringp xs
                                            return y::ys }
                        | [] -> parser { return [] }
