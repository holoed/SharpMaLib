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

    open System
    open StringUtils

    type Parser<'a> = Parser of (string -> ('a * string) list)
    
    let result x = Parser (fun s -> [(x, s)])

    let zero = Parser (fun _ -> [])

    let item = Parser (fun s -> match s with
                                | Cons(x, xs) -> [(x, xs)]
                                | Empty -> [])

    let parse (Parser p) s = p s

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
        member this.Return x = result x

        // m a -> m a
        member this.ReturnFrom (x:Parser<'a>) = x

        // () -> m a
        member this.Zero () = zero

        // m a -> m a -> m a
        member this.Combine (p, q) = Parser (fun s -> (parse p s) @ (parse q s))

        // (() -> M a>) -> M a 
        member this.Delay (f : (unit -> Parser<'a>)) = f()        
                

    let parser = ParserMonad()

    let sat p = parser { let! x = item
                         if (p x) then
                             return x }

    let char ch = sat (fun x -> ch = x)

    let digit = sat Char.IsDigit

    let lower = sat Char.IsLower

    let upper = sat Char.IsUpper

    let letter = lower +++ upper

    let rec many1 p = parser { let! y = p
                               let! ys = many p
                               return cons y ys }
    and many p = many1 p +++ (result "")

    let word = many1 letter

    let number = many1 digit

    let rec stringp s = parser { match s with
                                 | Empty -> return ""
                                 | Cons (x, xs) -> let! _ = char x
                                                   let! _ = stringp xs
                                                   return cons x xs }
    let natural = 
         let toDigit x = (int x) - (int '0')
         let op m n = 10 * m + n    
         let eval xs = xs |> Seq.map toDigit
                          |> Seq.reduce op 
         parser { let! xs = number
                  return eval xs }
     
