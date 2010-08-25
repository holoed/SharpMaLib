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

namespace Monad
module Parser = 

    open System
    open StringUtils
    open Utils
    open Combinators

    type Parser<'a, 'b> = Parser of (seq<'b> -> ('a * seq<'b>) list)
    
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
        member this.ReturnFrom (x:Parser<'a, 'b>) = x

        // () -> m a
        member this.Zero () = zero

        // m a -> m a -> m a
        member this.Combine (p, q) = Parser (fun s -> (parse p s) @ (parse q s))

        // (() -> M a>) -> M a 
        member this.Delay (f : (unit -> Parser<'a, 'b>)) = f()        
                

    let parser = ParserMonad()

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM parser f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 parser parser f ma mb

    // m (m a) -> m a
    let inline join z = joinM parser z  

    let sat p = parser { let! x = item
                         if (p x) then
                             return x }

    let char ch = sat (fun x -> ch = x)

    let digit = sat Char.IsDigit

    let lower = sat Char.IsLower

    let upper = sat Char.IsUpper

    let letter = lower +++ upper

    let whitespace = sat Char.IsWhiteSpace

    let rec many1 p = parser { let! y = p
                               let! ys = many p
                               return cons y ys }
    and many p = many1 p +++ (result Seq.empty)

    let word = many1 letter

    let number = many1 digit

    let whitespaces = many whitespace

    let token p = parser { let! x = p
                           let! _ = whitespaces
                           return x }

    let rec stringp s = parser { match s with
                                  | Empty -> return Seq.empty
                                  | Cons (x, xs) -> let! _ = char x
                                                    let! _ = stringp xs
                                                    return cons x xs }

    let symb cs = token (stringp cs)
        
    let natural = 
         let toDigit x = (int x) - (int '0')
         let op m n = 10 * m + n    
         let eval xs = xs |> Seq.map toDigit
                          |> Seq.reduce op 
         parser { let! xs = token number
                  return eval xs }

    let integer = 
        let negate x = -x
        let op = parser { let! _ = char '-'
                          return negate } +++ parser { return id }
        parser { let! f = op
                 let! n = natural
                 return f n }

    let sepBy1 p sep = parser { let! x = p
                                let! xs = many (parser { let! _ = sep 
                                                         return!  p })
                                return cons x xs }
     
    let sepBy p sep = (sepBy1 p sep) +++ (result Seq.empty)


    let chainl1 p op =         
        let rec rest l = parser { let! f = op
                                  let! r = p 
                                  return! rest (f l r) } +++ parser { return l }
        parser { let! l = p
                 return! rest l }

    let chainl p op l = (chainl1 p op) +++ parser { return l }


    let addOp = parser { let! _ = symb "+"
                         return (+) }
    let subOp = parser { let! _ = symb "-"
                         return (-) }
    let mulOp = parser { let! _ = symb "*"
                         return (*) }
    let divOp = parser { let! _ = symb "/"
                         return (/) }


    let rec factor = integer +++ parser { let! _ = symb "("
                                          let! n = expr
                                          let! _ = symb ")"
                                          return n }
    and expr = chainl1 term (addOp +++ subOp)
    and term = chainl1 factor (mulOp +++ divOp)

    let parseString p s = seq { let ret = parse p s
                                for (x, _) in ret do yield x }


// C# Support    
namespace MonadParserLinq    
    [<System.Runtime.CompilerServices.Extension>]
    module Monad =

        open System
        open System.Collections.Generic
        open System.Runtime.CompilerServices    
        open Monad
        open Monad.Utils
        open Monad.Parser
        open Monad.LinqCombinators

        [<Extension>]
        let inline Select (m, f) = select parser f m
            
        [<Extension>]
        let inline SelectMany(m, f, p) = selectMany parser parser f p m

        [<Extension>]
        let Where (m, p) = parser { let! x = m
                                    if (applyFunc p x) then
                                        return x }                              

        [<Extension>]
        let SepBy (p, sep) = sepBy p sep

        [<Extension>]
        let Many1 p = many1 p 

        [<Extension>]
        let AsString (p:Parser<IEnumerable<char>, 'a>) = parser { let! x = p
                                                                  return String.Join ("", x) }

        [<Extension>]
        let Parse (p, s) = parseString p s

        [<Extension>]
        let Eval s = parseString expr s
          

