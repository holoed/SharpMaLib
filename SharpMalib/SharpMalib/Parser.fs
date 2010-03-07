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
[<System.Runtime.CompilerServices.Extension>]
module ParserMonad = 

    open System    
    open StringUtils
    open System.Runtime.CompilerServices
    open SharpMalib.Utils

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

    [<Extension>]
    let Select(m, f) = parser.Bind(m, fun x -> applyFunc f x |> parser.Return)  
        
    [<Extension>]
    let SelectMany(m, f, p) = Parser (fun s -> match parse m s with
                                               | [] -> []
                                               | [(x, xs)] -> match parse (applyFunc f x) xs with
                                                              | [] -> []
                                                              | [(y, ys)] -> [(applyFunc2 p x y, ys)])

    [<Extension>]
    let Parse (p, s) = parseString p s

    [<Extension>]
    let Eval s = parseString expr s
      

