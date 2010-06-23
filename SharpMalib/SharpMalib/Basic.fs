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
module Combinators =

    let inline mapM b f m =         
        let unit x    = (^x: (member Return: ^b -> ^n) b, x)    
        let (>>=) m f = (^x: (member Bind: ^m -> (^a -> ^n) -> ^n) b, m, f)
        m >>= (fun x -> unit (f x))


    let inline joinM b m =            
        let (>>=) m f = (^x: (member Bind: ^m -> (^n -> ^n) -> ^n) b, m, f)
        m >>= id
