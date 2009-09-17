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

namespace SharpMalib
module ListMonad

// List Monad
// The List monad embodies the strategy of combining a chain of non-deterministic computations 
// by applying the operations to all possible values at each step. It is useful when computations must deal with ambiguity. 
// In that case it allows all possibilities to be explored until the ambiguity is resolved.

type 'a List = Nil | Cons of 'a * 'a List

let rec map f xs = match xs with
                   | Nil -> Nil
                   | Cons(x, xs') -> Cons(f(x), (map f xs'))

let rec append xs ys = match (xs, ys) with
                       | (Nil,_) -> ys
                       | (Cons(x,xs'), _) -> Cons(x,append xs' ys)
                
let rec join xs = match xs with
                  | Nil -> Nil
                  | Cons(x, xs') -> append x (join xs')

type ListBuilder() =
    member this.Return a = Cons(a, Nil)
    member this.Bind (m, f) = join (map f m) 
    
let list = ListBuilder()    

      

