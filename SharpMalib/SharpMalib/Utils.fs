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
module Utils = 

    open System

    let applyFunc (f:Func<'a,'b>) x = f.Invoke x
    let applyFunc2 (f:Func<'a,'b,'c>) x y = f.Invoke(x, y)