#region License

/* ****************************************************************************
 * Copyright (c) Edmondo Pentangelo. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/

#endregion

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using SharpMalib.State;
using FsCheck;

namespace SharpMaLibCS.Tests
{
    [TestFixture]
    public class StateTests
    {
        [Test]
        public void Select()
        {
            var getState = StateMonad.getState<int>();

            Spec.ForAny<int, int>((x,y) => x == StateMonad.Execute(from xp in getState 
                                                                   select xp, y)).QuickCheck("select");        
        }  
    }

    public static class StateExtensions
    {
        public static StateMonad.State<K, int> Select<T, K>(this StateMonad.State<T, int> m, Func<T, K> f)
        {
            return StateMonad.Select<T, int, K>(m, f);
        }
    }
}
