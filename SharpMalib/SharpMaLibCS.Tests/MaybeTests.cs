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
using SharpMalib.Maybe;
using FsCheck;
using System.Drawing;

namespace SharpMaLibCS.Tests
{
    [TestFixture]
    public class MaybeTests
    {
        [Test]
        public void Select()
        {
            Func<int, MaybeMonad.Maybe<int>> f = x => x.Just();
            Spec.ForAny<int>(x => f(x) == from y in f(x)
                                          select y).QuickCheck("select"); 
        }

        [Test]
        public void SelectMany()
        {
            Func<int, MaybeMonad.Maybe<int>> f = x => x > 0 ?
                x.Just() :
                Nothing<int>();

            Func<int, MaybeMonad.Maybe<int>> g = x => x > 1 ?
                x.Just() :
                Nothing<int>();

            Spec.ForAny<int>(x => Expected(f, g, x) == from y in f(x)
                                                       from z in g(y)
                                                       select new Point(y, z)).QuickCheck("selectMany");
        }

        [Test]
        public void Join()
        {
            Spec.ForAny<int>(x => x.Just() == x.Just().Just().Join());
        }

        [Test]
        public void Map()
        {
            Spec.ForAny<int>(x => x.Just().Map(xi => xi * xi) == (x * x).Just());
        }

        private MaybeMonad.Maybe<Point> Expected(Func<int, MaybeMonad.Maybe<int>> f, Func<int, MaybeMonad.Maybe<int>> g, int x)
        {
            var xp = f(x);
            if (xp.IsJust())
            {
                var yp = g(xp.Just1);
                if (yp.IsJust())
                    return new Point(xp.Just1, yp.Just1).Just();
            }
            return Nothing<Point>();
        }

        public MaybeMonad.Maybe<T> Nothing<T>()
        {
            return MaybeMonad.Maybe<T>.Nothing;
        }
    }
}
