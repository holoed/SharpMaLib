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
using NUnit.Framework;
using SharpMalib.Maybe;
using FsCheck;
using SharpMaLib.Tests;
using Microsoft.FSharp.Core;

namespace SharpMaLibCSharpTests
{
    [TestFixture]
    public class MaybeTests
    {
        private Configuration _configuration;

        [SetUp]
        public void SetUp()
        {
            _configuration = new Configuration { Runner = NUnitFsCheck.NUnitRunner };
        }

        [Test]
        public void Select()
        {          
            Func<int, MaybeMonad.Maybe<int>> f = x => x.Just();            
            Spec.ForAny<int>(x => Operators.Compare(f(x), from y in f(x) select y) == 0).Check(_configuration); 
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

            Spec.ForAny<int>(x =>
                                 {
                                     var left = Expected(f, g, x);
                                     var right = from y in f(x)
                                                 from z in g(y)
                                                 select Tuple.Create(y, z);
                                     return Operators.Compare(left, right) == 0;
                                 }).Check(_configuration);
        }

        [Test]
        public void Join()
        {
            Spec.ForAny<int>(x => Operators.Compare(x.Just(), x.Just().Just().Join()) == 0).Check(_configuration);
        }

        [Test]
        public void Map()
        {
            Spec.ForAny<int>(x => Operators.Compare(x.Just().Map(xi => xi * xi), (x * x).Just()) == 0).Check(_configuration);
        }

        private MaybeMonad.Maybe<Tuple<int, int>> Expected(Func<int, MaybeMonad.Maybe<int>> f, Func<int, MaybeMonad.Maybe<int>> g, int x)
        {
            var xp = f(x);
            if (xp.IsJust)
            {
                var yp = g(Value(xp));
                if (yp.IsJust)
                    return Tuple.Create(Value(xp), Value(yp)).Just();
            }
            return Nothing<Tuple<int, int>>();
        }

        public MaybeMonad.Maybe<T> Nothing<T>()
        {
            return MaybeMonad.Maybe<T>.Nothing;
        }

        private static T Value<T>(MaybeMonad.Maybe<T> maybe)
        {
            if (maybe.IsJust)
                return ((MaybeMonad.Maybe<T>.Just) maybe).Item;
            throw new ArgumentException();
        }
    }
}


