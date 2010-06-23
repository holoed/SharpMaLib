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
using Monad;
using MonadMaybeLinq;
using FsCheck;
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
            Func<int, FSharpOption<int>> f = x => x.Just();            
            Spec.ForAny<int>(x => Operators.Compare(f(x), from y in f(x) select y) == 0).Check(_configuration); 
        }

        [Test]
        public void SelectMany()
        {
            Func<int, FSharpOption<int>> f = x => x > 0 ? x.Just() : Nothing<int>();

            Func<int, FSharpOption<int>> g = x => x > 1 ? x.Just() : Nothing<int>();

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

        private FSharpOption<Tuple<int, int>> Expected(Func<int, FSharpOption<int>> f, Func<int, FSharpOption<int>> g, int x)
        {
            var xp = f(x);
            if (xp.IsSome())
            {
                var yp = g(Value(xp));
                if (yp.IsSome())
                    return Tuple.Create(Value(xp), Value(yp)).Just();
            }
            return Nothing<Tuple<int, int>>();
        }

        public FSharpOption<T> Nothing<T>()
        {
            return FSharpOption<T>.None;
        }

        private static T Value<T>(FSharpOption<T> maybe)
        {
            if (maybe.IsSome())
                return ((FSharpOption<T>) maybe).Value;
            throw new ArgumentException();
        }
    }
}


