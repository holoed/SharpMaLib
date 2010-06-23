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

using NUnit.Framework;
using Monad;
using MonadIdentityLinq;
using FsCheck;
using System.Drawing;

namespace SharpMaLibCSharpTests
{
    [TestFixture]
    public class IdentityTests
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
            Spec.ForAny<int>(x => x == from y in x 
                                       select y).Check(_configuration);
        }

        [Test]
        public void SelectMany()
        {
            Spec.ForAny<int, int>((x,y) => new Point(x, y) == from xp in x 
                                                              from yp in y 
                                                              select new Point(xp, yp)).Check(_configuration);
        }

        [Test]
        public void Join()
        {
            Spec.ForAny<int>(x => x == x.Join()).Check(_configuration);
        }

        [Test]
        public void Map()
        {
            Spec.ForAny<int>(x => x.Map(xi => xi * xi) == (x * x)).Check(_configuration);
        }
    }
}


