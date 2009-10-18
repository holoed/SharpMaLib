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
using SharpMalib.Identity;
using FsCheck;
using System.Drawing;

namespace SharpMaLibCS.Tests
{
    [TestFixture]
    public class IdentityTests
    {
        [Test]
        public void Select()
        {
            Spec.ForAny<int>(x => x == from y in x 
                                       select y).QuickCheck("select"); 
        }

        [Test]
        public void SelectMany()
        {
            Spec.ForAny<int, int>((x,y) => new Point(x, y) == from xp in x 
                                                              from yp in y 
                                                              select new Point(xp, yp)).QuickCheck("selectMany");
        }

        [Test]
        public void Join()
        {
            Spec.ForAny<int>(x => x == x.Join());
        }

        [Test]
        public void Map()
        {
            Spec.ForAny<int>(x => x.Map(xi => xi * xi) == (x * x));
        }
    }
}
