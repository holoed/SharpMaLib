using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using SharpMalib;
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
                                       select x).QuickCheck("select"); 
        }

        [Test]
        public void SelectMany()
        {
            Spec.ForAny<int, int>((x,y) => new Point(x, y) == from xp in x 
                                                              from yp in y 
                                                              select new Point(x, y)).QuickCheck("selectMany");
        }
    }
}
