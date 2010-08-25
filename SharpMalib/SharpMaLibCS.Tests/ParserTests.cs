using System;
using System.Collections.Generic;
using System.Linq;
using FsCheck;
using NUnit.Framework;
using Monad;
using MonadParserLinq;


namespace SharpMaLibCSharpTests
{
    [TestFixture]
    public class ParserTests
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
            var p = from x in Parser.item<char>()
                    select String.Format("({0})", x);
            CollectionAssert.AreEqual(new[] { "(H)" }, p.Parse("Hello"));
        }

        [Test]
        public void SelectMany()
        {
            var p = from x in Parser.item<char>()
                    from y in Parser.item<char>()
                    select String.Format("({0}{1})", x, y);
            CollectionAssert.AreEqual(new[] { "(He)" }, p.Parse("Hello"));
        }

        [Test]
        public void CreatingAParserToSplitStrings()
        {
            var letter = from x in Parser.item<char>()
                         where Char.IsLetter(x)
                         select x;
            var word = letter.Many1().AsString();
            var words = word.SepBy(Parser.whitespaces);

            CollectionAssert.AreEqual(new[] { "Hello", "World" }, words.Parse("Hello World").First());
        }

        [Test]
        public void Add()
        {
            var q = from x in "2 + 3".Eval()
                    from y in "3 + 2".Eval()
                    select x == y;
            Assert.IsTrue(q.First());
        }

        [Test]
        public void ArithmeticProperties()
        {
            Spec.ForAny<int>(x => CollectionAssert.AreEqual(Calculate("0 + {0}", x), Calculate("{0} + 0", x))).Check(_configuration);
            Spec.ForAny<int>(x => CollectionAssert.AreEqual(Calculate("1 * {0}", x), Calculate("{0} * 1", x))).Check(_configuration);

            Spec.ForAny<int, int>((x, y) => CollectionAssert.AreEqual(Calculate("{1} + {0}", x, y), Calculate("{0} + {1}", x, y))).Check(_configuration);
            Spec.ForAny<int, int>((x, y) => CollectionAssert.AreEqual(Calculate("{1} * {0}", x, y), Calculate("{0} * {1}", x, y))).Check(_configuration);
        }

        private static IEnumerable<int> Calculate(string format, params object[] args)
        {
            return string.Format(format, args).Eval().ToArray();
        }
    }
}