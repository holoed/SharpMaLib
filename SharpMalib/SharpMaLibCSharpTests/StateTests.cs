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
using MonadStateLinq;
using FsCheck;
using Microsoft.FSharp.Core;

namespace SharpMaLibCSharpTests
{
    [TestFixture]
    public class StateTests
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
            Spec.ForAny<DateTime>(x =>
                                    {                                    
                                        var y = State.Execute(from xp in GetState<DateTime>()
                                                              select xp, x);
                                        return x == y;
                                    }).Check(_configuration);        
        }

        [Test]
        public void SelectMany()
        {
            Spec.ForAny<string, string>((x, y) =>
                                            {
                                                var expected = x + y;
                                                string actual = State.Execute(from xp in GetState<string>()
                                                                                    from _ in SetState(y)
                                                                                    from yp in GetState<string>()
                                                                                    select xp + yp, x);
                                                return expected == actual;
                                            }).Check(_configuration);        
        }

        private State<T,T> GetState<T>()
        {
            return State.getState<T>();   
        }

        private State<Unit, S> SetState<S>(S state)
        {
            return State.setState<S>(state);
        }
    }
   
}


