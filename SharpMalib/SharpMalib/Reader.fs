namespace Monad

// Reader monad (experimental) 
//TODO: MonadError e m => MonadError e (ReaderT r m)

type Reader<'e, 'a> = Reader of ('e -> 'a)

module Reader = 
    
    open Combinators

    let runReader (Reader r) e = r e

    type ReaderBuilder () =
        
        member this.Bind (m, f) =  Reader (fun e -> runReader (f (runReader m e)) e)

        member this.Return a = Reader (fun _ -> a)

        member this.ReturnFrom(a:Reader<'r,'a>) = a
    
        member this.Zero() = this.Return ()
    
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
    
        member this.TryWith(m, h) =
          Reader (fun env -> try runReader m env
                             with e -> runReader (h e) env)
    
        member this.TryFinally(m, compensation) =
          Reader (fun env -> try runReader m env
                             finally compensation())
    
        member this.Using(res:#System.IDisposable, body) =
          this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
    
        member this.Delay(f) = this.Bind(this.Return (), f)
    
        member this.While(guard, m) =
          if not(guard()) then this.Zero() else
            this.Bind(m, (fun () -> this.While(guard, m)))
    
        member this.For(sequence:seq<_>, body) =
          this.Using(sequence.GetEnumerator(),
                     (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    let reader = ReaderBuilder ()

    // Specialized functions

    // Monad

    // (a -> b) -> m a -> m b
    let inline liftM f m = liftM reader f m

    // (a -> b -> c) -> m a -> m b -> m c
    let inline liftM2 f ma mb = liftM2 reader reader f ma mb

    // m (m a) -> m a
    let inline join z = joinM reader z

    // Reader specific functions

    // ask :: m r
    let ask = Reader id

    // asks :: (MonadReader r m) => (r -> a) -> m a
    let asks f = reader { 
        let! e = ask
        return (f e) }

    // local :: (r -> r) -> m a -> m a (actually allows type conversion, probably should be pohibited)
    let local f m = Reader (fun e -> runReader m (f e))

// C# Support
namespace MonadReaderLinq
[<System.Runtime.CompilerServices.Extension>]
module Monad =

    open System
    open System.Runtime.CompilerServices    
    open Monad
    open Monad.Utils
    open Monad.Reader
    open Monad.LinqCombinators

    [<Extension>]
    let inline Select (m, f) = select reader f m
        
    [<Extension>]
    let inline SelectMany(m, f, p) = selectMany reader reader f p m

    [<Extension>]
    let inline Map (m, f:Converter<'a,'b>) = liftM (FuncConvert.ToFSharpFunc f) m