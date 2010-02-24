module StringUtils

let (|Empty|Cons|) (xs:seq<'a>) : Choice<Unit, 'a * seq<'a>> = if (Seq.isEmpty xs) then Empty else Cons(Seq.head xs, Seq.skip 1 xs)

let cons ch s = seq { yield ch
                      yield! s }