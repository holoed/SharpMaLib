﻿module StringUtils

let (|Empty|Cons|) (xs:string) : Choice<Unit, char * string> = if (Seq.isEmpty xs) then Empty else Cons(Seq.head xs, xs.Substring 1)

let cons ch s = sprintf "%c%s" ch s