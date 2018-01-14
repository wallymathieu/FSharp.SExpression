module Tests
module S=FSharp.SExpression
type internal T=S.Token
type internal E=S.Expression
open System
open Xunit

[<Fact>]
let ``symbol value`` () =
  Assert.Equal("(symbol \"value\")", S.parse "(symbol \"value\")" |> S.printList)

[<Fact>]
let ``X`` () =
  Assert.Equal("(symbol 1 2 3)", S.parse "(symbol 1 2 3)" |> S.printList)

[<Fact>]
let ``Y`` () =
  let c= """(((S) (NP VP))
   ((VP) (V))
   ((VP) (V NP))
   ((V) died)
   ((V) employed)
   ((NP) nurses)
   ((NP) patients)
   ((NP) Medicenter)
   ((NP) "Dr Chan"))"""

  S.parse c |> ignore

