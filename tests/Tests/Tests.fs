module Tests
module S=FSharp.SExpression
type internal T=S.Token
type internal E=S.Expression
open System
open Xunit
open FsCheck
open FsCheck.Xunit

[<Fact>]
let ``symbol value`` () =
  Assert.Equal("(symbol \"value\")", S.parse "(symbol \"value\")" |> S.printList)

[<Fact>]
let ``X`` () =
  Assert.Equal("(symbol 1 2 3)", S.parse "(symbol 1 2 3)" |> S.printList)


type Foo={ alpha:string; beta:string}

type Bar={ gamma:string; foo:Foo}

type Baz={ alpha:string; beta:string}


type Qux = 
      |Ena of string
      |Dio of Foo
      |Trea of Bar
      |Tessera of Baz
  
module internal Z=

  module Foo=
   let serialize (this:Foo)= 
      E.List [ 
                E.Symbol "alpha"; E.String this.alpha
                E.Symbol "beta"; E.String this.beta
             ]
   let deSerialize(v:E) : Foo option= 
      match v with 
      | E.List [ 
                E.Symbol "alpha"; E.String alpha
                E.Symbol "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None

  module Bar=
    let serialize this= 
      E.List [ 
                E.Symbol "gamma"; E.String this.gamma
                E.Symbol "foo"; Foo.serialize this.foo
             ]
    let deSerialize(v:E)= 
      match v with 
      | E.List [ 
                E.Symbol "gamma"; E.String gamma
                E.Symbol "foo"; foo
               ] -> 
                 Foo.deSerialize foo |> Option.map (fun foo-> { gamma=gamma; foo= foo})
      | _ -> None

  module Baz=
    let serialize this= 
      E.List [ 
                E.Symbol "alpha"; E.String this.alpha
                E.Symbol "beta"; E.String this.beta
             ]
    let deSerialize(v:E)= 
      match v with 
      | E.List [ 
                E.Symbol "alpha"; E.String alpha
                E.Symbol "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None
  module Qux=
    let serialize (this:Qux)= 
      match this with
      | Ena s-> E.List [ E.Symbol "Ena"; E.String s ]
      | Dio foo -> E.List [ E.Symbol "Dio"; Foo.serialize foo ]
      | Trea bar -> E.List [ E.Symbol "Trea"; Bar.serialize bar ]
      | Tessera baz -> E.List [ E.Symbol "Tessera"; Baz.serialize baz ]
    let deSerialize(v:E)= 
      match v with 
      | E.List [ E.Symbol "Ena"; E.String s ] -> Some <| Ena s
      | E.List [ E.Symbol "Dio"; foo ] -> Foo.deSerialize foo |> Option.map Dio 
      | E.List [ E.Symbol "Trea"; bar ] -> Bar.deSerialize bar |> Option.map Trea
      | E.List [ E.Symbol "Tessera"; baz ] -> Baz.deSerialize baz |> Option.map Tessera
      | _ -> None

[<Property>]
let ``Can serialize and deserialize structure Union``(x :Qux) =
  (Some x) = ( Z.Qux.serialize x |> Z.Qux.deSerialize )

[<Property>]
let ``Can serialize and deserialize structure Record``(x :Baz) =
  (Some x) = ( Z.Baz.serialize x |> Z.Baz.deSerialize )

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

