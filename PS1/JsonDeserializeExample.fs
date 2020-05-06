module Ps1.DeserializeExample

open System
open System.IO

open System.Collections.Generic

open Newtonsoft.Json

//#I @"C:\Users\bobby\source\repos\MIT_6006_FSharp\PS1\bin\Debug\netcoreapp3.1\"
//#r @"Newtonsoft.Json.dll"

type Id () = 
    member val UniqueId = Guid() with get, set
    member val Name = "" with get, set
 
type Port () = 
    member val Id = Id() with get, set
    member val Type = "" with get, set

type Node () = 
    member val Id = Id() with get, set
    member val Ports = List<Port>() with get, set

let file = File.ReadAllText(@"C:\Users\bobby\source\repos\MIT_6006_FSharp\PS1\Problem1D.json")
let nodes = 
    JsonConvert.DeserializeObject<seq<Node>>(file)
    |> Seq.map(fun n -> n.Id.UniqueId.ToString(), n)
    |> Map.ofSeq
