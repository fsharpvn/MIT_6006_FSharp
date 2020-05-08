module Ps1.DeserializeExample

open System
open System.IO

open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization

//#I @"C:\Users\bobby\source\repos\MIT_6006_FSharp\PS1\bin\Debug\netcoreapp3.1\"
//#r @"Newtonsoft.Json.dll"
//open Microsoft.FSharp.Reflection

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
    JsonSerializer.Deserialize<seq<Node>>(file)
    |> Seq.map(fun n -> 
        //printfn "++++++++++Node %A" n.Id
        n.Id.UniqueId.ToString(), n)
    |> Map.ofSeq

//#r @"C:\Users\bobby\source\repos\MIT_6006_FSharp\PS1\bin\Debug\net472\System.Text.Json.dll"

type Person () = 
    member val Name : string = "" with get, set
type Customer () =
    inherit Person ()
    member val CreditLimit : decimal = (decimal 1.) with get, set 
type Employee () =
    inherit Customer ()
    member val OfficeNumber : string = "1" with get, set

type TypeDsicriminator = 
    | Customer = 1
    | Employee = 2


type PersonConverterWithTypeDiscriminator () =
    inherit JsonConverter<Employee>()
    override self.Read(
                        reader: byref<Utf8JsonReader>, 
                        typeToConvert: Type, 
                        options: JsonSerializerOptions) : Employee =                 
            
            
            if reader.TokenType <> JsonTokenType.StartObject then do
                printfn "++++++++++rt <> JsonTokenType.StartObject"
                raise (JsonException())
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.PropertyName then do
                printfn "++++++++++rt <> JsonTokenType.PropertyName %A:" JsonTokenType.PropertyName
                raise (JsonException())
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.Number then do
                printfn "++++++++++rt <> JsonTokenType.PropertyName %A:" JsonTokenType.Number
                raise (JsonException())
            printfn "++++++++++rt <> JsonTokenType.PropertyName %A:" (reader.GetInt32())
            let mutable person = Employee() // first loop to initiate the person type

            while (reader.Read()) do
                
                match reader.TokenType with      
                    | JsonTokenType.StartObject -> 
                        printfn "Reader Token Type: %A" JsonTokenType.StartObject
                    | JsonTokenType.PropertyName -> 
                        let propertyName = reader.GetString()
                        reader.Read() |> ignore
                        match propertyName with                        
                        | "TypeDiscriminator" -> 
                            printfn "TypeDiscriminator: %A" (reader.GetInt32()) 
                                
                        | "CreditLimit" -> 
                            person.CreditLimit <- reader.GetDecimal()
                            printfn "CreditLimit: %A " person.CreditLimit
                                
                        //| "OfficeNumber" -> 
                        //    person.OfficeNumber <- reader.GetString()
                        //    printfn "Office Number: %A " person.OfficeNumber
                                
                        | "Name" -> 
                            person.Name <- reader.GetString()
                            printfn "Name: %A " person.Name
                                
                        | _ -> 
                            printfn "no matching -------- Property %A" propertyName
                    | JsonTokenType.EndObject -> 
                        printfn "Reader Token Type: %A" JsonTokenType.EndObject
                    | JsonTokenType.EndArray -> 
                        printfn "Reader Token Type: %A" JsonTokenType.EndArray
                    | _ -> printfn "---------- No matching %A" reader.TokenType
            raise (JsonException())
            person
    
    override self.Write(writer: Utf8JsonWriter, employee: Employee, options: JsonSerializerOptions) =
        raise (InvalidOperationException("should not get here"))
        writer.WriteStartObject()
        writer.WriteNumber("TypeDiscriminator", int(TypeDsicriminator.Employee))
        //writer.WriteNumber("OfficeNumber", employee.OfficeNumber)
        writer.WriteString("Name", employee.Name)
        writer.WriteEndObject()

        
//module Main = 
    
//    [<EntryPoint>]
//    let main argv =

//        let options = JsonSerializerOptions()
//        //options.Converters.Add(PersonConverterWithTypeDiscriminator())
//        let file1 = File.ReadAllText(@"C:\Users\bobby\source\repos\MIT_6006_FSharp\PS1\Problem1D.json")
//        let nodes1 = JsonSerializer.Deserialize<seq<problemList>>(file1, options)
//        for node in nodes1 do             
//            printfn "+++++++++++ %A " node.problemList

//        0 // return an integer exit code      