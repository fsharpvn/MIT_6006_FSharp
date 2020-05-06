namespace Ps1

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Globalization
open System.Collections.Generic


type Temperature = struct 
    val IsCelsius : bool
    val Degrees: int
    new (degrees: int, celsius: bool) = {IsCelsius = celsius; Degrees = degrees}
    static member Parse(input: string) =
        let degrees = 1
        let celsius = true
        Temperature(degrees, celsius)    
end    

type WeatherForecastWithTemperatureStruct = struct 
    val Date: DateTimeOffset
    val TemperatureCelsius: int 
    val Summary: string 
    new (date: DateTimeOffset, tem: int, sum: string) = 
        {Date = date; TemperatureCelsius = tem; Summary = sum }
end

type TemperatureCnverter() =
    inherit JsonConverter<Temperature>()
    override self.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) : Temperature =
        Temperature.Parse(reader.GetString())

    override self.Write(writer: Utf8JsonWriter, temperature: Temperature, options: JsonSerializerOptions) : unit =
        writer.WriteStringValue(temperature.ToString())

type DateTimeOffsetConverter() =
    inherit JsonConverter<DateTimeOffset>()
    
    override self.Read(
                        reader: byref<Utf8JsonReader>, 
                        typeToConvert: Type, 
                        options: JsonSerializerOptions) : DateTimeOffset =
        printfn "+++++++ READER %A" (reader.GetString())

        DateTimeOffset.ParseExact(reader.GetString(), "MM/dd/yyyy", CultureInfo.InvariantCulture)
    
    override self.Write(writer: Utf8JsonWriter, value: DateTimeOffset, options: JsonSerializerOptions) : unit =
        writer.WriteStringValue(value.ToString("MM/dd/yyyy",CultureInfo.InvariantCulture))

type Data(file: string) =
    member x.Read() =
        use stream = new StreamReader(file)
        let mutable valid = true
        while (valid) do
            let line = stream.ReadLine()
            if isNull line then
                valid <- false
            else 
                printfn "%A" line

module trySerialize =
    //let data = Data(@"PS1\Problem1D.json")    
    
    let fileToWrite = @"PS1\Weather.json"

    let weather = WeatherForecastWithTemperatureStruct(DateTimeOffset.UtcNow, 30, "Good"  )
    
    /// serialize the weather object without convert
    let weatherJson = JsonSerializer.Serialize(weather)
    //File.WriteAllText(fileToWrite, weatherJson)
    
    let serializeOptions = JsonSerializerOptions()
    serializeOptions.Converters.Add(DateTimeOffsetConverter())
    serializeOptions.WriteIndented <- true

    /// serialize json with converter in Option
    let weatherJsonConvert = JsonSerializer.Serialize(weather, serializeOptions)
    //File.WriteAllText(fileToWrite, weatherJsonConvert)
