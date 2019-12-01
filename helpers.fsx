open System.IO

let readAllInt32 path =
    File.ReadAllLines(path)
    |> Array.map int