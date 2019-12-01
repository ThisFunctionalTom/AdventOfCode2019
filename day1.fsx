let getRequiredFuelForMass (moduleMass: int) =
    float moduleMass
    |> fun x -> x / 3.0
    |> floor
    |> int
    |> fun x -> x - 2

let example1 = getRequiredFuelForMass 12 // 2
let example2 = getRequiredFuelForMass 14 // 2
let example3 = getRequiredFuelForMass 1969 // 654
let example4 = getRequiredFuelForMass 100756

let getRequiredFuelForModule moduleMass =
    let initialFuel = getRequiredFuelForMass moduleMass
    
    let getRequiredFuelForFuel fuelMass =
        if fuelMass <= 0 then
            None
        else
            Some (fuelMass, getRequiredFuelForMass fuelMass)

    List.unfold getRequiredFuelForFuel initialFuel
    |> List.sum

let example2_1 = getRequiredFuelForModule 14
let example2_2 = getRequiredFuelForModule 1969
let example2_3 = getRequiredFuelForModule 100756

#load "helpers.fsx"

open Helpers

readAllInt32 "day1.input" 
|> Array.sumBy getRequiredFuelForMass

readAllInt32 "day1.input" 
|> Array.sumBy getRequiredFuelForModule