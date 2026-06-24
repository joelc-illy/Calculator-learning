// For more information see https://aka.ms/fsharp-console-apps

let multiply a b = a * b
let addition a b = a + b
let divide a b = a / b
let subtract a b = a - b
//let list = [|1;2;3|]
printfn "Type '/ 2 2' to see results"
printfn "Type one of '+' '-' '*' '/' then 'int' then 'int' to use calculator"

open System

type Operation = 
    | Add
    | Subtract
    | Multiply
    | Divide

module Operation =
    let ofString str = 
        match str with
        | "+" -> Add
        | "-" -> Subtract
        | "*" -> Multiply
        | "/" -> Divide
        | _ -> failwith "Unknown operator"

    let result op num1 num2 = 
        match op with
            | Add -> addition num1 num2
            | Subtract -> subtract num1 num2
            | Multiply -> multiply num1 num2
            | Divide -> divide num1 num2

let args = 
    Environment.GetCommandLineArgs()
    |> List.ofArray
    |> List.skip 1

printfn "%A" args

match args with
    | [opStr; num1Str; num2Str] ->
        let op = opStr |> Operation.ofString
        let num1 = num1Str |> decimal 
        let num2 = num2Str |> decimal 
        Operation.result op num1 num2 |> printfn "%A"
        //Operation.result (opStr |> Operation.ofString) (num1Str |> decimal) (num2Str |> decimal) |> printfn "%A"

    | _ -> failwith "Please match 3 parameters"

        
    
