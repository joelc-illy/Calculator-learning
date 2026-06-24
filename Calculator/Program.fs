// For more information see https://aka.ms/fsharp-console-apps


module Math =
    let multiply a b = a * b
    let addition a b = a + b
    let divide a b = 
        match b with
        | 0.0M -> 
            failwith "cannot divide by 0"
            0.0M
        | _ -> a / b
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
    | SquareRoot

module Operation =
    let ofString str = 
        match str with
        | "+" -> Add
        | "-" -> Subtract
        | "*" -> Multiply
        | "/" -> Divide
        | "sqrt" -> SquareRoot
        | _ -> failwith "Unknown operator"

    let result3Args op num1 num2 = 
        match op with
        | Add -> Math.addition num1 num2
        | Subtract -> Math.subtract num1 num2
        | Multiply -> Math.multiply num1 num2
        | Divide -> Math.divide num1 num2
        | SquareRoot -> failwith "Too many arguments"

    let result2Args op num1 =
        match op with
        | Add -> failwith "too few arguments"
        | Subtract -> failwith "too few arguments"
        | Multiply -> failwith "too few arguments"
        | Divide -> failwith "too few arguments"
        | SquareRoot -> Math.Sqrt num1

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
        Operation.result3Args op num1 num2 |> printfn "%A"
        //Operation.result (opStr |> Operation.ofString) (num1Str |> decimal) (num2Str |> decimal) |> printfn "%A"
    | [opStr; num1Str] ->
        let op = opStr |> Operation.ofString
        let num1 = num1Str |> float
        Operation.result2Args op num1 |> printfn "%A"

    | _ -> failwith "Please match 3 parameters"

        
    
