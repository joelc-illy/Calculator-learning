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
    //let squareRoot (a: decimal) : decimal = a |> float |> System.Math.Sqrt |> decimal
    let squareRoot: decimal->decimal = float >> System.Math.Sqrt >> decimal

//let list = [|1;2;3|]
printfn "Type '/ 2 2' to see results"
printfn "Type one of '+' '-' '*' '/' then 'decimal' then 'decimal' to use calculator"
printfn "Type sqrt then 'decimal' to use square roots"

open System

type BinaryOperation = 
    | Add
    | Subtract
    | Multiply
    | Divide

type UnaryOperation = 
    | SquareRoot

type Operation =
    | Binary of BinaryOperation
    | Unary of UnaryOperation

module BinaryOperation =
    let ofString str = 
        match str with
        | "+" -> Some Add
        | "-" -> Some Subtract
        | "*" -> Some Multiply
        | "/" -> Some Divide
        | _ -> None //failwith "Unknown operator"

    let eval op num1 num2 =
        match op with
        | Add -> Math.addition num1 num2
        | Subtract -> Math.subtract num1 num2
        | Multiply -> Math.multiply num1 num2
        | Divide -> Math.divide num1 num2

module UnaryOperation =
    let ofString str =
        match str with
        | "sqrt" -> Some SquareRoot
        | _ -> None //failwith "unknown operator"

    let eval op num1 =
        match op with
        | SquareRoot -> Math.squareRoot num1

module Operation =
    //let ofString str =
    //    match str with
    //    | "+" | "-" | "*" | "/" -> BinaryOperation.ofString str |> Binary
    //    | "sqrt" -> UnaryOperation.ofString str |> Unary
    //    | _ -> failwith "Unknown operator"
    let ofString str = 
        (str |> BinaryOperation.ofString |> Option.map Binary)
        |> Option.orElse (str |> UnaryOperation.ofString |> Option.map Unary)
        |> Option.defaultWith (fun () -> failwith "unknown operator")

    let eval op arg1 arg2 = 
        match op, arg2 with
        | Binary op, Some arg2 ->
            BinaryOperation.eval op arg1 arg2
        | Binary _, None -> failwith "no second argument provided for Binary"
            
        | Unary op, None ->
            UnaryOperation.eval op arg1
        | Unary _, Some _ -> failwith "second argument provided for Unary"


    //let ofString str = 
    //    match str with
    //    | "+" -> Add
    //    | "-" -> Subtract
    //    | "*" -> Multiply
    //    | "/" -> Divide
    //    | "sqrt" -> SquareRoot
    //    | _ -> failwith "Unknown operator"
    //
    //let result3Args op num1 num2 = 
    //    match op with
    //    | Add -> Math.addition num1 num2
    //    | Subtract -> Math.subtract num1 num2
    //    | Multiply -> Math.multiply num1 num2
    //    | Divide -> Math.divide num1 num2
    //
    //let result2Args op num1 =
    //    match op with
    //    | Add -> failwith "too few arguments"
    //    | Subtract -> failwith "too few arguments"
    //    | Multiply -> failwith "too few arguments"
    //    | Divide -> failwith "too few arguments"
    //    | SquareRoot -> Math.Sqrt num1



let args = 
    Environment.GetCommandLineArgs()
    |> List.ofArray
    |> List.skip 1

let length = args |> List.length

printfn "%A" (length > 1)
printfn "%A" args

match args with
    | [opStr; num1Str; num2Str] ->
        let op = opStr |> Operation.ofString
        let num1 = num1Str |> decimal 
        let num2 = num2Str |> decimal
        Operation.eval op num1 (Some num2) |> printfn "%A"
        //Operation.result (opStr |> Operation.ofString) (num1Str |> decimal) (num2Str |> decimal) |> printfn "%A"
    | [opStr; num1Str] ->
        let op = opStr |> Operation.ofString
        let num1 = num1Str |> decimal
        Operation.eval op num1 None |> printfn "%A"
    
    | _ -> failwith "Please match 3 parameters, or 2 for sqrt"

        
    
