let help = @"The following operations are supported:
    - ? or help  -> prints this text
    - reseed     -> reseeds the random number generator
    - exit       -> exit the application
    - <return>   -> rolls the die once
    - <n>        -> rolls the die n times
    - tell       -> prints the current application state
    - set <kind> -> sets the current application die kind"

type DieKind =
| d6 = 6
| d10 = 10
| d12 = 12
| d20 = 20

type Command =
| SetKind of DieKind
| Roll of int
| Reseed
| Help
| Exit
| Tell

type State = { Generator : System.Random; Kind : DieKind }

let concat items = 
    items |> Seq.map (fun o -> o.ToString()) |> String.concat ", "

let parseKind (kindString:string) =
    let parseOk, parsedKind = System.Enum.TryParse<DieKind> kindString
    if not parseOk then failwith (sprintf "Unrecognized die kind '%s'" kindString)
    else parsedKind

let parseCommand (commandString:string) =
    let segments = commandString.Split ([| ' '; '\t' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let fail' () = failwith "What?"
    match segments with
    //no command is allways a single roll
    | [] -> Roll 1
    //match all zero-argument commands
    | [command] ->
        //if the command is an integer, treat it as a roll count
        let parseOk, parsedNumber = System.Int32.TryParse(command)
        if parseOk then 
            Roll parsedNumber
        else 
            match command with
            | "?" | "help" -> Help
            | "reseed"     -> Reseed
            | "exit"       -> Exit
            | "tell"       -> Tell
            | _            -> fail' ()
    //match all single-argument commands
    | [command; arg] ->
        match command with
        | "set" -> SetKind (parseKind arg)
        | _     -> fail' ()
    //all other strings fail
    | _ -> fail' ()

let seedState kind = { Generator = new System.Random(); Kind = kind }

let doTell state =
    let allKinds = System.Enum.GetValues typeof<DieKind> |> Seq.cast<DieKind>
    printfn "Current die: %A, available: %s" state.Kind (allKinds |> concat)

let doRoll rolls state =
    let rolls = seq { for i in 1..rolls do yield state.Generator.Next (0, (int state.Kind) + 1) } 
    printfn "Rolled %s" (rolls |> concat)

let handleCommand state command =
    let none unit = None
    match command with
    | SetKind kind -> Some (seedState kind)
    | Reseed       -> Some (seedState state.Kind)
    | Roll times   -> none (doRoll times state)
    | Help         -> none (printfn "%s" help)
    | Tell         -> none (doTell state)
    | Exit         -> none (exit 0)
    
//input defines an infinite sequence of clean user commands
let input = seq {
    while true do
        printf "> "
        yield System.Console.ReadLine().Trim().ToLower()
}

//main handles parsing the command, applying it and handling any errors
let main state commandString =
    try
        let command = (parseCommand commandString)
        match handleCommand state command with
        | Some newState -> newState
        | None -> state

    with ex ->
        printfn "ERROR: %s" ex.Message
        state

let initialState = seedState DieKind.d20

printfn "Roll - type help or ? for help"
input |> Seq.fold main initialState |> ignore