let help = @"The following operations are supported:
    - ? or help        -> prints this text
    - reseed           -> reseeds the random number generator
    - exit             -> exit the application
    - <return> or roll -> rolls the die
    - tell             -> prints the current application state
    - set <die kind>   -> sets the current application die kind"

type DieKind =
| d6 = 6
| d10 = 10
| d12 = 12
| d20 = 20

let makeKind (kindString:string) =
    let ok, value = System.Enum.TryParse<DieKind>(kindString)
    if ok then value else failwith (sprintf "Unrecognized die kind '%s'" kindString)

type State = { Generator : System.Random; Kind : DieKind }
    with
    member s.RollDie () =
        s.Generator.Next(0, ((int) s.Kind) + 1)

type Command =
| SetKind of string
| Help
| Reseed
| Roll
| Exit
| Tell
| Unknown
    with 
        static member create (commandString:string) =
            let splitChars = [|' '; '\t'|]
            let arguments = commandString.Split(splitChars, System.StringSplitOptions.RemoveEmptyEntries);
            match arguments.[0] with
            | "?" | "help" -> Help
            | "reseed" -> Reseed
            | "exit" -> Exit
            | "roll" -> Roll
            | "tell" -> Tell
            | "set" -> SetKind arguments.[1]
            | _ -> Unknown

let seedState kind = { Generator = new System.Random(); Kind = kind }

let processCommand state command =
    let none' unit = None
    match command with
    | Unknown -> none' (printfn "What?")
    | Help -> none' (printfn "%s" help)
    | SetKind kind -> Some (seedState (makeKind kind))
    | Reseed -> Some (seedState state.Kind)
    | Exit -> none' (exit 0)
    | Tell -> none' (printfn "Current die kind: %A, available kinds: %A" state.Kind (System.Enum.GetValues(typeof<DieKind>)))
    | Roll -> 
        let roll = state.RollDie ()
        printfn "Rolled %i" roll
        none' ()
    
let input = seq {
    while true do
        printf "> "
        let line = System.Console.ReadLine().Trim().ToLower()
        if line.Length = 0 then yield Roll
        else yield Command.create line
}

let fold state command =
    try
        match processCommand state command with
        | Some newState -> newState
        | None -> state
    with ex ->
        printfn "ERROR: %s" ex.Message
        state

let initialState = seedState DieKind.d20

printfn "Roll - type help or ? for help"
Seq.fold fold initialState input |> ignore