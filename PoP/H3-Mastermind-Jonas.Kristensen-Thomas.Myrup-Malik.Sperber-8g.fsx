type codeColor =
  Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

//globale variabler:
let mutable xcode = 1
let rnd = System.Random()
let mutable realCode: code = []
let mutable realGuess: code = []
let mutable xplayer = ""
let mutable running = true
let mutable haveWon = false
let mutable guessCount = 0
let mutable plade: board = []
let mutable blackcount = 0
let mutable whitecount = 0
let mutable wb: answer = (0,0)
let mutable checkGuess: code = []
let mutable y = 0
let mutable cl = 0
let mutable i = 0
let mutable tempCode: code = []

/// Colored printf nakket fra Chr. Smith på https://blogs.msdn.microsoft.com/chrsmith/2008/10/01/f-zen-colored-printf/

let cprintf c fmt = 


    Printf.kprintf

        (fun s ->

            let old = System.Console.ForegroundColor

            try

              System.Console.ForegroundColor <- c;

              System.Console.Write s

            finally

              System.Console.ForegroundColor <- old)

        fmt


// Colored printfn nakket fra Chr. Smith på https://blogs.msdn.microsoft.com/chrsmith/2008/10/01/f-zen-colored-printf/

let cprintfn c fmt =

    cprintf c fmt

    printfn ""

///<summary>
///Creates titlescreen
///</summary>
let Titlescreen1 () =
  cprintf System.ConsoleColor.Green "
                ███╗   ███╗ █████╗ ███████╗████████╗███████╗██████╗ 
                ████╗ ████║██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
                ██╔████╔██║███████║███████╗   ██║   █████╗  ██████╔╝
                ██║╚██╔╝██║██╔══██║╚════██║   ██║   ██╔══╝  ██╔══██╗
                ██║ ╚═╝ ██║██║  ██║███████║   ██║   ███████╗██║  ██║
                ╚═╝     ╚═╝╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝"
  cprintfn System.ConsoleColor.Yellow"                                                    
                           ███╗   ███╗██╗███╗   ██╗██████╗                     
                           ████╗ ████║██║████╗  ██║██╔══██╗                    
                           ██╔████╔██║██║██╔██╗ ██║██║  ██║                    
                           ██║╚██╔╝██║██║██║╚██╗██║██║  ██║                    
                           ██║ ╚═╝ ██║██║██║ ╚████║██████╔╝                    
                           ╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═════╝"  
let Titlescreen2() =
  printfn "
                              By: Jonas, Malik & Thomas\n
                        Please resize terminal to fit header\n
                              -Press any key to play-\n"    

let Titlescreen3 () =
  printfn ""
  printfn "Choose from 6 different colors:"
  cprintf System.ConsoleColor.Red "Red: r, " 
  cprintf System.ConsoleColor.Green "Green: g, "
  cprintf System.ConsoleColor.Yellow "Yellow: y, " 
  cprintf System.ConsoleColor.Magenta "Purple: p, "
  cprintf System.ConsoleColor.White "White: w, "
  cprintfn System.ConsoleColor.Black "and Black: b" 
  printfn ""                                         

///<summary>
///playerType bruges til at bestemme hvad type en given spiller (Codemaker eller codeguesser) er (Human/Computer).
///</summary>
let rec playerType() =
  xplayer <- (System.Console.ReadLine()).ToLower()
  match xplayer with
  | "human"    -> Human
  | "computer" -> Computer
  | "cpu"      -> Computer
  | "h"        -> Human
  | "c"        -> Computer
  | _          -> 
    System.Console.Clear()
    Titlescreen1()
    Titlescreen3()
    printf "Invalid player type, please try again with [Computer/cpu/c or Human/h]:"
    playerType() 

///<summary>
/// genRandomCode laver en tilfældig kode (Realcode)
///</summary>
let genRandomCode codeLength =
    let mutable lst = []
    let mutable x = 1
    for i in 1 .. codeLength do
      x <- rnd.Next(6)
      match x with
      | 0 -> lst <- Red::lst
      | 1 -> lst <- Green::lst
      | 2 -> lst <- Yellow::lst
      | 3 -> lst <- Purple::lst
      | 4 -> lst <- White::lst
      | 5 -> lst <- Black::lst
      | _ -> failwith "wrong color"
    List.rev (lst)



///<summary>
/// cxcode bruges til at bestemme længden af en given kode (xcode) tager brugerens input.
///</summary>
let rec cxcode() =
  let n = System.Console.ReadLine()
  match System.Int32.TryParse(n) with
  | (true, x) -> 
    if x > 0 then
      x
    else
      System.Console.Clear()
      Titlescreen1()
      Titlescreen3()
      printf "Invalid code length please try again: "
      cxcode()     
  | _         ->
    System.Console.Clear() 
    Titlescreen1()
    Titlescreen3()
    printf "Invalid code length please try again: "
    cxcode()


///<summary>
/// playerInput laver en kode ud af brugerens input (ex. "r r r r" bliver til koden: Red Red Red Red), foolproof
///</summary>
let rec playerInput() =
  let rec subplayerInput codelist =
      match codelist with
      | "r":: xs      -> Red::subplayerInput xs
      | "g":: xs      -> Green::subplayerInput xs
      | "y":: xs      -> Yellow::subplayerInput xs
      | "p":: xs      -> Purple::subplayerInput xs
      | "w":: xs      -> White::subplayerInput xs
      | "b":: xs      -> Black::subplayerInput xs
      | "red":: xs    -> Red::subplayerInput xs
      | "green":: xs  -> Green::subplayerInput xs
      | "yellow":: xs -> Yellow::subplayerInput xs
      | "purple":: xs -> Purple::subplayerInput xs
      | "white":: xs  -> White::subplayerInput xs
      | "black":: xs  -> Black::subplayerInput xs
      | []            -> []
      | _ -> playerInput()

  let code = (System.Console.ReadLine()).ToLower()
  let codeArr = code.Split [|' '|]
  let codelist = codeArr |> Array.toList
  if codelist.Length = xcode then
    subplayerInput codelist
  else
    System.Console.Clear()
    Titlescreen1()
    Titlescreen3()
    printfn "The code is %i colors long" xcode
    printfn "Example of valid code: r Green YELLOW w"
    printf "Invalid code, please try again - remember spaces between colors: "
    playerInput()

///<summary>
///makeCode kalder playerInput og genRandomCode med xcode hvis det er henholdsvis en Human eller Computer playerType
///</summary>
let makeCode player =
  if player = Human then
    printf "type your code here: "
    realCode <- playerInput()
  elif player = Computer then
    realCode <- (genRandomCode xcode)
  else 
    failwith "unable to determine player type"
  realCode
///<summary>
///printBoard tilføjer det nye gæt til pladen, og printer den nye plade.
///</summary>
let printBoard realGuess =
  plade <- List.rev(plade)
  plade <- (realGuess,wb)::plade
  plade <- List.rev(plade)
  for i in 0 .. plade.Length-1 do
    printfn "%A" plade.[i]


///<summary>
///guess kaldes med playerType, guess gemmer spillerens/computerens gæt i realGuess og laver gættet på denne kode
///</summary>
let guess player =
  if player = Human then
    printfn "The code is %i colors long" xcode
    printf "place your guess: "
    realGuess <- playerInput()
  elif player = Computer then
    realGuess <- (genRandomCode xcode)
  else
    failwith "unable to determine player guess"
  realGuess
///<summary>
///delete er en hjælpefunktion til at fjerne de farver, der er rigtige i whitecount for at undgå duplikater.
///</summary>
let rec delete (xs:code) (y:codeColor) =
 match xs with
 | x::xs when x = y -> xs
 | x::xs -> x::delete xs y
 | _ -> []
///<summary>
///validate sammenligner gæt med kode og finder ud af, hvor mange hvide og hvor mange sorte stifter, der skal uddeles.
///y og i er tællevariabler, så vi kan kontrollere whilelykkerne. cl er en variabel, der sørger for, at længden af gættet
///bliver opdateret, så whilelykken ikke fejler.
///</summary>
let validate (code: code) (guess: code) =
  blackcount <- 0
  for i in 0 .. xcode-1 do
    if guess.[i] = code.[i] then
      blackcount <- blackcount + 1
  whitecount <- 0
  checkGuess <- realGuess
  y <- 0
  i <- 0
  tempCode <- code
  while y < checkGuess.Length do
    cl <- checkGuess.Length
    if y < cl then
      i <- 0
      while i < tempCode.Length do
        if tempCode.[i] = checkGuess.[y] then
          whitecount <- whitecount+1
          checkGuess <- delete checkGuess checkGuess.[y]
          tempCode <- delete tempCode tempCode.[i]
          y <- y-1
          i <- 100
        i <- i+1  
    y <- y+1
  whitecount <- (whitecount - blackcount)
  
  if blackcount = xcode then
    wb <- (whitecount, blackcount)
    System.Console.Clear()
    cprintfn System.ConsoleColor.DarkYellow "
██╗   ██╗ ██████╗ ██╗   ██╗    ██╗    ██╗ ██████╗ ███╗   ██╗██╗██╗
╚██╗ ██╔╝██╔═══██╗██║   ██║    ██║    ██║██╔═══██╗████╗  ██║██║██║
 ╚████╔╝ ██║   ██║██║   ██║    ██║ █╗ ██║██║   ██║██╔██╗ ██║██║██║
  ╚██╔╝  ██║   ██║██║   ██║    ██║███╗██║██║   ██║██║╚██╗██║╚═╝╚═╝
   ██║   ╚██████╔╝╚██████╔╝    ╚███╔███╔╝╚██████╔╝██║ ╚████║██╗██╗
   ╚═╝    ╚═════╝  ╚═════╝      ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═══╝╚═╝╚═╝"
    if xplayer = "computer" || xplayer = "cpu" || xplayer = "c" then
      printfn "The answer was: %A" realGuess
    haveWon <- true
  elif xplayer = "human" || xplayer = "h" then
    wb <- (whitecount, blackcount)
    System.Console.Clear()
    cprintfn System.ConsoleColor.DarkRed "
██╗    ██╗██████╗  ██████╗ ███╗   ██╗ ██████╗ ██╗
██║    ██║██╔══██╗██╔═══██╗████╗  ██║██╔════╝ ██║
██║ █╗ ██║██████╔╝██║   ██║██╔██╗ ██║██║  ███╗██║
██║███╗██║██╔══██╗██║   ██║██║╚██╗██║██║   ██║╚═╝
╚███╔███╔╝██║  ██║╚██████╔╝██║ ╚████║╚██████╔╝██╗
 ╚══╝╚══╝ ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝ ╚═╝"
    Titlescreen3()
    printf "Feedback is written: "
    cprintf System.ConsoleColor.White "White, "
    cprintfn System.ConsoleColor.Black "Black"
    printfn ""


let main() =
//Clear console + print titlescreen
  //System.Console.Clear()
  //Titlescreen ()

//gameloopet som holder spillet i gang, programmet vil terminerer når running = false
  while running do
//game setup disse ting kører kun 1 gang
    System.Console.Clear()
    Titlescreen1 ()
    Titlescreen2 ()
    match System.Console.Read() with
      | c when c = 27 -> failwith "
    ██████╗ ██╗   ██╗███╗   ██╗ █████╗ ███╗   ███╗██╗ ██████╗        
    ██╔══██╗╚██╗ ██╔╝████╗  ██║██╔══██╗████╗ ████║██║██╔════╝        
    ██║  ██║ ╚████╔╝ ██╔██╗ ██║███████║██╔████╔██║██║██║             
    ██║  ██║  ╚██╔╝  ██║╚██╗██║██╔══██║██║╚██╔╝██║██║██║             
    ██████╔╝   ██║   ██║ ╚████║██║  ██║██║ ╚═╝ ██║██║╚██████╗        
    ╚═════╝    ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝ ╚═════╝        
                                                                     
                   ███████╗██╗  ██╗██╗████████╗                     
                   ██╔════╝╚██╗██╔╝██║╚══██╔══╝                     
                   █████╗   ╚███╔╝ ██║   ██║                        
                   ██╔══╝   ██╔██╗ ██║   ██║                        
                   ███████╗██╔╝ ██╗██║   ██║                        
                   ╚══════╝╚═╝  ╚═╝╚═╝   ╚═╝"
      | _ -> 
        System.Console.Clear() 
        Titlescreen1()
        Titlescreen3()
    printf "Select code maker [Computer/cpu/c or Human/h]: "
    let codemaker = playerType()
    printf "Select code length: "
    xcode <- cxcode()
    makeCode (codemaker)
    System.Console.Clear()
    Titlescreen1()
    Titlescreen3()
    printf "Select code guesser [Computer/cpu/c or Human/h]: " 
    let codeguesser = playerType()
    printfn "%A" realCode
    System.Console.Clear()
    Titlescreen1()
    Titlescreen3()

//codeguesser gætter indtil 
    while haveWon = false do
      guess (codeguesser)
      validate realCode realGuess
      if xplayer = "human" || xplayer = "h" then
        printBoard realGuess
      guessCount <- guessCount + 1
//win scenariet (haveWon = true)
    printfn "A total of %i guesses were used" guessCount
    guessCount <- 0
    haveWon <- false
    plade <- []
//spilleren har mulighed for at starte spillet forfra eller stoppe spillet (running sættes til false)
    printfn "Press any key to play again, or ESC to exit."
    //let terminator = (System.Console.ReadLine()).ToLower()
    //if terminator = "start" then
      //System.Console.Clear()
    //else if terminator = "exit" then
    match System.Console.Read() with
      | c when c = 27 -> 
        running <- false
        System.Console.Clear()
      | _ -> 
        System.Console.Clear()

main()



