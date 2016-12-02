printfn "------ Øvelse 0 ------"

exception NErNegativ of string

let rec fakultet n =
  if n = 1 then
    1
  elif n > 0 then
    n*(fakultet (n-1))
  else
    raise (NErNegativ "n er negativ")

try
  printfn "%b" (fakultet 5 = 120)
  printfn "%b" (fakultet 7 = 5040)
  printfn "%b" (fakultet 4 = 24)
  printfn "%i" (fakultet -4)
with
  | NErNegativ msg -> printfn "n var negativ"

printfn "------ Øvelse 1 ------"

printfn "------ Version 1 ------"
let rec fak n =
  match n with
  | n when n > 1 -> Some (n* Option.get(fak (n-1)))
  | 1 -> Some 1
  | 0 -> Some 0
  | _ -> None

printfn "%A" (fak 5)
printfn "%A" (fak 7)
printfn "%A" (fak 4)
printfn "%A" (fak -4)

printfn "------ Version 2 ------"

let fak2 n =
  let rec realFak m =
    match m with
    | 1 -> 1
    | 0 -> 0
    | m -> m*(realFak (m-1))
  if (n < 0) then None else (Some (realFak n))

printfn "%A" (fak2 5)
printfn "%A" (fak2 7)
printfn "%A" (fak2 4)
printfn "%A" (fak2 -4)

printfn "------ Øvelse 2 ------"

let printFile (reader : System.IO.StreamReader) = 
  while not(reader.EndOfStream) do 
    let line = reader.ReadLine () 
    printfn "%s" line

//let filename =
  //printfn "Indtast filnavn"
  //System.Console.ReadLine()

//let reader = System.IO.File.OpenText filename 
//printFile reader

printfn "------ Øvelse 3 ------"

/// Set a url up as a stream 
let url2Stream url = 
  let uri = System.Uri url 
  let request = System.Net.WebRequest.Create uri 
  let response = request.GetResponse () 
  response. GetResponseStream ()

/// Read all contents of a web page as a string 
let readUrl url = 
  let stream = url2Stream url 
  let reader = new System.IO.StreamReader(stream) 
  reader.ReadToEnd ()

let url = "http://google.com" 
let html = readUrl url 
printfn "Downloaded %s. It says: %s" url html