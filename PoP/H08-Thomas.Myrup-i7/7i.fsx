printfn "------ Opgave 0 ------"
let anArray = [|1..10|] 

printfn "safeIndexTry:"

///<summary>
///Tager et pladsnummer i "anArray" som input og returnere pladsens værdi. Hvis pladsen ikke findes, 
///smider den en brugerdefineret exception.
///</summary>
let safeIndexTry i =
  try
    anArray.[i]
  with
    | _ when i > (Array.length anArray) || i < 0 -> failwith "i er større end anArray eller mindre end 0"

printfn "%b" (safeIndexTry 2 = 3)
printfn "%b" (safeIndexTry 0 = 1)
printfn "%b" (safeIndexTry 6 = 7)
//printfn "%i" (safeIndexTry -3) 
//Dette giver den brugerdefinerede fejl, men er kommenteret så resten af koden kan køre.

printfn "\nsafeIndexIf:"

///<summary>
///Returnerer, ligesom safeIndexTry, det der står på i's plads i anArray. Men hvis i
///ligger udenfor Arrayet, returnerer den i, i stedet for en exception.
///</summary>
let safeIndexIf i =
  if i < 0 || i > (Array.length anArray) then
    i
  else
    anArray.[i]

printfn "%b" (safeIndexIf 2 = 3)
printfn "%b" (safeIndexIf 0 = 1)
printfn "%b" (safeIndexIf 6 = 7)
printfn "%b" (safeIndexIf -3 = -3)
printfn "%b" (safeIndexIf 30 = 30)

printfn "\nsafeIndexOption:"

///<summary>
///Returener en option, i stedet for blot værdien, eller en exception.
///</summary>
let safeIndexOption i =
  if i < 0 || i > (Array.length anArray) then
    None
  else
    Some anArray.[i]

printfn "%b" (Option.get (safeIndexOption 2) = 3)
printfn "%b" (Option.get (safeIndexOption 0) = 1)
printfn "%b" (Option.get (safeIndexOption 6) = 7)
printfn "%b" (safeIndexOption -3 = None)
printfn "%b" (safeIndexOption 30 = None)


printfn "------ Opgave 1 ------"
open System
open System.IO

///<summary>
///laver en ny fil som funktionen fileReplace retter i.
///</summary>
let nyFil = File.CreateText("ThomasInput.txt")
nyFil.WriteLine("test tast test tast test")
nyFil.Close()

///<summary>
///printer inholdet af ThomasInput.txt inden rettelserne.
///</summary>
let before = File.OpenText("ThomasInput.txt")
let beforeRead = before.ReadLine()
before.Close()
printfn "%s" beforeRead

///<summary>
///fileReplace erstatter tast med test i den nyligt oprettede fil "ThomasInput.txt"
///ved at lave det oprindelige indhold til en string, rette i den string
///og så lave en ny fil med det nye indhold som overskriver den oprindelige fil.
///</summary>
let fileReplace (y:string) (z:string) =
  let opn = File.OpenText("ThomasInput.txt")
  let x = opn.ReadLine()
  opn.Close()
  let ny = x.Replace(y, z)
  let outp = File.CreateText("ThomasInput.txt")
  outp.WriteLine(ny)
  outp.Close()

(fileReplace "tast" "test")

let after = File.OpenText("ThomasInput.txt")
let afterRead = after.ReadLine()
after.Close()
printfn "%s" afterRead
///<summary>
///printer indholdet af ThomasInput.txt efter rettelserne.
///</summary>

printfn "------ Opgave 2 ------"
///<summary>
/// Set an url up as a stream 
///</summary>
let url2Stream url = 
  let uri = System.Uri url 
  let request = System.Net.WebRequest.Create uri 
  let response = request.GetResponse () 
  response. GetResponseStream ()
///<summary>
/// Read all contents of a web page as a string 
///</summary>
let readUrl url = 
  let stream = url2Stream url 
  let reader = new System.IO.StreamReader(stream) 
  reader.ReadToEnd ()

let url = "https://en.wikipedia.org/wiki/World_War_II" 
let html = readUrl url 
///<summary>
///fjerner det man søger efter fra den oprindelige html-kode og trækker de to længder fra hinanden.
///Resulatet divideres med længden af søgestrengen for at få antallet.
///</summary>
let countLinks (where:string) (what:string) =
  match what with
  | "" -> 0
  | _  -> (where.Length - where.Replace(what, "").Length)/what.Length

printfn "%i" (countLinks html "</a>")