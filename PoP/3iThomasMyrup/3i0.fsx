// Opgave a
printfn "\n ****** Opgave a ******"

let sum n =
  let mutable s = n
  let mutable x = 0
  while s >= 0 do
    x <- (n-s)+x
    s <- s - 1
  x

printfn "%i" (sum 10);;

//Hvor meget vil du have jeg forklarer? Det er en sumfunktion... 
//mutable er brugt for at kunne ændre variablerne


// Opgave b
printfn "\n ****** Opgave b ******"

let rec recSum n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> n+recSum(n-1)

printfn "%i" (recSum 10);;

//funktionen virker ikke med negative tal da standarttilfældene
//gælder for 0 og 1, men ikke mindre end det - kan fikses nemt, men så havde jeg intet at skrive her.


// Opgave c
printfn "\n ****** Opgave c ******"

let simpleSum n =
  (n*(n+1))/2

printfn "%i" (simpleSum 10)

//Hvad skal jeg sige? den hedder simple.


// Opgave d
printfn "\n ****** Opgave d ******"

let tabel n =
  let space = "   "
  printf "%s" "n"
  printf "%s" (space)
  printf "%s" "(sum n)"
  printf "%s" "(recSum n)"
  printfn "%s" "(simpleSum n)"
  printf "%i" n
  printf "%s" (space)
  printf "%i" (sum n)
  printf "%s" (space)
  printf "%s" (space)
  printf "%s" (space)
  printf "%i" (recSum n)
  printf "%s" (space)
  printf "%s" (space)
  printf "%s" (space)
  printfn "%i" (simpleSum n)
  sum n = recSum n && recSum n = simpleSum n

printfn "%b" (tabel 10);;

//Den er lidt rodet, I know, men så ser programmet pænt ud når man kører det.