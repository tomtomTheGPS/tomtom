let tabel = "\n
 1   2   3   4   5   6   7   8   9  10\n
 2   4   6   8  10  12  14  16  18  20\n
 3   6   9  12  15  18  21  24  27  30\n
 4   8  12  16  20  24  28  32  36  40\n
 5  10  15  20  25  30  35  40  45  50\n
 6  12  18  24  30  36  42  48  54  60\n
 7  14  21  28  35  42  49  56  63  70\n
 8  16  24  32  40  48  56  64  72  80\n
 9  18  27  36  45  54  63  72  81  90\n
10  20  30  40  50  60  70  80  90 100\n"

// Opgave a
printfn "\n ****** Opgave a ******"

let mulTabel n =
  tabel.[..40*n]

printfn "%s" (mulTabel 1)
printfn "%s" (mulTabel 2)
printfn "%s" (mulTabel 3)
printfn "%s" (mulTabel 10)

//En streng, hvor hver linje er 40 karakterer lang så jeg kalder bare det 
//antal linjer jeg skal bruge.


// Opgave b
printfn "\n ****** Opgave b ******"

let loopMulTable n =
  for x in 1..n do
  printfn ""
  for y in 1..10 do
    if ((x*y) < 10) then
      printf " %i  " (x*y)
    if ((x*y) > 9) then
      printf " %i " (x*y)
printfn "\n"

loopMulTable 1
printfn "%s" " "
loopMulTable 2
printfn "%s" " "
loopMulTable 3
printfn "%s" " "
loopMulTable 10

//En modifikation af opgave 5 i øvelserne men hvor x's max værdi er n og ikke 10.



// Opgave c
printfn "\n ****** Opgave c ******"

let rec recMulTable t max n =
  if max < n+1 then
    if t = max*10 then
      let t = max+1
      printf " %i  " (max*10)
      printfn "%s" " "
      recMulTable t (max+1) n
    else
      if t < 10 then
        printf " %i  " t
      else
        printf " %i " t

    if max*10 > t then
      recMulTable (t+max) max n

recMulTable 1 1 1
printfn "%s" " "
recMulTable 1 1 2
printfn "%s" " "
recMulTable 1 1 3
printfn "%s" " "
recMulTable 1 1 10

//Teknisk set bruger jeg ingen variable, men ved ikke om det jeg har gjort
//er helt lovligt? Jeg har dog fået en fungerende og ikke mindst PÆN tabel
//ud af det så håber jeg er tilgivet!
//Hvis det ikke er så tydeligt hvad jeg har gjort, så finder jeg 1-tabelen
//så 2-tabellen osv. op til det ønskede antal tabeller n.



// Opgave d
printfn "\n ****** Opgave d ******"

let compare n =
  printf "mulTabel %s" (mulTabel n)
  printf "%s" "loopMulTable"
  loopMulTable n
  printfn "%s" ""
  printfn "%s" "recMulTable"
  recMulTable 1 1 n

compare 1
compare 2
compare 3
compare 10

//Det er ikke en perfekt sammenligning, men håber du kan se at funktionerne
//giver stort set det samme output.



// Opgave e
printfn "\n ****** Opgave e ******"

printfn "%s" (mulTabel 3)
printfn "%A" (mulTabel 3)

//Som det kan ses når programmet køres, printer %A også citationstegnene med.