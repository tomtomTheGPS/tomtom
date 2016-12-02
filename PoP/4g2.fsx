//removeDuplicates skal kunne køre med en tom liste og en liste med kun
//ét element. samtidigt skal det kunne vises, at det er de første tal,
//der bliver beholdt.

let reverse xs =
  let rec rev list = function
    | [] -> list
    | x::xs -> rev (x::list) xs
  rev [] xs


let rec delete xs y =
  match xs with
  | x::xs when x = y -> xs
  | x::xs -> x::delete xs y
  | _ -> []


let rec minus xs ys =
  match ys with
  | [] -> xs
  | y::ys -> (minus (delete xs y) ys)


let findDuplicates l =
  let mutable duplicates = []
  let mutable remain = []
  for i in 0..((List.length l)-2) do
    let x = l.[i]
    for j in i+1..((List.length l)-1) do
      let y = l.[j]
      if y = x then
        duplicates <- l.[j]::duplicates
      else
        remain <- l.[j]::remain

  duplicates


let removeDuplicates xs =
  reverse (minus (reverse xs) (findDuplicates xs))

printfn "%b" (removeDuplicates [1;2;1;3;2] = [1;2;3])
printfn "%b" (removeDuplicates [5;5;4;4;3;3;2;2;1;1] = [5;4;3;2;1])
printfn "%b" (removeDuplicates [] = [])
printfn "%b" (removeDuplicates [1;2;3;4] = [1;2;3;4])




//Alternativ løsning:

//let lst = [1;2;3;2;1]

//let removeDuplicates l =
  //Seq.distinct l

//printfn "%A" (removeDuplicates lst)