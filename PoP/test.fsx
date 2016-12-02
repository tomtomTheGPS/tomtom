let rec recDown n =
  match n with
  | 0 -> []
  | x -> x::(recDown (x-1))

printfn "%A" (recDown 10)

let rec recUp n l =
  match n with
  | 0 -> l
  | _ -> recUp (n-1) (n::l)

printfn "%A" (recUp 10 [])



let rec recSum aList =
  match aList with
  | []    -> 0
  | head::tail -> head + (recSum tail)

printfn "%A" (recSum [1..10])



let rec multi n bList =
  match bList with
  | []         -> 0
  | head::tail -> 
    if (head = n) then 
      1+(multi n tail) 
    else 
      (multi n tail)

printfn "%A" (multi 4 [4;4;4;5;10;11;4])



let rec split l cList dList = 
 match l with
 | []         -> cList, dList
 | [x]        -> (x::dList, cList)
 | head::neck::tail -> split tail (neck::cList) (head::dList)

printfn "%A" (split [1..31] [] [])


let rec isTable xs =
  match xs with
  | [x]  -> true 
  | []   -> false
  | x1::(x2:: _ as xs) -> List.length x1 = List.length x2 && isTable xs

printfn "%b" (isTable [[1..5];[1..6]])

let lst = [1;2;3;4;5;6]
let lstNew = 1::lst
let three = lst.[2]

printfn "%A" three
printfn "%A" lstNew