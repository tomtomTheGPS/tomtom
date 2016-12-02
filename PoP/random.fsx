type codeColor = 
  Red | Green | Yellow | Purple | White | Black 
type code = codeColor list 
type answer = int * int 
type board = (code * answer) list 
type player = Human | Computer

let rnd = System.Random()

let randomCode count =
    let mutable lst = []
    let mutable x = 1
    for i in 1 .. count do
      x <- rnd.Next(6)
      match x with
      | 0 -> lst <- Red::lst
      | 1 -> lst <- Green::lst
      | 2 -> lst <- Yellow::lst
      | 3 -> lst <- Purple::lst
      | 4 -> lst <- White::lst
      | 5 -> lst <- Black::lst
      | _ -> failwith "wrong color"
    lst

let l = randomCode 4
printfn "%A" l