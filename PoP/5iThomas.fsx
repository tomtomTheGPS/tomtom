let arraySortD (anArray: 'a[]) =
  for j = 2 to (Array.length anArray) do
    let mutable key = anArray.[j]
    let mutable i = j - 1
    while i > 0 && anArray.[i] > key do
      anArray.[i + 1] <- anArray.[i]
    key <- anArray.[i + 1]

let aa = [|1; 7; 5; 2; 1|]
arraySortD aa
printfn "%A" aa