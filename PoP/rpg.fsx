type equipment = Sword | Shield | Health_potion | Mana_potion | Legendary_sword | Legendary_shield

type hero (name:string, health:int, mana:int, level:int) = class
    let mutable lv = level
    let mutable hp = health    
    let mutable mp = mana
    let mutable inventory = [Sword; Shield]

    member this.Name      = name
    member this.Health    = hp
    member this.Mana      = mp
    member this.Level     = lv
    member this.Inventory = inventory

    member this.RemoveInventory(item:equipment) =
      let rec removeItem (aList) =
        match aList with
        | []    -> []
        | x::xs -> if x = item then xs else x::(removeItem xs)
      inventory <- (removeItem inventory)

    member this.AddInventory(item:equipment) = inventory <- ( inventory@[item] )
    member this.LevelUp = lv <- lv + 1
end

let printHero (person:hero) =
  printfn "Name: %s  Health: %i  Mana: %i" person.Name person.Health person.Mana
  printfn "Level: %i" person.Level
  printfn "Inventory: %A" person.Inventory
  printfn ""


let addToInventory (person:hero) (item:equipment) =
  person.AddInventory item

let removeFromInventory (person:hero) (item:equipment) =
  person.RemoveInventory item

let tradeWithHero (from:hero) (target:hero) (item:equipment) =
  removeFromInventory from item
  addToInventory target item

let main() =
  let emil   = new hero("Emil", 100, 80, 10)
  let jon    = new hero("Jon", 100, 80, 100)  

  printHero emil
  printHero jon
  emil.LevelUp
  addToInventory emil Legendary_shield
  printHero emil
  tradeWithHero emil jon Legendary_shield
  printHero emil
  printHero jon

main()