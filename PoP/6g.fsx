type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
///<summary>
///Tager ugedagens plads i ugen som input og returner dagen
///</summary>
let numberToDay aNumber =
  match aNumber with
  | 1 -> Some Monday
  | 2 -> Some Tuesday
  | 3 -> Some Wednesday
  | 4 -> Some Thursday
  | 5 -> Some Friday
  | 6 -> Some Saturday
  | 7 -> Some Sunday
  | _ -> None

type point = int * int 
// a point (x, y) in the plane
type colour = int * int * int 
// (red, green, blue), 0..255 each
type figure =
  | Circle of point * int * colour
    // defined by center, radius, and colour
  | Rectangle of point * point * colour
    // defined by bottom-left corner, top-right corner, and colour
  | Mix of figure * figure
    // combine figures with mixed colour at overlap
  | Twice of figure * (int * int)
    // overlays figure with copy moved by vector

///<summary>
///laver en cirkel og et rekangel og kopierer dem dernæst og flytter dem med vektoren 50,70
///</summary>
let g63 = Twice ((Mix (Circle ((50,50), 45, (255,0,0)), Rectangle ((40,40), (90,110), (0,0,225)))), (50, 70))

///<summary>
///Sørger for, at man kan flytte en figur med en vektor. Det skal nævnes, at vi ikke matcher på "twice"
///da move allerede er indkorporeret i denne type.
///</summary>
let rec move figur (x,y) =
  match figur with
  | Circle ((x0,y0),r,col)          -> Circle ((x0+x, y0+x),r,col)
  | Rectangle ((x0,y0),(x1,y1),col) -> Rectangle ((x0+x,y0+y),(x1+x,y1+y),col)
  | Mix (fig1, fig2)                -> Mix (move fig1 (x,y), move fig2 (x,y))

///<summary>
///Finder ud af, hvilken farve hver pixel skal have.
///</summary>
let rec colourAt (x,y) figure =
  match figure with
    | Circle ((cx,cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
        // bruger Pythagoras sætning til at finde afstand til centrum
      then Some col else None
    | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0<=x && x <= x1 && y0 <= y && y <= y1 // indenfor hjørnerne
      then Some col else None
    | Mix (f1, f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c // overlapper ikke
      | (c, None) -> c // ditto
      | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
        Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2) // gennemsnitsfarve
    | Twice (f1, (v1, v2)) ->
      match (colourAt (x,y) f1, colourAt (x,y) (move f1 (v1,v2))) with
      | (None, c) -> c // overlapper ikke
      | (c, None) -> c // ditto
      | (Some (r1,g1,b1), Some (r2,g2,b2)) -> Some (r2,g2,b2)

///<summary>
///laver et billede ud fra informationer om, hvilke figurer dette skal indeholde.
///</summary>
let makePicture filename figur b h =
  makeBMP.makeBMP filename b h (fun f -> match colourAt f figur with
                                         | None   -> (128,128,128)
                                         | Some x -> x)

makePicture "g63" g63 150 200