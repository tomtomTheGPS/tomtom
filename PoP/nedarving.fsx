type Person(name:string) = class
  member this.Name = name

  abstract Greet : unit -> unit
  default this.Greet() = printfn "Hi, i'm %s" this.Name
end

type Student(name, studentID:int) = class
  inherit Person(name)

  let mutable karrakter = 0

  member this.StudentID = studentID
  member this.Karrakter = karrakter 
  member this.SetKarrakter(value:int) = karrakter <- value
end

type Worker(name, job:string) = class
  inherit Person(name)
  member this.Employer = job
end

type FrenchWorker(name, job:string) = class
  inherit Person(name)


  override this.Greet() = printfn "Bonjour, je m'appelles %s" this.Name
end

type Job(name:string) = class
  let mutable (workForce:Person list) = []

  member this.Name = name
  member this.GetPersons = workForce

  member this.AddToWorkForce(person:Person) = workForce <- workForce@[person]
end

let moreWorkers (job:Job) (person:Person) =
  job.AddToWorkForce person

let printWorkForce (job:Job) =
  for x in job.GetPersons do
    x.Greet()

let main() =
  let Susanne  = new Person("Susanne")
  let Birgitte = new Student("Birgitte", 1234)
  let Hanne    = new Worker("Hanne", "PoP-Instruktor")
  let Pierre   = new FrenchWorker("Pierre", "Louvre")
  let DIKU     = new Job("PoP-Instruktorererere")

  moreWorkers DIKU Susanne
  moreWorkers DIKU Birgitte
  moreWorkers DIKU Hanne
  moreWorkers DIKU Pierre
  printWorkForce DIKU

main()


