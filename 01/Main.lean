def inSort (lt : α → α → Bool) (a : α) : List α → List α
  | [] => [a]
  | b :: bs => if lt a b then a :: b :: bs else b :: inSort lt a bs

def sort (lt : α → α → Bool) : List α → List α 
  | [] => []
  | [a] => [a]
  | a :: as => inSort lt a (sort lt as)

def mostCalories (elves: List (List Int)) : Int :=
  List.foldl max 0 (List.map (List.foldl (. + .) 0) elves)

def topThree (elves : List (List Int)) : Int :=
  match sort (.>.) (List.map (List.foldl (.+.) 0) elves) with
  | a :: b :: c :: _ => a + b + c
  | _ => 0

def parseInput (file : String) : List (List Int) :=
  let chunks := String.splitOn file "\n\n"
  let splitChunk (x : String) := List.map String.toInt! (String.splitOn x "\n")
  List.map splitChunk chunks

def main : List String → IO UInt32
  | ["top", filename] => do
    let file <- IO.FS.readFile filename
    IO.println (mostCalories (parseInput file))
    pure 0
  | ["topthree", filename] => do
    let file <- IO.FS.readFile filename
    IO.println (topThree (parseInput file))
    pure 0
  | _ => do
    IO.println "bad arguments"
    pure 1