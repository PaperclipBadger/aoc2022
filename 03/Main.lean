import Compartments

def parse_line (line : String) : Backpack :=
  {
    compartment_a := (line.take (line.length / 2)).toList,
    compartment_b := (line.drop (line.length / 2)).toList,
  }

def parse_triplets (lines : List String) : List (String × String × String) :=
  match lines with
  | [] => []
  | a :: b :: c :: tail => ⟨a, b, c⟩ :: parse_triplets tail
  | _ => sorry 

def sum : List Int → Int := List.foldl (. + .) 0

def main : List String →  IO Unit
  | ["one", filename] => do
    let file ← IO.FS.readFile filename
    let lines := String.splitOn file "\n"
    let packs := List.map parse_line lines
    let shared' (backpack : Backpack) := shared backpack.compartment_a backpack.compartment_b
    let total := sum (List.map (sum ∘ (List.map priority) ∘ shared') packs)
    IO.println total
  | ["two", filename] => do
    let file ← IO.FS.readFile filename
    let lines := String.splitOn file "\n"
    let triplets := parse_triplets lines
    let shared' : String × String × String → List Char
      | ⟨ a, b, c ⟩ => shared (shared a.toList b.toList) (shared a.toList c.toList)
    let total := sum (List.map (sum ∘ (List.map priority) ∘ shared') triplets)
    IO.println total
  | _ => IO.println "bad arguments"
