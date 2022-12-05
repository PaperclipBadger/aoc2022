inductive Janken where
  | Rock : Janken
  | Paper : Janken
  | Scissors : Janken
  deriving BEq


inductive Outcome where
  | Win : Outcome
  | Draw : Outcome
  | Lose : Outcome


def cycle : Janken → Janken
  | Janken.Rock => Janken.Paper
  | Janken.Paper => Janken.Scissors
  | Janken.Scissors => Janken.Rock


def uncycle : Janken → Janken
  | Janken.Rock => Janken.Scissors
  | Janken.Paper => Janken.Rock
  | Janken.Scissors => Janken.Paper
  

def outcome (theirs : Janken) (mine : Janken) : Outcome :=
  if mine == cycle theirs then Outcome.Win
  else if theirs == cycle mine then Outcome.Lose
  else Outcome.Draw


def throw_for (theirs : Janken) : Outcome → Janken
  | Outcome.Win => cycle theirs
  | Outcome.Draw => theirs
  | Outcome.Lose => uncycle theirs


def shape_score : Janken → Int
  | Janken.Rock => 1
  | Janken.Paper => 2
  | Janken.Scissors => 3


def outcome_score : Outcome → Int
  | Outcome.Win => 6
  | Outcome.Draw => 3
  | Outcome.Lose => 0


def score (theirs : Janken) (mine : Janken): Int :=
  let outcome' := outcome theirs mine
  shape_score mine + outcome_score outcome'


def score' (theirs : Janken) (outcome : Outcome): Int :=
  let mine := throw_for theirs outcome
  shape_score mine + outcome_score outcome


def uncurry (f : a → b → c) : a × b → c
  | ⟨ x , y ⟩ => f x y


def total_score (games : List (Janken × Janken)) : Int :=
  List.foldl (. + .) 0 (List.map (uncurry score) games)


def total_score' (games : List (Janken × Outcome)) : Int :=
  List.foldl (. + .) 0 (List.map (uncurry score') games)


def parse_janken : String → Janken
  | "A" => Janken.Rock
  | "B" => Janken.Paper
  | "C" => Janken.Scissors
  | "X" => Janken.Rock
  | "Y" => Janken.Paper
  | "Z" => Janken.Scissors
  | _ => sorry


def parse_outcome : String → Outcome
  | "X" => Outcome.Lose
  | "Y" => Outcome.Draw
  | "Z" => Outcome.Win
  | _ => sorry


def parse_line (line : String) : Janken × Janken :=
  match String.splitOn line " " with
  | [theirs, mine] => ⟨ parse_janken theirs, parse_janken mine ⟩
  | _ => sorry


def parse_line' (line : String) : Janken × Outcome :=
  match String.splitOn line " " with
  | [theirs, mine] => ⟨ parse_janken theirs, parse_outcome mine ⟩
  | _ => sorry


def parse_file (file : String) : List (Janken × Janken) :=
   List.map parse_line (String.splitOn file "\n")


def parse_file' (file : String) : List (Janken × Outcome) :=
   List.map parse_line' (String.splitOn file "\n")


def main : List String → IO Unit
  | ["one", filename] => do
    let file <- IO.FS.readFile filename
    IO.println $ total_score (parse_file file)
  | ["two", filename] => do
    let file <- IO.FS.readFile filename
    IO.println $ total_score' (parse_file' file)
  | _ => IO.println "bad"