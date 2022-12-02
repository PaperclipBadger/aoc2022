inductive Janken where
  | Rock : Janken
  | Paper : Janken
  | Scissors : Janken


inductive Outcome where
  | Win : Outcome
  | Draw : Outcome
  | Lose : Outcome


def outcome : Janken → Janken → Outcome
  | Janken.Rock, Janken.Rock => Outcome.Draw
  | Janken.Rock, Janken.Paper => Outcome.Win
  | Janken.Rock, Janken.Scissors => Outcome.Lose
  | Janken.Paper, Janken.Rock => Outcome.Lose
  | Janken.Paper, Janken.Paper => Outcome.Draw
  | Janken.Paper, Janken.Scissors => Outcome.Win
  | Janken.Scissors, Janken.Rock => Outcome.Win
  | Janken.Scissors, Janken.Paper => Outcome.Lose
  | Janken.Scissors, Janken.Scissors => Outcome.Draw


def shape_score : Janken → Int
  | Janken.Rock => 1
  | Janken.Paper => 2
  | Janken.Scissors => 3


def outcome_score : Outcome → Int
  | Outcome.Win => 6
  | Outcome.Draw => 3
  | Outcome.Lose => 0


def score (theirs : Janken) (mine : Janken) : Int :=
  shape_score mine + outcome_score (outcome theirs mine)


def uncurry (f : a → b → c) : a × b → c
  | ⟨ x , y ⟩ => f x y


def total_score (games : List (Janken × Janken)) : Int :=
  List.foldl (. + .) 0 (List.map (uncurry score) games)


def parse_janken : String → Janken
  | "A" => Janken.Rock
  | "B" => Janken.Paper
  | "C" => Janken.Scissors
  | "X" => Janken.Rock
  | "Y" => Janken.Paper
  | "Z" => Janken.Scissors
  | _ => sorry


def parse_line (line : String) : Janken × Janken :=
  match String.splitOn line " " with
  | [theirs, mine] => ⟨ parse_janken theirs, parse_janken mine ⟩
  | _ => sorry


def parse_file (file : String) : List (Janken × Janken) :=
   List.map parse_line (String.splitOn file "\n")


def main : List String → IO Unit
  | [filename] => do
    let file <- IO.FS.readFile filename
    IO.println $ total_score (parse_file file)
  | _ => IO.println "bad"