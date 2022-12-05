def priority (item : Char) : Int :=
  let ascii := Char.toNat item

  if Char.isLower item
  then ascii - 96
  else ascii - 38

structure Backpack where
  compartment_a : List Char
  compartment_b : List Char

def shared {α : Type} [BEq α] (as : List α) (bs : List α) :=
  let accrue (pred : α → Bool) (l : List α) (a : α) : List α :=
    if pred a && !(l.contains a) then a :: l else l
  List.foldl (accrue bs.contains) [] as