import Lake
open Lake DSL

package compartments {
  -- add package configuration options here
}

lean_lib Compartments {
  -- add library configuration options here
}

@[default_target]
lean_exe compartments {
  root := `Main
}
