import Lake
open Lake DSL

package janken {
  -- add package configuration options here
}


@[default_target]
lean_exe janken {
  root := `Main
}
