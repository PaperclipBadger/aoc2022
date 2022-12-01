import Lake
open Lake DSL

package calories {
  -- add package configuration options here
}

@[default_target]
lean_exe calories {
  root := `Main
}
