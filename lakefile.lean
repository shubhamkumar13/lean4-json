import Lake
open Lake DSL

require std from git "https://github.com/leanprover/std4" @ "main"

package «lean4-json» where
  -- add package configuration options here

lean_lib «Lean4Json» where
  -- add library configuration options here

@[default_target]
lean_exe «lean4-json» where
  root := `Main
