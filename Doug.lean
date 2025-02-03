
import Init.System.FilePath
import Cli
open Cli

set_option relaxedAutoImplicit false

/-!
  # Doug

  A Lean implementation of the Unix `tree` command,
  rendering a directory structure as a visual tree.
-/
namespace Doug

/-- Represents a file system entry: either a file or a directory. -/
inductive FileEntry where
  | file (name : String)
  | dir  (name : String)
  deriving Repr, BEq, Hashable, DecidableEq

/-- Configuration for rendering the directory tree. -/
structure Config where
  /-- Use Unicode glyphs if true; otherwise, use ASCII symbols. -/
  unicode?   : Bool   := true
  /-- Current prefix for nested entries. -/
  currPrefix : String := ""

deriving instance Repr for InputFlag, ParseError.Kind
/-- Converts a ParseError to a string for error reporting. -/
instance : ToString ParseError :=
  ⟨fun err => s!"{repr err.kind}"⟩

/--
  Extract the base name from a file path as an optional FileEntry.
  Returns none when the basename is "." or "..".
-/
def toEntry (path : System.FilePath) : IO (Option FileEntry) := do
  match path.components.getLast? with
  | none                 => pure (some (.dir ""))
  | some "." | some ".." => pure none
  | some file            =>
      let entry := if (← path.isDir) then .dir file else .file file
      return (some entry)

/-- Returns the marker string for a directory entry. -/
def Config.leftDirMarker (cfg : Config) : String :=
  if cfg.unicode? then "│ " else "| "

/-- Returns the marker string for a file entry. -/
def Config.leftFileMarker (cfg : Config) : String :=
  if cfg.unicode? then "├── " else "|-- "

/--
  Updates the configuration with a directory prefix from the given path.
-/
def Config.withDir (cfg : Config)  : Config :=
  { cfg with currPrefix := leftDirMarker cfg ++ " " ++ cfg.currPrefix }

/--
  Composes the display string for a file entry.
-/
def Config.fileName (cfg : Config) (file : String) : String := s!"{cfg.currPrefix}{cfg.leftFileMarker} {file}"

/--
  Composes the display string for a directory entry.
-/
def Config.dirName (cfg : Config) (dir : String) : String := s!"{cfg.currPrefix}{cfg.leftDirMarker} {dir}/"

/--
  Prints a file entry using the configured display style.
-/
def Config.showFileName (cfg : Config) (file : String) : IO Unit := do
  IO.println (cfg.fileName file)

/--
  Prints a directory entry using the configured display style.
-/
def Config.showDirName (cfg : Config) (dir : String) : IO Unit := do
  IO.println (cfg.dirName dir)

/--
  Prints a file's name from its file path using the configuration's marker style.
-/
def Config.showfile (cfg : Config) (path : System.FilePath) : IO Unit :=
  IO.println (cfg.fileName path.fileName.get!)

/--
  Recursively prints the directory tree starting from the given path.
-/
partial def dirTree (cfg : Config) (path : System.FilePath) : IO Unit := do
  match (← toEntry path) with
  | some (.file name) =>
      cfg.showFileName name
  | some (.dir name)  =>
      cfg.showDirName name
      let newCfg := cfg.withDir
      for c in (← path.readDir) do
        dirTree newCfg c.path
  | none =>
      return ()

/--
  Executes the 'doug' command by parsing CLI flags and printing the rendering mode.
-/
def runDoug (parsed : Parsed) : IO UInt32 := do
  let ascii? := parsed.flag! "ascii" |>.as! Bool
  let cfg : Config := { unicode? := not ascii? }
  dirTree cfg (← IO.currentDir)
  if cfg.unicode? then
    IO.println "Rendering directory tree with Unicode characters"
  else
    IO.println "Rendering directory tree with ASCII characters"
  return 0

/--
  CLI command configuration for the 'doug' utility.
-/
def cmd : Cmd := `[Cli|
  doug VIA runDoug; ["0.1.0"]
    "Usage: doug [--ascii]
Options:
  --ascii    Use ASCII characters to display the directory structure"
  FLAGS:
    ascii : Bool; "Use ASCII characters to display the directory structure"
]

/--
  Main entry point with default arguments.
-/
def main (args : List String := ["--ascii", "false"]) : IO UInt32 := do
  match cmd.parse args with
  | .ok (_cmd, parsed) => runDoug parsed
  | .error err =>
      IO.eprintln s!"{err}"
      return 1

#eval main

end Doug
