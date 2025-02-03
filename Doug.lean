
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

/-- A monadic action. Just a map `Config → IO α` -/
abbrev ConfigIO (α) := Config → IO α

/--Lifts a pure value into the ConfigIO monad by ignoring the config -/
def liftPure (a : α) : ConfigIO α := fun _ => pure a

/--This is a monad because it has a pure and bind function-/
instance : Monad ConfigIO where
  /- Pure ignores the config-/
  pure := liftPure
  /- Bind takes a config and a function that takes a config and returns an IO action, and returns an IO action-/
  bind result next := fun cfg => do
    let newConfig  <- result cfg
    next newConfig cfg

def ConfigIO.run (cfg:Config)(action: ConfigIO α):IO α := action cfg

/--Returns the current config-/
def currentConfig : ConfigIO Config := pure

/--Locally changes the config for the duration of the action-/
def locally (change :Config->Config) (action: ConfigIO α) : ConfigIO α:=
  fun cfg =>
    let newCfg := change cfg
    action newCfg

/--Runs an IO action ignoring the config and returns the result in the ConfigIO monad-/
def runIO (action: IO α) : ConfigIO α := fun _ =>
  action

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
def showFileName  (file : String) : ConfigIO Unit := do
  runIO (IO.println ((←currentConfig).fileName file))

/--
  Prints a directory entry using the configured display style.
-/
def showDirName (dir : String) : ConfigIO Unit := do
  runIO (IO.println ((←currentConfig).dirName dir))
def doList [Applicative f] : List α → (α → f Unit) → f Unit
  | [], _ => pure ()
  | x :: xs, action =>
    action x *>
    doList xs action
/--
  Recursively prints the directory tree starting from the given path.
-/
partial def dirTree (path : System.FilePath) : ConfigIO Unit := do
  match (← runIO (toEntry path)) with
  | none =>
    return ()
  | some (.file name) =>
      showFileName name
  | some (.dir name)  =>
      showDirName name
      let newCfg := (←currentConfig).withDir
      let contents ← runIO (path.readDir)
      locally (·.withDir) (doList contents.toList (dirTree ·.path))

/--
  Executes the 'doug' command by parsing CLI flags and printing the rendering mode.
-/
def runDoug (parsed : Parsed) : IO UInt32 := do
  let ascii? := parsed.flag! "ascii" |>.as! Bool
  let cfg : Config := { unicode? := not ascii? }
  (dirTree (← IO.currentDir)).run cfg
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
#synth ToString Char
