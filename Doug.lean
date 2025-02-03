
import Init.System.FilePath
import Cli
import Init.System.IO

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

/-- Configuration for rendering the directory tree.

Files whose names begin with a dot character ('.') typically represent files that should usually be hidden, such as source-control metadata and configuration files. Modify doug with an option to show or hide filenames that begin with a dot. This option should be controlled with a -a command-line option.



-/
structure Config where
  /-- Use Unicode glyphs if true; otherwise, use ASCII symbols. -/
  unicode?   : Bool   := true
  /-- Current prefix for nested entries. -/
  currPrefix : String := ""
  /-- Show hidden files if true; otherwise, hide them. -/
  printHidden? : Bool   := false

deriving instance Repr for InputFlag, ParseError.Kind
/-- Converts a ParseError to a string for error reporting. -/
instance : ToString ParseError :=
  ⟨fun err => s!"{repr err.kind}"⟩

/--`Config → IO α` -/
abbrev ConfigIO := ReaderT Config IO

/--Returns the current config-/
def currentConfig : ConfigIO Config := pure

/--
  Extract the base name from a file path as an optional FileEntry.
  Returns none when the basename is "." or "..".
-/
def toEntry (path : System.FilePath) : IO (Option FileEntry) := do
  match path.components.getLast? with
  | none                 => pure (some (.dir ""))
  | some "." => pure (some (.dir (←IO.currentDir).toString))
  | some ".." =>
    let parentDir := match (←IO.currentDir).parent with
    | some parent => parent
    | none => System.FilePath.mk "/"

    return (some ( .dir (parentDir.toString)))
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
  (IO.println ((←currentConfig).fileName file))


/--
  Prints a directory entry using the configured display style.
-/
def showDirName (dir : String) : ConfigIO Unit := do
  match (←currentConfig).printHidden? with
  | true =>
    (IO.println ((←currentConfig).dirName dir))
  | false =>
    if dir != "." && dir != ".." then
      (IO.println ((←currentConfig).dirName dir))

def doList [Applicative f] : List α → (α → f Unit) → f Unit
  | [], _ => pure ()
  | x :: xs, action =>
    action x *>
    doList xs action
/--
  Recursively prints the directory tree starting from the given path.
-/
partial def dirTree (path : System.FilePath) : ConfigIO Unit := do
  match (←  toEntry path) with
  | none =>
    return ()
  | some (.file name) =>
      showFileName name
  | some (.dir name)  =>
      showDirName name
      let contents ←  (path.readDir)
      withReader (·.withDir) (doList contents.toList (dirTree ·.path))

/--
  Executes the 'doug' command by parsing CLI flags and printing the rendering mode.
-/
def runDoug (parsed : Parsed) : IO UInt32 := do
  let ascii? := parsed.flag! "ascii" |>.as! Bool
  let cfg : Config := { unicode? := not ascii?, printHidden? := parsed.flag! "hidden" |>.as! Bool }
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
    hidden : Bool; "Show hidden files"
]

/--
  Main entry point with default arguments.
-/
def main (args : List String := ["--ascii", "false", "--hidden", "true"]) : IO UInt32 := do
  match cmd.parse args with
  | .ok (_cmd, parsed) => runDoug parsed
  | .error err =>
      IO.eprintln s!"{err}"
      return 1

#eval main

end Doug
