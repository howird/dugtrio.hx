# Record from the repository root: nix develop -c vhs .github/assets/demo.tape
# dugtrio-demo creates an isolated Helix configuration and a throwaway project.
Require nix

Output .github/assets/preview.gif

Set Shell "bash"
Set Theme "VibrantInk"
Set FontFamily "Maple Mono NF"
Set FontSize 18
Set Width 1600
Set Height 1000
Set Padding 20
Set TypingSpeed 60ms
Set WindowBar Colorful

Hide
Type "nix run"
Enter
Sleep 5s
Show

# Open the explorer on the current file and browse its ancestor columns.
Type "-"
Sleep 1.5s
Left
Sleep 800ms
Right
Sleep 800ms
Down@300ms 1
Sleep 800ms
Up@300ms 1
Sleep 800ms

# Toggle visibility.
Type "."
Sleep 800ms
Type "."
Sleep 800ms
Type "q"
Sleep 500ms

Hide
Type ":q!"
Enter
Sleep 500ms
