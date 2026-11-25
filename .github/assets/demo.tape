# Record from the repository root: nix develop -c vhs .github/assets/demo.tape
# The devShell's dugtrio-demo launches Helix with an isolated orchard fixture.
Require dugtrio-demo

Output .github/assets/preview.gif

Set Shell "bash"
Set Theme "Dracula"
Set FontFamily "Maple Mono NF"
Set FontSize 21
Set Width 1440
Set Height 640
Set Padding 20
Set WindowBar Colorful
Set TypingSpeed 180ms

Hide
Type "dugtrio-demo src/ui/widgets/mod.rs"
Enter
Sleep 2s
Type "-"
Sleep 500ms
Show

# Browse from a file preview to a directory preview.
Sleep 1.4s
Up@160ms 2
Sleep 1.3s

# Dots in the orchard column disappear and return; focus stays put.
Type "."
Sleep 1.1s
Type "."
Sleep 1.1s

# Go mode: ll focuses the directory preview, hh focuses the parent.
Type "g"
Sleep 1.2s
Type "l"
Sleep 450ms
Type "l"
Sleep 500ms
Down
Sleep 800ms
Type "g"
Sleep 1.2s
Type "hh"
Sleep 800ms

# Jump directly from widgets/ to miller.rs in the directory preview.
Type "g"
Sleep 800ms
Type "l"
Sleep 450ms
Type "b"
Sleep 1.5s

Hide
Type ":q!"
Enter
Sleep 500ms
