#!/usr/bin/env bash
# Run from the repository root. Requires Nix, tmux, and a UTF-8 locale.
set -euo pipefail

demo=$(nix build --no-link --print-out-paths "path:$PWD")
tmp=$(mktemp -d)
tmux_demo() { tmux -S "$tmp/tmux" "$@"; }
cleanup() {
  tmux_demo kill-server 2>/dev/null || true
  rm -rf "$tmp"
}
trap cleanup EXIT

wait_for_pane() {
  local pattern=$1 screen i
  for ((i = 0; i < 50; i++)); do
    screen=$(tmux_demo capture-pane -p)
    if [[ $screen == *'error[E11]'* ]]; then
      printf '%s\n' "$screen" >&2
      return 1
    fi
    if grep -Eq "$pattern" <<< "$screen"; then return 0; fi
    sleep 0.1
  done
  printf 'Expected pane matching %s, got:\n%s\n' "$pattern" "$screen" >&2
  return 1
}

tmux_demo new-session -d -x 110 -y 32 \
  "TERM=xterm-256color '$demo/bin/dugtrio-demo' src/ui/widgets/mod.rs"
wait_for_pane 'NOR.*src/ui/widgets/mod\.rs'
tmux_demo send-keys -l '-'
wait_for_pane '╭widgets/.*╭mod\.rs'

# Read the filename in the pane immediately left of the preview, not an
# ancestor filename or the active row's icon/Go label.
selected_background() {
  tmux_demo capture-pane -ep | awk -v esc=$'\e' '
    BEGIN { sgr = esc "\\[[0-9;]*m" }
    !found {
      plain = $0
      gsub(sgr, "", plain)
      if (!match(plain, / mod\.rs *│ *│[^│]*pub mod preview;/)) next
      # Walk only the escapes at or before the filename in the plain text.
      offset = RSTART + 1
      row = $0
      bg = "49"
      while (match(row, sgr) && RSTART <= offset) {
        params = substr(row, RSTART + 2, RLENGTH - 3)
        n = split(params == "" ? "0" : params, codes, ";")
        offset -= RSTART - 1
        row = substr(row, RSTART + RLENGTH)
        for (i = 1; i <= n; i++) {
          code = codes[i] + 0
          if (code == 38 || code == 48) {
            if (codes[i + 1] == 2 && i + 4 <= n) {
              if (code == 48) bg = "48;2;" codes[i + 2] ";" codes[i + 3] ";" codes[i + 4]
              i += 4
            } else if (codes[i + 1] == 5 && i + 2 <= n) {
              if (code == 48) bg = "48;5;" codes[i + 2]
              i += 2
            }
          } else if (code == 0 || code == 49) {
            bg = "49"
          } else if ((code >= 40 && code <= 47) || (code >= 100 && code <= 107)) {
            bg = codes[i]
          }
        }
      }
      found = 1
    }
    END {
      if (!found) exit 1
      print bg
    }'
}

wait_for_blink_cycle() {
  local mode=$1 expected actual i
  local bright='48;2;184;184;184' dim='48;2;144;144;144' # base16_default_dark
  for expected in "$bright" "$dim" "$bright"; do
    for ((i = 0; i < 40; i++)); do
      if ! actual=$(selected_background); then actual='<row not found>'; fi
      if [[ $actual == "$expected" ]]; then break; fi
      sleep 0.05
    done
    if [[ $actual != "$expected" ]]; then
      printf '%s selection background: expected %s, got %s\n' "$mode" "$expected" "$actual" >&2
      return 1
    fi
  done
}

wait_for_blink_cycle Normal
tmux_demo send-keys g
wait_for_pane '│l mod\.rs '
wait_for_blink_cycle Go
tmux_demo send-keys Escape

# Normal-mode l does not open a file, while g l does. An invalid Go key is not replayed.
tmux_demo send-keys l g
wait_for_pane '│l mod\.rs '
tmux_demo send-keys q
wait_for_pane '╭widgets/.*╭mod\.rs'
tmux_demo send-keys Up g
wait_for_pane '│l list\.rs '
tmux_demo send-keys l
wait_for_pane 'NOR.*src/ui/widgets/list\.rs'
tmux_demo send-keys -l '-'
wait_for_pane '╭widgets/.*╭list\.rs'
# Without g, k and j still move rather than activating either neighbor.
tmux_demo send-keys k
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'
tmux_demo send-keys j
wait_for_pane '╭widgets/.*╭list\.rs'

# Go neighbors use the row's normal activation: k descends, j opens a file.
tmux_demo send-keys g
wait_for_pane '│k columns/ '
tmux_demo send-keys k
wait_for_pane '╭widgets/.*╭columns/.*╭layout\.rs'
tmux_demo send-keys Left
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'
tmux_demo send-keys j
wait_for_pane '╭widgets/.*╭list\.rs'
tmux_demo send-keys g
wait_for_pane '│j mod\.rs '
tmux_demo send-keys j
wait_for_pane 'NOR.*src/ui/widgets/mod\.rs'
tmux_demo send-keys -l '-'
wait_for_pane '╭widgets/.*╭mod\.rs'
tmux_demo send-keys Up Up

# A broken ancestor row must not discard the active widgets column.
wait_for_pane 'columns/.*layout\.rs'
tmux_demo send-keys g
wait_for_pane '│fn broken-link '
tmux_demo send-keys f n
wait_for_pane '╭orchard/.*╭widgets/.*╭columns/[^╭]*$'

# Start a preview label immediately after moving to columns/, while the
# preview would normally be deferred. After l, b must select lb, not the active b.
tmux_demo send-keys Down
wait_for_pane '╭widgets/.*╭list\.rs'
tmux_demo send-keys Up g
wait_for_pane '│j list\.rs.*│lb miller\.rs'
tmux_demo send-keys l b
wait_for_pane 'NOR.*src/ui/widgets/columns/miller\.rs'

# hh focuses widgets/ and ll focuses its directory preview without opening a row.
tmux_demo send-keys -l '-'
wait_for_pane '╭widgets/.*╭columns/.*╭miller\.rs'
tmux_demo send-keys g
wait_for_pane '╭hh widgets/'
tmux_demo send-keys h h
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'
tmux_demo send-keys g
wait_for_pane '╭ll columns/'
tmux_demo send-keys l l
wait_for_pane '╭widgets/.*╭columns/.*╭miller\.rs'
tmux_demo send-keys g
wait_for_pane '╭hh widgets/'
tmux_demo send-keys h h
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'
tmux_demo send-keys g
wait_for_pane '│hc theme\.rs'
tmux_demo send-keys h c
wait_for_pane 'NOR.*src/ui/theme\.rs'

# At this height the root has seven interior rows. Two overflow markers
# leave five row labels (a–c, j, k); the directory preview still uses ll.
tmux_demo resize-window -x 130 -y 11
tmux_demo send-keys -l '-'
wait_for_pane '╭orchard/.*╭ui/.*╭theme\.rs'
tmux_demo send-keys g
wait_for_pane '╭bb orchard/'
tmux_demo send-keys b b
wait_for_pane '╭orchard/.*╭src/[^╭]*$'
tmux_demo send-keys g
wait_for_pane '╭orchard/.*╭ll src/'
wait_for_pane '│c web/ '
screen=$(tmux_demo capture-pane -p)
if [[ $(grep -Ec '^ │ +\.\.\. +│' <<< "$screen") != 2 ]]; then
  printf 'Expected two overflow markers in the root column, got:\n%s\n' "$screen" >&2
  exit 1
fi
tmux_demo send-keys Escape
wait_for_pane '╭orchard/.*╭src/'

# Half of seven rows rounds down to three: src/ -> .gitignore -> src/.
tmux_demo send-keys C-d
wait_for_pane '╭orchard/.*╭\.gitignore'
tmux_demo send-keys C-u
wait_for_pane '╭orchard/.*╭src/'

# With three interior rows, overflow markers hide both neighbors of src/.
# Neither missing key is replayed as movement after Go mode cancels.
tmux_demo resize-window -x 130 -y 7
for key in j k; do
  tmux_demo send-keys g
  wait_for_pane '╭orchard/.*╭ll src/'
  wait_for_pane '│a src/ '
  tmux_demo send-keys "$key"
  wait_for_pane '╭orchard/.*╭src/'
done
