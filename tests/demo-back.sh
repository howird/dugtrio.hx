#!/usr/bin/env bash
# Run from the repository root. Requires Nix, tmux, and a UTF-8 locale.
set -euo pipefail

demo=$(nix build --no-link --print-out-paths)
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
tmux_demo send-keys Up Up Right
wait_for_pane '╭widgets/.*╭columns/.*╭layout\.rs'
tmux_demo send-keys Left
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'

# A broken ancestor row must not discard the active widgets column.
wait_for_pane 'columns/.*layout\.rs'
tmux_demo send-keys g
wait_for_pane '│hn broken-link '
tmux_demo send-keys h n
wait_for_pane '╭orchard/.*╭widgets/.*╭columns/[^╭]*$'

# Start a preview label immediately after moving to columns/, while the
# preview would normally be deferred. After i, b must select ib, not list.rs.
tmux_demo send-keys Down
wait_for_pane '╭widgets/.*╭list\.rs'
tmux_demo send-keys Up g
wait_for_pane '│b list\.rs.*│ib miller\.rs'
tmux_demo send-keys i
tmux_demo send-keys b
wait_for_pane 'NOR.*src/ui/widgets/columns/miller\.rs'

# dd focuses widgets/ without opening a row; fc then opens a row in ui/.
tmux_demo send-keys -l '-'
wait_for_pane '╭widgets/.*╭columns/.*╭miller\.rs'
tmux_demo send-keys g
wait_for_pane '╭dd widgets/'
tmux_demo send-keys d d
wait_for_pane '╭widgets/.*╭columns/[^╭]*$'
tmux_demo send-keys g
wait_for_pane '│fc theme\.rs'
tmux_demo send-keys f c
wait_for_pane 'NOR.*src/ui/theme\.rs'

# At this height the root has seven interior rows. Two overflow markers
# leave five row labels (a–e), so f is free for the preview's focus label.
tmux_demo resize-window -x 130 -y 11
tmux_demo send-keys -l '-'
wait_for_pane '╭orchard/.*╭ui/.*╭theme\.rs'
tmux_demo send-keys g
wait_for_pane '╭ee orchard/'
tmux_demo send-keys e e
wait_for_pane '╭orchard/.*╭src/[^╭]*$'
tmux_demo send-keys g
wait_for_pane '╭orchard/.*╭ff src/'
wait_for_pane '│e web/ '
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
