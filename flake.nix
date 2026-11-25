{
  description = "dugtrio.hx: a floating Miller-column file explorer for Helix";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      # `nix run` launches an isolated Helix with this checkout loaded as a cog.
      demoFor = pkgs:
        let
          glyph = pkgs.fetchFromGitHub {
            owner = "Ra77a3l3-jar";
            repo = "glyph.hx";
            tag = "0.2.0";
            hash = "sha256-TpYnGqROkKfoB9G+JTjADWvMtpRJbv4NVaTqiUfW1Eg=";
          };
        in
        # A throwaway project that exercises the explorer: deep nesting (columns
        # drop off the left), many file types (icons), long and wide names
        # (truncation), an empty directory and a broken symlink (placeholders),
        # and dotfiles (visibility toggle).
        pkgs.writeShellApplication {
          name = "dugtrio-demo";
          runtimeInputs = [ pkgs.coreutils pkgs.git pkgs.steelix ];
          text = ''
            tmp=$(mktemp -d)
            trap 'rm -rf "$tmp"' EXIT
            export XDG_CONFIG_HOME="$tmp/config"
            export XDG_DATA_HOME="$tmp/data"
            export XDG_CACHE_HOME="$tmp/cache"
            export XDG_STATE_HOME="$tmp/state"
            export HELIX_STEEL_CONFIG="$XDG_CONFIG_HOME/helix"
            export STEEL_HOME="$XDG_DATA_HOME/steel"
            export GIT_CONFIG_GLOBAL=/dev/null
            export GIT_CONFIG_NOSYSTEM=1

            mkdir -p "$HELIX_STEEL_CONFIG" "$STEEL_HOME/cogs" "$tmp/orchard"
            ln -s ${self} "$STEEL_HOME/cogs/dugtrio"
            ln -s ${glyph} "$STEEL_HOME/cogs/glyph"
            cat > "$HELIX_STEEL_CONFIG/init.scm" <<'EOF'
            (require "helix/keymaps.scm")
            (require "dugtrio/dugtrio.scm")
            (keymap (global)
                    (normal (- ":dugtrio-open")))
            EOF
            printf '%s\n' 'theme = "base16_default_dark"' > "$HELIX_STEEL_CONFIG/config.toml"
            touch "$HELIX_STEEL_CONFIG/helix.scm"

            cd "$tmp/orchard"

            # write stdin to a file, creating its directory
            w() {
              mkdir -p "$(dirname "$1")"
              cat > "$1"
            }

            w README.md <<'EOF'
            # orchard

            A small command-line tool for cataloguing fruit trees.

            Press `-` to open dugtrio.hx, then move with `h` `j` `k` `l`.

            ## Layout

            - `src/` the Rust crate
            - `web/` a static front end
            - `docs/` user and design notes
            EOF
            w Cargo.toml <<'EOF'
            [package]
            name = "orchard"
            version = "0.3.1"
            edition = "2021"

            [dependencies]
            clap = { version = "4", features = ["derive"] }
            serde = { version = "1", features = ["derive"] }
            EOF
            w flake.nix <<'EOF'
            {
              description = "orchard";
              outputs = { self }: { };
            }
            EOF
            w .gitignore <<'EOF'
            /target
            *.log
            EOF
            w .github/workflows/ci.yml <<'EOF'
            name: ci
            on: [push, pull_request]
            jobs:
              test:
                runs-on: ubuntu-latest
                steps:
                  - uses: actions/checkout@v4
                  - run: cargo test
            EOF

            w src/main.rs <<'EOF'
            use orchard::commands;
            use orchard::config::Config;

            fn main() {
                let config = Config::load().unwrap_or_default();
                let args: Vec<String> = std::env::args().skip(1).collect();

                match args.first().map(String::as_str) {
                    Some("open") => commands::open::run(&config, &args[1..]),
                    Some("close") => commands::close::run(&config),
                    Some("search") => commands::search::run(&config, &args[1..]),
                    _ => eprintln!("usage: orchard <open|close|search>"),
                }
            }
            // TODO: handle --help
            EOF
            w src/lib.rs <<'EOF'
            pub mod commands;
            pub mod config;
            pub mod ui;
            EOF
            w src/config.rs <<'EOF'
            #[derive(Debug, Default)]
            pub struct Config {
                pub root: String,
                pub show_hidden: bool,
            }

            impl Config {
                pub fn load() -> Option<Self> {
                    None
                }
            }
            EOF
            w src/commands/mod.rs <<'EOF'
            pub mod close;
            pub mod open;
            pub mod search;
            EOF
            w src/commands/open.rs <<'EOF'
            use crate::config::Config;

            pub fn run(config: &Config, paths: &[String]) {
                for path in paths {
                    println!("opening {path} under {}", config.root);
                }
            }
            EOF
            w src/commands/close.rs <<'EOF'
            use crate::config::Config;

            pub fn run(_config: &Config) {
                println!("closed");
            }
            EOF
            w src/commands/rename.rs <<'EOF'
            pub fn run(from: &str, to: &str) {
                println!("renaming {from} to {to}");
            }
            EOF
            w src/commands/search.rs <<'EOF'
            use crate::config::Config;

            pub fn run(_config: &Config, terms: &[String]) {
                println!("searching for {}", terms.join(" "));
            }
            EOF
            w src/ui/mod.rs <<'EOF'
            pub mod theme;
            pub mod widgets;
            EOF
            w src/ui/theme.rs <<'EOF'
            pub struct Theme {
                pub text: &'static str,
                pub selection: &'static str,
                pub border: &'static str,
            }

            pub const DARK: Theme = Theme { text: "#d8d8d8", selection: "#b8b8b8", border: "#585858" };
            pub const LIGHT: Theme = DARK;
            EOF
            w src/ui/widgets/mod.rs <<'EOF'
            pub mod columns;
            pub mod list;
            pub mod preview;
            pub mod title;
            EOF
            w src/ui/widgets/list.rs <<'EOF'
            pub struct List {
                pub items: Vec<String>,
                pub cursor: usize,
            }

            impl List {
                pub fn next(&mut self) {
                    self.cursor = (self.cursor + 1).min(self.items.len().saturating_sub(1));
                }

                pub fn previous(&mut self) {
                    self.cursor = self.cursor.saturating_sub(1);
                }
            }
            EOF
            w src/ui/widgets/preview.rs <<'EOF'
            pub fn preview(path: &str, max_lines: usize) -> Vec<String> {
                std::fs::read_to_string(path)
                    .map(|text| text.lines().take(max_lines).map(str::to_owned).collect())
                    .unwrap_or_default()
            }
            EOF
            w src/ui/widgets/title.rs <<'EOF'
            pub fn truncate(title: &str, width: usize) -> String {
                if title.chars().count() <= width {
                    title.to_owned()
                } else {
                    let head: String = title.chars().take(width.saturating_sub(1)).collect();
                    format!("{head}…")
                }
            }
            EOF
            w src/ui/widgets/columns/mod.rs <<'EOF'
            pub mod layout;
            pub mod miller;
            EOF
            w src/ui/widgets/columns/layout.rs <<'EOF'
            /// Drop the oldest columns until the rest fit the screen.
            pub fn fit(widths: &[u16], screen: u16) -> &[u16] {
                let mut visible = widths;
                while visible.len() > 2 && visible.iter().sum::<u16>() > screen {
                    visible = &visible[1..];
                }
                visible
            }
            EOF
            w src/ui/widgets/columns/miller.rs <<'EOF'
            pub struct Column {
                pub path: String,
                pub entries: Vec<String>,
                pub cursor: usize,
            }
            EOF

            w tests/integration.rs <<'EOF'
            #[test]
            fn opens_a_path() {
                assert!(true);
            }
            EOF
            w tests/fixtures/sample.json <<'EOF'
            {
              "trees": [
                { "name": "Bramley", "kind": "apple", "planted": 2019 },
                { "name": "Conference", "kind": "pear", "planted": 2021 }
              ]
            }
            EOF
            w tests/fixtures/an-exceptionally-long-fixture-file-name-that-has-to-be-truncated-to-fit.txt <<'EOF'
            This file exists for its name.
            EOF

            w docs/guide.md <<'EOF'
            # Guide

            1. Run `orchard open <path>` to start a catalogue.
            2. Run `orchard search <term>` to find a tree.
            EOF
            w docs/architecture.md <<'EOF'
            # Architecture

            Commands live in `src/commands`, one module each. The interface is
            built from the widgets in `src/ui/widgets`.
            EOF
            w docs/日本語/はじめに.md <<'EOF'
            # はじめに

            orchard は果樹を記録するための小さなツールです。
            EOF
            w docs/日本語/とても長い名前のドキュメントファイルの例です.md <<'EOF'
            全角文字の幅を確かめるためのファイルです。
            EOF

            w scripts/build.sh <<'EOF'
            #!/bin/sh
            set -eu
            cargo build --release
            EOF
            w scripts/release.py <<'EOF'
            import subprocess

            def main() -> None:
                subprocess.run(["cargo", "publish", "--dry-run"], check=True)

            if __name__ == "__main__":
                main()
            EOF
            chmod +x scripts/build.sh

            w assets/logo.svg <<'EOF'
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16">
              <circle cx="8" cy="9" r="6" fill="#c0392b"/>
              <path d="M8 3 q1 -3 4 -2" stroke="#27ae60" fill="none"/>
            </svg>
            EOF
            mkdir -p assets/empty

            w web/index.html <<'EOF'
            <!doctype html>
            <html lang="en">
              <head>
                <meta charset="utf-8">
                <title>orchard</title>
                <link rel="stylesheet" href="style.css">
              </head>
              <body>
                <main id="app"></main>
                <script type="module" src="app.js"></script>
              </body>
            </html>
            EOF
            w web/style.css <<'EOF'
            body { font-family: system-ui, sans-serif; margin: 2rem; }
            #app { max-width: 40rem; }
            EOF
            w web/app.ts <<'EOF'
            interface Tree {
              name: string;
              kind: string;
            }

            export function render(trees: Tree[]): string {
              return trees.map((tree) => `<li>''${tree.name} (''${tree.kind})</li>`).join("");
            }
            EOF
            w web/package.json <<'EOF'
            {
              "name": "orchard-web",
              "private": true,
              "scripts": { "build": "tsc" }
            }
            EOF

            ln -s missing-target broken-link

            w notes/todo.md <<'EOF'
            - [ ] catalogue the north row
            - [ ] label the grafts
            EOF

            git init -q

            if [ $# -eq 0 ]; then
              set -- src/ui/widgets/list.rs
            fi
            hx "$@"
          '';
        };
    in
    {
      packages = nixpkgs.lib.genAttrs systems (system:
        let demo = demoFor nixpkgs.legacyPackages.${system};
        in { dugtrio-demo = demo; default = demo; });

      # `nix develop -c vhs .github/assets/demo.tape` re-records the preview.
      devShells = nixpkgs.lib.genAttrs systems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in { default = pkgs.mkShell { packages = [ (demoFor pkgs) pkgs.vhs ]; }; });
    };
}
