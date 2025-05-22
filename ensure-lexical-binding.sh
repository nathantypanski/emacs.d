#!/usr/bin/env bash

INPUT_DIR="$HOME/.emacs.d"
OUTPUT_DIR="$HOME/.emacs.d"

add_lexical_bindings() {
    local f="$1"
    local rel_path dest_dir dest
    # Remove leading INPUT_DIR/ from path for relative path construction
    rel_path="${f#$INPUT_DIR/}"
    dest_dir="$OUTPUT_DIR/$(dirname "$rel_path")"
    dest="$OUTPUT_DIR/$rel_path"
    mkdir -p "$dest_dir"

    if ! head -1 "$f" | grep -q 'lexical-binding'; then
        first_line=$(head -1 "$f")
        rest=$(tail -n +2 "$f")
        if echo "$first_line" | grep -q '^;;'; then
            echo "appending to $dest"
            {
                echo "${first_line} -*- lexical-binding: t; -*-"
                echo "$rest"
            } > "$dest"
        else
            echo "inserting to $dest"
            {
                echo ";; -*- lexical-binding: t; -*-"
                echo "$first_line"
                echo "$rest"
            } > "$dest"
        fi
    else
        cp "$f" "$dest"
    fi
}

files=(
    "$INPUT_DIR"/*.el
    "$INPUT_DIR"/config/*.el
    "$INPUT_DIR"/config/languages/*.el
    "$INPUT_DIR"/config/eyecandy/*.el
)

for f in "${files[@]}"; do
    if [[ -f "$f" ]]; then
        add_lexical_bindings "$f"
    else
        echo >&2 "bad file: $f"
    fi
done
