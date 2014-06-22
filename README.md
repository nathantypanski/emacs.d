# emacs.d

This is my Emacs configuration.

It uses [Evil](http://www.emacswiki.org/emacs/Evil) to emulate Vim keybindings. But it's also tricked out with lots of Emacs goodness.

## Repo structure

- I build Emacs from GNU's [Git repository](http://savannah.gnu.org/git/?group=emacs). It's a submodule of this repo, so you can see what version I'm using.
- `[init.el](./init.el)` does the bare minimum for setting up the package management stuff. Then it loads other elisp files which are in `[./config](./config)`.
- All my custom config files are prefixed with ``my-``. Hopefully things I put in the global namespace are, also - this lets me keep them separate from the builtins and packages that I load.
