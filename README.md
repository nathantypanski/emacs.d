# emacs.d

This is my Emacs configuration.

It uses [Evil](http://www.emacswiki.org/emacs/Evil) to emulate Vim keybindings. But it's also tricked out with lots of Emacs goodness.

## Repo structure

- I build Emacs from GNU's [Git repository](http://savannah.gnu.org/git/?group=emacs) using [this AUR package](https://aur.archlinux.org/packages/emacs-git/). That means some of the commands used won't work with the Emacs package provided by your Linux distribution, since they haven't been released yet.
- [`init.el`](./init.el) does the bare minimum for setting up the package management stuff. Then it loads other elisp files which are in [`./config`](./config).
- All my custom config files are prefixed with `my-`. Hopefully things I put in the global namespace are, also - this lets me keep them separate from the builtins and packages that I load.
- I also try to avoid using the [easy customization interface](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html). In my opinion it doesn't produce readable elisp, and I want to have a maintainable set of config files, not just ones that I can access through a pretty GUI.
