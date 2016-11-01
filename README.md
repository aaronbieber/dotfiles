# All Hail Dotfiles

These are my UNIX-y configuration files, known colloquially as "dotfiles"
because most of them begin with a period (a "dot"). These configurations are
used in OS X, so some may not be applicable or may not work properly at all in
other environments. Most should work in flavors of Linux, though.


*Important note:* These are my personal configuration files, which are in a
constant state of change as I develop my environment across several systems. I
can't guarantee that these configurations will work for you, at all, so if you
encounter problems with them, you're pretty much on your own.

## Linking

I have also provided a handy bash script called `linkall` that will handle the
arduous process of symlinking each of these configurations into your home
directory. Why would I do this? So that these files can live in their own
directory as an isolated git repository and also function as configuration files
in your home directory at the same time. Simply run `linkall` and you're done.

## Usage

Usage is straightforward.

```bash
$ git clone https://github.com/aaronbieber/dotfiles.git ~/dotfiles
$ ~/dotfiles/linkall
```

My `.bashrc` file is now capable of environments. To take advantage of
environment partitions, export the `MY_LOCATION` environment variable from your
`.bash_profile` and then source the `.bashrc` file, like so:

```bash
#!/bin/bash

export MY_LOCATION=home

if [ -f ~/.bashrc ]; then
	source ~/.bashrc
fi
```
