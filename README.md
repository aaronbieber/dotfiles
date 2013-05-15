# All Hail Dotfiles
These are my UNIX-y configuration files, known colloquially as "dotfiles" 
because most of them begin with a period (a "dot"). These configurations are 
also used in OS X, so not all of them are even applicable to Linux (such as 
`.slate`), but linking them shouldn't hurt.

Wait, linking? What?

Yes, that's right, I have also provided a handy bash script called `linkall` 
that will handle the oh so arduous process of symlinking each of these 
configurations into your home directory. Why would I do this? So that these 
files can live in their own directory as an isolated git repository and also 
function as configuration files in your home directory at the same time. Simply 
run `linkall` and you're done.

## Usage
Usage is straightforward.

```
$ git clone https://github.com/aaronbieber/dotfiles.git ~/dotfiles
$ ~/dotfiles/linkall
```

My `.bashrc` file is now capable of environments. To take advantage of 
environment partitions, export the `MY_LOCATION` environment variable from your 
`.bash_profile` and then source the `.bashrc` file, like so:

```
#!/bin/bash

export MY_LOCATION=home

if [ -f ~/.bashrc ]; then
	source ~/.bashrc
fi
```
