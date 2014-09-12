# Evil Rebellion

The aim of the evil rebellion is to get more emacs mode working in a way more
in sync with default vi bindings.  Its main focus is hence to define new
keymaps.  Rebellion adheres to well-defined conventions in its rebuilding of
the emacs keybindings.  This makes it simple to switch between editing using
evil keybindings and other modes like buffer lists or magit revision control.

## Conventions

* Moving is always done using `hjkl`.  However, `h` and `l` may be rebound if
  charwise moving doesn't make sense in a buffer.
* Longer range jumps are commands prefixed by `g`.
* Refresh is bound to `r`.
* Quitting and hiding is bound to `q`.
* Searching is done using `/`, `?`, `n` and `N`.
* `d` is for *delete*
* `o` is for *open*
* `:` will always start `EX` mode – mode-specific comman evaluation is
  initialized using `;` where available.
* Shift-key gives access to a different flavor of the command.

## License

The *evil rebellion* against inconsistent emacs key bindings.
Copyright © 2013–2014  Albert Krewinkel

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.
