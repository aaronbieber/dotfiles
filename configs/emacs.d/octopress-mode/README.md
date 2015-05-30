# Octopress Mode #

Octopress Mode is a lightweight wrapper script to help you interact with
Octopress blog site and the related Jekyll programs. This mode is designed to be
unobtrusive and to defer to Octopress and Jekyll as often as possible.

This mode was built with the assumption of Octopress 3.0 and will probably not
work with previous (non-gem) versions of Octopress. Specifically, it expects to
be able to use commands like `octopress new post` rather than the old-style
`rake new_post[]`.

## Installation ##

Place octopress-mode.el in your load path. Eventually I'll get this thing into
MELPA, but until then, you're on your own.

## Configuration ##

There are some configuration options. Run `M-x configure-group octopress-mode` to
see them.
