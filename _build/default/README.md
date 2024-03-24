# PPF5-JeuDuMoulin-OCaml

Implementation of Nine men's morris (A.K.A `Mill`) in OCaml.

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system. Since your system runs
[Guix](https://guix.gnu.org/), you can easily obtain a suitable
throw-away programming environment using

```
$ guix shell -m .guix/manifest.scm
```

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
$ opam switch create . --deps-only --with-doc --with-test
$ eval $(opam env)
```

Configure your favorite [text
editor](http://dev.realworldocaml.org/install.html#editor-setup). You
will want to have a working, integrated type-checker in your editor,
as well as type-directed completion. Your favorite text editor being
Emacs, this leads to:

```
$ opam install user-setup tuareg ocamlformat merlin
$ opam user-setup install
```

Make sure that your text editor applies
[OCamlformat](https://ocaml.org/p/ocamlformat/0.22.4/doc/editor_setup.html#editor-setup)
each time a file is modified, this helps settle styling war and avoids
line-noisy patches down the line.

In Emacs, this amounts to adding the following lines to your `.emacs`
configuration file:

```elisp
(require 'ocamlformat)

(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))
```

If need be, you can invoke Dune to re-format the whole codebase:

```
$ dune fmt
```

## Building mill

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running mill

To play the game, type:

```
$ dune exec mill
```

## Testing mill

To test the project, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```

# Documentation

The internal project documentation can be compiled to HTML through

```
$ dune build @doc-private
```

# Groupe

- ESPANET Nicolas
- LEFORESTIER Meril
- PARIS Albin
- YAZICI Servan