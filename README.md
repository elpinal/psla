# Psla: Python Situated in a Limited Area

`Psla` is a tool for managing python environment.

## Requirement

[Stack](https://github.com/commercialhaskell/stack)

## Install

In this repository, use the following command:

```bash
$ stack install
```

## Usage

To install the specific version of Python:

```bash
$ psla install python3.7
```

To set the specific version of Python as the current version:

```bash
$ psla use python3.7
```

To uninstall the specific version of Python:

```bash
$ psla uninstall python3.7
```

## Motivation

[`Pyenv`](https://github.com/pyenv/pyenv) is not simple.
[`Anaconda`](https://www.continuum.io/downloads) assumes that Bash / Zsh is used.

## Contribution

1. Fork ([https://github.com/elpinal/psla/fork](https://github.com/elpinal/psla/fork))
1. Create a feature branch
1. Commit your changes
1. Rebase your local changes against the master branch
1. Run test suite and confirm that it passes
1. Create a new Pull Request

## Author

[elpinal](https://github.com/elpinal)
