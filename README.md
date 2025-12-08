# Pawe

Pathfinding Automatic Word Evolver - A tool for predicting word evolution and cognates

Language doesn't evolve randomly. It evolves with rules, rules that we can learn and apply on our own. A while ago, I got obsessed with using these rules to predict Modern English cognates of words in other Indo-European languages that come from roots that didn't actually descend into English, but it takes forever to do by hand. Pawe gives you the tools to do this automatically. By building the phonological system you wish to use and then writing rules that define the evolution from one language to another, you can use Pawe to automatically predict how arbitrary words would descend into entire language families. Because you define individual evolution between individual language nodes instead of overal pathways, sister languages can share their ancestors' code, and you can start and stop evolutions at any point in history.

## Configuration

Configuration files end in `.paw`. You can learn how to write them by reading [`doc/intro.md`](doc/intro.md). A few example files are provided in [`examples/`](examples/), but many of these are more for testing than explaining.

If a config path is not specified on the command line, Pawe will attempt to use `primary.paw` in the current directory. If the desired config is located elsewhere, you can pass `--config` or `-C` with either the config file to use or a directory to look for `primary.paw` in.

## Installation

A Windows binary built on a Windows 11 machine and a Linux binary built on an Arch Linux machine are given for each release. However, if you are able, it is recommended to download the source of the latest release (or clone the repo if you want the newest features) and then build it yourself with `cargo build --release` to make sure that it works on your system.

## Usage

You can evolve words with the `pawe evolve <WORD>` subcommand. Note that the input word must have its characters separated by whitespace (e.g. a space) so that Pawe knows how to separate them. By default, Pawe will start with the first defined language in the configuration file and end with the last defined language, but either of these can be changed with `--start`/`-s` and `--end`/`-e`. Pawe will show the word at the end of each language stage, but there are many output options that include showing every change, showing the rules that are being applied, not labeling languages, outputting as a csv, or just showing the final result. Refer to [Output control](#output-control) or run `pawe evolve --help` for more information on the available options.

You can also display a family tree of all descendants with the `pawe tree <WORD>` subcommand. Like the `pawe evolve` subcommand, the characters in the word must be separated by whitespace. The start language will also be the first defined language by default, but can be changed with `--start`/`-s`. The maximum depth of evolution can be limited with `--depth`/`-d`.

## Output control

Because different tasks and different researchers need different things, Pawe has several ways to customize the output:
- The selected stages can be displayed like a csv with `--csv`
- Just the final result can be shown with `--no-stages` or `-n`
- Language step labels can be disabled with `--no-labels`
- Rules can be displayed as they're being ran by with `--display-rules` or `-d`
- The word can be shown after every change instead of just at the end of languages with `--changes` or `-c`
- The result of every rule, not only effective ones, can be displayed with  `--all-results` or `-a`
- Reduction of phonemes to a base character for display can be disabled with `--no-base`
- Input and output normalization can be disabled with `--no-normalization`
