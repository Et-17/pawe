# Pawe

Pathfinding Automatic Word Evolver - A tool for predicting word evolution and cognates

## Cognate prediction

Language doesn't evolve randomly. It evolves with rules, rules that we can learn and apply on our own. A while ago, I got obsessed with using these rules to predict Modern English cognates of words in other Indo-European languages that come from roots that didn't actually descend into English, but it takes forever to do by hand. Pawe gives you the tools to do this automatically. By building the phonological system you wish to use and then writing rules that define the evolution from one language to another, you can use Pawe to automatically predict how arbitrary words would descend into entire language families. Because you define individual evolution between individual language nodes instead of overal pathways, sister languages can share their ancestors' code, and you can start and stop evolutions at any point in history.

## Configuration

Configuration files end in `.paw`. You can learn how to write them by reading [`doc/intro.md`](doc/intro.md). A few example files are provided in [`examples/`](examples/), but many of these are more for testing than explaining.

If a config path is not specified on the command line, Pawe will attempt to use `primary.paw` in the current directory. If the desired config is located elsewhere, you can pass `--config` or `-c` with either the config file to use or a directory to look for `primary.paw` in.

## Output control

If you are using the `evolve` subcommand, there are a few different output switches you can change. If you only want to see the final result, you can pass `--no-stages` or `-n`. If you want to show every change to the word instead of language stages, you can pass `--show-changes`. For use in scripts, you can either remove the labels with `--no-labels` or output the stages as a single line of csv with `--csv`.
