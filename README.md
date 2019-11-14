# Elementary Cellular Automata (ECA) Visualizer

## What is this?

This is a toy web app for running and visualizing [elementary cellular automata](https://en.wikipedia.org/wiki/Elementary_cellular_automaton). It is built in Reagent, which is basically React for ClojureScript.

## What's an ECA?

A cellular automaton consists of a grid of cells, each of which is in one of at least two possible states, and a set of rules that, when applied to the grid, will yield a new grid with new cell states.

In an ECA, the grid is one-dimensional, and each cell is either on or off (1 or 0). The ruleset decides the new state of each cell by looking at its current state along with the states of its two neighbours. There are 8 possible permutations of cell triads (000, 001, 010...111), and an ECA ruleset will yield either 1 or 0 for each permutation. This works out to 256 (2^8) possible rulesets.

## Build

First, ensure that you have Node.js and Java SDK 8 installed.

Then, navigate to the repository root and run `yarn install` (if you have Yarn) or `npm install` to install dependencies.

Now you can build the application as a static site by running `shadow-cljs release app` or simply `yarn release`.

## Run

After installing dependencies, use `shadow-cljs watch app` or `yarn watch` to run the app in a development server.

## License

Copyright © 2019 Decky Coss 

Distributed under the Eclipse Public License either version 2.0 or (at
your option) any later version.

