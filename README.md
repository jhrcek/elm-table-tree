# Table tree

Exploration of how table data can be merged into a tree and how the order of columns influences the size of the resulting tree.
See [example](https://github.com/jhrcek/elm-table-tree/tree/master/example/README.md) with some explanation.
You can play with live [demo](https://janhrcek.cz/elm-table-tree/). 

## Develop

```bash
elm-live src/Main.elm --dir docs -- --output docs/elm.js
```

## Build
```bash
./build.sh
```

## TODO

- [ ] make direct paste mode optional
- [ ] find the order of columns which minimizes the number of nodes in the tree
