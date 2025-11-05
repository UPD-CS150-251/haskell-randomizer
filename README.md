# haskell-randomizer

## Assumptions

- No duplicate last names within each lab class

## Usage

1. Create `data/` directory in repository root
1. Create each `<section>.txt` with one last name per line
1. Place each `<section>.txt` in `data/` directory
1. Create each `<requirement>-<section>.txt` with two last names per line, separated by comma + single space
1. Place each `<requirement>-<section>.txt` in `data/` directory
1. Run as follows:
    ```
    cabal run . <requirement> <section1> <section2> ...
    ```
1. Check `data/` for output files: `<requirement>-<section>.txt`

Example:

```
cabal run . lab07 fru fwx1 fwx2 fyz
```
