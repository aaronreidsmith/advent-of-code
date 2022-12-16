# Advent of Code

My solutions to the [Advent of Code](https://adventofcode.com/) in Scala.

Solutions are stored in the following directory structure:

```
src/main/scala/io/github/aaronreidsmith/<year>
```

Inputs are stored in the following directory structure:

```
src/main/resources/<year>/<day>.txt
```

Input files are encrypted in accordance with the [AoC creator's wishes](https://mobile.twitter.com/ericwastl/status/1465805354214830081),
so if you want to use this for yourself, you will have to provide your own input.

## Usage

### Run all solutions

```
sbt run
```

### Run all solutions for a given year

```
sbt "run --year <year>"
```

### Run all solutions for a given day across all years

```
sbt "run --day <day>"
```

### Run a specific solution

```
sbt "run --year <year> --day <day>"
```
