# hpc-threshold

`hpc-threshold` ensures the code coverage of your Haskell project is above configured thresholds. This program is meant to be used within a CI pipeline, in which the build will fail if the code coverage falls below the configured thresholds.

The program reads a configuration file named `.hpc-threshold` and parse [Haskell Program Coverage (HPC)](https://wiki.haskell.org/Haskell_program_coverage) text from `stdin`. The program outputs a report and will terminate with exit code 1 if the coverage falls below the configured threshold, and exit code 0 otherwise.

## User Guide

Install the program by using stack

```
stack install hpc-threshold
```

Then, create a configuration file named `.hpc-threshold`:

```
[ Threshold 
    { thresholdName = "Expressions used"
    , thresholdRegex = "(\\d+)% expressions used"
    , thresholdValue = 80.0
    }
, Threshold 
    { thresholdName = "Boolean coverage"
    , thresholdRegex = "(\\d+)% boolean coverage"
    , thresholdValue = 80.0
    }
, Threshold 
    { thresholdName = "Alternatives used"
    , thresholdRegex = "(\\d+)% alternatives used"
    , thresholdValue = 80.0
    }
, Threshold 
    { thresholdName = "Local declarations used"
    , thresholdRegex = "(\\d+)% local declarations used"
    , thresholdValue = 80.0
    }
, Threshold 
    { thresholdName = "Top-level declarations used"
    , thresholdRegex = "(\\d+)% top-level declarations used"
    , thresholdValue = 80.0
    }
]
```

- `thresholdRegex` is the regex to be used for extracting the coverage from HPC report. There should be one `(\\d+)` in the regex.
- `thresholdValue` is the threshold for the code coverage.
- `thresholdName` will be used for the threshold report.

Then, build the coverage report:

```
stack test --coverage
```

Then, generate a text report and feed that into `hpc-threshold`:

```
(stack hpc report --all 2&>1) | hpc-threshold
```

The stderr -> stdout redirection is necessary there because `stack hpc report` outputs the result in stderr, but we want to pipe that into `hpc-threshold`.

Then, you'll get an output similar to the following:

```
Code coverage threshold check: FAIL
· Expressions used: 67.0% (< 80.0%)
· Boolean coverage: 14.0% (< 80.0%)
· Alternatives used: 42.0% (< 80.0%)
✓ Local declarations used: 88.0% (≥ 80.0%)
✓ Top-level declarations used: 80.0% (≥ 80.0%)
```

If we check the exit code of the last process, we'll get `1` since some code coverage areas are below the configured threshold:

```
$ echo $?
1
```

For successful scenario, the output that you'll get is as follows:

```
Code coverage threshold check: PASS
✓ Expressions used: 67.0% (≥ 60.0%)
✓ Boolean coverage: 14.0% (≥ 10.0%)
✓ Alternatives used: 42.0% (≥ 40.0%)
✓ Local declarations used: 88.0% (≥ 80.0%)
✓ Top-level declarations used: 80.0% (≥ 80.0%)
```

And the exit code is 0:

```
$ echo $?
0
```
