# Sched

OCaml version 4.07.1

# Development Workflow Overview

Our development workflow will be to featurize the languauge parts of Schedch
into small pieces so that they can be implemented in manageable vertical
slices. To facilitate collaboration we will adopt a test-driven development
methodology supported by effective use of git branching.

# Feature Development Workflow

Our feature development workflow should proceed in four phases:

1. Test Creation.
2. Implementation.
3. Testing.
4. Merging.

## Test Creation

- Pick a feature to implement. Check the team Trello board to see which
  features are available. Join the card so that others can know someone is
  working on that feature.
- Create a new git branch for the feature. Note: Create the new git branch off
  of the `master` branch and pull `master` first to ensure that you have the
  most up to date consistent version of the Schedch compiler.
- Write a smoke test. A smoke test consists of:

  1. A minimal Schedch program that uses the feature.
  2. The expected output of successfully running our compiler on the minimal
     program once the feature has been implemented.

  Name the minimal program file `<feature>\_smoke\_test.sched` and the expected
  output file `<feature>\_smoke\_test.sched.output`. Place both files in the
  tests directory. Add the smoke test to the `testall.sh` script.
- Write an integration test. An integration test consists of:

  1. A Schedch program that uses the feature in combination with one or more
     other features.
  2. The expected output of successfully running our compiler on the minimal
     program once the feature has been implemented.

  Name the program file `<feature>\_int\_test.sched` and the expected output
  file `<feature>\_int\_test.sched.output`. Place both files in the tests
  directory. Add the integration test to the `testall.sh` script.

## Implementation

- Implement the feature. Happy hacking!

## Testing

- Test the feature: Ensure that it successfully passes your smoke test and
  integration test.
- Test feature integration: Run all the tests in `testall.sh` on the updated
  version of the compiler that supports your feature.
- Update the `master` branch and then merge the

## Merging

- Update your local `master` branch.
- Merge your updated local `master` branch into your feature branch.
- Resolve any merge conflicts. Run all tests again. When in doubt, reach out to
  the author of any code you may be having particular issues with.
- Merge your feature branch into your local `master` branch and push to the
  GitHub repository.

# Git Conventions

The `master` branch should always be in a consistent state. Changes should only
be made to master once you have fully implemented a feature, tested it, and
merged the latest version of `master` into your feature branch. This is a
workflow that leverages the flexibility of Git and its branch features. Read
more about it in Scott Chacon's lovely [Git
Book](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell).


