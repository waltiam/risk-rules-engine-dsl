# Risk Rules Engine

An Open Source Risk Rules Engine with an inherent DSL

## Goal

To abstract the _Domain of Risk_ into a language that the _subject matter experts_ (SME) and the developers into a language that is understood AND machine parsable.  For example:

```sh
when {country of primary address} is in ({list of countries}) then risk level is {defined as}

when {country of operation} is in ({list of countries}) and {sector} is in ({list of sectors}) then risk is {defined as}
```

These are the general outline of the rules.

We would also need to define the evaluation data and the standard data.

All the rules take the form:

```
All {evaluation data} meet {predicate} then {risk result}
Any {evaluation data} meet {predicate} then {risk result}
None {evaluation data} meet {predicate} then {risk result}
```

The {predicates} are `closures` such as `in list of [...]`, 

- `All`, by design returns true if the evaluation data list is empty.
- `Any`, by design returns false if the evaluation data list is empty.

Further generalization of the rules becomes:

```
When {item} {predicate} then {risk result}
```

Where:

- {item} is a list of zero or more _evaluation data_ points.
- {predicate} is a closure that may contain a {target}
  - exists
  - does not existS
  - matches [exact | fuzzy] {target}
  - does not match [exact | fuzzy] {target}
  - contains [none | any | all] of [exact | fuzzy] {target}
  - contained in [none | any | all] {target}
- {risk result} is a record structured as:
  - score (0 to 100)
  - value (low, medium, high, etc)
  - list of evidence (open records)
