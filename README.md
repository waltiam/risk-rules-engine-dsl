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

```sh
All {evaluation data} meet {predicate} then {risk result}
Any {evaluation data} meet {predicate} then {risk result}
None {evaluation data} meet {predicate} then {risk result}
```

The {predicates} are `closures` such as `in list of [...]`

- `All`, by design returns true if the evaluation data list is empty.
- `Any`, by design returns false if the evaluation data list is empty.

Further generalization of the rules becomes:

```sh
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

## Model definition

In addition to defining the rules it will be required to define the models in play, for example:

```yaml
--- # evaluation model definitions
name: Country of Primary Address
fields:
- name: Name
  type: string
- name: Description
  type: string
---
name: Primary Industry
fields: 
- name: Naics Code
  type: string
- name: Description
  type: string
```

```yaml
--- # standard model defintion
name: Country
fields:
- name: Name
  type: string
- name: 
```

## Alternately

Standard data defined in a yaml file:

```yml
---
CountryRatings
- name:        Gondor
  code:        GON
  score:       85
  restriction: Sanctioned
- name:        Mordor
  code:        MOR
  score:       45
  restriction: Unrestricted
- name:        Elbonia
  code:        ELB
  score:       20
  restriction: Unrestrictyed
- name:        Whoville
  code:        WHO
  score:       55
  restriction: Unrestricted
- name:        Terabithinia
  code:        TER
  score:       25
  restriction: Unrestricted
- name:        El Dorado
  code:        ELD
  score:       35
  restriction: Sanctioned
- name:        Atlantis
  code:        ATL
  score:       90
  restriction: Prohibited
- name:        Borduria
  code:        BOR
  score:       95
  restriction: Prohibited
# - name:        Mare Tranquillitats
#   code:        SOT
#   score:       0
#   restriction: Unrestricted
# - name:        Shangri-la
#   code:        SGL
#   score:       0
#   restriction: Unrestricted
```
