---
title: Concepts
category: Documentation
categoryindex: 1
index: 0
---

# Concepts

FsMake's core concepts are:

  - [Pipelines](../reference/fsmake-pipeline.html)
  - [Stages](../reference/fsmake-stage.html)
  - [Steps](../reference/fsmake-step.html)

A `pipeline` has a name and contains many `stages`.

A `stage` has and name and contains one or more `steps`. Depending on the `stage`, these `steps` could be conditional and/or parallel.

A `step` has a name and a special [Make<'T>](../reference/fsmake-make-1.html) function.<br />
A step is where you define what you want to run.

## Make<'T>

[Make<'T>](../reference/fsmake-make-1.html) is the core abstraction and extensibility point in FsMake.

A `step` is composed of glued together `Make<'T>` functions.

FsMake includes some computation expression builders for working with them.<br />
See [MakeBuilders](../reference/fsmake-makebuilders.html).
