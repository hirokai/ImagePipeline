# Image Pipeline, task runner for image processing

[![Build Status](https://travis-ci.org/hirokai/ImagePipelineNew.svg?branch=master)](https://travis-ci.org/hirokai/ImagePipelineNew)
[![Coverage Status](https://img.shields.io/coveralls/hirokai/ImagePipelineNew.svg)](https://coveralls.io/r/hirokai/ImagePipelineNew?branch=master)

> Task runner for image processing

## Getting Started

Work in progress.

## Examples of task definitions

```scala
import imagepipeline._
val path = new FilePath
val calc = start(path).then(autocontrast).then(blur).end()
```

### Goals

* Represent image processing tasks as a directed acyclic graph with static type checking.
* Composable tasks with type safety.
* Separate algorithm and data/parameters to make image processing more reproducible and manageable.
* Caching of intermediate results, and partial recalculation upon change of algorithm and/or input data using a dependency graph.
* Reporing and visualization of task execution progress.
* Interop with various softwares (Matlab, ImageJ, Igor, etc.)

## License
MIT license.