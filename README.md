# verse

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

**verse** is currently a concept and may not reach maturity.

## Your package dependencies, Chapter and **verse** 

It's common practice in R to utilise the default user library as the install
location for packages. Managing R package dependencies is often overlooked until
a user installs a new version of a package and later finds that code within 
another project has started to produce errors.

**verse** offers a simple and lightweight approach to managing R package
dependencies.

**verse** offers;

Reproducibility - non base packages are installed in a project specific library
and the current state of the project library is recorded in a lockfile. 
Recreating the project library from the lockfile is fast and simple. Moving to
a new machine or environment is simplified; the lockfile can easily be shared
and is platform independent.

Minimal impact - minimal changes are made to the users environment, the project
specific library becomes the first place that a package is looked for when
called and a custom `.Rprofile` file is added to the project root.

## Installation

Install **verse** from GitHub;

```
devtools::install_github("https://github.com/jrh-dev/verse")
```

## Usage

Use **verse** explicitly via `verse::init()`; never load with `library(verse)`.

To use **verse** for dependency management a project must be initiallised. With
the working directory set to the root of the project call `verse::init()`.

```
> setwd("C:/Users/a_user/verse_project")

> init()

Initializing verse project in directory; `C:/Users/a_user/verse_project`
```

Install packages using `verse::verse_install()`.

```

```





