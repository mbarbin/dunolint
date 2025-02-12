---
slug: initial-draft
title: Initial Draft
authors: [mbarbin]
tags: [release, ci]
---

https://discuss.ocaml.org/t/dunolint-status-update/16112

To my fellow OCaml & Dune users,

A while ago, I mentioned that I was using an internal tool to help me maintain the dune files of my monorepo.

This tool wasn't really designed to be shared from the beginning of its conception, but it has proven to be quite useful in my day-to-day work.

Recently, I began a complete refactor of this tool with the goal of turning it into a distributable package.

<!-- truncate -->

I have reached a point where I still have my internal layer (with many hacks, bells, and whistles), but it is now built on top of a project that I am happy to start publishing and iterating on publicly.

You can find the project page [here](https://github.com/mbarbin/dunolint).

Eventually, I hope that my internal layer will fade away, and I will be able to integrate all the features I currently rely on into dunolint. If I had to estimate, I'd say I'm about 10% done with that (though please don't quote me on that).

In this early version, the tool can enforce things like sorting dependencies, instrumentation, ppx and their flags, and can easily be integrated to a ci pipeline (see this [example](https://github.com/mbarbin/dunolint/pull/4) where dunolint is getting dunolinted in a GitHub Actions Workflow..).

It would take some effort to turn this project into a production-ready application, and I am not suggesting broad usage at this point. However, to early adopters and those interested in this kind of tool, please feel free to get in touch if you have any interest in influencing my work on this.

Happy linting, and thanks for reading!
