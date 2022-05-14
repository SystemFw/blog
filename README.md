# README

Hakyll source code for my blog.

## Dev flow

| Command                   | Scenario                                |
|---------------------------|-----------------------------------------|
| `stack exec site watch`   | Changes to content                      |
| `stack exec site build`   | Changes to `talks.md` and `writings.md` |
| `stack exec site rebuild` | build and clean cache                   |
| `stack build`             | Changes to `site.hs`                    |
|---------------------------|-----------------------------------------|
| `cabal run site watch`    | Changes to content                      |
| `cabal run site build`    | Changes to `talks.md` and `writings.md` |
| `cabal run site rebuild`  | build and clean cache                   |
| `cabal build`             | Changes to `site.hs`                    |


Make sure you are in the root directory of the blog when running these
commands.

## Editing flow

Use the `content/staging` folder for drafts.
- For Scala, `mdoc.sh` will start an
  [mdoc](https://scalameta.org/mdoc/) live session.
- The Haskell flow is still unspecified.
- Use `talks.md` and `writings.md` to quickly add a talk or an
  external link to a writing. Slides for the talks are served through
  Github Pages on their own repo.

## Deploy
`deploy.sh` will deploy using git submodules and github pages.
You will have to git push after deploy, to make sure the submodule
state is correctly sync'd.

Note: when cloning the repo on a new laptop for the first time, you
have to pass the `--recurse-submodules` option to `clone` to
initialise the `deploy` submodule properly, or it won't be sync'd and
deploying won't work. If you forgot to do this, call:
```
git submodule update --init --recursive
```
More info [here](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
