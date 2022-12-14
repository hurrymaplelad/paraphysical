[![CI](https://github.com/hurrymaplelad/paraphysical/actions/workflows/release.yaml/badge.svg)](https://github.com/hurrymaplelad/paraphysical/actions/workflows/release.yaml)
[![npm version](https://badge.fury.io/js/paraphysical.svg)](https://badge.fury.io/js/paraphysical)

## Getting Started

1. Install [Deno](https://deno.land)
2. Run tests: `$ deno test -A`

## Continuous Deployment

Uses Github Actions for CI/CD. See `.github/`.

Every push to the `main` branch triggers committing a corresponding node module build to the release branch. `npm install hurrymaplelad/paraphysical#release` to depend on the mainline.

Tags starting with `v` have their release builds published to NPM. Create release tags via Github for extra features like release notes.

### Manual Publishing 

```
deno run -A scripts/build.ts 0.0.0
cd npm
npm publish
```
See [DNT for details](https://github.com/denoland/dnt)

## Read More

- PPCL Docs: https://github.com/mitchpaulus/vim-siemens-ppcl/blob/master/doc/ppcl.txt
- Examples of good PPCL code: https://github.com/delphian/ppcl-library