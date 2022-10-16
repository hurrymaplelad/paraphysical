## Getting Started

1. Install [Deno](https://deno.land)
2. Run tests: `$ deno test -A`

## Publishing

Tags starting with `v` are published to NPM via Github actions. See `.github/workflows/ci-cd.yaml`. Create release tags via Github for extra features like release notes.

### Manual Publishing 

```
deno run -A scripts/build.ts 0.0.0
cd npm
npm publish
```
See [DNT for details](https://github.com/denoland/dnt)

## Read More

- Examples of good PPCL code: https://github.com/delphian/ppcl-library