# implicit-hie
```bash
cd your-stack-or-cabal-package
stack install implicit-hie # or cabal install implicit-hie
gen-hie > hie.yaml
```
`gen-hie` should be run the root of a cabal or stack project.
The config type (cabal or stack) is determined by the existence of
`dist-newstyle`, `.stack-work`, `stack.yaml`, if none are found the default is cabal.
You can use `--cabal`, or `--stack` options to override config type.

# Cabal Multi project example
```bash
❯ git clone https://github.com/well-typed/optics.git
❯ cd optics
❯ gen-hie
cradle:
  cabal:
    - path: "indexed-profunctors/src"
      component: "lib:indexed-profunctors"

    - path: "optics-th/src"
      component: "lib:optics-th"

    - path: "optics-th/tests"
      component: "optics-th:test:optics-th-tests"

    - path: "optics-vl/src"
      component: "lib:optics-vl"

    - path: "codegen/./Subtypes.hs"
      component: "optics-codegen:exe:optics-codegen-subtypes"

    - path: "optics-core/src"
      component: "lib:optics-core"

    - path: "optics-sop/src"
      component: "lib:optics-sop"

    - path: "optics-extra/src"
      component: "lib:optics-extra"

    - path: "template-haskell-optics/src"
      component: "lib:template-haskell-optics"

    - path: "optics/src"
      component: "lib:optics"

    - path: "optics/tests"
      component: "optics:test:optics-tests"

    - path: "metametapost/src/Cli.hs"
      component: "metametapost:exe:metametapost-optics"

```

## Features

All common Cabal and Stack configurations should just work.
If you use more advanced features, the generated config may not be complete.

- [x] multi component cabal, stack projects
- [x] multiple executables under a single path
- [x] multiple paths provided to `hs-source-dirs`
- [x] lookup nested packages in `cabal.project` or `stack.yaml`
- [x] Handle Exe and Bench `other-modules`
- [ ] common stanzas

### Work, Twitter
- Follow me on twitter [@AviDessauer](https://twitter.com/AviDessauer).
