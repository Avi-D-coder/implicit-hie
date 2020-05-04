# implicit-hie
```bash
cd your-stack-or-cabal-package
gen-hie
```
`gen-hie` should be run the root of a cabal or stack project.
The config type (cabal or stack) is determined by the existence of
`dist-newstyle`, `.stack-work`, `stack.yaml`, if none are found the default is cabal.

# Cabal Multi project example
```bash
❯ git clone https://github.com/well-typed/optics.git
❯ cd optics
❯ gen-hie
wrote Cabal /home/user/optics/indexed-profunctors/hie.yaml
wrote Cabal /home/user/optics/optics-th/hie.yaml
wrote Cabal /home/user/optics/optics-vl/hie.yaml
wrote Cabal /home/user/optics/codegen/hie.yaml
wrote Cabal /home/user/optics/optics-core/hie.yaml
wrote Cabal /home/user/optics/optics-sop/hie.yaml
wrote Cabal /home/user/optics/optics-extra/hie.yaml
wrote Cabal /home/user/optics/template-haskell-optics/hie.yaml
wrote Cabal /home/user/optics/optics/hie.yaml
wrote Cabal /home/user/optics/metametapost/hie.yaml
```

## Features

All common Cabal and Stack configurations should just work.
If you use more advanced features, the generated config may not be complete.

- [x] multi component cabal, stack projects
- [x] multiple executables under a single path
- [ ] multiple paths provided to `hs-source-dirs`
- [ ] common stanzas

### Sponsorship, Work, Twitter
- If you value my contributions to Haskell tooling, FP oriented GC, or open source FP in general, please consider sponsoring me via Github.
- I'm currently looking for work as a Haskell or Rust developer. I can be reached via email or a twitter message.
- Follow me on twitter [@AviDessauer](https://twitter.com/AviDessauer).
