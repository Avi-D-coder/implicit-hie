cradle:
  cabal:
    - path: "src"
      component: "lib:haskell-language-server"

    - path: "exe/Main.hs"
      component: "haskell-language-server:exe:haskell-language-server"

    - path: "exe/Wrapper.hs"
      component: "haskell-language-server:exe:haskell-language-server-wrapper"

    - path: "test/functional"
      component: "haskell-language-server:test:func-test"

    - path: "test/utils"
      component: "lib:haskell-language-server:hls-test-utils"
