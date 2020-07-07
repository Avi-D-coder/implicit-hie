# Rename this file to hie.yaml to open this project in HIE and use Cabal as build system

cradle:
  cabal:
    - path: "src"
      component: "lib:implicit-hie"

    - path: "app/Main.hs"
      component: "implicit-hie:exe:gen-hie"

    - path: "test"
      component: "implicit-hie:test:implicit-hie-test"
