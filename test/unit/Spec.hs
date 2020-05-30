{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#ifdef CABAL_HELPER_SUPPORT
import qualified CabalHelperSpec
#endif
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
#ifdef CABAL_HELPER_SUPPORT
    CabalHelperSpec.spec
#else
    pure ()
#endif
