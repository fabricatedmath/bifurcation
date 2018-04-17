module Lib where

import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Quote
import Language.Haskell.TH

transformE :: Exp -> ExpQ
transformE = pure

hs :: QuasiQuoter
hs = QuasiQuoter
      { quoteExp = either fail transformE . parseExp
      , quotePat = error "Not supported"
      , quoteType = error "Not supported"
      , quoteDec = error "Not supported"
      }

hs_f :: QuasiQuoter
hs_f = quoteFile hs
