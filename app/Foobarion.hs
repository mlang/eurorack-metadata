{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eurorack.Synthesizers

a100lc9 = [ [A110_1, A110_1, A114, A115, A116, A138b, A120, A106_6, A131, A130]
          , [A190_4, A100_bl2, A118, A148, A145, A146, A160, A161, A180_1, A138a, A170, A140, A140]
          , [A119, A136] ]

foobarion :: System
foobarion =
  [ [ [ A180_2, A100_bl8, A143_9, A110_1, A138b, A111_4, VScale, A143_2
      , A115, A184_1, A110_1, CO, A180_3, A143_2]
    , [ A190_4, A180_3, A151, A152, A132_3
      , A101_2, A106_6, A116, A120, A124, A136, Evolution, ATC
      , A132_3, A180_3, A160_5, A156, A185_2, A100_bl8, A100_bl4 ]
    , [ A119, A100_bl2, A160, A161, A118, A145, A146, Maths, A132_3
      , SubMix
      , A132_3, ErbeVerb, DLD, Mixer, A138s, Outs ] ]
  , [ [A180_2, QCD, QCDExp, A166, M303, Grids, BIA, SD808, Branches
      , Hats808, RS808, CP909, A138b, One, A100_bl42, A100_bl4, A100_bl2, A162]
    , [Autobot, Robokop, A182_1, A100_bl8, A100_bl8, A100_bl4, A100_bl42 ] ]
  ]

main :: IO ()
main = renderToDirectory "html" foobarion
