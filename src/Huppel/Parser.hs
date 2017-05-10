{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Huppel.Parser where

--------------------------------------------------------------------------------
import Control.Applicative ( (<|>) )
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator ( lookAhead, manyTill )
--------------------------------------------------------------------------------

import Common.Parser
import Huppel.Internal
--------------------------------------------------------------------------------


parseHuppel :: Parser Huppel
parseHuppel = do
    i <- string "huppel" *> whitespace *> decimal
    return $ Huppel { id = i }
