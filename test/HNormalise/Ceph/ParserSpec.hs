{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - * Redistributions of source code must retain the above copyright
 - notice, this list of conditions and the following disclaimer.
 -
 - * Redistributions in binary form must reproduce the above
 - copyright notice, this list of conditions and the following
 - disclaimer in the documentation and/or other materials provided
 - with the distribution.
 -
 - * Neither the name of Author name here nor the names of other
 - contributors may be used to endorse or promote products derived
 - from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Ceph.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Data.Time                 (LocalTime(..), TimeOfDay(..), fromGregorian)
import qualified Net.IPv4                  as NT
import           Test.Hspec
import           Test.Hspec.Attoparsec

--------------------------------------------------------------------------------
import           HNormalise.Ceph.Parser
import           HNormalise.Ceph.Internal
import           HNormalise.Common.Internal

--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec =
    describe "parseCephSlowrequest" $
        it "parse regular info" $ do
            let s = "ceph-mon::  mds.0 10.141.16.2:6800/2006482174 331 : slow request 34.323061 seconds old, received at 2017-10-23 14:50:39.956270: client_request(client.6640582:5183868 readdir #1000da74e85 RP11_319E16_2.GseaPreranked.1482608163758.rpt 2017-10-23 14:50:39.949952) currently acquired locks" :: Text
            s ~> parseCephSlowRequest `shouldParse` ("ceph", CephSlowRequest
                { device = "mds.0"
                , address = CephAddress
                    { host = IPv4 $ NT.fromOctets 10 141 16 2
                    , fieldA = 6800
                    , fieldB = 2006482174
                    }
                , since = 34.323061
                , received = LocalTime
                    { localDay = fromGregorian 2017 10 23
                    , localTimeOfDay = TimeOfDay 14 50 39.956270
                    }
                , reason = "acquired locks"
                })
