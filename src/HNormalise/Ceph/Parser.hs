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

module HNormalise.Ceph.Parser where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Text                  (Text)
--------------------------------------------------------------------------------

import           HNormalise.Common.Parser
import           HNormalise.Ceph.Internal
--------------------------------------------------------------------------------

{-
CEPH_BYTES %{NONNEGINT} (?:(?:k|M|G|T|P)?B)

# 63 active+remapped+wait_backfill+backfill_toofull, 958 active+clean, 3 active+degraded+remapped+backfilling, 4 active+degraded+remapped+wait_backfill, 1 active+remapped, 12 active+degraded+remapped+backfill_toofull, 6 active+clean+scrubbing+deep, 109 active+remapped+backfilling, 26 active+remapped+wait_backfill, 7 active+degraded+remapped+wait_backfill+backfill_toofull, 50 active+degraded, 73 active+remapped+backfill_toofull
CEPH_PGMAP_DETAIL_ACTIVE_CLEAN %{NONNEGINT:active_clean:int} active\+clean
CEPH_PGMAP_DETAIL_REST %{NONNEGINT} (?:[^ ,;]+)
CEPH_PGMAP_DETAIL_PAT (?:%{CEPH_PGMAP_DETAIL_ACTIVE_CLEAN}|%{CEPH_PGMAP_DETAIL_REST})
CEPH_PGMAP_DETAIL (?:%{CEPH_PGMAP_DETAIL_PAT}(?:, %{CEPH_PGMAP_DETAIL_PAT})*)

CEPH_PGMAP_VOLUME %{CEPH_BYTES:volumedata} data, %{CEPH_BYTES:volumeused} used, %{CEPH_BYTES:volumeavail} / %{CEPH_BYTES:volumetotal} avail
CEPH_PGMAP_ACT (%{CEPH_BYTES:actread}/s rd, )?(%{CEPH_BYTES:actwrite}/s wr, )?%{NONNEGINT:actops:int} op/s
CEPH_PGMAP_OBJ_DEGRADED %{NONNEGINT:objdegraded:int}/%{NONNEGINT:objtotal:int} objects degraded [^;]*
CEPH_PGMAP_OBJ_MISPLACED %{NONNEGINT:objmisplaced:int}/%{NONNEGINT:objtotal:int} objects misplaced [^;]*
CEPH_PGMAP_OBJ_RECOV %{CEPH_BYTES:objrecovthr}/s, %{NONNEGINT:objrecovhz:int} objects/s recovering

CEPH_PGMAP .*? pgmap v(?<pgmapv>[0-9]+): %{NONNEGINT:pgs:int} pgs: (?:%{CEPH_PGMAP_DETAIL})(?:; %{CEPH_PGMAP_VOLUME})?(?:; %{CEPH_PGMAP_ACT})?(?:; %{CEPH_PGMAP_OBJ_DEGRADED})?(?:; %{CEPH_PGMAP_OBJ_MISPLACED})?(?:; %{CEPH_PGMAP_OBJ_RECOV})?$

# NONNEGINT to support osd.0
CEPH_SLOW_REQUEST_SECONDS (?<slowrequestseconds>%{NONNEGINT})(?:\.%{NONNEGINT})?
CEPH_SLOW_REQUEST_SINGLE slow request
CEPH_SLOW_REQUEST_OLDEST %{NONNEGINT:slowrequestnumber:int} slow requests, .*? oldest blocked for .*?

CEPH_SLOW_REQUEST .*? (?<osd>osd.%{NONNEGINT}) .*? (?:%{CEPH_SLOW_REQUEST_SINGLE}|%{CEPH_SLOW_REQUEST_OLDEST}) %{CEPH_SLOW_REQUEST_SECONDS} (?:secs|seconds old)

CEPH_MSG %{DATA} (?:%{CEPH_PGMAP}|%{CEPH_SLOW_REQUEST})


<28>1 2017-10-23T14:51:22.261878+02:00 mds03 ceph-mon: - ceph-mon::  mds.0 10.141.16.2:6800/2006482174 330 : 1 slow requests, 1 included below;
oldest blocked for > 34.323061 secs

<28>1 2017-10-23T14:51:22.261019+02:00 mds01 ceph-mon: - ceph-mon::  mds.0 10.141.16.2:6800/2006482174 331 : slow request 34.323061 seconds old,
received at 2017-10-23 14:50:39.956270: client_request(client.6640582:5183868 readdir #1000da74e85 RP11_319E16_2.GseaPreranked.1482608163758.rpt
2017-10-23 14:50:39.949952) currently acquired locks

<28>1 2017-10-23T13:48:29.067300+02:00 mds02 ceph-mon: - ceph-mon::  osd.127 10.141.16.25:6801/1801 107735 : slow request 30.651866 seconds old,
received at 2017-10-23 13:47:56.393335: osd_op(client.6649350.1:270320 4.ceb71918 10015b01df2.00000000 [write 0~23750 [1@-1]] snapc 1=[]
ondisk+write+known_if_redirected e80260) currently waiting for blocked object

-}
parseCephAddress :: Parser CephAddress
parseCephAddress = do
    host <- skipSpace *> hostnameOrIPParser
    fieldA <- char ':' *> decimal
    fieldB <- char '/' *> decimal
    return CephAddress
        { host = host
        , fieldA = fieldA
        , fieldB = fieldB
        }

parseCephSlowRequest :: Parser (Text, CephSlowRequest)
parseCephSlowRequest = do
    string "ceph-mon::"
    device <- skipSpace *> Data.Attoparsec.Text.takeWhile (/= ' ')
    address <- skipSpace *> parseCephAddress
    skipSpace *> decimal *> skipSpace *> char ':' *> skipSpace *> string "slow request"  -- since the 1+ slow request line always is followed by the actual cause, we need only save this one
    since <- skipSpace *> double
    skipSpace *> string "seconds old,"
    received <- skipSpace *> string "received at" *> skipSpace *> parseLocalTime
    skipSpace *> manyTill anyChar (string "currently")
    reason <- skipSpace *> takeTill (`elem` ("\n"::String))
    return ("ceph", CephSlowRequest
        { device = device
        , address = address
        , since = since
        , received = received
        , reason = reason
        })
