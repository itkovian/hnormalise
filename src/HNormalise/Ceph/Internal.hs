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
-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module HNormalise.Ceph.Internal where

--------------------------------------------------------------------------------
import           Data.Text
import           Data.Time             (LocalTime)
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal

--------------------------------------------------------------------------------

data CephAddress = CephAddress
    { host :: !Host
    , fieldA :: !Int
    , fieldB :: !Int
    } deriving (Eq, Show, Generic)

data CephSlowRequest = CephSlowRequest
    { device   :: !Text
    , address  :: !CephAddress
    , since    :: !Double
    , received :: !LocalTime
    , reason   :: !Text
    } deriving (Eq, Show, Generic)

data CephPGMap = CephPGMap
    { foo :: !Text
    } deriving (Eq, Show, Generic)
