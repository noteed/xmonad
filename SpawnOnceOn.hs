{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  SpawnOnceOn
-- Copyright   :  (c) Spencer Janssen 2009, Vo Minh Thu 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  noteed@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a command once, on a given workspace.  This combines
-- spawnOn and spawnOnce.
--
-----------------------------------------------------------------------------

module SpawnOnceOn (spawnOnceOn) where

import XMonad
import Data.Set as Set
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.SpawnOn (spawnOn)
import Control.Monad

data SpawnOnceOn = SpawnOnceOn { unspawnOnceOn :: (Set (WorkspaceId, String)) }
  deriving (Read, Show, Typeable)

instance ExtensionClass SpawnOnceOn where
  initialValue = SpawnOnceOn Set.empty
  extensionType = PersistentExtension

-- | The first time 'spawnOnceOn' is executed on a particular command, that
-- command is executed on a particular workspace.  Subsequent invocations for a
-- command do nothing.
spawnOnceOn :: WorkspaceId -> String -> X ()
spawnOnceOn ws cmd = do
  b <- XS.gets (Set.member (ws, cmd) . unspawnOnceOn)
  when (not b) $ do
    spawnOn ws cmd
    XS.modify (SpawnOnceOn . Set.insert (ws, cmd) . unspawnOnceOn)
