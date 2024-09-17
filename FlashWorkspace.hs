{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  FlashWorkspace
-- Copyright   :  (c) Vo Minh Thu 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  noteed@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for displaying a workspace a short time, then switch back to the
-- previous workspace.
-- The flashed workspace is currently hard-coded to "9", where I have a kind of
-- dashboard with date and time, current battery status, ...
--
-----------------------------------------------------------------------------

module FlashWorkspace (flashWorkspace, flashbackWorkspace) where

import XMonad
import Data.Set as Set
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.SpawnOn (spawnOn)
import Control.Monad

import Control.Monad
import Data.Monoid
import qualified Data.Map as M

import XMonad
import XMonad.Actions.OnScreen
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Layout.NoBorders (noBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)

-- | Store the current workspace to remember where to switch back.
data FlashWorkspace = FlashWorkspace { unFlashWorkspace :: Maybe String }
  deriving (Read, Show, Typeable)

instance ExtensionClass FlashWorkspace where
  initialValue = FlashWorkspace Nothing
  extensionType = PersistentExtension

-- | Remember the current workspace so we can go back to it.
flashWorkspace :: X ()
flashWorkspace = do
  ws <- join . gets $ screenWorkspace . W.screen . W.current . windowset
  XS.put (FlashWorkspace ws)
  windows (viewOnScreen 0 "9")

flashbackWorkspace :: X ()
flashbackWorkspace = do
  mws <- XS.gets unFlashWorkspace
  case mws of
    Just ws -> do
      XS.put (FlashWorkspace Nothing)
      windows (viewOnScreen 0 ws)
    Nothing -> return ()
