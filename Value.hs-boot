module Value where

import {-# SOURCE #-} Abstract

data Val
instance Eq Val
instance Ord Val
instance Show Val

type TeleVal = [TBinding Val]
