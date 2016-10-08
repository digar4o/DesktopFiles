{-# LANGUAGE DeriveDataTypeable #-}
module Monitor
  ( monitorEventHook
  , requestMonitorEvents
  , Monitor(..)
  , Config(..)
  , Rotation
  )
where

import Data.List (nub)
import Data.Maybe (catMaybes)
import System.IO
import Control.Monad.Reader (ask)
import Graphics.X11.Xrandr
import Graphics.X11.Types (rrScreenChangeNotifyMask)
import Graphics.X11.Xlib.Types (Display)
import Data.Monoid (All(..))
import Graphics.X11.Xlib.Extras (Event(..))
import XMonad.Core

import qualified XMonad.Util.ExtensibleState as XS


data Monitor
  = Monitor
  { monWidht :: Word
  , monHeight :: Word
  , monRotation :: Rotation
  } deriving (Eq, Show, Read, Typeable)

newtype Config = Config [Monitor] deriving (Eq, Show, Read, Typeable)

instance ExtensionClass Config where
  initialValue = Config []
  extensionType = StateExtension
  --extensionType = StatePersistent

requestMonitorEvents :: X ()
requestMonitorEvents = do
  dpy  <- display <$> ask
  root <- theRoot <$> ask
  io $ xrrSelectInput dpy root rrScreenChangeNotifyMask
  XS.put =<< getCurrentConf

getLastConf :: X Config
getLastConf = XS.get

xrrMonToMon :: Display -> XRRScreenResources -> XRROutputInfo -> X [Monitor]
xrrMonToMon dpy scr mon = do
  rot <- io $ catMaybes <$> mapM (xrrGetCrtcInfo dpy scr) (xrr_oi_crtcs mon)
  return $ map toMon rot
  where toMon x = Monitor (fromIntegral $ xrr_ci_width x) (fromIntegral $ 
xrr_ci_height x) (xrr_ci_rotation x)

getCurrentConf :: X Config
getCurrentConf = do
  dpy  <- display <$> ask
  root <- theRoot <$> ask
  (Just scr) <- io $ xrrGetScreenResourcesCurrent dpy root
  mons <- io $ mapM (xrrGetOutputInfo dpy scr) (xrr_sr_outputs scr)
  Config . nub . concat <$> mapM (xrrMonToMon dpy scr) (catMaybes mons)


doTheDo :: ([Monitor] -> X ()) -> ([Monitor] -> X ()) -> Config -> Config -> X ()
doTheDo mon other (Config old) (Config new)
  | old == new = return ()
  | otherwise = do
      spawn "notify-send xmoand \"got something new\""
      if length old /= length new
         then do
           spawn "notify-send xmoand \"Going with monitor update\""
           mon new
         else do
           spawn "notify-send xmoand \"Going with other update\""
           other new

monitorEventHook :: ([Monitor] -> X ()) -> ([Monitor] -> X ()) -> Event -> X All
monitorEventHook mon other ev@(RRScreenChangeNotifyEvent {}) = do
  new <- getCurrentConf
  old <- getLastConf
  XS.put new
  doTheDo mon other old new
  spawn "notify-send xmonad \"Done doing the do\""
  return $ All True
monitorEventHook _ _ _ = return $ All True
