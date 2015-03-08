{-
 - Util.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Error where

import Control.Monad.Trans.Either

guardBool :: Bool -> String -> EitherT String IO ()
guardBool True _ = return ()
guardBool False s = left s

onFalse :: Bool -> IO String -> EitherT String IO ()
onFalse True _ = return ()
onFalse False m = EitherT . fmap Left $ m

guardMaybe :: Maybe a -> String -> EitherT String IO a
guardMaybe Nothing s = left ("Missing expected value: " ++ s)
guardMaybe (Just a) _ = return a
