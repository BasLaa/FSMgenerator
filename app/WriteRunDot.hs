{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module WriteRunDot where

import           Control.Monad   (forM_)
import Data.GraphViz
    ( addExtension,
      runGraphvizCommand,
      GraphvizOutput(Png),
      GraphvizCommand(Dot),
      PrintDotRepr )
import System.FilePath ( combine )

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = forM_ cases createImage

createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)