{-# LANGUAGE OverloadedStrings #-}
module Tikz ( tikzFilter ) where

import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import qualified Text.HTML.TagSoup as TS (parseTags, Tag(TagOpen))
import qualified Data.ByteString.Lazy.Char8 as BS (pack, unpack)
import qualified Network.URI.Encode as URI (encode)
import qualified Data.Text as T

import Data.String (fromString)
import Data.Functor ((<&>))

import Control.Monad.State (StateT, modify, get, lift, evalStateT)

import Debug.Trace (traceShowId)

type TikzM = StateT [T.Text] Compiler

tikzFilter :: Pandoc -> Compiler Pandoc
tikzFilter doc = evalStateT (walkM compileTikzBlock doc) []

compileTikzBlock :: Block -> TikzM Block
compileTikzBlock cb@(CodeBlock (_,"tikzpicture":_,_) _) 
  = do preamble <- get
       lift (compileTikzBlockWith 
                 "templates/tikzpicture.tex"
                 preamble
                 FreeSize
                 cb)
compileTikzBlock cb@(CodeBlock (_,"tikzcd":_,_) _)
  = do preamble <- get
       lift (compileTikzBlockWith 
                 "templates/tikzcd.tex"
                 preamble
                 (FixedScale 0.2 "em")
                 cb)
compileTikzBlock rb@(RawBlock (Format "tex") defLine) = do modify (++[defLine])
                                                           return rb
compileTikzBlock x = return x

compileTikzBlockWith :: FilePath -> [T.Text] -> SizeOption -> Block -> Compiler Block
compileTikzBlockWith templatename preamble size (CodeBlock (elemId, classes, namevals) contents) =
  makeItem (T.unpack contents)
    >>= loadAndApplyTemplate
          (fromFilePath templatename)
          (bodyField "body" `mappend` constField "preamble" (T.unpack $ T.unlines preamble))
    <&> itemBody
    <&> BS.pack
    >>= unixFilterLBS "rubber-pipe" ["--pdf"]
    >>= unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
    <&> BS.unpack
    <&> filter (/= '\n')
    <&> imageBlockFromSVG size elemId classes namevals
compileTikzBlockWith _ _ _ x = return x

data SizeOption = FreeSize | FixedScale Float String

imageBlockFromSVG :: SizeOption -> T.Text -> [T.Text] -> [(T.Text,T.Text)] -> String -> Block
imageBlockFromSVG size id classes namevals code =
        let dataurl = "data:image/svg+xml;utf8," ++ (URI.encode code)
            tags = TS.parseTags code
            findSvgTagAttrs [] = error "malformed SVG"
            findSvgTagAttrs (TS.TagOpen "svg" attrs : _) = attrs
            findSvgTagAttrs (_:rest) = findSvgTagAttrs rest
            sizeAttrs = [(name,val) | (name,val) <- findSvgTagAttrs tags, name `elem` ["height", "width"]]
            resize scale unit val = let (no,_):_ = reads val
                                     in show (scale * no :: Float) ++ unit
            extraAttrs = case size of
                          FreeSize -> []
                          FixedScale scale unit -> [(fromString name, fromString (resize scale unit val)) | (name,val) <- sizeAttrs]
         in Para [Image (id, classes, extraAttrs ++ namevals) [] (fromString dataurl, "")]
