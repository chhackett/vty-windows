{-# OPTIONS_HADDOCK hide #-}
-- This makes a kind of trie. Has space efficiency issues with large
-- input blocks. Likely building a parser and just applying that would
-- be better.
module Graphics.Vty.Platform.Windows.Input.Classify
  ( classify
  )
where

import Graphics.Vty.Input.Events

import Graphics.Vty.Platform.Windows.Input.Classify.Types
import Graphics.Vty.Platform.Windows.Input.Mouse
import Graphics.Vty.Platform.Windows.Input.Focus
import Graphics.Vty.Platform.Windows.Input.Paste
import Graphics.Vty.Platform.Windows.ScreenSize

import Codec.Binary.UTF8.Generic (decode)

import Control.Arrow (first)
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )

import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

compile :: ClassifyMap -> ByteString -> KClass
compile table = cl' where
    -- take all prefixes and create a set of these
    prefixSet = S.fromList $ concatMap (init . BS.inits . BS8.pack . fst) table
    maxValidInputLength = maximum (map (length . fst) table)
    eventForInput = M.fromList $ map (first BS8.pack) table
    cl' inputBlock | BS8.null inputBlock = Prefix
    cl' inputBlock = case M.lookup inputBlock eventForInput of
            -- if the inputBlock is exactly what is expected for an
            -- event then consume the whole block and return the event
            Just e -> Valid e BS8.empty
            Nothing ->
                if S.member inputBlock prefixSet
                then Prefix
                -- look up progressively smaller tails of the input
                -- block until an event is found The assumption is that
                -- the event that consumes the most input bytes should
                -- be produced.
                -- The test verifyFullSynInputToEvent2x verifies this.
                -- H: There will always be one match. The prefixSet
                -- contains, by definition, all prefixes of an event.
                else
                    let inputPrefixes = reverse . take maxValidInputLength . tail . BS8.inits $ inputBlock
                    in case mapMaybe (\s -> (,) s `fmap` M.lookup s eventForInput) inputPrefixes of
                        (s,e) : _ -> Valid e (BS8.drop (BS8.length s) inputBlock)
                        -- neither a prefix or a full event.
                        [] -> Invalid

classify :: ClassifyMap -> ClassifierState -> ByteString -> KClass
classify table = process
    where
        standardClassifier = compile table

        process ClassifierStart s =
            case BS.uncons s of
                _ | bracketedPasteStarted s ->
                    if bracketedPasteFinished s
                    then parseBracketedPaste s
                    else Chunk
                _ | isMouseEvent s      -> classifyMouseEvent s
                _ | isFocusEvent s      -> classifyFocusEvent s
                _ | isScreenSizeEvent s -> classifyScreenSize s
                Just (c,cs) | c >= 0xC2 -> classifyUtf8 c cs
                _                       -> standardClassifier s

        process (ClassifierInChunk p ps) s | bracketedPasteStarted p =
            if bracketedPasteFinished s
            then parseBracketedPaste $ BS.concat $ p:reverse (s:ps)
            else Chunk
        process ClassifierInChunk{} _ = Invalid

classifyUtf8 :: Word8 -> ByteString -> KClass
classifyUtf8 c cs =
  let n = utf8Length c
      (codepoint,rest) = BS8.splitAt (n - 1) cs

      codepoint8 :: [Word8]
      codepoint8 = c:BS.unpack codepoint

  in case decode codepoint8 of
       _ | n < BS.length codepoint + 1 -> Prefix
       Just (unicodeChar, _)           -> Valid (EvKey (KChar unicodeChar) []) rest
       -- something bad happened; just ignore and continue.
       Nothing                         -> Invalid

utf8Length :: Word8 -> Int
utf8Length c
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4
