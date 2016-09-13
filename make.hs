module Main where

import Control.Applicative        ((<$>))
import Control.Monad              ((<=<), mzero)
import Data.List                  (intersperse, isSuffixOf)
import Development.Shake
import Development.Shake.FilePath
import SoHFilter (FixupMode(Consolidate, Remove), sohFilter)
import System.Exit (ExitCode(..))
import Prelude hiding ((*>))

chapters :: [FilePath]
chapters = [ "src/title.txt"
           , "src/Intro.md"
           ]

these           :: [FilePath]
these           = ["make.hs","SoHFilter.hs"]
allChapters    = "_build/allChapters.md"
allChaptersSoH = "_build/allChaptersSoH.md"
-- srclink        = "http://www.happstack.com/docs/book/src"

main :: IO ()
main = shakeArgs shakeOptions $ do
         let src  = map (\f -> "_build/src" </> (f -<.> "hs")) $ filter (isSuffixOf ".lhs") chapters
             tgts = [ "_build/hyperdrive-book.html", "_build/theme.css" ]
--             tgts = ["_build/hyperdrive-book.html", "_build/hyperdrive-book.pdf", "_build/hyperdrive-book.epub", "_build/hyperdrive-book.mobi", "_build/hyperdrive-book.md", "_build/theme.css", "_build/src/messages.zip"] ++ src
         want tgts
         allChapters *> \out ->
             do need (these ++ chapters)
                let loadFile :: FilePath -> Action String
                    loadFile fp
                        | isSuffixOf ".cpp.lhs" fp =
                            do (Stdout r) <- command [] "cpphs" ["--noline",fp]
                               return r
--                               sohFilter Remove r
                        | isSuffixOf ".lhs" fp =
                            do c <- readFile' fp
--                               sohFilter Remove c
                               return c
                        | otherwise = readFile' fp
--                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM (loadFile) chapters
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM loadFile chapters
                writeFileChanged allChapters allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", allChapters]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", allChapters]
         "_build/theme.css" *> \out ->
             do need $ "src/theme.css" : these
                copyFile' "src/theme.css" "docs/theme.css"
         "_build/src//*.hs" *> \out ->
             do let inLhs = drop 11 $ out -<.> "lhs"
                need $ inLhs : these
                system' "sed" ["-n","s/^> \\?//w " ++ out, inLhs]
         "_build/hyperdrive-book.html" *> \out ->
             do need $ allChapters : these
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css","-o", out, allChapters]
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://happstack.com/docs/crashcourse/theme/theme.css","-o", out, allChapters]
                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","theme.css","-o", out, allChapters]
                system' "sed" ["-i","s/srclink/www\\.happstack\\.com\\/docs\\/crashcourse\\/src/g", out]
--                system' "ln"  ["-s", "-f", "hyperdrive-book.html", "docs/index.html"]
                system' "cp" ["_build/hyperdrive-book.html", "docs/index.html"]
--                copyFile' out  "docs/index.html"
         "_build/hyperdrive-book.pdf" *> \out ->
             do need $ allChapters:these
                system' "pandoc" ["-V", "documentclass:book", "-f", "markdown+lhs","--latex-engine","pdflatex","--toc","--chapters","-o", out, allChapters]
         "_build/hyperdrive-book.epub" *> \out ->
             do need $ allChapters:these
                system' "pandoc" ["-f", "markdown+lhs","-o", out, allChapters]
         "_build/hyperdrive-book.mobi" *> \out ->
             do need $ "_build/hyperdrive-book.epub":these
                (Exit c) <- command [] "kindlegen" ["_build/hyperdrive-book.epub"]
                if (c == ExitSuccess) || (c == ExitFailure 1)
                   then return ()
                   else error $ "kindlgen failed with exit code: " ++ show c
         "_build/hyperdrive-book.md" *> \out ->
             do need $ chapters++these
                     -- FIXME: probably needs to call loadFile
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM ((sohFilter Consolidate) <=< readFile') chapters
                writeFileChanged out allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", out]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", out]
                system' "sed" ["-i", "s/import Happstack.Server/import Happstack.Server.Env/", out]
                system' "sed" ["-i", "s/sourceCode literate haskell/haskell web active/", out]
--                system' "pandoc" ["-f", "markdown+lhs","-t","markdown_soh","-o", out, allChapters_]
         "_build/src/messages.zip" *> \out ->
             do msgs <- getDirectoryFiles "." ["messages//*.msg"]
                need msgs
                system' "zip" (out:msgs)
         phony "check-demos" $
               do need src
                  mapM_ (\hs -> system' "ghc" ["-fno-code", hs]) src
         phony "publish" $
             do need tgts
                system' "rsync" ["-avxz", "--exclude","*.o","--exclude", "*.hi", "_build/", "ubuntu@aws1.seereason.com:/home/jeremy/public_html/happstack-crashcourse/"]

--	rsync -avxz --exclude '*.o' --exclude '*.hi' html/ jeremy@happstack.com:public_html/happstack-crashcourse/

