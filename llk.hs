import Data.List (foldl')
import System.Environment (getArgs)
import Control.Monad (when)
import System.Console.GetOpt

import Types
import LLkProperties (find_order, is_sll)
import PDA
import Codegen


options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
    [ Option "d" ["debug"]  (NoArg  (\ opts -> opts{verbosity = V1})                ) "generate gebuggable output"
    , Option "o" ["output"] (ReqArg (\ f opts -> opts{dest    = f})   "<c/cpp-file>") "destination file"
    ]


parse_args :: [String] -> IO (CmdOptions, [String], [String])
parse_args argv =
    let usage    = "Usage: ./llk [OPTIONS]"
        def_opts = CmdOpts "1.cpp" V0
    in  case getOpt' Permute options argv of
            (o, n, u, []  ) -> return (foldl' (flip id) def_opts o, n, u)
            (_, _, _, errs) -> error $ concat errs ++ usageInfo usage options


main :: IO ()
main = do
    ((CmdOpts fdest v), non_opts, unknown_opts) <- getArgs >>= parse_args
    when (non_opts /= []) $
        error $ "unparsed cmd arguments: " ++ unwords non_opts
    when (unknown_opts /= []) $
        error $ "unknown cmd-options: " ++ unwords unknown_opts

    let k    = find_order
        tbl  = llk_table k
        code = pda2cpp k (is_sll k) tbl v

    print $ "This grammar is LL-" ++ show k
    print $ is_sll k
    print tbl
    writeFile fdest code



