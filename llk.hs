import Debug.Trace

import LLkProperties (find_order, is_sll)
import PDA
import Codegen


trace' :: Show a => a -> a
trace' a = trace (show a) a


main :: IO ()
main = do
    let k = find_order
        t = llk_table k
        code = pda2cpp k (is_sll k) t

    print $ "This grammar is LL-" ++ show k
    print $ is_sll k
    print t
    writeFile "1.cpp" code



