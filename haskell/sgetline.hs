import System.IO

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

--sgetLine :: IO String
--sgetLine = do x <- getCh
--		if x == '\n' then
--		  do putChar x
--		    return []
--		else
--		  do putChar '-'
--		    xs <- sgetLine
--		    return (x:xs)