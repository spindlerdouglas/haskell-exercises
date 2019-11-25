import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
		 | Succ Nat
		deriving Show
		
natToInteger :: Nat -> Integer
--natToInteger Zero = 0
natToInteger = \ n -> genericLength [c | c <- show n, c == 'S']