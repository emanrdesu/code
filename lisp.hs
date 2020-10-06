
import Data.List

car = head
cdr = tail

caar = car . car
caaar = car . caar
cddr = cdr . cdr
cdddr = cdr . cddr

yesOrNo :: String -> IO Bool
yesOrNo question
  = do putStr (question ++ "?(y/n): ")
       ans <- getLine
       case ans of
         "yes" -> return True
         "y"   -> return True
         "no"  -> return False
         "n"   -> return False
         _     -> do putStrLn "Enter yes, y, n, or no. Try again."
                     yesOrNo question
