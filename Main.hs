import           Types
import           Genetic
import           Runner
import           Control.Monad.State
import           Control.Lens
import           System.Random

main = do
  -- Create a prog with some haskell code
  let prog = (def :: StateMachine)  & code .~ (read "[+>++<].")
  -- Execute the prog
  let res = execState (exec 5000) prog
  -- Now we have access to everithing (strip state, code, output, etc.)
  showVm res

-- Display the strip around the current position, the output, and
-- the illness state (tell if the vm "crashed").
showVm res = do
  putStrLn . showStrip 5 $ (res ^. strip)
  putStrLn $ (res ^. output)
  putStrLn . show $ (res ^. illField)

main' str = do
    -- Create a prog with some haskell code
  let prog = (def :: StateMachine)  & code .~ (read str)
  -- Execute the prog
  let res = execState (exec 5000) prog
  -- Now we have access to everything (strip state, code, output, etc.)
  showVm res

-- Generate a small sample of 50 instruction, and try to run it
randomTry = do
   randLi <- sequence (take 100 $ repeat randomIO) :: IO [Sym]
   let vmState = (def :: StateMachine) & code .~ (Code randLi)
   let vm = execState (exec 800) vmState
   showVm $ vm
   return vm
