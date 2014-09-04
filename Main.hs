import           Types
import           Runner
import           Control.Monad.State
import           Control.Lens

main = do
  -- Create a prog with some haskell code
  let prog = (def :: StateMachine)  & code .~ (read "[+>++<].")
  -- Execute the prog
  let res = execState (exec 5000) prog
  -- Now we have access to everithing (strip state, code, output, etc.)
  showVm res

showVm res = do
  putStrLn . showStrip 5 $ (res ^. strip)
  putStrLn $ (res ^. output)

main' str = do
    -- Create a prog with some haskell code
  let prog = (def :: StateMachine)  & code .~ (read str)
  -- Execute the prog
  let res = execState (exec 5000) prog
  -- Now we have access to everithing (strip state, code, output, etc.)
  showVm res
