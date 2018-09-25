import qualified TestHS as T
import Test.Meta.Effects as Effects
import Test.Meta.CommonEffect as CommonEffect

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ Effects.ioTests 
  T.reportTestsIO $ CommonEffect.ioTests 
