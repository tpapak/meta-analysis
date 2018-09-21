import qualified TestHS as T
import Test.Meta.Effects as Effects

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ Effects.ioTests 
