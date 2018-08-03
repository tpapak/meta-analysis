import qualified TestHS as T
import Test.Meta as Meta

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ Meta.ioTests 
