import qualified TestHS as T
import Test.Meta.Effects as Effects
import Test.Meta.Multiarm as Multiarm
import Test.Meta.CommonEffect as CommonEffect
import Test.Meta.RandomEffects as RandomEffects

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ Effects.ioTests 
  T.reportTestsIO $ Multiarm.ioTests 
  T.reportTestsIO $ CommonEffect.ioTests 
  T.reportTestsIO $ RandomEffects.ioTests 
