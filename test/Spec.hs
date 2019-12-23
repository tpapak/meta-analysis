import qualified TestHS as T
import Test.Meta.Studies as Studies
import Test.Meta.Effects as Effects
import Test.Meta.Multiarm as Multiarm
import Test.Meta.CommonEffect as CommonEffect
import Test.Meta.NMA as NMA
--import Test.Meta.RandomEffects as RandomEffects

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ Studies.ioTests 
  T.reportTestsIO $ Effects.ioTests 
  T.reportTestsIO $ Multiarm.ioTests 
  T.reportTestsIO $ CommonEffect.ioTests 
  T.reportTestsIO $ NMA.ioTests 
  --T.reportTestsIO $ RandomEffects.ioTests 
