import qualified RiscV.RV32ISpec as RV32I
import           Test.Hspec

main :: IO ()
main = hspec RV32I.spec
