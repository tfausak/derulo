import qualified Data.Maybe as Maybe
import qualified Derulo

main :: IO ()
main = interact (Derulo.showJSON . Maybe.fromJust . Derulo.readJSON)
