import qualified Data.Set as Set

set = Set.fromList "erik salaj"
set2 = Set.fromList "tajm"

main = do
    print set
    print set2
    print (Set.union set set2)