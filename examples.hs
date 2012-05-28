import Control.Category((>>>))
import Control.Monad((>=>))
import Data.Lens.Zipper
import Data.Yall.Lens


-- ----------------------------------------------------------------------------

-- This should really use partial lenses, but for now we'll be a bit stupid:
data Tree a = Br { _lBranch :: Tree a
                 , _node    :: a
                 , _rBranch :: Tree a }
            | Nil
             deriving Show

-- START BOILERPLATE
lBranch = lens _lBranch (\(Br _ n r) l-> Br l n r)
rBranch = lens _rBranch (\(Br l n _) r-> Br l n r)
node = lens _node (\(Br l _ r) n-> Br l n r)
-- END BOILERPLATE

test1 = do
    let tr0 = Br (Br Nil 1 (Br Nil 2 Nil)) 3 (Br Nil 4 Nil)
    print tr0
    let z = zipper tr0
        -- this is required if we don't use type equality constraints from
        -- TypeFamilies:
        --zops :: Zipper Top (Tree Int) -> Tree Int
        incNode = moveP node >>> modf (+1) >>> moveUp
        zops = incNode >>> moveP lBranch >>> moveP rBranch >>> incNode >>> 
                moveUp >>> incNode >>> moveUp >>> moveP rBranch >>> 
                incNode >>> close  
    print $ zops z



-- ----------------------------------------------------------------------------
--
-- partial lenses and a complicated mutually-recursive type:
-- TODO: finish this

data Odd = OddNode { _evenLeft :: EvenL
                   , _oddInt :: Int
                   , _evenRight :: EvenR }
         | OddNil
         deriving Show

data EvenL = EvenNodeL { _oddLeftL :: Odd
                       , _oddRightL :: Odd }
                       deriving Show

data EvenR = EvenNodeR { _oddLeftR :: Odd
                       , _oddRightR :: Odd }
                       deriving Show

ex2 = OddNode (EvenNodeL OddNil OddNil) 3 (EvenNodeR (OddNode (EvenNodeL OddNil OddNil) 4 (EvenNodeR OddNil OddNil)) (OddNode (EvenNodeL OddNil OddNil) 6 (EvenNodeR OddNil OddNil)))
