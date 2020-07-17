{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.List (sort)
import Data.Tree
import Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps empsF) = GL (emp:emps) ((empFun emp) + empsF)

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  (GL e1 fun1) <> (GL e2 fun2) = GL (e1 ++ e2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold b f (Node root sub) = f root $ map (treeFold b f) sub

getEmployees :: GuestList -> [Employee]
getEmployees (GL emps _) = emps

getFun :: GuestList -> Fun
getFun (GL _ f) = f

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss [] = glCons boss mempty
combineGLs boss gls =
  foldr moreFun mempty $
  map (\gl ->
    if (head (getEmployees gl)) == boss
    then gl
    else glCons boss gl) $
  gls

nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel boss recs =
  (
    maxGl ((glCons boss mempty) : map (glCons boss) (map snd recs)),
    maxGl (map (uncurry moreFun) recs)
  )
  where maxGl = foldr moreFun mempty

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold (mempty, mempty) nextLevel

barry, howie :: Employee
barry = Emp "Barry" 10
howie = Emp "Howie" 50

-- Exercise 5

-- Reads company hierarchy from company.txt, and then prints
-- out a formatted guest list, sorted by first name.
main :: IO ()
main =
  readFile "company.txt" >>= (
    putStrLn .
    formatGl .
    maxFun .
    read
  )

formatGl :: GuestList -> String
formatGl (GL emps f) =
  "Total fun: " ++ show f ++ "\n" ++ unlines sortedEmps
    where sortedEmps = sort $ map empName emps
