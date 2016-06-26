#Haskell Cheat Sheet

##Functions on lists


| ###++```haskell--Concatenates two lists[1,2,3,4] ++ [5,6,7,8]-- [1,2,3,4,5,6,7,8]``` |   |   |
|--------------------------------------------------------------------------------------|---|---|
|                                                                                      |   |   |
|                                                                                      |   |   |
###++

```haskell
--Concatenates two lists
[1,2,3,4] ++ [5,6,7,8]
-- [1,2,3,4,5,6,7,8]
```
###:
```haskell
--Cons operator, adds something to the beginning of a list.
 0 : [1,2,3]
 -- [0,1,2,3,]
 1 : 2 : 3 : [] == [1,2,3]
```

###maximum
```haskell
--Takes a list and returns largest value.
maximum [1,2,3]
-- 3
```

###minimum
```haskell
--Takes a list and returns smallest value.
minimum [1,2,3]
-- 1
```

###init
```haskell
--Takes a list and returns everything except the last element
init [1,2,3]
-- [1,2]
```
###tail
```haskell
--Takes a list and returns 
tail [4,3,2,1]
-- [3,2,1]
```
###head
```haskell
--Takes list and returns first element.
head [1,2,3,4,5]
-- 1
```

###last
```haskell
--Takes a list and returns its first element
last [5,4,3,2,1,0]
-- 0
```

###!!
```haskell
--Access list by index
[1,2,3] !! 0
-- 1
```

### <, <=, >=, >
Compares two list lexicographically

###length
```haskell
-- Takes a list reaturns its length.
length [1,2,3,4]
-- 4
```

###reverse
```haskell
-- Reverses the list given.
reverse [1,2,3,4]
-- [4,3,2,1]
```

###sum
```haskell
--Takes sum of entire list
sum [1,2,3,4,5]
-- 15
```

###product
```haskell
--Takes product of entire list
product [1,2,3,4]
-- 24
```

###elem
```haskell
--Test if given element is a memeber of the list.
4 'elem' [1,4,3]
-- true
0 'elem' [1,4,3]
-- false
```

###null

```haskell
{-Returns true if the list is empty false otherwise-}
null [1,4,3]
-- false
null []
-- true
```

###drop

```haskell
{-Removes elements from the beginning of the list.-}
drop 1 [1,4,3]
-- [4,3]
drop 2 [1,4,3]
-- [3]
```
###take
```haskell
{-Extracts elements from the beginning of the list.-}
take 1 [1,4,3]
-- [1]
take 2 [1,4,3]
-- [1,4]
```