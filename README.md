# Final Exam 

## Deadline: 22.05.2024, 17:00 (CEST)

The first 4 tasks are pen-and-paper, the last 4 tasks are programming exercises. There are 100 points in total; you can get partial points if you did only part of the task correctly. The exam counts for 30% of your final grade. 

Some tasks have versions, they are numbered with a roman numeral. See what version is yours in the [HW table, column E](https://docs.google.com/spreadsheets/d/1CwcKMJj9YnkSST4f_wEzgfIknCJWyZB5QcZSVXecNis/edit?usp=sharing)

Submit the solutions by opennning a PR into this branch. 

You can use a pen and paper or a stilus and tablet to write your answers, but make sure your answers are legible. 

Code tasks should be submitted in form of a Haskell project. Each code assignment should have tests that show that your implementation works. If your code does not compile, you get 0 points, so make sure to check it before submitting. 

Good luck!

## Tasks 

1. (10 points) Provide an inhabitant of each of the following polymorphic types: 

    1. `[a -> b] -> a -> [b]` 
       
    2. `(a -> b) -> Tree a -> [b]`, provided `data Tree a = Leaf | Node (Tree a) a (Tree a)` 
       
    3. `Ord a => [a] -> [a]`
       
    4. `(a -> b -> c) -> (a -> b) -> a -> c`
       
    5. `((a -> b) -> c -> d) -> (a -> c -> b) -> c -> d`

2. (10 points) Provide 3 different examples of lambda terms that cannot be normalized. Explain, why they cannot be normalized. 

3. (15 points) Provide a data type that is isomorphic to the given one. Explain why they are isomorphic.
 
    1. `Int` 
       
    2. `data Tree a = Leaf | Node (Tree a) a (Tree a)`
       
    3. `data List a = Nil | Cons a (List a)` 
       
    4. `Either a (Either b (c, d))` 
       
    5. `(a -> b, a -> c)`
    

4. (15 points) Infer the type of the following lambda-term in STLC. Provide a proof tree. 

    1. `\x f g. f (g x)` 
       
    2. `\x y f. f x y (x y)`
       
    3. `S K K`
       
    4. `K S I`
       
    5. `S K' I`

    * Here are the definitions of the combinators
       - `S f g x = f x (g x)`
       - `K x y = y`
       - `K' x y = x`
       - `I x = x`
  
5. (15 points) Implement integer numbers base `n` where `n` is a parameter. 

    * Design a data structure suitable to represent such numbers. 

    * Provide instances of `Show`, `Num`, and `Ord`. 


6. (10 points) Implement a function for list rotation `rotate :: Int -> [a] -> [a]`. If the first argument is positive, the list is rotated to the left; otherwise--to the right. 

    * `rotate 2 "abcdef" == "cdefab"` 

    * `rotate (-2) "abcdef" == "efabcd"`

    * What does your implementation do when the input list is infinite? 

7. (20 points) Make the binary tree data structure `data Tree a = Leaf | Node (Tree a) a (Tree a)` an instance of `Foldable`.  

    * Implement three traversals: in-order, pre-order and post-order. See [wiki](https://en.wikipedia.org/wiki/Tree_traversal) if you are not familiar with the terms. 

8. (5 points) Implement the following functions in the continuation-passing style.
    
    1. `lengthCPS` that computes the length of list. 
       
    2. `mapCPS` that functions as the `map` on lists. 
       
    3. `filterCPS` that functions as the `filter` on lists. 
       
    4. `sizeCPS` that works over the binary tree `data Tree a = Leaf | Node (Tree a) a (Tree a)` and computes the number of nodes. 
       
    5. `heightCPS` that works over the binary tree `data Tree a = Leaf | Node (Tree a) a (Tree a)` and computes its height. 