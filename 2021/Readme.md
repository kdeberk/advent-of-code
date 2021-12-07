# Advent of Code 2021

This year I will try to solve the challenges using Common Lisp.

## Day 1: Sonar Sweep
### Part 1: Consecutive pairs
Goal: Given a list of numbers, count how many times a number is bigger than its predecessor.

One of the many features that `loop` provides, is a sliding window over a sequence, with the form `loop for (a b) on l`.  For example, to print all consecutive pairs in the list `(1 2 3 4)`:

```lisp
> (loop for (a b) on (list 1 2 3 4)
         do (print (list a b)))

(1 2)
(2 3)
(3 4)
(4 NIL)
```

We will count the number of times that the second number in the pair is larger than the first number, but we must ignore the final pair because not only does it not represent an actual pair in the list, but also because `>` will raise an error when one it's argument is not a number.

```lisp
> (loop for (a b) on (list 199 200 208 210 200 207 240 269 260 263)
        count (and b (< a b)))
7
```

# Part 2: 3-Measurement windows
Goal: Given a list of numbers, count how many times the sum of a 3-window pair is bigger than the sum of the preceding window.

We can solve this in nearly the same way as we solved the first part: we convert each window to a number and we will again count how often there is an increase in consecutive numbers.  The new part is to generate a number (the sum) of each window.  For this we can use the same `loop for on` feature and extend the window by one element:

```lisp
> (loop for (a b c) on (list 1 2 3 4)
        do (print (list a b c)))
(1 2 3)
(2 3 4)
(3 4 NIL)
(4 NIL NIL)
```
Also:

- we use `if` to only consider the windows without a `NIL` value
- we `collect` the sum of the values, instead of counting

```lisp
> (loop for (a b c) on (list 199 200 208 210 200 207 240 269 260 263)
        if (and b c)
        collect (+ a b c))
(607 618 618 617 647 716 769 792)
```

Feeding this sequence into the part1 solver gives us the answer for part 2.


## Day 2:
## Part 1
