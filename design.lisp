;; Syntax is important

;; Most things should be s-expressions, but maybe there should be something else
;; too, where things would be more concise.

;; We should keep parentheses to a minimum
;; Perhaps some whitespace-based structure is also OK?

;; Lambda
;; We would like some syntax to define multi argument lambdas

(\x y z. (+ a b))

;; Varargs

;; If varargs functions are more usefull because lisp doesn't have infix, then
;; they only make sense where haskell would have infix operators

;; Many of haskell's infix operators work on Monoids/Semigroups, so repeated use
;; would be equivalent to E.G., (foldMap + [0..n]) -> (+ 0 .. n)

;; WE NEED A SIMPLE BASE LANGUAGE
