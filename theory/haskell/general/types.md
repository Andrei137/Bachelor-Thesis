### Value constructors
- Example
```
data Bool = False | True
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```
- The parts after the equal sign are value constructors
- They are nullary if they don't have any fields (Bool is nullary, Shape isn't)
### Type constructors and parameters
- Example
```
data Maybe a = Nothing | Just a
```
- <kbd>Maybe</kbd> is a type constructor
- <kbd>a</kbd> is a type paremter and must be a concrete type
- A new concrete type is produced when the type constructor is given all type paremeters needed
### Records
- Example
```
data Person = Person { firstName :: String, lastName :: String } deriving (Show)
```
- Every field is also a function that returns the value from that field
### Type classes
- Eq -> can define ==  and /=
- Ord -> can be ordered, smallest = leftmost
- Enum -> value constructors and nullary
- Bounded -> can be bounded, for Enum, minBound is smallest, maxBound is largest
- Example
```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```
- To make an instance of a type class, the type a from class Eq a needs to be a concrete type
- Thus, <kbd>instance Eq Maybe where</kbd> would not work
- <kbd>instance Eq (Maybe m) where</kbd> could work, but it is not enough, because it is not known whether the type m can be used with Eq
- So <kbd>instance (Eq m) => Eq (Maybe m) where</kbd> is the desired syntax
### Constrains
- Class constrains are used in class declarations to make a type class a subclass of another type class, and in instance declarations to express requirements
- Example of a class constrain in class declarations
```
class (Eq a) => Num a where
```
- Example of a class constrain in instance declarations
```
instance (Eq m) => Eq (Maybe m) where
```
### The type keyword
- Example
```
type IntList = [Int]
```
- It is used to give an existing type another name, easier to refer to
### The newtype keyword
- Example
```
newtype CharList = CharList { getCharList :: [Char] }
```
- It is used to wrap an existing type, to separate it from the original one
### The data keyword
- Example
```
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
```
- It is used to define new types
