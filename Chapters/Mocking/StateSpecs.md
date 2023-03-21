## Object validation with StateSpecs

_Chapter contribution by Denis Kudriashov._

StateSpecs is object state specification framework developed by Denis Kudriashov.
it describes particular object states with first class specifications.

StateSpecs introduces two little DSLs: should expression and word classes.
Should expressions were originally invented by Dave Astels in project SSpec as part of general rethinking 
the Testing Driven Development \(TDD\) methodology in favor of  Behavior Driven Design \(BDD\).
SSpec approach has been ported to many different languages \(NSpec in C#, RSpec in Ruby for example\).

StateSpecs provides fluid DSL to validate objects over these specification.

StateSpecs offers different kind of validations. For the classes `SpecOfCollectionItem`, `SpecOfObjectClass`, and `SpecOfObjectSuperclass` verify different properties of an object.

### How to load StateSpecs

You can load StateSpecs using the following expression.

```
Metacello new
  baseline: 'StateSpecs';
  repository: 'github://dionisiydk/StateSpecs';
  load.
```


### Basic


Specifications can match and validate objects. 
In case an object does not satisfy a specification you will get failure result with detailed descriptiong about the problem.
For example the following snippet use `SpecOfObjectClass` and validate objects.

We create a specification stating that we expect a small integer and we validate whether different objects verify it. 

```
spec := SpecOfObjectClass requiredClass: SmallInteger.
spec validate: 10. 
>>> a SpecOfValidationSuccess

spec validate: 'some string'. 
>>> a SpecOfValidationFailure(Got 'some string' but it should be an instance of SmallInteger)
```


Instead of validation you can simply match objects with a specification using the `matches:` predicate.

```
spec matches: 10. 
>>> true
```


```
spec matches: 'string'. 
>>> false
```


Specifications can be negated using the message `not`.

```
spec not validate: 10.  "==> a SpecOfValidationFailure(Got 10 but it should not be an instance of SmallInteger)"
spec not validate: 'some string'. "==> a SpecOfValidationSuccess"
```


The message `not` creates a new spec instance. 
You can also negate the current one with the message `invert`.


### Two little DSLs


To easily create specifications and validate objects using them, StateSpecs provides two DSLs: `should` expressions and "word" classes.

The first allows you to write "assertions" of the following form:

```
x should be: 2
y should equal: 10
```


Such expressions verify that the receiver follows the constraints they represent.

The "word" classes DSL allows you to instantiate specifications using natural readable words.
They are builders that return expectation objects.

```
Kind of: Number. 
>>> a SpecOfObjectSuperclass(should be a kind of Number)
Instance of: String. 
>>> a SpecOfObjectClass(should be an instance of String)
Equal to: 'test'. 
>>> a SpecOfEquality(should equal 'test')
```


Word classes were introduced to get fluid interface for mock expectations like for example
stating that the results of a given message should be a string. 
But they are very handy shorcuts to access specifications in general. 
The same word can return different specifications in different expressions which allows very fluid instantiation interface:

```
Equal to: 'test'. 
>>> a SpecOfEquality(should equal 'test')

Equal to: 10.0123 within: 0.01. 
>>>  a SpecOfApproxEquality(should be within 0.01 of 10.0123)
```




### Should expressions

Should expressions were created with the goal to replace SUnit assertions \(`self assert: a equals: b)`.
The implementation of should expression creates specific specifications and it verifies that receiver satisfies it. 
When an object is not valid, a should expression signals `SpecOfFailed` error. 
Such validation error can be inspected in the debugger to analyze and understand the reason.

Sending a `not` message to a `should` expression negates the logic of following expression:

```
3 should not equal: 3. 
>>> fail with message: Got '3' but it should not equal '3'
```




To explore complete set of expressions look at the class `SpecOfShouldExpression`. 
It is also place where to extend them. The test class `SpecOfShouldExpressionTests` describes them using tests.

#### Specification of object identity


The message `be:` is used to verify that receiver is identical to a given argument:
The following two expressions will fail, obviously.

```
1 should be: 2.
1 should not be: 1.
```


#### Specification of object equality


The `equal:` message is used to verify that receiver is equivalent to given argument:
The following two expressions will fail, obviously.

```
3 should equal: 2.
3 should not equal: 3.
```





### About equality for specification


In many languages, the equality operation `=` is redefined by many classes according to their domain logic.
This is a problem because such redefinition may not be adapted from the specification point of view. 
Imagine that we want compare two different types of collections:

```
#(1 2 3) asOrderedCollection = #(1 2 3). 
>>> false
```


It returns false which is correct from the point of view of collection library. 
But what should we expect from a specification perspective?
We could express like the following snippet:

```
#(1 2 3) asOrderedCollection should equal: #(1 2 3).
```


But it is not fully adapted because it forces us to always think about collection types when we would like assert their equality.
In fact we are supposed to assert collection _items_ with this expression and not necessarily instances of collections.

In StateSpecs we define the equality specification using a specific message named `checkStateSpecsEqualityTo:` instead of #=.


The idea is that general equality specification should be as much simple equality as possible.
And if you want some extra details you should use a different explicit specification which describes them.
In case of collections you should check for collection class explicitly if it is important for your business case where you use specification:

The following snippet illustrates the point: the messahe `beInstanceOf:` will fail.

```
actual := #(1 2 3) asOrderedCollection.
expected := #(1 2 3).

actual should beInstanceOf: expected class.
actual should equal: expected.
```


Following this logic, StateSpecs does not check order when comparing basic collection classes:

```
#(1 2 3) should equal: #(2 1 3). 
>>> true 
#(1 2 3) asSet should equal: #(2 1 3). 
>>> true 
```


When the order is important,  use the message `equalInOrder:` as illustrated below:

```
#(1 2 3) asOrderedCollection should equalInOrder: #(2 1 3).
>>> Fails #(1 2 3)" but it should equal in order to "#(2 1 3)
```



### Specific case of String and ByteArray


There are collection classes such as `String` or `ByteArray` whose order and type are important. 
For them theses properties are always taken into account for equality comparison. 
The two following expressions fail:

```
'123' should equal: #($1 $2 $3).
>>> Fails
```


It fails with message: Got '123' but it should equal "#\(\$1 \$2 \$3\)".

```
'123' should equal: '132'.
```


It fails with message: Got '123' but it should equal '132'.


### Floats


Floats are another example where specification behaves differently then standard language comparison:

```
0.1 + 0.2 = 0.3 
>>> false"
```


It is correct result because of rounding errors in float arithmetics. 
But it is completelly not suitable to be part of specification. So in StateSpecs following expression will succeed:

```
(0.1 + 0.2) should equal: 0.3  
>>> true 
```


Float implements the method `checkStateSpecsEqualityTo:` by comparing numbers with default accuracy.


In addition, the message `equals:within:` should be used for special specifications for floats when concrete accuracy is important:

```
10.123 should equal: 10.1 within: 0.1  
>>> true 
10.123 should equal: 10.1 within: 0.01 "
>>> Fails 
```


Last expression fails with message: Got 10.123 but it should be within 0.01 of 10.1.

The same logic is used by equality specification of Point class.

### Specification of class relationship

StateSpec offers also a way to express class validation with two different messages: `bekindOf:` and `beInstanceOf:`.

```
3 should beKindOf: String
>>> Fails
```


It will fail with the message: Got 3 but it should be a kind of String.

```
3 should beInstanceOf: String
>>> Fails
```


It fails with the message: Got 3 but it should be an instance of String.

### Collection Specifications


StateSpec proposes specific messages to handle different facets of collection. 

To specify the size of an expected collection,  use the `haveSize:` message:

```
#(1 2) should haveSize: 10
>>> Fails
```


It fails with message: Got #\(1 2\) but it should have 10 elements.


There is a simple expression for checking empty collections.
It uses the predicate syntax explained below in Section *@predicate@*.

```
#(1 2) should be isEmpty
>>> Fails
```


It fails with message: #\(1 2\) should be isEmpty. 


To require concrete item in collection use one of `include:` messages:

```
#(1 2) should include: 10.
>>> Fails
```


#### Index position

It fails with message: Got #\(1 2\) but it should include 10.

```
#(1 2) should include: 10 at: 1
>>> Fails
```


It fails with message: Got 1 at key 1 of #\(1 2\) but should equal 10.


Note that the argument of the `include` messages can be specification itself:

```
#(1 2) should include: (Instance of: String) at: 1.
>> Fails
```

It fails with message: Got 1 at key 1 of #\(1 2\) but should be an instance of String.

```
#(1 2) should include: (Instance of: String)
>>> Fails
```

It fails with message: Got #\(1 2\) but should include \(be an instance of String\).

The following expression will succeed without error:
```
#(1 2) should include: [:each | each > 1]
```




To specify expected key in dictionary, use the `includeKey:` message:

```
{ #key1 -> #value1 } asDictionary should includeKey: #key2
```

It fails with message: Got a Dictionary{#key1->#value1} but it should include key #key2

### String Specifications


To specify the substring of expected string use `includeSubstring:` message:
The following fails with message: Got 'some test string' but it should include 'test2'.

```
'some test string' should includeSubstring: 'test2'
>>> Fails
```



#### Prefix and suffix

To specify prefix of expected string use `beginWith:` message.
The following fails with message: Got 'string for test' but it should begin with 'test':

```
'string for test' should beginWith: 'test'
>>> Fails
```



To specify suffix of expected string use `endWith:` message. 
The following fails with message: Got 'test string' but it should end with 'test':
```
'test string' should endWith: 'test'
>>> Fails
```



To specify regex expression which expected string should satisfy use `matchRegex:` message:

```
'string for test' should matchRegex: '^test'
>>> Fails
```


It fails with message: Got 'string for test' but it should match regex '^test'.


By default all these specifications validate strings ignoring case.
If you want case sensitive specs just add `caseSensitive: true` keyword to all examples:

```
'some test string' should includeSubstring: 'Test' caseSensitive: true
'test string' should beginWith: 'Test' caseSensitive: true
'string for test' should endWith: 'Test' caseSensitive: true
'test string' should matchRegex: '^Test' caseSensitive: true
```


### Raising exception

Specifying the signalling of exceptions is important. 
StateSpecs proposes the `raise:` message to specify expected failure of given block:
The following fails with message: Got no failures but should be an instance of ZeroDivide.

```
[1 + 2] should raise: ZeroDivide.
>>> Fails
```



```
[1/0] should raise: ZeroDivide.
>>> true
```


The following snippet fails with message: Got ZeroDivide but it should be an instance of Error.
```
[1/0] should raise: Error.
```


The following does not fail because `ZeroDivide` is kind of `Error`.

```
[1/0] should raise: (Kind of: Error).
```




In addition, you can use instance of expected exception instead of class.
For example, the following snippet does not fail.

```
| errorInstance |
errorInstance := Error new messageText: 'test error'.
[ error signal ] should raise: errorInstance
```


But the following fails  with message: Got "Error: another error" but it should equal "Error: test error".

```
[self error: 'another error'] should raise: errorInstance
```


### The fail message

StateSpec proposes a simple message `fail` to expect general failure:

```
[ 1 / 0 ] should fail
```


The previous snippet does not fail beause block is really failed as expected.
The following one fails with message: Got `ZeroDivide` but it should not be a kind of `Error`.
```
[ 1 / 0 ] should not fail
>>> Fails
```




```
[ 1 + 2 ] should fail.
```


It fails with message: Got no failures but should be a kind of Error.

### Predicate syntax

@predicate

In many cases the only thing that we want to specify is some boolean state of objects using their own methods.
For this purpose the special `SpecOfBooleanProperty` specification is available. 
It should be created with a given boolean message as follows:

```
| spec |
spec := SpecOfBooleanProperty fromMessage: (Message selector: #isEmpty)
spec validate: #()
>>> true
```


A message can include arguments as follows: 

```
spec := SpecOfBooleanProperty fromMessage: (Message selector: #between:and: arguments: #(1 10))
spec validate: 5
```



#### Explaining object properties


One of the last feature of StateSpecs is ability to explain why a given object does not validate a specification.
Imaging that we want to validate the x coordinate of a rectangle' origin. It could be done like this:

```
(1@3 corner: 20@1) origin x should equal: 100
>>> Fail
```

Such expression fails with message: "Got 1 but it should equal 100". 
However, it is unclear which exact property of the rectangle is wrong. 

StateSpecs introduces the message `where` which should be sent to receiver and all following messages up to `should` will be recorded as object property. 
At the end a should expression will validate the retrieved property instead of receiver itself.

```
(1@3 corner: 20@30) where origin x should equal: 100.
```


It fails with message: Got 1 from \(1@3\) corner: \(20@30\) origin x but it should equal 100.

### Conclusion


StateSpecs is flexible object validation framework. 
StateSpecs is a basic brick of other libraries such as Mocketry.


