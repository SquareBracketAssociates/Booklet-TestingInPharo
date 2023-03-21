## Miscellaneous


In this little chapter we present some little points around tests.


### Executable comments


An important problem that developers are facing is how to document their code. 
Often they would like to provide a little example to help their users.
Unit Tests are already a good foundation for this.

Since Pharo 7.0 you can add to your method comment, executable expressions. This way a user can execute an expression and see its results. Since Pharo 8.0 and the introduction of DrTests executable comments are crawled by the DrTests plugin and report the executable comments that are not working. 

This way you gain on multiple levels:
- you get more user friendly and self explainable comments,
- you get the insurance that your comments are correct,
- you get tests that your code is doing what you specified.


### Executable comment example 

To define an executable comment you should use the message `>>>` to separate an expression from its results. 
Here is an example ` 'a' asByteArray >>> #[97]`


```
String >> asByteArray
	"Convert to a ByteArray with the ascii values of the string."
	"'a' asByteArray >>> #[97]"
	"'A' asByteArray >>> #[65]"
	"'ABA' asByteArray >>> #[65 66 65]"

	| b |
	b := ByteArray new: self byteSize.
	1 to: self size * 4 do: [:i |
		b at: i put: (self byteAt: i)].
	^ b
```


Note that the message `>>>` is a real message, a binary message, so you have to pay attention when documenting other binary messages about the execution order. You can simply surround the first expressions with parantheses.
Here we show `contractTo:` and there is no need to put extra parenthesis as in the example.
```
String >> contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	"('abcd' contractTo: 10) >>> 'abcd'"
	"('Pharo is really super cool' contractTo: 10) >>> 'Phar...ool'"
	"('A clear but rather long-winded summary' contractTo: 18) >>> 'A clear ...summary'"
	
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize := smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
```


##### DrTests plugin

Once you defined an executable comment, you can check that it is valid by comparing the results with the expected one. You can also use the DrTests plugin for executable comments.


### Supporting examples with assertions


Some people wants to be able to have test methods \(with assertions\) that can call each others and without been forced
that the method starts with the 'test' prefix.
SUnit can be simply extended to support such example testing methods. 

This is as simple as overriding the method `testSelectors` in a subclass of `TestCase`.

```
TestCase << #ExampleTest
	package: 'SUnit-Extensions'
```


Here we decided to only consider test methods when they are tagged with the pragma `#example`.
In addition we check that the method should not expect an argument.

```
ExampleTest class >> testSelectors

	^ self methods
		select: [ :each | (each hasPragmaNamed: #example) and: [ each selector isUnary ] ]
		thenCollect: [ :each | each selector ]
```


Here is an example taken from Bloc

```
simulateMouseMoveOutside
	<gtExample>
	| element mouseLeave mouseMove mouseEnter |

	element := self blueElement.
	element size: 100@100.
	element relocate: 100@100.

	mouseLeave := mouseMove := mouseEnter := 0.

	element addEventHandlerOn: BlMouseMoveEvent do: [ mouseMove := mouseMove + 1 ].
	element addEventHandlerOn: BlMouseEnterEvent do: [ mouseEnter := mouseEnter + 1 ].
	element addEventHandlerOn: BlMouseLeaveEvent do: [ mouseLeave := mouseLeave + 1 ].
	
	BlSpace simulateMouseMoveOutside: element.

	self assert: mouseMove equals: 0.
	self assert: mouseEnter equals: 0.
	self assert: mouseLeave equals: 0.

	^ element
```


