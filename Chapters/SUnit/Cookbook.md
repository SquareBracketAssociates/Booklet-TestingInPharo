## The SUnit cookbook

@cha:cookbook

This chapter will give you more details on how to use SUnit. If you have used
another testing framework such as JUnit, much of this will be familiar, since
all these frameworks have their roots in SUnit. Normally you will use SUnit's
GUI to run tests, but there are situations where you may not want to use it.
But let us start with a powerful feature of SUnit: parametrized tests.


### Parameterized tests


Since Pharo 7.0 you can express parameterized tests. 
Parametrized tests are tests that can be executed on multiple configurations: your tests will run
on different contexts that you can specify basically as test arguments. 
Parametrized tests are really powerful when you want to check whether two implementations pass the same set of tests. 

To declare a parameterized test you have to:
- define your test case class as a subclass of `ParametrizedTestCase` instead of `TestCase`. This class should define accessors that will be used to configure the tests.
- define a _class_ method named `testParameters` which specifies the actual parameters.


#### A simple example first

Here is an example taken from the Enlumineur project which is a pretty printer for Pharo code.
Using paramterized tests let us know whether two different pretty printers produce the same outputs.

We define the class `BIEnlumineurTest`. 
It has different parameters expressed as instance variables such as `formatterClass` and `contextClass`.

```
ParametrizedTestCase <<: #BIEnlumineurTest
	slots: { #configurationSelector .  #formatterClass . #contextClass};
	package: 'Enlumineur-Tests'
```


This class should define accessors for its parameters, here for `formatterClass` and `contextClass`.
The tests should use the tests instance variables and should not refer directly to the classes held by the instance variables. 
Else this would shortcut the idea of a parametrized test itself.

Then we define the class method `testParameters` as follows. 

```
BIEnlumineurTest class >> testParameters
	^ ParametrizedTestMatrix new
		addCase: { (#formatterClass -> BIEnlumineurPrettyPrinter) . (#contextClass -> BIEnlumineurContext) };
		yourself
```


Now the framework will run the test using the parameters we mentioned. 
To add a new variation we just have to add a case using the `addCase:` message. 

#### Controlling configuration


The following example generates 2 cases. 
Exactly the 2 cases listed in `testParameters` method.
The values for `number1` and `number2` will be set and the test will be executed.

```
PaSelectedCasesExampleTest class >> testParameters

	^ ParametrizedTestMatrix new
		addCase: { #number1 -> 2. #number2 -> 1.0. #result -> 3 };
		addCase: { #number1 -> (2/3). #number2 -> (1/3). #result -> 1 };
		yourself
```

	
### Matrix: a more advanced case


Sometimes you do not want to enumerate all the combinations by hand.
In that case you can use a matrix and specify all the possible values of a parameter.
The class `PaSimpleMatrixExampleTest` contains some examples.

The following test executes 27 different cases. 
All the combinations in the matrix are executed, i.e. item1 values will be enumerated, and for each ones, all the 
values of the other parameters will be also enumerated. 
This way all possible combinations are generated and tests run for each of them.

```
PaSimpleMatrixExampleTest class >> testParameters

	^ ParametrizedTestMatrix new
		forSelector: #item1 addOptions: { 1. 'a'. $c };
		forSelector: #item2 addOptions: { 2. 'b'. $d };
		forSelector: #collectionClass addOptions: { Set. Bag. OrderedCollection }
```


The test matrix generates using a cartesian product the configurations or a set of well known cases.
Each option is constituted from a set of possible values and a selector that is the name of the parameter to set in the test case instance.
Another example of `testParameters` is: 

```
testParameters

	^ ParametrizedTestMatrix new
		forSelector: #option1 addOptions: #(a b c);
		forSelector: #option2 addOptions: {[1].[2].[3]};
		yourself.
```


This example will generate 9 different configuration. One per each combination of `option1` and `option2`.  
Do not forget that the test case should have a setter for each option.

In addition each option can be a literal or a block to generate that value. The block has an optional parameter, the parameter is the test case to configure.



### Classes vs. objects as parameters


There is a subtle but important point about the kind of the parameters. Indeed, we may wonder whether it is better to pass a class or an instance as parameter of a test. Theoritically there is not much difference between passing a class or an object.  
However in practice there is a difference because when we pass an object, as in the following configuration, the framework does not recreate the object
during each test execution. Therefore if your object accumulates information, then such information will be shared amongs your tests and this is a bad idea. 

```
CbkDlittleImporterTest class >> testParameters

	^ ParametrizedTestMatrix new
			addCase: { #importer -> CBkCollectorDLittleImporter new };
		yourself.
```


The solution is to favor passing classes as follows and to explicitly create objects in the `setUp`. 
This way you are sure that your object does not hold state from previous execution.

```
CbkDlittleImporterTest class >> testParameters

	^ ParametrizedTestMatrix new
			addCase: { #importerClass -> CBkCollectorDLittleImporter };
		yourself.

CbkDlittleImporterTest >> setUp 
	super setUp.
	importer := importerClass new.
	
CbkDlittleImporterTest >> importerClass: anImporterClass
	importerClass := anImporterClass
```


In conclusion, we suggest to pass instances as parameters when the objects are not complex
and to favor classes otherwise.

### Other assertions


In addition to `assert:` and `deny:`, there are several other methods that
can be used to make assertions.

First, `TestAsserter >> assert:description:` and `TestAsserter >>
deny:description:` take a second argument which is a message string that
describes the reason for the failure, if it is not obvious from the test itself.
These methods are described in Section *@sec:descriptionStrings@*.

Next, SUnit provides two additional methods, `TestAsserter >> should:raise:`
and `TestAsserter >> shouldnt:raise:` for testing exception propagation.

For example, you would use `self should: aBlock raise: anException` to test
that a particular exception is raised during the execution of `aBlock`. The
method below illustrates the use of `should:raise:`.

```caption=Testing error raising
	MyExampleSetTest >> testIllegal
		self should: [ empty at: 5 ] raise: Error.
		self should: [ empty at: 5 put: #zork ] raise: Error
```


Try running this test. Note that the first argument of the `should:` and
`shouldnt:` methods is a block that contains the expression to be executed.

Note that this is usually not really good to catch exception using the `Error` class, since
it is catching basically everything. In that current case, the `at:` primitive signals an instance of `Error` so 
we have deal with it. 

#### Using `assert:equals:`


In addition to `assert:`, there is also `assert:equals:` that offers a
better report in case of error \(incidentally, `assert:equals:` uses `assert:description:`\).

For example, the two following tests are equivalent. However, the second one
will report the value that the test is expecting: this makes easier to
understand the failure. In this example, we suppose that `aDateAndTime` is an
instance variable of the test class.

```
testAsDate
	self assert: aDateAndTime asDate = ('February 29, 2004' asDate translateTo: 2 hours).

testAsDate
	self
		assert: aDateAndTime asDate
		equals: ('February 29, 2004' asDate translateTo: 2 hours).
```



#### Assertion description strings

@sec:descriptionStrings

The `TestAsserter` assertion protocol includes a number of methods that allow
the programmer to supply a description of the assertion. The description is a
`String`; if the test case fails, this string will be displayed by the test
runner. Of course, this string can be constructed dynamically.

```
...
e := 42.
self assert: e = 23 description: 'expected 23, got ', e printString
...
```


The relevant methods in `TestAsserter` are:

```
assert:description:
deny:description:
should:description:
shouldnt:description:
```







### Running tests


#### Running a single test


Normally, you will run your tests using the Test Runner or using your code
browser. You can also run a single test as follows:

```testcase=true
MyExampleSetTest run: #testRemove
>>> 1 run, 1 passed, 0 failed, 0 errors
```


#### Running all the tests in a test class


Any subclass of `TestCase` responds to the message `suite`, which builds a test suite that contains all the methods in the class whose names start with the string _test_.

To run the tests in the suite, send it the message `run`. For example:

```testcase=true
MyExampleSetTest suite run
>>> 5 run, 5 passed, 0 failed, 0 errors
```






### Advanced features of SUnit


In addition to `TestResource` that we present just in subsequent section, SUnit contains assertion description strings,
logging support, the ability to skip tests, and resumable test failures.


#### Logging support


The description strings mentioned above may also be logged to a `Stream`, such
as the `Transcript` or a file stream. You can choose whether to log by
overriding `isLogging` in your test class; you can also choose where to log
by overriding `failureLog` to answer an appropriate stream. By default, the
`Transcript` is used to log.

To enable logging, you should redefine the method `isLogging` to say so.

```
MyExampleSetTest class >> isLogging
	^ true
```




#### Skipping tests


Sometimes in the middle of a development, you may want to skip a test instead of
removing it or renaming it to prevent it from running. 
You can simply invoke the `TestAsserter` message `skip` on your test case instance. 
For example, the following test uses it to define a conditional test.

```
OCCompiledMethodIntegrityTest >> testPragmas

	| newCompiledMethod originalCompiledMethod |
	(self class environment hasClassNamed: #Compiler) ifFalse: [ ^ self skip ].
	...
```


It is better to use `skip` than to use a simple `^ self` because in the latter case
you may think that you test is executed while it is not!

#### Continuing after a failure


SUnit also allows us to specify whether or not a test should continue after a failure. 
This is a really powerful feature that uses Pharo's exception mechanisms. 
To see what this can be used for, let's look at an example. 

Consider the following test expression:

```
aCollection do: [ :each | self assert: each even ]
```


In this case, as soon as the test finds the first element of the collection that isn't `even`, the test stops. 
However, we would usually like to continue, and see both how many elements, and which elements, aren't `even` \(and maybe also
log this information\). 

You can do this as follows:

```
aCollection do: [ :each |
	self
		assert: each even
		description: each printString, ' is not even'
		resumable: true ]
```


This will print out a message on your logging stream for each element that fails. 
It doesn't accumulate failures, i.e, if the assertion fails 10 times in your test method, you'll still only see one failure. 
All the other assertion methods that we have seen are not resumable by default;
`assert: p description: s` is equivalent to
`assert: p description: s resumable: false`.

### Test resources

One of the important features of a suite of tests is that they should be
independent of each other.
The failure of one test should not cause an avalanche of failures of other tests that depend upon it, nor should the order in which
the tests are run matter.
Performing `setUp` before each test and `tearDown` afterwards helps to reinforce this independence.

However, there are occasions where setting up the necessary context is just too time-consuming for it to be done before the execution of each test. 
Moreover, if it is known that the test cases do not disrupt the resources used by the tests, then it is wasteful to set them up afresh for each test. 
It is sufficient to set them up once for each suite of tests. 
Suppose, for example, that a suite of tests needs to query a database, or do analysis on some compiled code. 
In such cases, it may make sense to set up the database and open a connection to it, or to compile some source code, before any of the tests
start to run.


![`setUp` and `teardDown` in action.](figures/setUpAndTearDownWithResources.pdf width=80&label=fig:setUpAndTearDownWithR)


Where should we cache these resources, so that they can be shared by a suite of tests? The instance variables of a particular `TestCase` subclass won't
do, because a TestCase instance persists only for the duration of a single test \(as mentioned before, the instance is created anew _for each test method_\). 
A global variable would work, but using too many global variables pollutes the name space, and the binding between the global and the tests that depend on it will not be explicit. 

A better solution is to define a TestResource and use it. 
The class `TestResource` implements a singleton to manage the execution of setUp and tearDown around a complete test suite as shown in Figure *@fig:setUpAndTearDownWithR@*.
Each subclass of `TestResource` understands the message `current`, which will answer a singleton instance of that subclass. 
Methods `setUp` and `tearDown` should be overridden in the subclass to ensure that the resource is initialized and finalized.

One thing remains: somehow, SUnit has to be told which resources are associated with which test suite. A resource is associated with a particular subclass of
`TestCase` by overriding the _class_ method `resources`.

By default, the resources of a `TestSuite` are the union of the resources of the `TestCase`s that it contains.

Here is an example. We define a subclass of `TestResource` called `MyTestResource`.
Then we associate it with `MyTestCase` by overriding the class method `MyTestCase class >> resources` to return an array of the test resource classes that `MyTestCase` will use.

```caption=An example of a TestResource subclass
TestResource << #MyTestResource
	package: 'MyPackage'

MyTestResource >> setUp
	...

MyTestCase class >> resources
	"Associate the resource with this class of test cases"

	^ { MyTestResource }
```


You can also defines the instance side method `isAvailable` to indicate whether the resource is available.
But if you need this, better read the code of the `TestResource` class. 


#### Checking the described behavior


The following trace \(written to the `Transcript`\) illustrates that a global
set up is run before and after each test in a sequence. Let's see if you can
obtain this trace yourself.

```
MyTestResource >> setUp has run.
MyTestCase >> setUp has run.
MyTestCase >> testOne has run.
MyTestCase >> tearDown has run.
MyTestCase >> setUp has run.
MyTestCase >> testTwo has run.
MyTestCase >> tearDown has run.
MyTestResource >> tearDown has run.
```


Create new classes `MyTestResource` and `MyTestCase` which are subclasses of
`TestResource` and `TestCase` respectively. Add the appropriate methods so
that the following messages are written to the `Transcript` when you run your
tests.

#### Solution

You will need to write the following six methods.

```
MyTestCase >> setUp
	Transcript show: 'MyTestCase>>setUp has run.'; cr

MyTestCase >> tearDown
	Transcript show: 'MyTestCase>>tearDown has run.'; cr

MyTestCase >> testOne
	Transcript show: 'MyTestCase>>testOne has run.'; cr

MyTestCase >> testTwo
	Transcript show: 'MyTestCase>>testTwo has run.'; cr

MyTestCase class >> resources
	^ Array with: MyTestResource

MyTestResource >> setUp
	Transcript show: 'MyTestResource>>setUp has run'; cr

MyTestResource >> tearDown
	Transcript show: 'MyTestResource>>tearDown has run.'; cr
```




### Customising tests


In this section, we show that SUnit offers two hooks to define what a test selector is and how to perform the test.
Imagine that we want to support a class method on a given class returned by `classWithExamplesToTest` whose selector follow the pattern `example*` methods are considered as tests.
We can define method `testSelectors` as follows:

```
HiExamplesTest class >> testSelectors [
	^ self classWithExamplesToTest class methods 
		select: [ :each | (each selector beginsWith: 'example') and: [ each numArgs = 0 ] ]
		thenCollect: [ :each | each selector ]
```

Then we can redefine the method `performTest` to example the method on the class itself. 

```
HiExamplesTest >> performTest
 	example := self class classWithExamplesToTest perform: testSelector asSymbol
```



### Inheriting TestCase


A new `TestCase` can inherit tests from a superclass.
The logic is a bit cumbersome. 
By default a new test case class inherits from a subclass of `TestCase` that is abstract. 
If your new subclass as no test methods it will inherit from its superclass.

Otherwise if your new class has selectors and inherits from a concrete superclass, you should redefine `shouldInheritSelectors`
to return `true`.

What developers use in practice is the last part: to redefine the method `shouldInheritSelectors`.
For example, this is what the `CoCompletionEngineTest` class is doing to inherit the tests of `CompletionEngineTest`

```
CoCompletionEngineTest >> shouldInheritSelectors

	^ true
```


 Here is the definition of the method `shouldInheritSelectors`. 
```
TestCase class >> shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, 
	unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  
	If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."
	
	^self ~~ self lookupHierarchyRoot
		and: [self superclass isAbstract or: [self testSelectors isEmpty]]
```




### Conclusion


SUnit is a simple framework but it already provides a powerful set of mechanisms to take real advantage of writing tests.
In particular parametrized tests are a powerful method when you have several objects that expose the same API, then you can reuse your 
tests.
