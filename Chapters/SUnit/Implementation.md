## SUnit implementationThe implementation of SUnit makes an interesting case study of a Pharoframework. Let's look at some key aspects of the implementation by following theexecution of a test.### Running one testTo execute one test, we execute the expression `(aTestClass selector: aSymbol)
run.`![Running one test.](figures/sunit-scenario.png width=60&label=fig:sunit-scenario)The method `TestCase >> run` creates an instance of `TestResult` that willaccumulate the results of the test, then it sends itself the message`TestCase >> run:` \(See Figure *@fig:sunit-scenario@*\).```TestCase >> run
	| result |
	result := self classForTestResult new.
	[ self run: result ]
		ensure: [ self classForTestResource resetResources: self resources ].
	^ result```The method `TestCase >> run:` sends the message `runCase:` to the test result:```caption=Passing the test case to the test resultTestCase >> run: aResult
	aResult runCase: self```The method `TestResult >> runCase:` sends the message `TestCase >> runCase`to an individual test, to execute the test. `TestResult >> runCase` deals withany exceptions that may be raised during the execution of a test, runs a`TestCase` by sending it the `runCase`, and counts the errors, failures, andpasses.```caption=Catching test case errors and failuresTestResult >> runCase: aTestCase
	[
	aTestCase announce: TestCaseStarted withResult: self.
	aTestCase runCase.
	aTestCase announce: TestCaseEnded withResult: self.
	self addPass: aTestCase ]
		on: self class failure, self class skip, self class warning, self class error
		do: [ :ex | ex sunitAnnounce: aTestCase toResult: self ]```The method `TestCase >> runCase` sends the messages `TestCase >> setUp` and`TestCase >> tearDown` as shown below.```TestCase >> runCase
		self resources do: [ :each | each availableFor: self ].
		[ self setUp.
		self performTest ] ensure: [
			self tearDown.
			self cleanUpInstanceVariables ]```### Running a TestSuiteTo run more than one test, we send the message `run` to a `TestSuite` thatcontains the relevant tests. `TestCase class` provides some functionality tobuild a test suite from its methods. The expression `MyTestCase
buildSuiteFromSelectors` returns a suite containing all the tests defined inthe `MyTestCase` class. The core of this process is:```caption=Auto-building the test suiteTestCase class >> testSelectors
	^ (self selectors select: [ :each | (each beginsWith: 'test') and: [ each numArgs isZero ]])```The method `TestSuite >> run` creates an instance of `TestResult`, verifiesthat all the resources are available, and then sends itself the message`TestSuite >> run:`, which runs all the tests in the suite. All the resourcesare then released.```TestSuite >> run: aResult
	self setUp.
	[ self tests
		do: [ :each |
			each run: aResult.
			self announceTest: each.
			self changed: each ] ]
		ensure: [ self tearDown ]

TestSuite >> setUp
	self resources do: [ :each |
				each isAvailable ifFalse: [ each signalInitializationError ]
		].

TestSuite >> tearDown
	self resourceClass resetResources: self resources```The class `TestResource` and its subclasses keep track of their currentlycreated singleton instances that can be accessed and created using the classmethod `TestResource class >> current`. This instance is cleared when thetests have finished running and the resources are reset.The resource availability check makes it possible for the resource to bere-created if needed, as shown in the class method `TestResource
class >> isAvailable`. During the `TestResource` instance creation, it isinitialized and the message `setUp` is sent to a test resource.```caption=Test resource availabilityTestResource class >> isAvailable
	"This is (and must be) a lazy method. If my current has a value, an attempt to make me available has already been made: trust its result. If not, try to make me available."

	current ifNil: [ self makeAvailable ].
	^ self isAlreadyAvailable``````caption=Test resource creationTestResource class >> current
	"This is a lazy accessor: the assert of self isAvailable does no work unless current isNil. However this method should normally be sent only to a resource that should already have been made available, e.g. in a test whose test case class has the resource class in its #resources, so should never be able to fail the assert.
	If the intent is indeed to access a possibly-unprepared or reset-in-earlier-test resource lazily, then preface the call of 'MyResource current' with 'MyResource availableFor: self'."

	self
		assert: self isAvailable
		description:
			'Sent #current to unavailable resource ', self name,
				'. Add it to test case'' class-side #resources (recommended) or send #availableFor: beforehand'.
	^ current```- To maximize their potential, unit tests should be fast, repeatable, independent of any direct human interaction and cover a single unit of functionality.- Tests for a class called `MyClass` belong in a class named `MyClassTest`, which should be introduced as a subclass of `TestCase`.- Initialize your test data in a `setUp` method.- Each test method should start with the word _test_.- Use the `TestCase` methods `assert:`, `deny:` and others to make assertions.- Run tests!