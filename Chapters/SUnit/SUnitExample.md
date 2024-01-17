## SUnit by example

@cha:sunitexample
In this chapter we present a small example showing how simple it is to use SUnit. 
Before going into the details of SUnit \(see next Chapter\), we will show a step-by-step example. 
We use an example that tests the class `Set`. Try entering the code as we go along.
We will create a test i.e., create a context \(also called a fixture\), execute a stimulus, and verify that some assertions 
are working.

If you already read the SUnit chapter on Pharo by Example book you can skip this chapter since 
the contents are the same.

### Step 1: Create the test class


First, you should create a new subclass of `TestCase` called
`MyExampleSetTest` that looks like this:

```label=scr:exSet&caption=An Example Set Test class&language=Smalltalk
TestCase << #MyExampleSetTest
	package: 'MySetTest'
```


The class `MyExampleSetTest` groups all the tests related to the class `Set`. 
As we will show later, we will use it to define the context in which the tests will run. 

The name of the class is not critical, but by convention, it should end in
`Test`. If you define a class called `Pattern` and call the corresponding
test class `PatternTest`, the two classes will be alphabetized together in the
browser \(assuming that they are in the same package\). It _is critical_ that
your class is a subclass of `TestCase`.


#### Must I subclass TestCase?

In JUnit you can build a TestSuite from an arbitrary class containing `test*` methods. In SUnit you can do the same but you will then have to create a suite
by hand and your class will have to implement all the essential `TestCase` methods like `assert:`. We recommend, however, that you not try to do this.
The framework is there: use it.


### Step 2: A  first test


We start by defining a method named `testIncludes`.
Pay attention to the `'test'` part is important.
Each method represents one test. 
The names of the methods should start with the string `'test'` so that SUnit will collect them into test suites.
Test methods take no arguments.

This method creates two sets one empty and one full.
This is the context or fixture of the test.
Second we perform some action on the test: here we execute the method `includes:`,
and third we validate the output using assertion via the ` assert:` message. 
The method `assert:` checks that the argument is a boolean true. 


Define the following test methods. The first test, named `testIncludes`, tests
the `includes:` method of `Set`. For example, the test says that sending the message
&nbsp;`includes: 5` to a set containing 5 should return `true`.

```caption=Testing includes
MyExampleSetTest >> testIncludes
	| full empty |
	full := Set with: 5 with: 6.
	empty := Set new.
	self assert: (full includes: 5).
	self assert: (full includes: 6).
	self assert: (empty includes: 5) not
```



### Step 3: Run the tests


The easiest way to run the tests is directly from the browser. Press on the icon on the side of the class name, or on an individual test method.
The test methods will be flagged depending on whether they pass or not \(as shown in *@fig:browsertests2@*\).

![Running SUnit tests from the System Browser.](figures/updatedbrowsertests.png width=80&label=fig:browsertests2)



### Step 4: Another test

Following the same pattern, here is a test verifying that the method `occurrencesOf:` works as expected.
The second test, named `testOccurrences`, verifies that the number of occurrences of 5 in `full` set is equal to one, even if we
add another element 5 to the set. 


```caption=Testing occurrences
MyExampleSetTest >> testOccurrences
	| full empty |
	full := Set with: 5 with: 6.
	empty := Set new.
	self assert: (empty occurrencesOf: 0) equals: 0.
	self assert: (full occurrencesOf: 5) equals: 1.
	full add: 5.
	self assert: (full occurrencesOf: 5) equals: 1
```


Note that we use the message `assert:equals:` and not `assert:` as in the first test.
We could have used `assert:`.
But the message `assert:equals:` is better since it reports the error in a much better way. 
When an assertion is failing, `assert:equals:` shows the expected value and the value received.
While `assert:` just mentions that something is not true.

Now make sure that your test is passing too.

### Step 5: Factoring out context


As you see in the two previous steps, we started to repeat the same context. 
This is not really nice, so we will factor the fixture out of the tests by defining instance variables in the class and 
a method `setUp` to initialize them.

A `TestCase` class defines the context in which the tests will run. 
We will add the two instance variables `full` and `empty` that
we will use to represent a full and an empty set.

```label=scr:exSet2&caption=An Example Set Test class&language=Smalltalk
TestCase << #MyExampleSetTest
	slots: { #full . #empty};
	package: 'MySetTest'
```


The next step is to initialize such instance variables.

### Step 6: Initialize the test context

The message `TestCase >> setUp` defines the context in which the tests will
run, a bit like an initialize method. `setUp` is invoked before the execution
of each test method defined in the test class.

Define the `setUp` method as follows, to initialize the `empty` variable to
refer to an empty set and the `full` variable to refer to a set containing two
elements.

```caption=Setting up a fixture
MyExampleSetTest >> setUp
	super setUp.
	empty := Set new.
	full := Set with: 5 with: 6
```


In testing jargon, the context is called the _fixture_ for the test and the `setUp` method is responsible to initialize such fixture.

#### Updating existing tests

We change the two test methods to take advantage of the shared initialization. 
We remove the fixture code and obtain the following methods:

```caption=Testing includes
MyExampleSetTest >> testIncludes
	
	self assert: (full includes: 5).
	self assert: (full includes: 6).
	self assert: (empty includes: 5) not
```



```caption=Testing occurrences
MyExampleSetTest >> testOccurrences
	
	self assert: (empty occurrencesOf: 0) equals: 0.
	self assert: (full occurrencesOf: 5) equals: 1.
	full add: 5.
	self assert: (full occurrencesOf: 5) equals: 1
```



Clearly, these tests rely on the fact that the `setUp` method has already run.


#### Taking advantage of setUp 


We test that the set no longer contains the element 5 after we have removed it.

```caption=Testing removal
MyExampleSetTest >> testRemove
	full remove: 5.
	self assert: (full includes: 6).
	self deny: (full includes: 5)
```


Note the use of the method `TestCase >> deny:` to assert something that should not be true. 
&nbsp;`aTest deny: anExpression` is equivalent to
&nbsp;`aTest assert: anExpression not`, but is much more readable and expresses the intent more clearly.

% !!!Step 6: Other ways to run the tests
% +Should use DrTests.>file://figures/updatedbrowsertests.png|width=80|label=fig:browsertests+

% You can also select sets of test suites to run, and obtain a more detailed log
% of the results using DrTests, which you can open by selecting ==World > Test Runner==.

% The ''Test Runner'', is designed to make it easy to execute groups of tests.

% You can also run a single test (and print the usual pass/fail result summary) by
% executing a ''Print it'' on the following code: ==MyExampleSetTest run:
% #testRemove==.

% Some people include an executable comment in their test methods that allows
% running a test method with a ''Do it'' from the browser, as shown below.

% [[[caption=Executable comments in test methods
% MyExampleSetTest >> testRemove
% 	"self run: #testRemove"
% 	full remove: 5.
% 	self assert: (full includes: 6).
% 	self deny: (full includes: 5)
% ]]]

### Step 7: Debugging a test


Introduce a bug in `MyExampleSetTest >> testRemove` and run the tests again.
For example, change `6` to `7`, as in:

```caption=Introducing a bug in a test
MyExampleSetTest >> testRemove
	full remove: 5.
	self assert: (full includes: 7).
	self deny: (full includes: 5)
```


The tests that did not pass \(if any\) are listed in the right-hand panes of the
&nbsp;_Test Runner_. If you want to debug one, to see why it failed, just click on
the name. Alternatively, you can execute one of the following expressions:

```
(MyExampleSetTest selector: #testRemove) debug

MyExampleSetTest debug: #testRemove
```


### Step 8: Interpret the results


The method `assert:` is defined in the class `TestAsserter`. This is a
superclass of `TestCase` and therefore all other `TestCase` subclasses and
is responsible for all kinds of test result assertions. The `assert:` method
expects a boolean argument, usually the value of a tested expression. When the
argument is true, the test passes; when the argument is false, the test fails.

There are actually three possible outcomes of a test: _passing_, _failing_,
and _raising an error_.

- **Passing**. The outcome that we hope for is that all of the assertions in the test are true, in which case the test passes. In the test runner, when all of the tests pass, the bar at the top turns green. However, there are two other ways that running a test can go wrong.
- **Failing**. The obvious way is that one of the assertions can be false, causing the test to _fail_.
- **Error**. The other possibility is that some kind of error occurs during the execution of the test, such as a _message not understood_ error or an _index out of bounds_ error. If an error occurs, the assertions in the test method may not have been executed at all, so we can't say that the test has failed; nevertheless, something is clearly wrong!


In the _test runner_, failing tests cause the bar at the top to turn yellow,
and are listed in the middle pane on the right, whereas tests with errors cause
the bar to turn red, and are listed in the bottom pane on the right.

Modify your tests to provoke both errors and failures.

### Conclusion


- To maximize their potential, unit tests should be fast, repeatable, independent of any direct human interaction, and cover a single unit of functionality.
- Tests for a class called `MyClass` belong in a class named `MyClassTest`, which should be introduced as a subclass of `TestCase`.
- Initialize your test data in a `setUp` method.
- Each test method should start with the word _test_.
- Use the `TestCase` methods `assert:`, `deny:` and others to make assertions.
- Run tests!
