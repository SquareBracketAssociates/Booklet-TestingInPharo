## Two minutes of theory


The interest in testing and Test Driven Development is not limited to Pharo.
 Automated testing has become a hallmark of the _Agile software
development_ movement, and any software developer concerned with improving
software quality would do well to adopt it.
Indeed, developers in many languages have come to appreciate the power of unit testing.  
And versions of _xUnit_ now exist for every programming language.

What you will discover while programming in Pharo is that Pharo supports _Xtreme TDD_: you define your tests first as in TDD
but you execute your program which breaks and then you code in the debugger. 
This is simply such a productivity boost that we urge you to try it. 
We are just addicted to it. In Pharo this is super cool to define a test, run the code, and code in the debugger: this is such a speed-up and positive energy. 

Now lets us go for two minutes of theory around Unit tests and TDD.


### Automated tests

Neither testing nor the building of test suites is new. 
By now, everybody knows that tests are a good way to catch errors. eXtreme Programming, by making
testing a core practice and by emphasizing _automated_ tests, has helped to
make testing productive and fun, rather than a chore that programmers dislike.
The Pharo community has a long tradition of testing because of the incremental
style of development supported by its programming environment. 

#### Favor executable tests

During incremental development sessions in Pharo, the old-fashioned programmer would write code snippets in a playground as soon as a
method was finished.
Sometimes a test would be incorporated as a comment at the head of the method that it exercised, or tests that needed some setup would be included as example methods in the class. 
The problem with these practices is that tests in a playground are not available to other programmers who modify the code. 
Comments and example methods are better in this respect, but there is still no easy way to keep track of them and to run them automatically. 
Tests that are not **systematically** run do not help you to find bugs! 
Moreover, an example method does not inform the reader of the expected result: you can run the example and see
the \(perhaps surprising\) result, but you will not know if the observed behavior is correct.

#### Favor TDD and XtremeTDD


With Test-driven development, you write a little automated test and execute it to make sure that it fails. 
Then in Pharo, you use the test execution and failure to create methods inside the debugger.
Once your test passes, you rerun all your tests and commit if they are all passing.

SUnit is valuable to support this scenario. 
It allows us to write tests that are automated and self-checking:
the test itself defines what the correct result should be.
 It also helps us to organize tests into groups, describe the context in which the tests must run,
and run a group of tests automatically. 
In less than two minutes you can write tests using SUnit, so instead of writing small code snippets in a
playground, we encourage you to use SUnit and get all the advantages of stored
and automatically executable tests.

### Why testing is important

@sec:whytest

Unfortunately, many developers believe that tests are a waste of their time.
After all, _they_ do not write bugs, only _other_ programmers do that. 
Most of us have said, at some time or other: _I would write tests if I had more
time._ 
If you never write a bug, and if your code will never be changed in the
future, then indeed tests are a waste of your time. 
However, this most likely also means that your application is trivial, or that it is not used by you or
anyone else. 
Think of tests as an investment for the future: having a suite of
tests is quite useful now, but it will be _extremely_ useful when your
application, or the environment in which it runs, changes in the future.

Tests play several roles. 
First, they provide documentation of the functionality that they cover. 
This documentation is active: watching the tests pass tells you that the documentation is up to date. 
Second, tests help developers to confirm that some changes that they have just made to a package have not broken anything
else in the system, and to find the parts that break when that confidence turns
out to be misplaced. Finally, writing tests during, or even before, programming
forces you to think about the functionality that you want to design, _and how
it should appear to the client code_, rather than about how to implement it.

By writing the tests first, i.e., before the code, you are compelled to state
the context in which your functionality will run, the way it will
interact with the client code, and the expected results.
Your code will improve. Try it.

We cannot test all aspects of any realistic application. 
Covering a complete application is simply impossible and should not be the goal of testing. 
Even with a good test suite, some bugs will still creep into the application, where
they can lay dormant waiting for an opportunity to damage your system. 
If you find that this has happened, take advantage of it! 
As soon as you uncover the bug, write a test that exposes it, run the test, and watch it fail. 
Now you can start to fix the bug: the test will tell you when you are done.

### What makes a good test?


Writing good tests is a skill that can be learned by practicing.
Let us look at the properties that tests should have to get the maximum benefit.

_Tests should be repeatable._ You should be able to run a test as often as you
want, and always get the same answer.

_Tests should run without human intervention_. You should be able to run them
unattended.

_Tests should tell a story._ Each test should cover one aspect of a piece of
code. A test should act as a scenario that you or someone else can read to
understand a piece of functionality.

_Tests should have a change frequency lower than that of the functionality they
cover_. You do not want to have to change all your tests every time you modify
your application. One way to achieve this is to write tests based on the public
interfaces of the class that you are testing. It is OK to write a test for a
private _helper_ method if you feel that the method is complicated enough to
need the test, but you should be aware that such a test may have to be changed,
or thrown away entirely when you think of a better implementation.

One consequence of such properties is that the number of tests should be
somewhat proportional to the number of functions to be tested: changing one
aspect of the system should not break all the tests but only a limited number.
This is important because having 100 tests fail should send a much stronger
message than having 10 tests fail. However, it is not always possible to achieve
this ideal: in particular, if a change breaks the initialization of an object 
or the set-up of a test, it is likely to cause all of the tests to fail.

Several software development methodologies such as _eXtreme Programming_ and
Test-Driven Development \(TDD\) advocate writing tests before writing code. This
may seem to go against our deep instincts as software developers. All we can say
is: go ahead and try it. We have found that writing the tests before the code
helps us to know what we want to code, helps us know when we are done, and helps
us conceptualize the functionality of a class and to design its interface.
Moreover, test-first development gives us the courage to go fast, because we are
not afraid that we will forget something important.

Writing tests is not difficult in itself. Choosing _what_ to test is much more
difficult. Some developers coined the "right-BICEP" principle. It
stands for:

- Right: Are the results right?
- B: Are all the boundary conditions correct?
- I: Can you check inverse relationships?
- C: Can you cross-check results using other means?
- E: Can you force error conditions to happen?
- P: Are performance characteristics within bounds?





### A piece of advice on testing


While the mechanics of testing are easy, writing good tests is not. Here is some
advice on how to design tests.

#### Self-contained tests


You do not want to have to change your tests each time you change your code,
so try to write the tests so that they are self-contained. This can be
difficult but pays off in the long term. Writing tests against stable
interfaces supports this effort.

#### Do not over-test


Try to build your tests so that they do not overlap. It is annoying to have many
tests covering the same functionality because one bug in the code will then
break many tests at the same time. This is covered by Black's rule, below.

#### Feathers' rules for unit tests


Michael Feathers, an agile process consultant, and author, writes:

_A test is not a unit test if: it talks to the database, it communicates across
the network, it touches the file system, it can't run at the same time as any of
your other unit tests, or you have to do special things to your environment
\(such as editing config files\) to run it. Tests that do these things aren't bad.
Often they are worth writing, and they can be written in a unit test harness.
However, it is important to be able to separate them from true unit tests so
that we can keep a set of tests that we can run fast whenever we make our
changes. Never get yourself into a situation where you don't want to run your
unit test suite because it takes too long._

#### Unit tests vs. Acceptance tests


Unit tests capture one piece of functionality, and as such make it easier to
identify bugs in that functionality. As far as possible try to have unit tests
for each method that could possibly fail, and group them per class. However, for
certain deeply recursive or complex setup situations, it is easier to write
tests that represent a scenario in the larger application. These are called
acceptance tests (or integration tests, or functional tests).

Tests that break Feathers' rules may make good acceptance tests. Group
acceptance tests according to the functionality that they test. For example, if
you are writing a compiler, you might write acceptance tests that make
assertions about the code generated for each possible source language statement.
Such tests might exercise many classes and might take a long time to run
because they touch the file system. You can write them using SUnit, but you
won't want to run them each time you make a small change, so they should be
separated from the true unit tests.

#### Black's rule of testing


For every test in the system, you should be able to identify some property for
which the test increases your confidence. It's obvious that there should be no
important property that you are not testing. This rule states the less obvious
fact that there should be no test that does not add value to the system by
increasing your confidence that a useful property holds. For example, several
tests of the same property do no good. In fact, they harm in two ways.
First, they make it harder to infer the behavior of the class by reading the
tests. Second, because one bug in the code might then break many tests, they
make it harder to estimate how many bugs remain in the code. So, have a property
in mind when you write a test.


### Pharo testing rules as a conclusion

We can argue over and over why tests are important. 
The only real way to understand for real their values is by experience.

The Pharo core development has three basic rules for testing.
Here they are: 

##### Important A test that is not automated is not a test.

##### Important Everything that is not tested does not exist.

##### Important Everything that is not tested will break.

With this in mind, we urge ourselves to write tests. 
Sometimes we are sloppy and lazy but most of the time we push ourselves to stay bold.
We encourage you to do the same.

