## Xtreme test-driven Development  by Example



In this chapter we will describe eXtreme Test-Driven Development (XTDD). 
We show that XTDD is Test-Driven Development on steroids.
What is really exciting is that XTDD takes live programming at its best.
It shows that in Pharo we can develop smart tools that offer to developers
absolutely grogeous development flow.


### A simple and powerful principle

The main idea behind XTDD is to avoid to break the development flow.
It is as simple as: 
- First, you write a test.
- Second, you execute the test.
- When it breaks, you define the method on the fly in the debugger.
- Then, you resume the execution from the debugger and continue the execution until the test is green.

What you should see is that there is no border between specifying a test and developing the code under test.
You develop in the flow of the executed program.

### Studying an example

Let us illustrate  eXtreme Test-Driven Development. 
We use a dead simple counter. Nothing simpler.
This way we will focus on the essence of the process.
We want to show you that you can do it.

#### A package and an empty test case class

First we define a package `Counter` and define a subclass of `TestCase` named `CounterTest`.

```
TestCase << #CounterTest
	package: 'Counter'
```
#### A first test

We define a first test that 
- Creates a new instance of the class `Counter`
- Defines its count value using the setter `count:`
- And verifies that the count value is correct. 

```
CounterTest >> testSetAndGetCounter
	self assert: (Counter new count: 22) count equals: 22
```

Now during the definition of the method the system will notify you because the class `Counter` does not exist. 
Figure *@fig:gettingDefined@* shows that the IDE presents the class `Counter` slanted to show that this class
does not exist. 

![Pharo class browser  shows the class `Counter` slanted because that class
does not exist.](figures/XTDD3.png width=50&label=fig:gettingDefined)

When you compile the method, the system asks you to define the missing class (see Figure *@fig:DefinesPrompt@*).

![Pharo class browser  request actions to handle the fact that the class `Counter` is undefined.](figures/XTDD4.png width=50&label=fig:DefinesPrompt)

![Pharo class browser  shows the class `Counter` slanted because that class
does not exist.](figures/XTDD5.png width=50&label=fig:gettingDefined)




#### Test defined but not executed

figures/XTDD6.png|width=90+


#### Running the test

figures/XTDD7.png|width=90+

#### First Error

figures/XTDD8.png width=80

Of course, we did not define any method, yet!

#### Create a method on the fly}$

figures/XTDD9.png width=90

#### Create a method on the fly (II)

figures/XTDD10.png width=90

#### Edit the method in the debugger

![](figures/XTDD11.png width=80)

- But there is no instance variable!
- So what?

### Add an instance variable on the fly

![](figures/XTDD12.png width=80)

We compile and obtain

![](figures/XTDD13.png width=100)

and continue the execution as shown in 

![](figures/XTDD14.png width=80)



#### Supporting the programmer flow

The system 
- ""created"" a new method for us
- ""Removed"" the stack element with Error
- ""Replaced"" it with a call to the new method
- ""Relaunched"" execution
We edited it and recompiled the method.
The system ""continued"" execution.

#### New method

The system:
- Created a new method
- Removed the stack element with Error
- Replaced it with a ""call"" to the new method

```
count: anInteger
	self shouldBeImplemented
```

The message `shouldBeImplemented` is just an exception so that the debugger stops again.


#### Same story....

![](figures/XTDD15.png  width=90)

#### Debugger also precompiles methods}$

![](figures/XTDD16.png width=70)

A method with the same name as an attribute is probably an accessor

#### Test is green

![](figures/XTDD17.png width=90)

### One Cycle

- Run all the tests
- Ready to commit
- New test

### Why XTDD is powerful

- Avoid ""guessing"" context when coding
- Much much better context 
-- inspect that ""specific"" instance state
-- talk to that ""specific"" object
- Inspectable / interactable context
- Tests are not a side effect artifact but the ""driving"" force

### Protip from expert Pharo developers

- Grab ""as fast as"" possible one object
- ""Cristalize"" your scenario with a test
- Xtreme TDD
- Loop
