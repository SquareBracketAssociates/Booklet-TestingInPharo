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

### Before executing a test

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

Finally the class browser propose you a class definition so that you can define the missing class on the spot (See Figure *@fig:Defined@*).
Once the class is defined you will see that it is not slanted anymore in the class browser. 
And you are ready to execute the test even if you did not define any method yet!
This is the all point of XTDD.

![Pharo class browser proposes a class definition for the class `Counter`.](figures/XTDD5.png width=50&label=fig:Defined)

### Executing a test to define missing methods
Event if we did not define any methods yet, we will execute the test. 
We will just press the little grey button on the left of the method name in the right most list as shown in Figure *@readyToTun@*. 
It will raise an error because the methods `count:` and `count` are not defined. 

![Executing a test pressing the grey button on the left of the method name will raise an error.](figures/XTDD7.png width=50&label=readyToTun)

Figure *@error1@* shows the debugger: It indicates that an instance of the class Counter did not understand the message `count:`.
So far so good, this is what we expected. 

![A debugger showing that the method `count:` is not defined hence led to an error.](figures/XTDD8.png width=50&label=error1)

##### Define a method on the fly.
Now we are ready to create a method on the fly.
Just press the button Create. You will get prompt for the class, pick up the class `Counter`.

The debugger will show you that the system created a generic method named `count:` for you. 
It is generic because there is no magic. As shown in Figure *@genericMethodDefined@*, this method is 

```
count: anInteger
	self shouldBeImplemented
```

The message `shouldBeImplemented` is just a method to raise a specific error. So that the debugger reopens and that you can redefine the method. 

![The system defined a generic method and restarted the execution: it is now waiting for a definition.](figures/XTDD10.png width=50&label=genericMethodDefined)



#### Edit the method in the debugger
Now just edit the method right in the debugger as follows and shown in Figure *@countInDebu@*.
Here we just want a setter that sets the value of the instance variable `count`.
So we just type it!

```
count: anInteger
	count := anInteger
```

Here we define the method as we want it and yes the instance variable `count` does not exist yet.
Do it and compile the method. The class browser will prompt you for the creation of `count` as an instance variable.

![Before compiling the method, the class browser shows us that the instance variable `count` does not exist yet.](figures/XTDD11.png width=50&anchor=countInDebu)


![Defining a new instance variable from within the debugger.](figures/XTDD13.png width=50&anchor=InstVarInDebu)

Compile the method, answer the prompt and you should get the method that we defined previously.
Now continue the execution by pressing the Proceed button
%![](figures/XTDD13.png width=100)
%![](figures/XTDD14.png width=80)



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
