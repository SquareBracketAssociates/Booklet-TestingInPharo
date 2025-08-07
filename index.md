## Introduction

Pharo is a unique environment in which you can write a test, execute it, and from the raised debugger grow your program. 
We coined this powerful technique Xtreme Test-Driven Design.
This is powerful because it gives you a unique situation where you are in close contact with the specific state of your program.
There you can write your code interacting with a live organism \(the set of objects that are currently executing your program\).
It gives you a unique opportunity to query and interact deeply with your objects. 
Another important point is that tests are at the center of your development flow, not just because you were told to do so but 
because tests help you develop faster and more robust software.

Describing Xtreme Test-Driven Development feels like describing swimming among the fishes in a scuba diving session.
It is difficult to transmit the sensation. Still, we explain Xtreme TDD in a full chapter using a simple example.
Since Xtreme Test-Driven Development takes its root in unit testing, we will describe unit testing in Pharo.

### Outline

In this book, we will present how to test and develop testing strategies in Pharo.
We will present the SUnit framework and its extensions.
We show that contrary to what is commonly believed, testing UI is possible and that you can take advantage of it.
We present how to connect your GitHub repository to take advantage of integration services.
We also present how to test web applications. 
We also show some mocking approaches and show that benchmarks can be also supported even if they 
are not tests per se. 
We also describe the framework and its implementation.

### About SUnit

Testing is getting more and more exposure. What is interesting to see is that Pharo inherits
SUnit from its ancestors \(Smalltalk\) and it is worth knowing that most of the Unit frameworks are inheriting
from the Smalltalk original version developed by K. Beck.

SUnit is a minimal yet powerful framework that supports the creation and deployment of tests. 
As might be guessed from its name, the design of SUnit focuses on _Unit Tests_, but in fact, it can be used for integration tests and functional tests as well. 
SUnit was originally developed by Kent Beck and subsequently extended by Joseph Pelrine and others to incorporate the notion of a resource. 
Note that the version documented in this chapter and used in Pharo is a modified version of SUnit3.3.

### About typographic conventions


In this book, we use the following conventions.
We use the new fluid class syntax introduced in Pharo 9. Fluid means that we use a cascade to define the class elements and omit the empty ones.

When you were used to defining a class as follows:

```
TestCase subclass: #MyExampleSetTest
	instanceVariableNames: 'x y'
	classVariableNames: ''
	package: 'MySetTest'
```


We use the following fluid definition 

```
TestCase << #MyExampleSetTest
	slots: { #x . #y };
	package: 'MySetTest'
```



Another point is that we always prefix the method source code with the class of the method.
The book shows it as: 
```
MyExampleSetTest >> testIncludes
    | full empty |
    full := Set with: 5 with: 6.
    empty := Set new.
    self assert: (full includes: 5).
    self assert: (full includes: 6).
    self assert: (empty includes: 5) not
```


And it you want to type it into Pharo you should type the following in the corresponding class.

```
testIncludes
    | full empty |
    full := Set with: 5 with: 6.
    empty := Set new.
    self assert: (full includes: 5).
    self assert: (full includes: 6).
    self assert: (empty includes: 5) not
```


### About this book


In Pharo by Example current revision \(9\), we decided to go to the essential of SUnit and removed parts that were too detailed and long.
This gave us the idea that a "Testing in Pharo" book was missing. 
Therefore instead of losing the parts that we removed, they grew in a new book.
Therefore a part of the text of this book was written originally in Pharo by Example by Andrew P. Black, Stéphane Ducasse, Oscar Nierstrasz, Damien Pollet, Damien Cassou, and Marcus Denker, it is mainly the motivation, description of SUnit and SUnit implementation. 
We thank them for this material that we revised. 
Our ultimate goal is to revisit the implementation of SUnit and to keep this book up to date.

##### Acknowledgments. 
We want to thank Jimmie Houchin for improving the English of this book.



### Getting started


We encourage you to experience Test-Driven Development and in particular Xtreme Test-Driven Development.
Yes, writing tests looks like an extra effort but it is really worth it.
Tests force you to design APIs. They give you insurance so that you will be able to change your code 
without fear to break your code and not getting notified about it. 


<!inputFile|path=Chapters/SUnit/Testing.md!>

<!inputFile|path=Chapters/SUnit/SUnitExample.md!>

<!inputFile|path=Chapters/XTDD/XTDD.md!>

<!inputFile|path=Chapters/SUnit/SUnit.md!>

<!inputFile|path=Chapters/SUnit/Cookbook.md!>

<!inputFile|path=Chapters/SUnit/Implementation.md!>

<!inputFile|path=Chapters/UITesting/UITesting.md!>

<!inputFile|path=Chapters/Web/Web.md!>

<!inputFile|path=Chapters/Mocking/SimpleMock.md!>

%<!inputFile|path=Chapters/Mocking/Mocketry.md!>

%<!inputFile|path=Chapters/Mocking/StateSpecs.md!>

<!inputFile|path=Chapters/Benchs/Smark.md!>

%<!inputFile|path=Chapters/Misc/Misc.md!>
