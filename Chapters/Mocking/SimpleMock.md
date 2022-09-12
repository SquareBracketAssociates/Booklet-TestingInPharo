## MockObject and Teachable: Two super simple mocking approachesImagine that you want to test a network protocol, you do now want to rely on real network but you would liketo fix certain parameters and make that your protocol is reacting correctly. You can use Mock objects to represent a part of the world during your tests.A mock object is an object that supports testing in isolation and represents a behavior that should be fixed from a test point of view. There are several frameworks for defining mock objects: BabyMock and Mocketry are two of the most sophisticated. In this chapter we present two super simple and minimalist mocking approach. We start with an extension that has been introduced as an extension of SUnit in Pharo 9. It has been designed by Giovanni Corriga. ### About MockObject designWhile simpler and less sophisticated than other libraries, this implementation is still quite powerful.This implementation takes a different approach compared to BabyMock/BabyMock2 and Mocketry:- there are no methods such as `should`, `can`, or `be:`, - the mocks are stricter in their behaviour -- users need to send all and only the defined messages, in the same order as they were defined.- the mocks need to be manually verified using the new message `TestCase>>verify:` defined on `TestCase`.These limitations are on purpose, mainly for two reasons:- to discourage the use of these objects unless they are really needed.- to keep the implementation simple so that it can be integrated in Pharo's SUnit library, as opposed to being its own framework.This extension is similar to Pharo Teachable that you can find at: [https://github.com/astares/Pharo-Teachable](https://github.com/astares/Pharo-Teachable) that we present below as a complement. ### MockObject I am a test double object that can be used as a stub, a fake, or a mock. I provide a simple protocol so that the user can teach me what messages to expect, and how to behave or respond to these messages. #### UsageA new object can be created with`MockObject new`, or using the utility methods on the class side 'instance creation' protocol.The main message to teach a MockObject is`on:withArguments:verify:`; the other methods in the 'teaching' protocol all send this message.This message takes a selector symbol, a collection of arguments, and a block. By sending this message, the mock object is trained to evaluate the block when receiving a message matching the selector and the arguments. Other variations of this message have more specialized behavior:`on:withArguments:respond:` will simply return its third argument when the mock receives a matching message; likewise `on:withArguments:` will return the mock itself.The other methods in the 'teaching' protocol provide an ergonomic API for this behaviour.  A mock object will expect to receive only the messages it has been trained on, in the same order and number as it was trained. If it receives an unknown message, or a known message but in the wrong order, it will simply return itself.### Stubs, Fakes, and MocksA MockObject can be used as a stub by not using the `verify:` variants of the 'teaching' protocol.To use the MockObject as a real mock, the user needs to verify its use. This is done by means of the `TestCase>>verify:` message. Verification needs to be triggered by the user - it's not automatic.The`verify:` message will assert- that the mock has received all the messages it has been trained on - that it has not received only those messages- that it has received the messages it has been trained on.### Example```TestCase << #MyClassUsingMock
	slots: { #mock };
	package: 'SUnit-MockObjects-Tests'```We create a mock object and teach it two messages `meaningOfLife` and `secondMeaning` that should be send one after the other.```MyClassUsingMock >> setUp

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife 
		respond: 42. 
	mock 
		on: #secondMeaning
		respond: 84.	```Then we can write a test that will send the message to the object and expect a given result. We can also verify that all the messages have been sent. ```MyClassUsingMock >> testVerifyChecksThatAllTheMessageGotSent

	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
	self verify: mock```We can also specify which precise argument a message should be passed using the message `on:with:respond:` or `on:with:with:respond:`.```MyClassUsingMock2 >> setUp

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife: 
		with: 22
		respond: 42. 
	mock 
		on: #secondMeaning:and:
		with: 32
		with: 64
		respond: 84.```The following ```MyClassUsingMock2 >> testMeaningOfLifeDoesNotPassCorrectValue

	self should: [ self assert: (mock meaningOfLife: 33) equals: 42] raise: TestFailure``````MyClassUsingMock2 >> testMeaningOfLife

	self assert: (mock meaningOfLife: 22) equals: 42.
	self assert: (mock secondMeaning: 32 and: 64) equals: 84```### About matching argumentsRegarding the arguments matching, the code always goes through the matching logic but it's still quite flexible. The following three possibilities are supported:- ignoring the arguments: if the message send has no arguments of interest,- full match: if all arguments of the actual message send match the expected arguments,- partial match: the user of the mock should use `MockObject class>>any` for any argument that can be ignored. This is a special wildcard object to be used when we don't care about an argument.For example, let's assume we have this code under test:```aMock messageToBeMocked: 1 withArguments: 'two'```This can be mocked the three following way: ignoring arguments, full match or partial match. #### Ignoring arguments.```aMock on: #messageToBeMocked:withArguments:```#### Full match.```aMock 
	on: #messageToBeMocked:withArguments:
	with: 1
	with: 'two'```#### Partial match.```aMock 
	on: #messageToBeMocked:withArguments:
	with: MockObject any
	with: 'two'```The above training methods are to be used when we don't care about the behaviour when mocking, but only that the message is sent.If we want to control what is returned by the message send, we can use the `respond:` variants.If we want even finer control e.g. side effects, raising exceptions  we can use the `verify:` variants.### Teachable The other simple library for mocking object is Pharo Teachable. It is not integrated into SUnit but still worth checking it. It is developed by Torsten Bergman. #### Installation You can install teachable using the following expression: ```Metacello new 
	repository: 'github://astares/Pharo-Teachable/src';
	baseline: 'Teachable';
	load ``` Teachable is a class whose instances can be taught to respond to messages. It's usefull for creating mocks who should behave like other objects \(for instance inside of a test case\) without actually implementing a real mock class. Here is an example how it can be used:```| teachable |
teachable := Teachable new.
teachable
    whenSend: #help return: 'ok';
     whenSend: #doit evaluate: [ 1 inspect ];
     acceptSend: #noDebugger;
     whenSend: #negate: evaluate: [ :num | num negated ].``` After teaching the object we can use it as if it had a normal implementation in a class:```teachable help. 
	"this will return the string 'ok'"
teachable doit. 
	"this will open the inspector on the SmallInteger 1"
teachable noDebugger. 
	"this will accept the send of #noDebugger and return the teachable"
teachable negate: 120 
	"this will return -120"```### ConclusionWe presented two little approaches to support simple mock objects by providing a way to teach values or action to be performed by the mock. More mature and powerful frameworks exist: BabyMock and Mocketry.