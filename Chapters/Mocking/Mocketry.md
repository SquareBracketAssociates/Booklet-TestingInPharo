## Mocketry


Mocketry is an alternate mock framework developed by Denis Kudriashov.
It provides a simple way to stub any message to any object and to verify any behavior.

### How to install Mocketry?


You can install Mocketry using the following expression
```
Metacello new
  baseline: 'Mocketry';
  repository: 'github://dionisiydk/Mocketry';
  load.
```


### About Test-Driven Development and Mock Objects


Test-Driven Development is a software development technique in which programmers write tests before they write the code itself. 
In the tests they make assertions about the expected behavior of the object under test.
There are two kinds of assertions, the ones where you assert that an object answered correctly in response to a message.
Here is an example expressed using Mocketry:

```
result := calculator add: 'V' to: 'VII'.
result should equal: 'XII'.
```


And the ones where you assert that an object sent the correct message to another object.
 A mock object library helps in the latter case by providing a framework for setting up those assertions:

```
publisher publish: testEvent.
subscriber should receive eventReceived: testEvent.
```


A mock library also provides tools to specify the expected behavior of interacting objects to simulate particular test case.
Here is an example: 

```
order := Order on: 50 of: #product.
warehouse := Mock new.
(warehouse stub has: 50 of: #product) willReturn: true.

order fillFrom: warehouse.

warehouse should receive remove: 50 of: #product.
```


### TDD is not just about testing


Test-Driven Development combined with mock objects supports good object-oriented design by emphasizing how the objects collaborates with each other rather than their internal implementation. 
Mock objects can help you discover object roles as well as design the communication protocols between the objects.

We'll describe how to use mock objects and outside-in TDD in practice. 
We're going to use SUnit as our test framework \(we assume you're familiar with it\) and Mocketry as our mock object library. This will be more like a message \(or interaction\) oriented approach of TDD, so we choose the example codes accordingly. Depending on what problem you're trying to solve and what design strategy you're following, you might want to use simple assertions instead of mocks and expectations. But in an object oriented code there are plenty of places where objects _tell each other to do things_ therefore mocks will come in handy.

### How does outside-in TDD work?


We start with the action that triggers the behavior we want to implement and work our way from the outside towards the inside of the system. We work on one object at a time, and substitute its collaborator objects with mocks. Thus the interface of the collaborators are driven by the need of the object under test. We setup expectations on the mocks, which allows us to finish the implementation and testing of the object under test without the existence of concrete collaborators. After we finished an object, we can move on towards one of its collaborator and repeat the same process.
Now In the following sections, we will develop a complete sample application written with TDD.

### Getting started


With SUnit you create test case classes by subclasing TestCase. Then in any test you can create mock by simple `new` message:

```
yourMock := Mock new.
```


Mocketry does not require any special context variables for this.
Also Mocketry implements auto naming logic to retrieve variable names from test context. 
Inside test yourMock will be shown as "a Mock\(yourMock\)" \(in debugger\).

But if you need special name you can specify it:

```
Mock named: 'yourMock'
```


Also there is way to get multiple mocks at once:
```
[ :mockA :mockB | "your code here" ] runWithMocks
```


Then to specify expected behaviour for mock just send `stub` message to it. Following expression will be recorded as expectation.
And to verify what was going on with objects just send `should receive` messages and following expression will be verified.
Look at *@stubMessageSends@* section for all detailes on Mocketry API.

Now In the next sections, we will develop a complete sample application written with TDD.

### A simple shopping cart


Let's design a simple shopping cart that will calculate the price of the products and charge the customer's credit card. The total charge is calculated by subtracting the discount price from the total order amount. During this process we are going to discover several roles that mock objects will play.

### Writing our first test

After an incoming checkout message, the shopping cart will send a `charge:` message to a credit card object if it is not empty. This outgoing message and its argument is what we need to test in order to verify the behaviour of our shopping cart.

Let's start with the simplest test when the shopping cart is empty. In this case no `charge:` message should be sent.

```
ShoppingCartTest >> testNoChargeWhenCartIsEmpty
	| creditCard cart |
	creditCard := Mock new.
	cart := ShoppingCart payment: creditCard.

	cart checkout.

	creditCard should not receive charge: Any.
```


The `creditCard` is a dependency for the shopping cart, so we pass it through a constructor method. Then we trigger the behavior we want to test by sending a `checkout` message to the shopping cart. And at the end we check our expected behaviour - credit card should not be charged by any amount because nothing exists in the cart.

And empty implementation of `checkout` method will pass the test:

```
ShoppingCart >> checkout
	"empty implementation"
```


Usually we want to start with a failing test because we want to be sure that the test is capable of failing, then we can write just enough production code pass the test. This is an exceptional case, so we'll keep an eye on this test and come back to this later.

### One product, no discount


Now let's add one product id \(we'll call it SKU which is an abbreviation of stock keeping unit\) to the shopping cart and check the price being sent to the credit card. But to do so we need to know the price of the product. It would be helpful if there were an object that could find out the price of an item by its SKU. We can bring this object to existence with the help of the mock library.

Let's create a mocked `priceCatalog` and move the initialization of the shopping cart in the `setUp` method.

```
ShoppingCartTest >> setUp
	super setUp.
	priceCatalog := Mock new.
	creditCard := Mock new.
	cart := ShoppingCart catalog: priceCatalog payment: creditCard.
```


We converted the `cart` and `creditCard` temporary variables to instance variables because we are using them in both tests.

Now we can setup and use the `priceCatalog` without worrying about its internal structure of an actual catalog.

```
ShoppingCartTest >> testChargingCardAfterBuyingOneProduct
	(priceCatalog stub priceOf: #sku1) willReturn: 80.
	cart addSKU: #sku1.
	cart checkout.
	(creditCard should receive charge: 80) once
```


This test will fail if:

- the message `charge:` is not sent to the `creditCard` during the test.
- the message `charge:` is sent more than once.
- the argument of the message `charge:` is different than 80.


This test gives us our first failure:

```
a Mock(creditCard) never received charge: 80
```


Notice that Mocketry complains only about the missing `charge:` message. It doesn't care about the `priceOf:`. Stubbed behaviour is only used to simulate reaction of system on particular messages. Stubs do not add any requirements to tested behaviour. If business rules will require to check catalog for every `checkout` operation then we will add extra assertion in our test. Or better we will add separate test to describe this aspect of system. But right now we don't care about it.
Also imagine we're introducing a price caching mechanism in the shopping cart. So after we asked the price of a product we store it and next time we won't ask it again. This will not break our test.
Charging the credit card twice would be a serious mistake and Mocketry will report this failure. But querying the product price twice is an implementation detail.

The following implementation passes the test.

```
ShoppingCart >> addSKU: aString
	sku := aString.
```


```
ShoppingCart >> checkout
	creditCard charge: (priceCatalog priceOf: sku)
```


This implementation isn't complete but it is just enough to pass the second test. We don't want to do more than it necessary to pass the test. This practice ensures high code coverage and helps keeping the implementation simple.

Now our first test is finally broken because we charge the credit card \(with nil\) even if the cart is empty. This can be easily fixed by a nil check.

```
ShoppingCart >> checkout
	sku ifNotNil:
		[creditCard charge: (priceCatalog priceOf: sku)]
```


### Many products, no discount


The shopping cart that can hold only one product isn't particularly useful. The next test will force us to generalize our implementation.

```
ShoppingCartTest >> testChargingCardAfterBuyingManyProducts
	(priceCatalog stub priceOf: #sku1) willReturn: 10.
  (priceCatalog stub priceOf: #sku2) willReturn: 30.
  (priceCatalog stub priceOf: #sku3) willReturn: 50.
	cart addSKU: #sku1.
	cart addSKU: #sku2.
	cart addSKU: #sku3.

	cart checkout.

  creditCard should receive charge: 90.
```


This generates a failure message:
```
a Mock(creditCard) never received charge: 90
```


And in debugger you can see what messages was actually sent between objects. For this select top item on stack pane of debugger. And then select `subject` on variables pane. It will show single message:

```
a Mock(creditCard) charge: 50 returned default mock(73422)
```


The shopping cart keeps the last SKU only that's why it reports the wrong price. First we rename the `sku` instance variable to `skus` and intialize it to an empty `OrderedCollection`.

```
ShoppingCart >> initialize
	super initialize.
	skus := OrderedCollection new.
```


Then we replace the assignment to an `add:` message in the `addSKU:` method.

```
ShoppingCart >> addSKU: aString
	skus add: aString.
```


Finally we change the nil check to an empty check and add the `orderPrice` calculation.

```
ShoppingCart >> checkout
	skus ifNotEmpty:
		[ creditCard charge: (skus sum: [ :each | priceCatalog priceOf: each ])]
```


Refactoring is part of TDD. We should check after each test if there is anything we can do to make the test and the code better. The setup of the `priceCatalog` adds a lot of noise to the last test. Let's extract it to a private method.

```
ShoppingCartTest >> testChargingCardAfterBuyingManyProducts
	self prices: {#sku1 -> 10. #sku2 -> 30. #sku3 -> 50}.
	cart addSKU: #sku1.
	cart addSKU: #sku2.
	cart addSKU: #sku3.

	cart checkout.

	creditCard should receive charge: 90.
```


```
ShoppingCartTest >> prices: associations
	associations do: [:each |
			(priceCatalog stub priceOf: each key) willReturn each value].
```


Then extract the `orderPrice` calculation into a new method:

```
ShoppingCart >> checkout
	skus ifNotEmpty:
		[creditCard charge: self orderPrice]
```


```
ShoppingCart >> orderPrice
	^ skus sum: [:each | priceCatalog priceOf: each]
```


### Removing a product


The shopping cart should provide methods for removing a product. We don't want to test the internal state of the shopping cart, because it would make our tests brittle. We assume that the interface of the shopping cart is more stable than its implementation, so we want to couple our tests to the interface of the object under test. We can verify the behaviour by checking that a product removal is reflected in the price.

```
ShoppingCartTest >> testProductRemovalReflectedInPrice
	self prices: {#sku1 -> 30. #sku2 -> 70.}.
	cart addSKU: #sku1.
	cart addSKU: #sku2.
	cart removeSKU: #sku2.

  cart checkout.

  creditCard should receive charge: 30.
```


Making the test pass is fairly simple.

```
ShoppingCart >> removeSKU: aSymbol
	skus remove: aSymbol.
```


### Products with discount price


We know that there are rules for calculating the discount price. Those rules take the total order amount into account. Luckily we don't need to worry about what exactly the rules are. Let's introduce a new mock that will play the role of the `DiscountCalculator`.

```
ShoppingCartTest > >setUp
	super setUp.
	priceCatalog := Mock new.
	discountCalculator := Mock new.
	creditCard := Mock new.
	cart := ShoppingCart catalog: priceCatalog discount: discountCalculator payment: creditCard.
```


```
ShoppingCartTest >> testChargingCardWithDiscount
	self prices: {#sku1 -> 20. #sku2 -> 30}.
	(discountCalculator stub calculateDiscount: 50) willReturn: 10.
	cart addSKU: #sku1.
	cart addSKU: #sku2.

	cart checkout.

  creditCard should receive charge: 40.
```


Mocketry complains that the `creditCard` was charged with an incorrect price \(50 instead of 40\). We need to subtract the `discountPrice` from the `orderPrice` to make the test pass.

```
ShoppingCart >> checkout
	| orderPrice discountPrice |
	skus ifNotEmpty:
		[orderPrice := self orderPrice.
		discountPrice := self discountPrice: orderPrice.
		creditCard charge: orderPrice - discountPrice]
```


```
ShoppingCart >> discountPrice: orderPrice
	^ discountCalculator calculateDiscount: orderPrice
```


Now the last test passed.

Interesting that other tests will pass too while we are not mentioned `discountCalculator` there. In these tests `calculateDiscount:` message will be sent to mock object without explicit stub for it. In such cases \(when stub behaviour is not specified\) Mocketry returns new special mock as result of message. This mock plays role of zero in arithmetic operations and false in boolean logic. And in our case it will lead to zero result of `calculateDiscount:`.
It is very usefull behaviour which makes tests less brittle and allows to specify only needed requirement for given aspect of functionality. In our first tests we don't care about discount and tests continue work without it.

### Reusing the cart for different sales


We want the shopping cart to unload itself after the checkout so we'll able to reuse the same instance over and over. We'll simulate two separated purchase in the next test to verify this behaviour. This will trigger two `charge:` messages. We'll check if the correct price is sent when the shopping cart is in the correct state \(`firstPurchase` or `secondPurchase`\).

```
ShoppingCartTest>>testUnloadsItselfAfterCheckout
	|purchase|
	self prices: {#sku1 -> 10. #sku2 -> 30}.
	cart addSKU: #sku1.
	cart checkout.
	cart addSKU: #sku2.
	cart checkout.

  [creditCard receive charge: 10.
  creditCard receive charge: 30] should beDoneInOrder
```


`should beDoneInOrder` expression verifies that messages were sent and it was happen in same order as defined in given block.

The test generates a failure message:

```
a Mock(creditCard) never received charge: 30
```

And in debugger you can see actual messages:

```
a Mock(creditCard) charge: 10 returned default mock(73422).
a Mock(creditCard) charge: 40 returned default mock(34614).
```


But cleaning the `skus` collection in the `checkout` fixes the test.

```
ShoppingCart>>checkout
	| orderPrice discountPrice |
	skus ifNotEmpty:
		[orderPrice := self orderPrice.
		discountPrice := self discountPrice: orderPrice.
		skus removeAll.
		creditCard charge: orderPrice - discountPrice]
```


### Testing errors during checkout


We expect the `creditCard` to report `PaymentError` when the payment was unsuccessful. When this happens, the shopping cart should not forget the content because we may want to retry the checkout later.

In the next test case we'll send two `checkout` message to the shopping cart, whereof the first will fail. The `creditCard` will signal `PaymentError` then answer `true` consecutively.

```
ShoppingCartTest>>testFirstPaymentFailedSecondSucceed
	self prices: {#sku -> 25}.
	cart addSKU: #sku.

	(creditCard stub charge: 25) willRaise: PaymentError new.
	[cart checkout] should raise: PaymentError.

	(creditCard stub charge: Any) willReturn: true.
	cart checkout.
	(creditCard should receive charge: 25) twice.
```


The failure report is:

```
a Mock(creditCard) should receive charge: 10 2 times but it was 1 times
```


The test failed because we clean the `skus` collection before charging the credit card. So after the first checkout the shopping cart is empty, and the second time it won't charge the credit card.

Let's move the `skus removeAll` at the end, to make the test pass.

```
ShoppingCart>>checkout
	| orderPrice discountPrice |
	skus ifNotEmpty:
		[orderPrice := self orderPrice.
		discountPrice := self discountPrice: orderPrice.
		creditCard charge: orderPrice - discountPrice.
		skus removeAll]
```


### What's next


We finished the shopping cart, so what's next? We can continue with either of its dependencies: `CreditCard`, `PriceCatalog`, `DiscountCalculator`.

For example we may want to use a simple in-memory object for the `priceCatalog` and implement its internals using a `Dictionary`. In this case a simple assertion based test would cover its behaviour.

```
PriceCatalogDictionaryTest >> testKnowsThePriceOfAproduct
	| catalog |
	catalog := PriceCatalogDictionary withPrices {#sku1 -> 10}.
	(catalog priceOf: #sku1) should equal: 10.
```


Another implementation might be an HTTP based remote price catalog which would use a REST web service to find out the price.

```
RemoteHttpPriceCatalog >> priceOf: sku
	| response |
	response := ZnClient new
		url: 'http://example.com/rest/sku/', sku;
		get;
		response.
	^ self parseResponse: repsonse.
```


We could mock the http client to test the `RemoteHttpPriceCatalog` but it would result duplications and wouldn't improve our confidence in the code. Mocking a third-party code often causes brittle test and doesn't tell us too much about code itself \(its design and correctness\). So we recommended wrapping the third-party API with an adapter \(See Alistair Cockburn Ports And Adapters Architecture\) that will provide a high level of abstraction. Think about the adapter like a translator that translates messages between two different domains \(`priceOf:` to http `get`\). But here the `RemoteHttpPriceCatalog` is already an adapter, introducing a new one would just delay the problem. We free to mock the adapter just as we did in the shopping cart tests, but we still need to test the `RemoteHttpPriceCatalog` somehow. We recommend testing the adapter itself with integration tests. In the contrast with unit tests \(what we did before\), integration tests don't test object in isolation. Writing a test like this would require firing up a local http server that could provide canned responses in an appropriate format.

We'll leave those as an exercise for the reader.


### Features overview

@overview
#### Create mocks easily

To create mock just use the message `new`.
```
yourMock := Mock new.
```


Mocketry does not require any special context variables for this. Also Mocketry implements auto naming logic to retrieve variable names from test context. In the debugger, inside a test yourMock will be shown as "a Mock\(yourMock\)".

But if you need special name you can specify it:
```
Mock named: 'yourMock'
```


You can look at it live in MockTests.

#### Multiple Mocks at once


Also there is way to get multiple mocks at once:
```
[ :mockA :mockB | "your code here" ] runWithMocks
```



#### Stub any message sends

@stubMessageSends

To stub a message send just send message **stub** to an object and the following message send will create an expectation.

!!todo I not understand why following example is not enough to demonstrate previous sentence. I would stay only example

Once you have a stub object and send it the message `stub`, you can then send it messages such as `willReturn:` and `should be:` to
verify certain condition as follows:

```
mock := Mock new.
mock stub someMessage willReturn: 100.
mock someMessage should be: 100.
```


You can stub any objects. It is not only about mocks:
```
rect := 0@0 corner: 2@3.
rect stub width willReturn: 1000.

rect area should be: 3000 "area = width * height"
```


Note that `stub` message activates,  message interception for real objects. Without it following sentence will not work.

```
rect area should be: 300.
```


### Stubbing classes


And you can do this with globals and classes too but this can be dangerous.
```
DateAndTime stub now willReturn: #constantValue.

DateAndTime now should be: #constantValue.
```


But you should be carefull with classes and globals. Don't try
```
Array stub new.
```


It will crash image because stub performs some black magic so that the object
understands the mocking messages and `Array` is not a class that can be modified because it is central to the execution engine.

Now for the classes that are stubbed, you can turn them back to their default status \(we call this behavior to recover\).
So if you stub a class from a workspace, it is your responsibility to _recover_ it from stub behaviour. Do it using the message `recoverFromGHMutation` as follows:

```
DateAndTime recoverFromGHMutation.
```


In case when you stub a global inside test Mocketry will automatically recover all global stubs when test completes.


### Expectations for set of objects


Also with Mocketry you can define expectations for set of objects. For example you can stub message to **ANY object**:

```
Any stub width willReturn: 100.
```




Or you can stub **ANY message** to particular object:
```
mock := Mock new.

mock stub anyMessage willreturn:: 100.

mock someMessage should be: 100.
mock someMessage2 should be: 100.
```


And both variants are supported:

```
Any stub anyMessage willReturn: 100.

mock := Mock new.
mock someMessage should be: 100.

rect := 0@0 corner: 2@3.
rect stub.

rect area should be: 100.
rect width should be: 100.
```


`Any` class is a specific object specification which means "any" object. You can use any kind of specifications:

```
(Kind of: Rectangle) stub width willReturn: 100.

rect := 0@0 corner: 2@3.
rect stub.

rect area should be: 300.

rect2 := 0@0 corner: 4@5.
rect2 stub.

rect2 area should be: 500
```



### Stub message sends with arguments


In place of message arguments you can use expected objects itself. Or you can put specifications for expected arguments:

```
mock stub messageWith: arg1 and: arg2
mock stub messageWith: Any and: arg2
mock stub messageWith: [:arg | true]
mock stub messageWith: (Kind of: String) and: arg2
mock stub messageWith: (Instance of: Float) & (Satisfying for: [:arg | arg > 10]).
```


Last defined expectation has more priority than previous one. It allows you to define default expectations in `setUp` methods and override it in particular tests. Following example shows it:

```
mock := Mock new.
(mock stub messageWith: (Instance of: SmallInteger)) willReturn: #anyInt.
(mock stub messageWith: (Kind of: String)) willReturn: #anyString.
(mock stub messageWith: 10) willReturn: #ten.

(mock messageWith: 10) should be: #ten.
(mock messageWith: 20) should be: #anyInt.
(mock messageWith: 'test' should be: #anyString
```


#### Expected actions for stubs

There are different kind of expected actions

!!todo look.

##### Return value from message


```
mock stub someMessage willReturn: #result.
mock someMessage should be: #result.
```


##### Raise error on message send


```
mock stub someMessage willRaise: ZeroDivide new.
[mock someMessage] should raise: ZeroDivide.
```


##### Evaluate block on message send and return it result


```
(mock stub someMessageWith: #arg) will: [ #result ].
(mock someMessageWith: #arg) should be: #result.
```


Given block can accept argument of message in original order:

```
(mock stub someMessageWith: #arg1 and: #arg2) will: [:arg1 :arg2 | arg1, arg2].
(mock someMessageWith: #arg1 and: #arg2) should equal: 'arg1arg2'.
```


##### Return values for subsequent series of messages


```
mock stub someMessage willReturnValueFrom: #(result1 result2).
mock someMessage should be: #result1.
mock someMessage should be: #result2
```


##### Return receiver of message


```
mock stub someMessage willReturnYourself.
mock someMessage should be: mock.
```


#### Extra conditions on message sends


It is possible to verify arbitrary condition when expected message is going to be executed. For example:
```
mock := Mock new.
mock stub someMessage
	when: [ flag ] is: (Kind of: Boolean);
	when: [ flag ] is: true;
	when: [ flag ] satisfy: [ :object | true or: [ false ] ].

flag := true.
mock someMessage. "does not fail"

flag := false.
mock someMessage "will fail immediately on call by last condition: flag should be true"

flag := #flag.
mock someMessage "will fail immediately on call by first condition: flag should be boolean"
```


!!todo I do not get the flag should be boolean. We should discuss it.


#### Process condition


Mocketry implements process related condition to check that a message was synchronously sent \(relative to test process\).
Using the messages `shouldBeSentInThisProcess` and `shouldBeSentInAnotherProcess`, you can control the process where the execution
of the test should happen.

```
mock stub someMessage shouldBeSentInThisProcess.
[ mock someMessage ] fork. "will fail immediately on call".

mock stub someMessage shouldBeSentInAnotherProcess.
[ mock someMessage ] fork. "will not fail".
mock someMessage. "will fail immediately on call"
```



#### Message sends usage rules

It is possible to specify how many times expectation can be used using the messages ` use:` `useTwice` and `useOnce`.

```
mock := Mock new.

mock stub someMesage willReturn: #default.
mock stub someMessage willReturn: 300; use: 3.
mock stub someMessage willReturn: 200; useTwice.
mock stub someMesage willReturn: 100 useOnce.
```

Note that in case of conflict the last defined expectation takes precedence over previously defined ones.

```
mock someMessage should be: 100.

mock someMessage should be: 200.
mock someMessage should be: 200.

mock someMessage should be: 300.
mock someMessage should be: 300.
mock someMessage should be: 300.

mock someMessage should be: #default
```




### Unexpected messages: Automocks


Mock returns another special mock for unexpected messages \(when no expectation is defined for received message\):

```
mock := Mock new.

automock := mock someMessage.

automock should beInstanceOf: MockForMessageReturn.
```

And any message to this mock will produce another automock.
It means that your tests will not fail if you will not define any expectation for your mocks.
It allows you put only required detailes inside your tests which really make sense for tested aspect of functionality. Anything else does not matters.

Also to improve this idea automock try to play role of false in boolean expressions.
```
mock := Mock new.
returnedMock := mock someMessage.

result := returnedMock ifFalse: [ #falseBranch ] ifTrue: [ #trueBranch ].

result should be: #falseBranch.
returnedMock should be: false
```


And play zero in arithmetic
```
mock := Mock new.
returnedMock := mock someMessage.

result := 1 + returnedMock.
result should equal: 1.
returnedMock should equal: 0
```



### Stub group of message sends


There is way to stub multiple message sends at once:
```
mock := Mock new.
rect := 0@0 corner: 2@3.
rect stub.

[ mock someMessage willReturn: 10.
rect width willReturn: 1000 ] should expect.

mock someMessage should be: 10.
rect area should be: 3000.
```

Inside "should expect" block you don't need to send **extra #stub** message to objects

### Verify message sends


With Mocketry you can check that particular object received particular message. Use **"should receive**" expression for this:
```
mock := Mock new.

mock someMessage.

mock should receive someMessage.
mock should not receive anotherMessage
```

You can verify that message was send to real objects. It is not only about mocks:
```
rect := 0@0 corner: 2@3.

rect stub "it should be here to enable message interception"
rect area

rect should receive width. "area = width * height"
```

And you can do this with globals too:
```
DateAndTime stub.
DateAndTime midnight.

DateAndTime should receive now. "inside midnight #now is called"
```

But you should be carefull with globals. Look at section *@stubMessageSends@*.

Also with Mocketry you can verify that message was sent to set of objects.
For example you can verify that message was sent to **ANY object**:
```
mock := Mock new.
rect := 0@0 corner: 2@3.
rect stub.

mock width.
rect area.

Any should receive width. "it will check that mock and rect received message #width"
Any should receive area "it will fail because mock not received #area message".
```

Also you can verify that **ANY message** was sent to particular object:
```
mock := Mock new.

mock someMessage should be: 100.

mock should receive anyMessage.
```

And both variants are supported:
```
mock := Mock new.
rect := 0@0 corner: 2@3.
rect stub.

mock someMessage.

Any should receive anyMessage. "will fail because rect not received any message".

rect width.

Any should receive anyMessage. "will not fail because both objects received at least one message"
```

**Any** class is specific object spec which means "any" object. You can uses any kind of specs to verify message send for set of objects:

```
rect := 0@0 corner: 2@3.
rect stub.

rect area.

rect2 := 0@0 corner: 4@5.
rect2 width.

(Kind of: Rectangle) should receive width. "will not fail because both rect's received message #width"
(Kind of: Rectangle) should receive area "will fail because rect2 not received message #area"

mock := Mock new.
(Kind of: Rectangle) should receive width. "will not fail because mock is not kind of Rectangle"
```


### Verify message sends with arguments


In place of message arguments you can use expected objects itself. Or you can put specifications for expected arguments:
```
mock := Mock new.

(mock messageWith: 10) should be: #ten.
(mock messageWith: 'test' should be: #anyString.

mock should receive messageWith: 10.
mock should receive messageWith: (Instance of: SmallInteger).
mock should receive messageWith: 'test'.
mock should receive messageWith: (Kind of: String).
mock should receive messageWith: [:arg | arg isNumber].
```


### Capture message arguments

Mocketry provides suitable tool to capture arguments of messages for subsequent verification:
```
mock := Mock new.
mock someMessageWith: Arg argName.

mock someMessageWith: #argValue.

Arg argName should be: #argValue.
```


An argument specification can play the role of any object. So it does not restrict message send expectation. Capture will store all received argument values. To verify concrete arguments use message `fromCall:`.
```
Arg argName fromFirstCall should be: #value1.
Arg argName fromLastCall should be: #value3.
(Arg argName fromCall: 2) should be: #value2.
```



Short version:
```
Arg argName should be: #argValue.
```

will signal error if there are multiple different captured values.

Also "should" expression on capture will verify that owner message send occurred required number of times.

When argument is captured it value is stubbed. It allows you to verify subsequent message sends to captured arguments:
```
mock stub someMessageWith: Arg rectangle.

rect := 0@0 corner: 2@3.
mock someMessageWith: rect.
rect area.

Arg rectangle should be: rect.
Arg rectangle should receive width.
```


### Verify message sends count


Mocketry allows one to verify how many times object received particular message:
```
mock := Mock new.

mock someMessage.
mock should receive someMessage once.

mock someMessage.
mock should receive someMessage twice.

mock someMessage.
mock should receive someMessage exactly: 3.
mock should receive someMessage atLeast: 2.
mock should receive someMessage atMost: 3.
mock should receive someMessage atLeast: 1 atMost: 5.
```

Same works to verify that set of objects received particular message expected number of times:
```
mock := Mock new.
mock2 := Mock new.

mock someMessage; someMessage.
mock2 someMessage.

Any should receive someMessage twice. "will fail because mock2 received #someMessage only once"

mock2 someMessage.
Any should receive someMessage twice. "will not fail because both mocks received #someMessage twice"
```


### Verify message send result

There are two ways how to verify result of occurred message:

First you can continue `should receive` expression with `which should` clause to validate actual returned value:
```
rect := 0@0 corner: 2@3.
rect stub.

rect area.

rect should receive area which should equal: 6.
rect should receive width which should beKindOf: Number
```


And you can validate sender message of any object:

```
mock := Mock new.
result := mock someMessage.
result should beReturnedFrom: [ mock someMessage ].
```


### Verify group of message sends


There is way to verify group of message sends at once:

```
mock := Mock new.
rect := 0@0 corner: 2@3.
rect stub.

mock someMessage.
rect area.

[ rect width.
mock someMessage ] should beDone.

[ mock someMessage.
rect width ] should beDoneInOrder.
```


Messages
- **beDone** measn that we don't care about order of message sends.
- **beDoneInOrder** verifies that messages were set in same order as they defined inside given block


### Verify all expectations

There is way how to verify that all defined expectations were occurred:
```
mock1 := Mock new.
mock2 := Mock new.

[ mock1 someMessage. mock2 someMessage2 ]
   should lenient satisfy:
[ mock2 someMessage2.
mock1 someMessage willReturn: 'some' ].
```



- **lenient** means that we don't care about order in which expected messages were happened.

```
mock1 := Mock new.
mock2 := Mock new.

[ mock1 someMessage. mock2 someMessage2 ]
   should strictly satisfy:
[ mock1 someMessage willReturn: 'some'.
mock2 someMessage2 ].
```


- **strictly** means that we want expected messages were happened in same order in which they were defined.


### Conclusion


Mocketry is a powerful system and we hope that it will help you to improve your tests and development. 