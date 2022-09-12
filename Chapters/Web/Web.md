## Testing web applications with Parasol_Chapter Contributors:_ Evelyn Cusi and Daniel AparicioDuring the construction of a web application, it is exhausting to have to test the entire flow of the application each time you modify it, and much more as the application grows. This chapter introduce Parasol to allow developers to automate tests with a collection of language specific bindings, giving us the facility to test complex interactions in matter of seconds.Parasol, or also called _Beach Parasol_, is a Smalltalk framework to write functional/acceptance tests using Selenium WebDriver. Through Parasol, you can access to all functionalities of Selenium WebDriver in an intuitive way. Parasol gives a convenient API to access to the WebDrivers of Selenium such as: Chrome, Firefox and others. Actually, Parasol supports Pharo 6.0 and 7.0, also supports GemStone version 3.1, 3.2 and 3.3.Parasol was developed by Johan Brichau and Andy Kellens from Two Rivers.### Getting startedTo load Parasol into your Pharo image, you can execute the following script in Playground:```Metacello new
    baseline: 'Parasol';
    repository: 'github://SeasideSt/Parasol/repository';
    load: 'tests'.```Once this is done you can start ```ZnZincServerAdaptor startOn: 8080.```Since, Parasol uses Selenium WebDriver, you must download Selenium WebDriver and a browser driver, this last one depends on which browser you want to use to test your application.#### Downloading Selenium web driverSelenium server is a project that contains a set of tools and libraries that enable and support the automation of web browsers.Selenium is a Java program, therefore it requires a Java Runtime Environment \(JRE\) 1.6 or a new version to run it.If Java Runtime Environment \(JRE\) is not installed on your system, you can download the JRE from the Oracle website.In this chapter, we use Selenium server 3.141.x to run our examples. You can download it from the official page of Selenium. The name of file should contain the following prefix:```selenium-server-standalone-3.141.x.jar```You may use the following commant to run Selenium server in your computer.```java -Dwebdriver.chrome.driver=chromedriver -jar selenium-server-standalone-3.141.x.jar```Note that for running the previous command you need to configure  java in the PATH \(environmente variable\).#### Browser driverSelenium requires a driver to interact with the web browser. For instance, Chrome requires Chromedriver, which must be installed before the following examples can be run. The table below shows browsers compatible with Parasol and their respective links to download.| Browser Driver | URL || --- | --- || Chrome | https://sites.google.com/a/chromium.org/chromedriver/downloads || Firefox | https://github.com/mozilla/geckodriver/releases || Safari | https://webkit.org/blog/6900/webdriver-support-in-safari-10/ |We use Chromedriver version 80.0.3987.106 to run the chapter examples, but you can use other browser drivers or another version that you want.### First steps with SeleniumThis section describes a simple test wrote using parasol. This section assumes that you have already installed Parasol and Selenium server is already running.#### A first testLet's start easy and assume we want to test if the title of the [http://pharo.org](http://pharo.org) website is correct. For this, first we need to create a class that inherits from `TestCase`.```TestCase subclass: #PharoOrgTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'FT-Parasol'```Then, we create a method called `testTitleOfPharoPage` as  follows:```PharoOrgTest >> testTitleOfPharoPage

	| driver |
	driver := BPRemoteWebDriver withCapabilities: BPChromeOptions new.
	driver get: 'https://pharo.org/'.
	self assert: 'Pharo - Welcome to Pharo!' equals: driver getTitle.
	driver close.```#### Step by step explanationFirst, we create a subclass of `TestCase`, we do not need instance or class variables for now. We placed this subclass is in the FT-Parasol package.```TestCase subclass: #PharoOrgTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'FT-Parasol'```The `testTitleOfPharoPage` method contains a temporary variable that we use to save our instance of the Chrome WebDriver.```| driver |
driver := BPRemoteWebDriver withCapabilities: BPChromeOptions new.```Note that if you want to use another driver, you must change the class `BPChromeOptions` to another browser driver compatible with Parasol.The `get` method loads a given URL and allow you to navigate through the website.The WebDriver will wait that the page is fully loaded before returning the control to the test. If your page loads a large amount of AJAX, then WebDriver may not know when the page has been fully loaded.```driver get: 'https://pharo.org/'.```The next line of our method is an assertion to confirm that the title of the page is equal to: `'Pharo - Welcome to Pharo!'`.```self assert: 'Pharo - Welcome to Pharo!' equals: driver getTitle.```Finally, we close browser window.```driver close.```You may also use the method `quit` instead `close`. The difference is that `quit` will come out of the browser, and `close` will close a tab.Note that the `close` method will close a tab only if there is an open tab, by default most browsers will close completely.#### Improve the structure of your testIn our previous example, we wrote the entire test in one method, however, is a best practice add all the prerequisites of the tests on the `setUp` method and all the steps of cleaning on the ` tearDown ` method.Therefore, to improve the structure of these tests, we first will convert the temporary variable driver into an instance variable.```TestCase subclass: #PharoOrgTest
	instanceVariableNames: 'driver'
	classVariableNames: ''
	package: 'FT-Parasol'```Next, we will place the statements that load the drive the `setUp` method:```PharoOrgTest >> setUp
	super setUp.
	driver := BPRemoteWebDriver withCapabilities: BPChromeOptions new.
	driver get: 'https://pharo.org/'```In the same way, we move the cleaning statements in the ` tearDown ` method:```PharoOrgTest >> tearDown
	super tearDown.
	driver quit```Finally, our test method will be rewrited as follows:```PharoOrgTest >> testTitleOfPharoPage
	self assert: 'Pharo - Welcome to Pharo!' equals: driver getTitle.```Now, if you run this test again, you should behave in the same way as the first version of our test we saw.### Locating elements with Parasol: The basicsIn our tests, we would like to verify also if some particular HTML elements display the information we want to. But before to perform this verification, we first need to find these elements.Parasol uses what are called _Locators_ to find and match the elements of the web page. Parasol has eight locators as shown in the following table.| Locator | Example || --- | --- || ID | findElementByID: 'user' || Name | findElementByName: 'username' || Link Text | findElementByLinkText: 'Login' || Partial Link Text | findElementByPartialLinkText: 'Next' || XPath | findElementsByXPath: '//div\[@id="login"\]/input' || Tag Name | findElementsByTagName: 'body' || Class Name | findElementsByClassName: 'table' || CSS | findElementByCSSSelector: '#login > input\[type="text"\]' |You can use any of them to find the element that you are looking for in your application. The following paragraphs briefly describe how to use a number of these locators.#### Find element by IDThe use of ID is the easiest and probably the safest way to locate an element on HTML. Test scripts that use IDs are less prone to changes in the application.For example, consider the following HTML form:```<form method="get">
    <input id="fullName" name="textInfo" type="text"/>
    <input id="submitButton" type="submit"/>
</form>```We may locate the input field by its ID, as follows:```textField := driver findElementByID: 'fullName'.```If no element has the ID attribute that match with the provided one, a `BPNoSuchElementException` exception will be raised.#### Find element by nameAn HTML element may have an attribute called `name`, they are normally used in the forms such as text fields and selection buttons. The values of the attributes of name are passed to the server when a form is sent. In terms of lower probability of change, the attribute name is probably the second in importance after ID.Considering the previous HTML code of example, you can locate the elements using the `findElementByName` method:```textField := driver findElementByName: 'textInfo'.```#### Find element by link and partial linkIt is possible to find an element based on its link.  You can use this way of location of elements when you know the link text used inside of an anchor tag. With this strategy, the first element with the value of text link that matches with the location will be returned.For instance, we may find the following anchor element:```<a href="https://pharo.org/">Go to Pharo!</a>```The HTML element can be located in the following ways:```linkPharo := driver findElementByLinkText: 'Go to Pharo!'.
linkPharo := driver findElementByPartialLinkText: 'Go to'.```Notice that with `findElementByPartialLinkText` we don't need to give the full text link, just part of it.#### Find element by tag nameFinding elements by a tag name is used when you want to locate an element by the name of its label. However, because there is a limited set of tag names, it is very possible that more than one element with the same name of tag exists, so this locator is not normally used to identify an element, instead, it is more common to use it in chained locations.Consider our previous example form:```<form method="get">
  <input id="inputField" name="textInfo" type="text"/>
  <input id="submitButton" type="submit"/>
</form>```We could locate the input element \(input\) as follows:```input := driver findElementByTagName: 'input'```#### Find element by class nameThe class attribute of a HTML element is used to add style to our pages. And it can be used to identify elements too.In the next example:```<p id="testParagraph" class="testclass1">Bla bla</p>
<a class="testclass2">foobar</a>```We can use any of the following line to locate the _testclass_ elements.```testClassOne := driver findElementByClassName: 'testclass1'.
testClassTwo := driver findElementByClassName: 'testclass2'.```It is common in web applications to use the same class attribute in several elements, so be careful if you try to get the elements by class name.#### Find element by CSS selectorYou can locate an element through the syntax of the CSS selector.By example, the element `p` of the next HTML code:```<p class="content">Hello, how are you?</p>```It could be located like this:```paragraphHello := driver findElementByCSSSelector: 'p.content'```### Finding elements using XPathXPath, the XML Path Language, is a query language for selecting nodes from an XML document. When a browser renders a web page, the HTML contents may be parsed into a DOM tree or similar. Therefore, it is possible to find a specific node in the tree using XPath.  To illustrate how to find a element with XPath we will use the next HTML code as example:```    <div id="testDiv1">
 	<p id="testDiv1p" class="c1"></p>
    </div>
    <div id="testDiv2">
 	<p id="testDiv2p1" class="c2"></p>
 	<p id="testDiv2p2" class="c1"></p>
 	<p id="testDiv2p3" class="c1"></p>
    </div>```Imagine that we need to get the div with ID equal to testDiv2, so we use the next code snippet to get it:```testDiv := driver findElementByXPath: '//div[@id=''testDiv2'']'```For locating elements with XPath, we first need to understand their syntax. Therefore, to use this option, you will first need to learn XPath syntax.Note that XPath is a very powerful way to locate web elements when by id, by name or by link text are not applicable. However, XPath expression are vulnerable to structure changes around the web element, because the path from the root to the target element may change.### Finding multiple elementsThere are some cases in which you can have more than one element with the same attributes.#### Chaining findElement to find a child elementThe following example ```<div id = "div1">
	<input type="checkbox" name = "same" value="on">Same checkbox in Div1</input>
</div>
<div id = "div2">
	<input type="checkbox" name = "same" value="on">Same checkbox in Div2</input>
</div>```In these cases, XPATH can be used, however, there is a simpler way, using nested selectors which is nothing else than locating elements in the result of a previous location.For example the following locates the div entry with id "div2" :```inputOfDiv2 := (driver findElementByID: 'div2') findElementByName: 'same'```#### Multiple elementsAs the name suggests, `findElementsByTagName` returns a list of matching elements. Its syntax is exactly the same as `findElement`, but in plural `findElements`.For example, to get all div of our previous example:```divElements := driver findElementsByTagName: 'div'.```Sometimes, findElement fails due to multiple matching elements on a page, of which one was not aware. findElements will be useful to find them.### Interacting with the elements Until now we saw how to navigate through a URL and how to select elements.Now, our next step is to interact with these elements.You can do different things with these elements depending on their type.Using the following form as an example we will show how to interact with it and its elements.```<html>
<body>
<h1>Sign in</h1>
<form id="loginForm">
    <input name="username" type="text" />
    <input name="password" type="password" />
    <button name="login" type="submit" class= "btn btn-primary">Login</button>
    <a href="forgotPassworsd.html">Do you forgot your password?</a>
    <p class="content">
        "Are you new here?"
        <a href="register.html">Create an account</a>
    </p>
</form>
</body>
<html>```#### Filling text in a text fieldTo fill the username and password fields in this form, we first have to select them, for this, we will use the following code:```name := driver findElementByName: 'username'.
password := driver findElementByName: 'password'```Now it is possible to fill text in these fields as follows:```name sendKeys: 'John'.
password sendKeys: 'xxxxxxx'.```It is possible  to  send the message `sendKeys:` in any element: This makes it possible to validate keyboard shortcuts.However, writing something in a text field does not automatically delete it.Instead, what you write will be attached to what is already there. You can easily delete the content of a text field or text area with the message `clear`.#### Activating links and buttonsAnother very useful action in the navigation of web pages is to click on links and buttons, such action is activated using the message `click` on the selected element.Below is an example of how to use it with our form.```loginButton := driver findElementByName: 'login'.
loginButton click.```### Parasol in actionPrevious sections introduced Parasol features though basic examples. This section applies eveything learned during the chapter to create a number of tests for a small but real website called _Mercury Tours_._Mercury Tours_ is an agency that offers trips. Maybe if you worked with automated web test before, you are familiar with it.#### Setting up testsFirst, we will create a subclass of test called `EPTest`:```TestCase subclass: #EPTest
	instanceVariableNames: 'driver'
	classVariableNames: ''
	package: 'Example-Parasol-Tests'```As we see in previous sections, the instance variable driver represents our browser driver necessary to work with Parasol and its methods.Second, we need to initiallize the driver and load the page, since we need to do this step for all the tests we will place this in the `setUp` method.```EPTest >> setUp
	super setUp.
	driver := BPRemoteWebDriver withCapabilities: BPChromeOptions new.
	driver get: self baseURL

EPTest >> baseURL
	^ 'http://newtours.demoaut.com/'```As you notice, we use `self baseURL` to get the URL. It is not necessary to separate the URL in a method.Finally, we define the `tearDown` method:```EPTest >> tearDown
	driver quit.
	super tearDown```It is important to use the `quit` message to close the browser when a test ends, as we mentioned in second section of this chapter.With the `setUp` and ` tearDown ` method, we are able to do any test on the page.### Testing the page titleOur first test will verify if our URL is the principal URL of web page, which is defined as follows:```EPTest >> testPageEntry
	| title |
	title := driver getTitle.
	self assert: title equals: 'Welcome: Mercury Tours'```Remember that if we run Parasol tests we must first run the Selenium server. With the Selenium server and the test running, we can see that our browser was opened and can see the home of our tested page:![The Mercury Tours WebSite.](figures/newtours width=80&label=newtours)After the web browser is closed, we can see that the test passed because our page have the title _Welcome: Mercury Tours_ in our tab. So it's time to do something more complex.### Testing displayed informationThe Mercury Tours page has a lot of information on its home page. So, why not try to test this information?As an example, we will write a test to verify the information in the table called _specials_.Assume that you want to test that the first row is the flight from Atlanta to Las Vegas and the price is \$398. How do we do that? First step is to know how our page represents this information.Use the developer tools of Chrome browser and select the desired row. We get the following HTML code:```<tr bgcolor="#CCCCC">
    <td width="80%">
    <font face="Arial, Helvetica, sans-serif, Verdana" size="2">
        Atlanta to Las Vegas
    </font>
    </td>
    <td width="20%">
    <div align="right">
    <font face="Arial, Helvetica, sans-serif, Verdana" size="2">
        <b>$398</b>
    </font>
    </div>
    </td>
</tr>```We can see the following details:- The row is inside a table.- The table or the row does not contain an ID or a Class that we can use to find this element.- The CSS or the Tag Name can't help us to find this row.In this special  cases, XPath is a useful locator in this particular case.First, we need to create a subclass of `EPTest`. We are using `EPTest` as a base class for future test classes.This decision give us a number of benefits, for instance, we may find easily all test classes by inspecting the subclasses of `EPTest`, and we can reuse some methods in the subclasses, in particular, the `setUp` and ` tearDown ` methods.Therefore, we define the following class:```EPTest subclass: #EPHomePageTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example-Parasol-Tests'````EPHomePageTest` is a class only dedicated to testing the home page and all elements that are part of this.Taking our HTML code as a base, create a method to get the flight and and its price, then test if the value of these elements is the same as the expected result.The following method give us the expected result, but it contains some things that we can improve.```EPHomePageTest >> testPriceAndInfoForFlightFromAtlantaToLasVegas

self assert: ((driver findElementByXPath: '/html/body/div/table/tbody/tr/td[2]/table/tbody/tr[4]/td/table/tbody/tr/td[2]/table/tbody/tr[2]/td[1]/table[1]/tbody/tr[3]/td/table/tbody/tr[1]/td[1]/font') getText)
     equals: 'Atlanta to Las Vegas'.
self assert: ((driver findElementByXPath: '/html/body/div/table/tbody/tr/td[2]/table/tbody/tr[4]/td/table/tbody/tr/td[2]/table/tbody/tr[2]/td[1]/table[1]/tbody/tr[3]/td/table/tbody/tr[1]/td[2]/div/font/b') getText)
    equals: '$398'```We will refactor this method to make it more understandable. We will add a method to get the controller element.```EPHomePageTest >> descriptionFlightFromAtlantaToLasVegas
	^ driver findElementByXPath: '/html/body/div/table/tbody/tr/td[2]/
	table/tbody/tr[4]/td/table/tbody/tr/td[2]/table/tbody/tr[2]/td[1]/
	table[1]/tbody/tr[3]/td/table/tbody/tr[1]/td[1]/font'

EPHomePageTest >> priceFlightFromAtlantaToLasVegas
	^ driver findElementByXPath: '/html/body/div/table/tbody/tr/td[2]/
	table/tbody/tr[4]/td/table/tbody/tr/td[2]/table/tbody/tr[2]/
	td[1]/table[1]/tbody/tr[3]/td/table/tbody/tr[1]/td[2]/div/font/b'```Then, we may now refactor the test method as follows:```EPHomePageTest >> testFlightFromAtlantaToLasVegas
	self 
		assert: self descriptionFlightFromAtlantaToLasVegas getText
		equals: 'Atlanta to Las Vegas'.
	self 
		assert: self priceFlightFromAtlantaToLasVegas getText
		equals: '$398'```We test a part of the home page, if you want to test other parts, follow the previous steps and try to construct multiple tests.The tests above are a good example of how we get elements from a HTML page and test if the displayed page contains the correct information we want to display.### Testing interactionsWe will now show how to test interactions. It's an important step to test, so we will explain how you could you do it.In this example, we will create a test that registers a user in the website.First, we will create another subclass of `EPTest` called `EPRegisterUserTest`.```EPTest subclass: #EPRegisterUserTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example-Parasol-Tests'```In this website to register a user we need first click the _REGISTER_ button in the bar menu.Therefore, we will create a method that emulates a click in the _REGISTER_ button.```EPRegisterUserTest >> clickInRegisterButton
	(driver findElementByLinkText: 'REGISTER') click```We use this method in our `setUp` method as follows:```EPRegisterUserTest >> setUp
	super setUp.
	self clickInRegisterButton```With this little change, the first step that each test will perform is to move to the _REGISTER_ form.You may see the _REGISTER_ form in Figure *@registerForm@*.![Registration From](figures/registerForm width=80&label=registerForm)Now, we are prepared to create tests in `EPRegisterUserTest`.Our first test is to verify if we are in the correct page when we click on the register button. So, create the test.```EPRegisterUserTest >> testEntryToRegisterPage
	| title |
	title := driver getTitle.
	self assert: title equals: 'Register: Mercury Tours'```In this page we have a registration form with multiple fields.If we want to register on the page we must complete three sections in the form.Contact, mailing and user information, so we define three tests to complete these sections.As you remember, the first step is to see how the page render the elements in the form. For this, we inspect the HTML code using the browser tools.The following code shows the contact information HTML code:```<tr>
<td align=right>
<font face="Arial, Helvetica, sans-serif" size="2">
<b>First Name: </b>
</font>
</td>
<td>
<input maxlength=60 name="firstName" size="20">
</td>
</tr>
 <tr>
<td align=right>
<font face="Arial, Helvetica, sans-serif" size="2">
<b>Last Name: </b>
</font>
</td>
<td>
<input maxlength=60 name="lastName" size="20">
</td>
</tr>
<tr>
<td align=right>
<font face="Arial, Helvetica, sans-serif" size="2">
<b>Phone:</b>
</font>
</td>
<td>
<input maxlength="20" name="phone" size="15">
</td>
</tr>
<tr>
<td align=right>
<font face="Arial, Helvetica, sans-serif" size="2">
<b>Email:</b>
</font>
</td>
<td>
<input name="userName" id="userName" size="35" maxlength="64">
</td>
</tr>```We can draw the following conclusions from the HTML code:- All inputs have a name or an ID.- All forms are in a table.With these conclusions we can start with the tests.The test `firstNameField` tests whether the correct information is entered into the contact information section.Like our previous methods, we need to create multiple methods to get the fields elements to put some information into them.At following we show one method that returns the field with the name `firstName`.```EPRegisterUserTest >> firstNameField
	^ driver findElementByName: 'firstName'```You need to create as many of these you need to get all the form fields you need for your test.The next step is create a method to set information to fields.We use the message `sendKeys` to set the information that we want.```EPRegisterUserTest >> fillInformationToContactSection
	self firstNameField sendKeys: 'Jhon'.
	self lastNameField sendKeys: 'Stuart'.
	self phoneField sendKeys: '10276123'.
	self emailField sendKeys: 'example@mail.com'```Finally, we create the following test to verify if the introduced information is the same as the fields.```EPRegisterUserTest >> testIntroduceInformationInContactSection
	self fillInformationToContactSection.
	self assert: (self firstNameField getAttribute: 'value') equals: 'Jhon'.
	self assert: (self lastNameField getAttribute: 'value') equals: 'Stuart'.
	self assert: (self phoneField getAttribute: 'value') equals: '10276123'.
	self assert: (self emailField getAttribute: 'value') equals: 'example@mail.com'```If we run the test we can see how Parasol introduces the information to the fields \(see Figure *@contactInfo@*\).![Filling fields using Parasol.](figures/contactInfo width=80&label=contactInfo)Now, we can create tests to fill information to other sections. An important observation is the select box in mailing information section.If we want to test it, we need to use another method to assert it.In this case, the selected option contains the attribute called `selected`. So, we define the next method to get this element:```EPRegisterUserTest >> getSelectedOfCountry
	(self countryField findElementsByTagName: 'option') do: [ :each |
		(each getAttribute: 'selected')
			ifNotNil: [ ^ each ] ]```When you create the test, with this method you only get the element that contain the information that we need.So we use the `getText` message and assert if the fields contain the correct information.```EPRegisterUserTest >> testIntroduceInformationInMailingSection
	self assert: (self getSelectedOfCountry getText) equals: 'UNITED STATES '.```It is also posible change the value of the combo box using the `sendKeys` message as we saw in previous examples.Now, with all fields tested, we define a test to the user registration on the page.If you created the `fillInformationToMailingSection` and `fillInformationToUserSection` methods, we will use these methods.If you didn't, you need to create these methods.We need another method to find the submit button and click it, so we create it.```EPRegisterUserTest >> clickInSubmitButton
	(driver findElementByName: 'register') click```In the page, when you register a user you obtain the following view:![Registration successful view.](figures/endRegistration width=80&label=endRegistration)We have two options to assert if the user is registered:- Get the text 'sign-off' of sign-off option- Get the description text below the register title.You can use any of these options, as an example we will get the description text. So define the method to find the description text.```EPRegisterUserTest >> descriptionText
	^ driver findElementByXPath: '/html/body/div/table/tbody/tr/td[2]/
table/tbody/tr[4]/td/table/tbody/tr/td[2]/table/tbody/tr[3]/td/p[2]/font'```And finally create the test to verify if the user is registered successfully.```EPRegisterUserTest >> testRegistrationOfUser
	<timeout: 10>
	self sendInformationToContactSection.
	self sendInformationToMailingSection.
	self sendInformationToUserSection.
	self clickInSubmitButton.
	self 
		assert: (self descriptionText getText)
		equals: 'Thank you for registering.
You may now sign-in using the user name
and password you''ve just entered.'```Sometimes this test can fail because the page needs to save the new user and load the successfully message.Another problem that can cause the test failure is the time it takes for our Selenium server to use the browser driver.So we use the timeout pragma to try to avoid this, but you can also use the `Delay` class.If you don't have this problem, delete the line.Finally run the test and observe its result. If the test passed, you are now able to create multiple tests using Parasol. ### ConclusionThe purpose of this chapter was to introduce the basics of Parasol to create a test suite.You should now be able to find elements in a website, fill information in fields, and interact with the website throught links and buttons.