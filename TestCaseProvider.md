##What's this ?##

This change introduces a new attribute for tests called "TestCaseProvider". This way, a user can pass the needed
data manually and dynamically to the framework, to create
test cases.

##Why ?##

Well, typing the test cases can be an annoying work. Especially, if the data is more or less the same. Also, the overview within the source isn't that good. So the idea came up, to deliver the test data from outside the framework.

##Ok, how does this work ?##

It's done as simple as possible. So a user has to do 3 Steps.

1. The Provider-class
2. Registering
3. The Attribute

The first step, is to write and implement a class derived from
TTestDataProviderBase. This class has to provider 5 Methods

The constructor, called only once, to initialize the data to provide. The destructor for clean up works.

The GetCaseAmount function, that is called for each test function, with the TestCaseProvider-Attribute. The result of the function has to be the amount of test cases, that has to be created.

The GetCaseName function, that is also called for each test function, with the TestCaseProvider-Attribute. The result has to be the name of the cases. A numbering is automatically append to this. 

Both functions getting the name of the test function as parameter, so you do not need to create different class for each. 

At last, the GetCaseParams function, that is called for each test case. It has to provide the parameters for calling the test function as Array of TValue. The given parameters have to be in the same order, as defined in the test function.
As Parameters, the function gets the name of the test function and
the number of the case, for which it needs the parameters. 

When this is done, the class has to be registered.

To do so, just call

TestDataProviderManager.RegisterProvider(Name,Class)

within the initialization section of the provider class. 
Name is just a unique name, to identify the specific provider class. Class is the class, that you just defined. 

From this time on, the TestDataProviderManager can create an instance of the class, so it can create test cases.

The last step, is simply use this class. To do so, just define a TestCaseProvider-Attribute above your test function, with the registered name as parameter.

##And then ?##

The rest, is done internally and works like the normal Test case-Attribute. In compare to that, it just calls the GetCaseAmount and GetCaseName function, to get the amount and desired name, then loop from 0 to amount-1 and calls the GetCaseParams function, to get the data. With this, it creates the test case for that function.


##Finally##

This way, the user gets more flexibility on providing the test data to the tests. The user can get the data out of a wide
range of sources (files, database....). When the user has to create a large amount of test cases, this way should be easier. 