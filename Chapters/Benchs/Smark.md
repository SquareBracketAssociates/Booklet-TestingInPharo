## Performance testing with SMark

Measuring performance is not an easy activity. 
It involves many considerations that you need to take into account such as the pertinence of what you want to measure, how you can isolate it from other computations, ... 
Indeed an execution may be affected by different factors, for instance, the hardware you have, the VM you are using, the run-time optimizations that the VM perform while your program is running, the unpredictable garbage collection, among many others. 
All these factors combined make it difficult to have a good estimation of the performance of an execution.

This chapter introduces SMark, a  testing framework for benchmarking Pharo applications.
SMark is developed and maintained by Stephan Marr.
SMark encapsulates a number of benchmarking best practices that are useful to take into account these factors and help us to have a decent execution time estimation.


### Installing SMark

You can install SMark with the following code snippet:

```
Metacello new
    baseline: 'SMark';
    repository: 'github://smarr/SMark';
    load.
```


The examples in this chapter were performed in Pharo 8.

### Measuring execution time

Measuring execution time needs to be done carefully, otherwise, we can get a very far estimation of it.
To illustrate the extent of this situation, we will use the recursive Fibonacci function as a subject under analysis.
Define a new class named `Math` and the following method: 
 
```
Math >> fib: n
  ^n <= 1 
  	ifTrue: [ n ] 
	ifFalse: [ (self fib: (n - 1)) + (self fib: (n - 2)) ]
```


We now will measure how many seconds take to compute the 40th Fibonacci, using the message `timeToRun` method of the BlockClosure class as follows:

```
[ Math new fib: 40 ] timeToRun
```


The method `timeToRun` basically records the system clock time before and after the execution of the code block. 
We execute this script 5 times, you may find the results of each execution in the following lines:

```
1 execution: 1091 milliseconds
2 execution: 1046 milliseconds
3 execution: 1098 milliseconds
4 execution: 1069 milliseconds
5 execution: 1085 milliseconds
```


As you can see, even in this small example, there is a variation between the results in the five iterations.
Even though the measurements were done on the same computer, in the same Pharo Image. 
What happened? As we mentioned in the introduction the VM and the Hardware perform plenty of activities at the moment to execute a piece of code, and some of these activities are not deterministic, for instance, without going too far, some collections like the class Set that save objects without a defined \(or let say, deterministic\) order.

For this reason, it is necessary to consider a number of good practices to measure execution time, and minimize the measurement bias and uncertainty.
For instance, execute the benchmarks a number of times before starting our measurements to let the VM perform the dynamic optimizations.
Another recommendation is to execute multiple times the benchmark, use the media as a point of reference, and consider the error margin.
These recommendations may vary depending on the VM you are using, and the characteristics of your benchmark.
For instance, it is well known that time measurements have more variations in micro-benchmarks than macro-benchmarks.

But, don’t worry, SMark takes many of these recommendations into account to help you in executing your benchmarks and get a decent execution time estimation.

### A first benchmark in SMark

SMark has been designed to run benchmarks in a similar fashion to we run tests. 
Therefore, their creation is quite similar too. 
First, we need to create a subclass of the class `SMarkSuite`.

```
SMarkSuite << #MyBenchSuite
	package: 'SMark-Demo'
```


Then, we need to write the piece of code in which we are interested to measure the execution time \(the benchmark\), in our case, we will use our Fibonacci benchmark that we introduced in the previous section.

```
MyBenchSuite >> benchFib40
	self fib: 40.
```


To execute our benchmark, we only need to execute the following script in a playground.

```
MyBenchSuite runOnly: #benchFib40
```


If you print the script answer it shows a small report like this one:
```
Report for: MyBenchSuite
Benchmark Fib40
Fib40 total: iterations=1 runtime: 1099ms
```


As you can see, the execution time is similar to the ones we got in the previous section, but now we can use SMark to execute multiple times the benchmark and get a better time estimation.

```
MyBenchSuite runOnly:#benchFib40 iterations: 25
```


And we got the following report:
```
Report for: MyBenchSuite
Benchmark Fib40
Fib40 total: iterations=25 runtime: 1022.7ms +/-1.5
```


There you go, now we have the average execution time and error margin, the error margin measured with a 90% confidence degree assuming that the variable distribution is normal.

### Setup and teardown 


Similarly to SUnit, SMark offers two hook methods to define the actions to be done before and after a benchmark is executed. 



### SMark benchmark runners


The way that SMark will run benchmarks depends on the Smark runner you use. 
There are five different runners: 
- `SMarkRunner`
- `SMarkAutosizeRunner`, 
- `SMarkCogRunner`, 
- `SMarkProfileRunner`, and 
- `SMarkWeakScalingRunner`.


Each runner provides a particular way to perform the benchmarks. 
You may specify which runner you want to use by overriding  the following method.

```
SMarkSuite class >> defaultRunner
  ^self onCog: [ SMarkCogRunner ] else: [ SMarkRunner ]
```


The message `defaultRunner` returns a CogRunner or a normal Runner depending on the VM on which the benchmark is executed. The next subsection describes each one of these runners.

**SMarkRunner.** It is the standard way to run a benchmark provided by SMark. `SMarkRunner` only performs the benchmark N times, where N is the number of iterations defined by the user, as we saw in the previous section. It records the time measurements of each execution to build a report.

**SMarkCogRunner.** It adds warning up instructions before executing the benchmark. `SMarkCogRunner` executes a benchmark twice before taking the measurements. The first execution is to execute the inline cache optimization done by the VM. The second execution is to trigger the JIT compiler to produce code. Once these two executions are performed the benchmark is executed N times similarly as SMarkRunner does.

**SMarkAutosizeRunner.** It increases the execution time of the benchmark to reach a target time, this is used mostly with micro-benchmarks. The default target time is 300 milliseconds. Therefore, if a benchmark takes less than 300 milliseconds to run, this runner will measure how many times this benchmark needs to be executed to meet the target time. Once the benchmark execution time meets the target time, this runner executes this increased benchmark N iterations to take the time measurements similar to SMarkRunner.

**SMarkProfileRunner.** It automatically opens and uses Pharo standard time profiler to monitor the execution of all the benchmarks in the suite. Then, the time profilers show the call context tree of this execution reporting which methods were executed by the benchmarks, and how much time each method consumes.

**SMarkWeakScallingRunner.** It is specific to platforms with support for real parallelism. Weak scaling is defined as how the solution time varies with the number of processors for a fixed problem size per processor.


### Benchmark suites

Having a good benchmark already packaged and identified is important to be able to compare changes for example in the compiler or other aspects of execution.
SMark provides  four benchmarks suites:

- _SMarkCompiler_, a  benchmark that measures the time needed to compile a regular-size method.
- _SMarkLoops_, a set of microbenchmarks measuring a number of basic aspects such as message send, instance field access, and array access cost.
- _SMarkSlopstone_, a Smalltalk Low-level OPeration Stones Portable Low-Level Benchmarks for ST80 and ST/V \(using 16-bit SmallIntegers\) - Placed in public domain January 1993 \(c\) Bruce Samuelson Permission is given to place this in public Smalltalk archives.
- _SMarkSmopstone_, a Smalltalk Medium level OPeration Stones Portable Medium level Benchmarks for ST80 and ST/V \(using 16-bit SmallInts\) Placed in public domain January 1993  \(c\) Bruce Samuelson Permission is given to place this in public Smalltalk archives.


You may run any of these benchmark suites by performing the class method run of any of the previous classes. For instance:
```
SMarkLoops run
```


### Result reports

SMark provides an option to export the benchmarking results using the class `SMarkSimpleStatisticsReporter`. 
For instance, consider the following example:

```
| stream result textReport|
result := SMarkLoops run.
stream := TextStream on: String new.
SMarkSimpleStatisticsReporter reportFor: result on: stream.
testReport := stream contents.
```


It runs the benchmark suite SMarkLoops and uses the class `SMarkSimpleStatisticsReporter` to export the results in a stream, in this particular case on a TextStream. 
This report is particularly useful to integrate SMark in a continuous integration environment.

### Conclusion


This chapter introduces SMark, a framework to help you define, execute and report your benchmarks.
 It provides different ways to run your benchmarks, collect the results, and perform a simple statistical analysis to measure the error margin. 
 In addition, SMark provides four benchmark suites that are useful to test the performance of a number of core functionalities in Pharo.
