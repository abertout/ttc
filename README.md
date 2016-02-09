ttc
=========
**t**ool for the **t**ask **c**lustering is a free sofware to minimize the number of real-time tasks by clustering while keeping the task set schedulable. 
It supports DM and EDF scheduling policies in uniprocessor and multiprocessor partitionned settings. 
Different criteria (called CostFunctions) may be used as objective function when minimizing the number of tasks.

**ttc** implements the work described in<br />
[Antoine Bertout, Julien Forget, and Richard Olejnik. Minimizing a real-time task set through Task Clustering. 
In Proceedings of the 22nd International Conference on Real-Time Networks and Systems (RTNS 2014), Versailles, France, October 2014](http://dl.acm.org/citation.cfm?id=2659820)




## Dependencies

ttc requires Scala 2.11 and 

- scala-xml_2.11
- scala-parser-combinators_2.11
- scalatest_2.11
- breeze 0.11.2

## Usage

This project can be built with sbt 0.13. Code is documented.

```scala
object Main extends App {

  val path = getClass.getResource("/taskSet").getFile /* task set description according to taskSet.ebnf grammar */
  val taskSet: TaskSet = TaskSetParser.taskSetFromFile(path)
  
  /* Uniprocessor */
  val minTaskSet = MonoClustering.greedyBFSClustering(taskSet, DMresponseTimeAnalysis, MinDensity, None)
  
  /* Partitionned multiprocessor */
   val taskSetMultiDep = ByLevel(taskSet, 0.65, 0.50)
   val minFlows = MultiClustering.cluster(taskSetMultiDep, GlobalHeuristic, EDFqPA, MinDensity, EDFqPA, None)
  
}
```

## Licence

**ttc** is under [CeCILL](http://www.cecill.info/index.en.html) licence<br />
CeCILL licence is compatible with GNU GPL

Copyright CNRS - CRIStAL Laboratory - Emeraude Team<br />
contributor: Antoine Bertout (2012-2015)

Copyright Antoine Bertout (2015-2016) 

bertout.antoine@gmail.com

--------------------



Disambiguation : <a href="https://en.wikipedia.org/wiki/TTC_(band)">TTC</a>  is also the name of a french rap band composed by ideal sons-in-law
 
