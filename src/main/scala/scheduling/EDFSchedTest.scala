/*
 * Copyright  (c) CNRS - CRIStAL Laboratory - Emeraude Team
 * contributor: Antoine Bertout (2012-2015)
 * Copyright  (c)  Antoine Bertout (2015-2016)
 *
 * bertout.antoine@gmail.com
 *
 * This software is a computer program whose purpose is to minimize the
 * number of tasks of a real-time system by clustering.
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 */

package ttc.scheduling

import ttc.taskmodel.{Task, TaskSet}
import ttc.utils.Numbers
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


trait EDFSchedTest extends SchedTest with SchedTestMethod{
  override def RTA(taskSet: TaskSet): (Boolean, TaskSet) = EDFresponseTimeAnalysisSpuri(taskSet)
  override def suffTest(taskSet: TaskSet): Boolean = EDFsufficientTestDevi(taskSet)._1
  override def toString: String = "EDF"




  /**
    *  Implementation of the cumulative request bound function (rbf) that quantifies the maximum cumulative execution requests by jobs of a set of tasks within interval t
    *
    * @param taskSet the set of tasks
    * @param t the time interval from 0 to t
    * @return
    */
  def requestBoundFunction(taskSet: TaskSet, t: Int): Int = taskSet.set.view.map (task => requestBoundFunction(task, t)) sum

  /**
    * Implementation of the request bound function (rbf) that quantifies the maximum cumulative execution requests by jobs of a task within interval t
    * BEFORE The formula orginally works for interval [0,t) then we add 1 to consider [0,t]
    *
    * @param task the task
    * @param t the time interval from 0 to t
    * @return
    */
  def requestBoundFunction(task: Task, t: Int): Int = task.c * math.ceil((t + 1 + task.o) / task.t.toDouble) toInt


  //Versions sans offset
  def synchronousRbf(task: Task, t: Int): Int = (task.c * math.ceil((t + 1) / task.t.toDouble)) toInt
  def synchronousRbf(taskSet: TaskSet, t: Int): Int = taskSet.set.view.map (task => synchronousRbf(task, t)) sum


  /**
    * Implementation of the demand bound function (dbf)  that quantifies the maximum cumulative execution requests by jobs of a set of tasks respecting their absolute deadlines within interval t
    * Baruah, S. K., Mok, A. K., & Rosier, L. E. (1990, December). Preemptively scheduling hard-real-time sporadic tasks on one processor. In Real-Time Systems Symposium, 1990. Proceedings., 11th (pp. 182-190). IEEE.
    *
    * @param taskSet the set of tasks
    * @param t the time interval from 0 to t
    * @return the demand bound function for the given interval t
    */
  def demandBoundFunction(taskSet: TaskSet, t: Int): Int = taskSet.set.view.map (task => demandBoundFunction(task, t)) sum

  /**
    * Implementation of the demand bound function (dbf)  that quantifies the maximum cumulative execution requests by jobs of a task respecting its absolute deadlines within interval t
    * Baruah, S. K., Mok, A. K., & Rosier, L. E. (1990, December). Preemptively scheduling hard-real-time sporadic tasks on one processor. In Real-Time Systems Symposium, 1990. Proceedings., 11th (pp. 182-190)
    *
    * @param task the task
    * @param t  the time interval from 0 to t
    * @return
    */
  def demandBoundFunction(task: Task, t: Int): Int = task.c * math.max(math.floor((t + task.t - task.d) / task.t.toDouble), 0) toInt


  /**
    * Implementation of the mixed bound function (mbf) that quantifies the maximum cumulative execution request by job within interval t2 having absolute deadline within t1 (t2 >= t1 >= 0)
    * Guan, N., & Yi, W. (2014, March). General and efficient response time analysis for EDF scheduling. In Proceedings of the conference on Design, Automation & Test in Europe (p. 255). European Design and Automation Association.** @param taskSet the set of tasks
    *
    * @param t1 the first interval (the largest)
    * @param t2 the second interval (the shortest)
    * @return the mixed bound function for the given interval t
    */
  def mixedBoundFunction(taskSet: TaskSet, t1: Int, t2: Int): Int = {
    if((t1 < t2) || (t1 < 0) || (t2 < 0)) throw new IllegalArgumentException
    taskSet.set.view.map(task => math.min(demandBoundFunction(task, t1), synchronousRbf(task, t2))) sum
  }

  protected def maxDinf(taskSet: TaskSet, t: Int): Int = {
    var dMin = 0
    var dj = 0
    for(task <- taskSet.set){
      if (task.d < t){
        dj = math.floor((t - task.d) / task.t.toFloat).toInt * task.t + task.d
        if (dj == t)
          dj -= task.t
        if (dj > dMin)
          dMin = dj
      }
    }
    dMin
  }

  /**
    * Implementation of the demand function (df) that quantifies the amount of time demanded by the task in the interval [t1, t2)
    * Baruah, S. K., Mok, A. K., & Rosier, L. E. (1990, December). Preemptively scheduling hard-real-time sporadic tasks on one processor. In Real-Time Systems Symposium, 1990. Proceedings., 11th (pp. 182-190)
    *
    * @param task the task
    * @param t1 start of the interval included
    * @param t2 end of the interval excluded
    * @return
    */
  def demandFunction(task: Task, t1: Int, t2: Int): Int = math.max((math.floor((t2 - task.o - task.d) / task.t.toDouble) - math.ceil((t1 - task.o) / task.t.toDouble) + 1) * task.c toInt, 0)

  /**
    * Implementation of the demand function (df) that quantifies the amount of time demanded by a task set in the interval [t1, t2)
    * Baruah, S. K., Mok, A. K., & Rosier, L. E. (1990, December). Preemptively scheduling hard-real-time sporadic tasks on one processor. In Real-Time Systems Symposium, 1990. Proceedings., 11th (pp. 182-190)
    *
    * @param taskSet the set of tasks
    * @param t1 start of the interval included
    * @param t2 end of the interval excluded
    * @return
    */
  def demandFunction(taskSet: TaskSet, t1: Int, t2: Int) : Int = taskSet.set.view.map(task => demandFunction(task, t1, t2)) sum

  protected def minimalDistance(tauI: Task, tauJ: Task) : Int = {
    val periodsGcd = Numbers.gcd(tauI.t, tauJ.t)
    tauJ.o - tauI.o + math.ceil((tauI.o - tauJ.o) / periodsGcd.toDouble).toInt * periodsGcd
  }

}



object EDFresponseTimeAnalysisGuan extends EDFSchedTest with ResponseTimeAnalysis {
  /**
    *Implements Response Time Analysis for EDF (exact test)
    *Made for periodic ressource model then here sbf(bound,bound,x) = x and inverseSbf(bound, bound, x) = x because we consider full ressource model
    *Only for synchronous task set
    *Guan, N., & Yi, W. (2014, March). General and efficient response time analysis for EDF scheduling.
    *In Proceedings of the conference on Design, Automation & Test in Europe (p. 255). European Design and Automation Association.
    *
    * @param taskSet taskSet
    * @return a tuple with a boolean (true if the system is schedulable false otherwise) and a taskSet(with response times if the system is schedulable and the initial one otherwise)
    */
  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {
    if (taskSet.uFactor > 1) return (false, taskSet)

    val sortedTaskSet = TaskSet(taskSet.set.sortWith(_.d < _.d),taskSet.tasksAndSuccs ) //sort by non-decreasing deadline
    val slackTimes = new ArrayBuffer[Int](sortedTaskSet.size)
    val worstCaseSlackTimes = new ArrayBuffer[Int](sortedTaskSet.size)
    var outTaskSet = ArrayBuffer[Task]()


    //Use later to compute delta values (all abs deadlines)
    val allCurrentAbsDl: Array[Int] = Array.ofDim(sortedTaskSet.size)
    var firstMin = sortedTaskSet.set.head.d
    var firstMinIdx = 0

    for(i <- 0 until sortedTaskSet.size){
      if (sortedTaskSet.set(i).d < 0) return (false, taskSet) //because encoding can create deadline smaller to zero

      //init with very large values (theoretically infinity)
      slackTimes += Int.MaxValue
      worstCaseSlackTimes += -1

      allCurrentAbsDl(i) = sortedTaskSet.set(i).d  //Use to compute delta values (all abs deadlines)
      if(allCurrentAbsDl(i) < firstMin){
        firstMin = allCurrentAbsDl(i)
        firstMinIdx = i
      }
    }
    val maximumBound = maxBusyPeriodSize(sortedTaskSet) + sortedTaskSet.set.last.d
    var delta = 0

    delta = firstMin
    var i = 0
    while(delta < maximumBound) {

      /* Trick in the case of equal relative deadlines */

      var idxEq = 1
      while (((i + idxEq) < sortedTaskSet.set.size && (i + 1 + idxEq) < sortedTaskSet.set.size) &&
        (sortedTaskSet.set(i + idxEq).d == sortedTaskSet.set(i + 1 + idxEq).d)) {
        idxEq += 1
      }

      var oldI = i
      if ((i + idxEq) < sortedTaskSet.set.size && delta >= sortedTaskSet.set(i + idxEq).d){
        oldI += 1
        i += idxEq
      }

      for (j <- oldI to i ) {
        if ((delta - pseudoInverseSbf(delta, delta, demandBoundFunction(sortedTaskSet, delta))) < slackTimes(j)) {
          var gOld: Int = 0
          val mbf = try {
            mixedBoundFunction(sortedTaskSet, delta, 0)
          } catch {
            case _: Throwable => return (false, taskSet)
          }
          var gNew: Int = pseudoInverseSbf(delta, delta, mbf)
          while (gNew != gOld) {
            gOld = gNew
            val mbf = try {
              mixedBoundFunction(sortedTaskSet, delta, gOld)
            } catch {
              case _: Throwable => return (false, taskSet)
            }
            gNew = pseudoInverseSbf(delta, delta, mbf)
          }
          slackTimes(j) = math.min(slackTimes(j), delta - gNew)
        }
      }

      //Find the next absolute deadlines
      var minAbsDlIdx = 0
      var minAbsDl = delta
      while (minAbsDl == delta){
        minAbsDl = Int.MaxValue
        for (i <- 0 until sortedTaskSet.size) {
          if (allCurrentAbsDl(i) < minAbsDl) {
            minAbsDlIdx = i
            minAbsDl = allCurrentAbsDl(i)
          }
        }
        allCurrentAbsDl(minAbsDlIdx) += sortedTaskSet.set(minAbsDlIdx).t
      }
      delta = minAbsDl
    }

    for(j <- sortedTaskSet.size - 1 to 0 by -1){
      worstCaseSlackTimes(j) = slackTimes(j)
      sortedTaskSet.set(j).r = Some(sortedTaskSet.set(j).d - worstCaseSlackTimes(j))
      outTaskSet += sortedTaskSet.set(j).copy(r =  sortedTaskSet.set(j).r)
      if(j > 0)
        slackTimes(j - 1) = math.min(slackTimes(j), slackTimes(j - 1))
    }
    val finalTaskSet = TaskSet(outTaskSet, taskSet.tasksAndSuccs)
    (!outTaskSet.exists(task => task.r.getOrElse(0) > task.d || task.r.getOrElse(0) < 0), finalTaskSet)
  }

  /**
    * Implementation of the pseudo-inverse supply bound function  for a periodic ressource
    * Give the maximal interval length needed for a certain amount of capacity x
    *
    * @param period period of the ressource
    * @param allocationTime allocation time of the ressource
    * @return  maximal interval length required
    *
    */
  def inverseSbf(period: Int, allocationTime: Int, x: Int): Int = { //((x + period - allocationTime) * (math.ceil(x / allocationTime.toDouble) + 1)).toInt
    if(period == allocationTime) //If y unit of time are available at each date y then all interval x is available
      return x
    val eps =
      if(x - allocationTime * math.floor(x / allocationTime.toDouble) > 0)
        period - allocationTime + x - allocationTime * math.floor(x / allocationTime.toDouble)
      else 0
    ((period - allocationTime) + period * math.floor(x / allocationTime.toDouble) + eps).toInt
  }

  /**
    * Implementation of the pseudo-inverse supply bound function  for a periodic ressource
    * Give the minimal interval length needed for a certain amount of capacity x
    *
    * @param period period of the ressource
    * @param allocationTime allocation time of the ressource
    * @return  minimal interval length required
    */
  def pseudoInverseSbf(period: Int, allocationTime: Int, x: Int): Int = inverseSbf(period, allocationTime, x)

  /**
    * Compute the maximum busy period size for EDF from definition given by Spuri
    *
    * @param taskSet taskSet
    * @return the maximum busy period size
    */
  def maxBusyPeriodSize(taskSet: TaskSet): Int = {
    var fixedPointReached: Boolean = false
    var lastWiValue, newWiValue: Int = 0

    var l: Int = 0
    val l0: Int = taskSet.set.map( _.c ) sum

    lastWiValue = l0

    while(!fixedPointReached){

      for (task <- taskSet.set)
        newWiValue +=  math.ceil(lastWiValue / task.t.toFloat).toInt * task.c

      if(lastWiValue == newWiValue){
        fixedPointReached = true
        l = newWiValue
      }
      else{
        lastWiValue = newWiValue
        newWiValue = 0
      }
    }
    l
  }
}


object EDFresponseTimeAnalysisSpuri extends EDFSchedTest with ResponseTimeAnalysis{
  /**
    *  Implements Response Time Analysis for EDF  of Spuri with only the synchronous case (exact test with exponential complexity)
    *  Spuri, M. (1996). Analysis of deadline scheduled real-time systems.
    *  Does not compute all the response times to tasks if the taskSet is not schedulable. It stops before.
    *
    * @param taskSet task set
    * @return
    */
  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {

    val l = maxBusyPeriodSize(taskSet)
    var lastRTValue,newRTValue, responseTime,maxResponseTime, intf: Int = 0
    var fixedPointReached: Boolean = false
    var outTaskSet = ArrayBuffer[Task]()


    //C'est par tâche par a
    for (task <- taskSet.set) {
      if(task.d < 0) return (false, taskSet) //because encoding can create deadline smaller to zero

      for (a <- 0 to l - task.c){
        lastRTValue = 0

        val s = startTime(a,task)

        //init Li_0
        for (task2 <- taskSet.set){
          if (task2 != task && (task2.d <= a + task.d)){

            lastRTValue += task2.c
          }
        }
        intf = if (s == 0) 1 else 0
        lastRTValue += intf * task.c

        //compute Li_+1
        while(!fixedPointReached){
          newRTValue = workload(a, lastRTValue, task, taskSet,s)


          if(lastRTValue == newRTValue)
            fixedPointReached = true
          else{
            lastRTValue = newRTValue
            newRTValue = 0
          }
        }
        fixedPointReached = false
        responseTime = math.max(task.c, newRTValue - a)

        if (responseTime > maxResponseTime)
          maxResponseTime = responseTime
      }
      outTaskSet += task.copy(r = Some(maxResponseTime))
      if(maxResponseTime > task.d)
        return (false, taskSet)
      maxResponseTime = 0
    }
    (true, TaskSet(outTaskSet, taskSet.tasksAndSuccs))
  }

  /**
    * Compute the maximum busy period size for EDF from definition given by Spuri
    *
    * @param taskSet task set
    * @return
    */
  private def maxBusyPeriodSize(taskSet: TaskSet): Int = {
    var fixedPointReached: Boolean = false
    var lastWiValue, newWiValue: Int = 0

    var l: Int = 0
    val l0: Int = taskSet.set.map( _.c ) sum

    lastWiValue = l0

    while(!fixedPointReached){

      for (task <- taskSet.set)
        newWiValue +=  math.ceil(lastWiValue / task.t.toFloat).toInt * task.c

      if(lastWiValue == newWiValue){
        fixedPointReached = true
        l = newWiValue
      }
      else{
        lastWiValue = newWiValue
        newWiValue = 0
      }
    }
    l
  }
  private def workload(a: Int, t: Int, task: Task, taskSet: TaskSet, s: Int): Int = {
    var wl = 0
    for (task2 <- taskSet.set){
      if (task2 != task && task2.d <= a + task.d ){
        wl += math.min(math.ceil(t / task2.t.toFloat).toInt, 1 + math.floor((a + task.d - task2.d) / task2.t.toFloat).toInt) * task2.c
      }
    }
    wl + delta(a, t, task, s) * task.c
  }

  private def startTime(a:Int, task: Task): Int =  a - math.floor(a / task.t.toFloat).toInt * task.t

  private def delta(a:Int, t :Int, task:Task, s:Int): Int = {
    if(t > s){
      math.min(math.ceil((t - s)/task.t.toFloat).toInt, 1 + math.floor(a / task.t.toFloat).toInt)
    }else 0
  }
}

object EDFsufficientTestDevi extends EDFSchedTest {

  /**
    * Implements Devi sufficient schedulability test in O(n) complexity
    * Devi, U. C. (2003, July). An improved schedulability test for uniprocessor periodic task systems. In Real-Time Systems, 2003. Proceedings. 15th Euromicro Conference on (pp. 23-30). IEEE.
    *
    * @param taskSet task set
    * @return
    */

  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {


    var task: Task = null
    var test: Float = 0
    var test2: Float = 0

    //Sort task set by increasing deadline order as noticed in DM
    val newTaskSet = TaskSet(taskSet.set.sortWith((t1, t2) => t1.d < t2.d))

    for (i <- newTaskSet.set.indices) {
      for (j <- 0 to i) {
        task = newTaskSet.set(j)
        test += task.c / task.t.toFloat
        test2 += ((task.t - math.min(task.t, task.d)) / task.t.toFloat) * task.c
      }

      val finaltest = test + 1 / newTaskSet.set(i).d.toFloat * test2
      if (finaltest > 1)
        return (false, taskSet)

      test = 0
      test2 = 0
    }
    (true, taskSet)
  }
}


object EDFqPA extends EDFSchedTest {

  /** Implement QPA algorithm from Zhang et Burns that provide an exact boolean test
    * Zhang, F., & Burns, A. (2009). Schedulability analysis for real-time systems with EDF scheduling. Computers, IEEE Transactions on, 58(9), 1250-1258.
    *
    * @param taskSet task set
    * @return
    */
  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {

    //Step 1,compute utilization factor (note that it won't change in our case cause we do not modify periods)
    if (taskSet.uFactor > 1) return (false, taskSet)

    //Step 2, compute L_a upper bound, we must check later that that L_b is not always better with relative deadlines
    var la: Int = 0
    var maxD = 0
    var maxRightSide: Float = 0

    for (task <- taskSet.set) {
      if (task.d > maxD)
        maxD = task.d
      maxRightSide += (task.t - task.d) * (task.c / task.t.toFloat)
    }
    la = math.max(maxD, (maxRightSide / (1 - taskSet.uFactor).toFloat).toInt)

    //Step 3, compute L_b upper bound,
    var lb: Int = 0
    var fixedPointReached: Boolean = false
    var lastWiValue, newWiValue: Int = 0

    val w0: Int = taskSet.set.view.map(task => task.c) sum

    lastWiValue = w0

    while (!fixedPointReached) {

      for (task <- taskSet.set) {
        newWiValue += math.ceil(lastWiValue / task.t.toFloat).toInt * task.c
      }
      if (lastWiValue == newWiValue) {
        fixedPointReached = true
        lb = newWiValue
      }
      else {
        lastWiValue = newWiValue
        newWiValue = 0
      }
    }

    //Step 4 Compute h(t) with minimum L , with relative dl
    val l = math.min(la, lb)
    val Dmin = taskSet.set.view.map(task => task.d) min
    var t = 0

    //Main algo
    t = maxDinf(taskSet, l)
    var ht = demandBoundFunction(taskSet, t)

    while (ht <= t && ht > Dmin) {
      if (ht < t) t = ht
      else t = maxDinf(taskSet, t)
      ht = demandBoundFunction(taskSet, t)
    }

    if (ht <= Dmin)
      return (true, taskSet)
    (false, taskSet)
  }

}


object EDFasynchronousSufficientTest extends EDFSchedTest {
  /**
    * Implements feasability analysis for asynchronous tasks (sufficient schedulability test for EDF) with one fixed task
    * Pellizzoni, R., & Lipari, G. (2005). Feasibility analysis of real-time periodic tasks with offsets. Real-Time Systems, 30(1-2), 105-128.
    *
    * @param taskSet task set
    * @return
    */
  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {
    if (taskSet.uFactor > 1) return (false, taskSet)
    for (i <- taskSet.set.indices) {
      val copiedTaskSet = TaskSet(set = taskSet.set.map(_.copy()))
      val syncFixedTask = copiedTaskSet.set(i)
      val asynchFixedTask = Task(syncFixedTask.id, syncFixedTask.c, syncFixedTask.d, syncFixedTask.t, 0)
      val tmpNewTaskSet = for (task <- copiedTaskSet.set if task != asynchFixedTask)
        yield Task(task.id, task.c, task.d, task.t, minimalDistance(asynchFixedTask, task), task.r)
      val newTaskSet = TaskSet(set = tmpNewTaskSet :+ asynchFixedTask)
      var lastLStarValue = 0
      var lStar = asynchFixedTask.c

      do {
        lastLStarValue = lStar
        lStar = newTaskSet.set.view.map(task => math.max(math.ceil((lastLStarValue - task.o) / task.t.toDouble), 0) * task.c toInt).sum
      } while (lastLStarValue != lStar)
      var l = 0

      val allCurrentAbsDl: Array[Int] = Array.ofDim(newTaskSet.set.size)
      var firstMin = newTaskSet.set.head.o + newTaskSet.set.head.d
      var firstMinIdx = 0


      //init array of abs deadlines
      for (i <- newTaskSet.set.indices) {
        allCurrentAbsDl(i) = newTaskSet.set(i).o + newTaskSet.set(i).d
        if (allCurrentAbsDl(i) < firstMin) {
          firstMin = allCurrentAbsDl(i)
          firstMinIdx = i
        }
      }

      l = firstMin
      allCurrentAbsDl(firstMinIdx) += newTaskSet.set(firstMinIdx).t

      while (l <= lStar) {
        //println(l+","+demandFunction(newTaskSet, 0, lStar))
        if (demandFunction(newTaskSet, 0, l) > l)
          return (false, taskSet)

        //Find the next absolute deadlines
        var minAbsDlIdx = 0
        var minAbsDl = l

        while (minAbsDl == l) {
          minAbsDl = Int.MaxValue
          for (i <- newTaskSet.set.indices) {
            if (allCurrentAbsDl(i) < minAbsDl) {
              minAbsDlIdx = i
              minAbsDl = allCurrentAbsDl(i)
            }
          }
          allCurrentAbsDl(minAbsDlIdx) += newTaskSet.set(minAbsDlIdx).t
        }
        l = minAbsDl
      }
    }
    (true, taskSet)
  }
  
}

