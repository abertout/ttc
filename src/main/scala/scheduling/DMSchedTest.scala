/*
 * Copyright (c) CNRS - CRIStAL Laboratory - Emeraude Team
 * contributor: Antoine Bertout (2012-2015)
 * Copyright (c) Antoine Bertout (2015-2016)
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

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


trait DMSchedTest extends SchedTest with SchedTestMethod{
  override def RTA(taskSet: TaskSet): (Boolean, TaskSet) = DMresponseTimeAnalysis(taskSet)
  override def suffTest(taskSet: TaskSet): Boolean = DMsufficientSchedulabilityTest(taskSet)._1
  override def toString: String = "DM"
}

object DMresponseTimeAnalysis extends DMSchedTest with ResponseTimeAnalysis{

  /**
    * Implements Response Time Analysis for DM (exact test with pseudo-polynomial complexity) without saving intermediate
    * results that will be recomputed
    * Joseph, M., & Pandya, P. (1986). Finding response times in a real-time system. The Computer Journal, 29(5), 390-395.
    * Audsley, N. C., Burns, A., Richardson, M. F., & Wellings, A. J. (1990). Deadline monotonic scheduling. University of York, Department of Computer Science.
    *
    * @param taskSet task set
    * @return
    */
  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {
    //Sort task set by increasing deadline order as noticed in DM
    val newTaskSet = new TaskSet(set = taskSet.set.sortWith((t1, t2) => t1.d < t2.d), taskSet.tasksAndSuccs)
    var task, task2: Task = null
    var lastRTValue, newRTValue, responseTime: Int = 0
    var fixedPointReached: Boolean = false
    var outTaskSet = ArrayBuffer[Task]()

    for (i <- newTaskSet.set.indices) {
      if(newTaskSet.set(i).d < 0) return (false, taskSet) //because encoding can create deadline smaller to zero
      task = newTaskSet.set(i)
      lastRTValue = task.c
      newRTValue = task.c

      while(!fixedPointReached){
        for (j <- 0 until i) {
          task2 = newTaskSet.set(j)
          var value =  math.ceil(lastRTValue / task2.t.toFloat).toInt * task2.c
          newRTValue +=  value
        }
        if(newRTValue > task.d){
          return (false, taskSet)
        }

        if(lastRTValue == newRTValue)
          fixedPointReached = true
        else{
          lastRTValue = newRTValue
          newRTValue = task.c
        }
      }
      fixedPointReached = false
      responseTime = newRTValue
      outTaskSet += task.copy(r = Some(responseTime))
    }
    (true, TaskSet(outTaskSet, taskSet.tasksAndSuccs))
  }
}

object DMAsyncResponseTimeAnalysis extends DMSchedTest with ResponseTimeAnalysis {

  def hepTasks(taskSet: TaskSet, task: Task): Seq[Task] = taskSet.set.filter(other => other.d <= task.d)
  def hpTasks(taskSet: TaskSet, task: Task): Seq[Task] = taskSet.set.filter(other => other.d < task.d)

  def stabiliationTime(setOfTasks: Seq[Task], task: Task): Int = {
    require(task.o == 0, s"hypothesis is made that offset of $task is equal to 0")
    (math.ceil(setOfTasks.maxBy(_.o).o / task.t.toDouble) * task.t).toInt
  }

  def arrivalIn(task: Task, start: Int, end: Int): Seq[(Int, Int)] = {

    @tailrec
    def arrivalInAux(task: Task, start: Int, end: Int, currArr: Int, seq: Seq[(Int,Int)]): Seq[(Int,Int)] = {
      if(currArr >= end) return seq
      val newSeq = if(currArr >= start && currArr < end) seq.+:(task.c,currArr) else seq
      arrivalInAux(task,start,end,currArr + task.t, newSeq)
    }
    arrivalInAux(task,start,end,task.o,Seq.empty)

  }



  def remainingItf(taskSet: TaskSet, task: Task, t: Int, l: Int, tmin: Int): Int = {

    if(t == 0) return 0
    val initTime = if(t == tmin) 0 else t - task.t + task.d
    val beta = hpTasks(taskSet, task).flatMap(tau => arrivalIn(tau, initTime, t))
    //println("t=",t)
    val sortedBeta = (if(l > 0) beta.+:((l,initTime)) else beta).sortBy(_._2)
    //println(sortedBeta)


    val (time,remTime) = sortedBeta.foldLeft((initTime, 0)){
      case ((currTime, currRem),(c,tr)) =>
        val upRt = if(tr > currTime + currRem) 0 else currRem + c
        (tr, upRt)
    }
    val newRt = remTime - (t - initTime)
    if(newRt < 0) 0 else newRt
  }

  def createdItf(taskSet: TaskSet, task: Task, t: Int, r:Int): (Int,Int) = {
    val firstNextFree = r + t
    val eta = hpTasks(taskSet, task).flatMap(tau => arrivalIn(tau, t + r, t + task.d)).sortBy(_._2)
    //println(eta)
    val(_, totalCreated,k) = eta.foldLeft((firstNextFree,r,0)){
      case((currNextFree,currTotalCreated,currK),(c,tr)) =>
        val nf = if(currNextFree < tr) tr else currNextFree
        val newK = currK + math.min(t + task.d - nf, c)
        val newNextFree = math.min(t + task.d, nf + c)
        (newNextFree, currTotalCreated + c, newK)
    }
    val nextLt = totalCreated - k - math.max(task.d, r)
    (k,nextLt)
  }


  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {
    //val interval = BigInt(taskSet.set.maxBy(_.o).o) until + 2 * taskSet.periodsLCM by 1
    val sortedTaskSet = TaskSet(set = taskSet.set.sortWith((t1, t2) => t1.d >= t2.d), taskSet.tasksAndSuccs)
    val bufferSchedTasks = ArrayBuffer[Task]()

    for(task <- sortedTaskSet.set){
      val encodedSetOfTasks = Encoding.audleyOffsetAdjusting(sortedTaskSet.set,task)
      val encodedTaskSet = TaskSet(encodedSetOfTasks)
      //println("currTask",task)
      //println("enc",encodedTaskSet)
      val encodedTask = encodedTaskSet.set.last
      val s = stabiliationTime(encodedTaskSet.set, encodedTask)
      val p = encodedTaskSet.periodsLCM
      val interval = BigInt(s) until (s + p) by 1
      var t = interval.min.toInt
      var Lti = 0
      var rMax = 0
      while(t < interval.max){
        val r = remainingItf(encodedTaskSet, encodedTask, t, Lti, interval.min.toInt)
        val (k,lt) = createdItf(encodedTaskSet,encodedTask, t, r)
        val rt = encodedTask.c + r + k
        //println(task,rt)
        if(rt > encodedTask.d)
          return (false,taskSet)
        rMax = math.max(rMax, rt)
        t += encodedTask.t
        Lti = lt
      }
      bufferSchedTasks += task.copy(r = Some(rMax))
    }
    (true,TaskSet(set = bufferSchedTasks, taskSet.tasksAndSuccs))
  }

}

object DMsufficientSchedulabilityTest extends DMSchedTest{
  /**
    * Implements Audsly sufficient schedulability test in O(n) complexity
    * Audsley, N. C., Burns, A., Richardson, M. F., & Wellings, A. J. (1990). Deadline monotonic scheduling. University of York, Department of Computer Science.
    *
    * @param taskSet task set
    * @return true if the task set is schedulable considering the sufficient test
    */

  def apply(taskSet: TaskSet): (Boolean, TaskSet) = {
    //Sort task set by increasing deadline order as noticed in DM
    val newTaskSet = new TaskSet(set = taskSet.set.sortWith((t1, t2) => t1.d < t2.d))

    var task, task2: Task = null
    var intf, test: Float = 0

    for (i <- newTaskSet.set.indices) {
      task = newTaskSet.set(i)
      for (j <- 0 until i) {
        task2 = newTaskSet.set(j)
        intf += math.ceil(task.d.toFloat / task2.t.toFloat).toFloat * task2.c
      }
      test = task.c / task.d.toFloat + intf / task.d.toFloat

      if (test > 1)
        return (false, taskSet)
      intf = 0
      test = 0
    }
    (true, taskSet)
  }
}
