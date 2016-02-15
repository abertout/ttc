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

package main.scala.scheduling

import main.scala.taskmodel.{TaskSet, Task}


trait DMSchedTest extends SchedTest with SchedTestMethod{
  override def RTA(taskSet: TaskSet): Boolean = DMresponseTimeAnalysis(taskSet)
  override def suffTest(taskSet: TaskSet): Boolean = DMsufficientSchedulabilityTest(taskSet)
  override def toString: String = "DM"
}

object DMresponseTimeAnalysis extends DMSchedTest with ResponseTimeAnalysis{

  /**
   * Implements Response Time Analysis for DM (exact test with pseudo-polynomial complexity) without saving intermediate
   * results that will be recomputed
   * Joseph, M., & Pandya, P. (1986). Finding response times in a real-time system. The Computer Journal, 29(5), 390-395.
   * Audsley, N. C., Burns, A., Richardson, M. F., & Wellings, A. J. (1990). Deadline monotonic scheduling. University of York, Department of Computer Science.
   * @param taskSet task set
   * @return
   */
  def apply(taskSet: TaskSet): Boolean = {
    //Sort task set by increasing deadline order as noticed in DM
    val newTaskSet = new TaskSet(set = taskSet.set.sortWith((t1, t2) => t1.d < t2.d), taskSet.tasksAndSuccs)
    var task, task2: Task = null
    var lastRTValue, newRTValue, responseTime: Int = 0
    var fixedPointReached: Boolean = false

    for (i <- newTaskSet.set.indices) {
      if(newTaskSet.set(i).d < 0) return false //because encoding can create deadline smaller to zero
      task = newTaskSet.set(i)
      lastRTValue = task.c
      newRTValue = task.c

      while(!fixedPointReached){
        for (j <- 0 until i) {
          task2 = newTaskSet.set(j)
          var value =  math.ceil(lastRTValue.toFloat / task2.t.toFloat).toInt * task2.c
          newRTValue +=  value
        }
        if(newRTValue > task.d){
          return false
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
      task.r = Some(responseTime)
    }
    true
  }
}



object DMsufficientSchedulabilityTest extends DMSchedTest{
  /**
   * Implements Audsly sufficient schedulability test in O(n) complexity
   * Audsley, N. C., Burns, A., Richardson, M. F., & Wellings, A. J. (1990). Deadline monotonic scheduling. University of York, Department of Computer Science.
   * @param taskSet task set
   * @return true if the task set is schedulable considering the sufficient test
   */

  def apply(taskSet: TaskSet): Boolean = {
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
        return false
      intf = 0
      test = 0
    }
    true
  }
}
