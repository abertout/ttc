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

package main.scala.taskmodel

import main.scala.utils.AllImplicits._
import main.scala.utils.Numbers

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


case class TaskSet(set: Seq[Task], tasksAndSuccs: Option[Map[Task, Set[Task]]] = None) {


  lazy val uFactor: Double = initUtilizationFactor
  lazy val nPeriods: Int = initNbOfPeriods
  lazy val periodsLCM: BigInt = initPeriodsLCM
  val adjMap: (Map[Task, Int], Map[Int, Task]) = initAdjMap()
  val adjMatrix: Vector[Vector[Int]] = initAdjMatrix(tasksAndSuccs)


  require(!set.containsDuplicate, "The task set should contains no duplicate")
  require(set.nonEmpty, "The task set can not be empty")

  def size = set.length

  override def toString: String = {
    "TaskSet[set("+set.mkString("(",", ",")")+"), Dep("+tasksAndSuccs.getOrElse(Map.empty[Task, Set[Task]]).map{
      case (task,succs) => s"${task.name} -> ${succs.map(succ => succ.name).mkString("(",",",")")}"
    }.mkString(", ")+")]"
  }

  /* Get the processor utilisation factor */
  private def initUtilizationFactor: Double = set.view.map(task => if(task.c == 0) 0 else task.c / task.t.toDouble) sum


  override def equals(other: Any) = other match {
    case that: TaskSet => this.set.toSet == that.set.toSet && this.tasksAndSuccs == that.tasksAndSuccs
    case _ => false
  }


  def equalContent(other: Any) = {

    @tailrec def eqTaskSeq(taskSeq1: Seq[Task], taskSeq2: Seq[Task]): Boolean = {
      if(taskSeq1.length != taskSeq2.length) return false
      if(taskSeq1.isEmpty) return true
      if(!taskSeq1.head.equalContent(taskSeq2.head)) return false
      eqTaskSeq(taskSeq1.tail, taskSeq2.tail)
    }
    other match {
      case that: TaskSet =>
        val ordThatSet = that.set.sortBy(_.name)
        val ordThisSet = this.set.sortBy(_.name)
        val sameContent = eqTaskSeq(ordThatSet, ordThisSet)
        sameContent && this.tasksAndSuccs == that.tasksAndSuccs

      case _ => false
    }
  }


  private def initAdjMap(): (Map[Task, Int], Map[Int, Task]) = {
    val adjMapTaskToIdx = set.view.zipWithIndex.toMap
    val adjMapIdxToTask = adjMapTaskToIdx.map(_.swap)
    (adjMapTaskToIdx,adjMapIdxToTask)
  }

  private def initAdjMatrix(dep: Option[Map[Task, Set[Task]]]): Vector[Vector[Int]] = {
    val adjMatrix: Vector[Vector[Int]] = dep match {
      case Some(relations) =>
        val array = Array.ofDim[Int](set.length * set.length)
        relations.foreach{ case(task, succs) =>
          val idTask = idByTask(task)
          succs.foreach(succ => {
            val idSucc = idByTask(succ)
            array(idTask * set.length + idSucc) = TaskSet.EDGE
          })
        }
        array.toVector.grouped(set.length).toVector
      case None => Vector.fill(set.length, set.length)(TaskSet.EMPTY)
    }
    adjMatrix
  }

  private def initPeriodsLCM: BigInt = {
    val periods: Seq[BigInt] = set map (task => BigInt(task.t))
    Numbers.getLCM(periods.distinct:_*)
  }

  private def initNbOfPeriods: Int = set.map(_.t).view.distinct.length

  /**
    * Computes set of direct predecessors
    * @param task task
    * @return direct predecessors of the task
    */
  def directPreds(task: Task): Seq[Task] = {
    val idTask = idByTask(task)
    for(row <- adjMatrix.indices if adjMatrix(row)(idTask) == TaskSet.EDGE)
      yield taskById(row)
  }

  /**
    * Computes set of direct successors
    * @param task task
    * @return direct successors of the task
    */
  def directSuccs(task: Task): Seq[Task] = {
    val idTask = idByTask(task)
    for(col <-  adjMatrix.indices if adjMatrix(idTask)(col) == TaskSet.EDGE)
      yield  taskById(col)
  }


  private def idByTask(task: Task): Int = {
    adjMap._1.get(task) match {
      case Some(idx) => idx
      case None => throw new NoSuchElementException(s"$task")
    }
  }
  private def taskById(idx: Int): Task = {
    adjMap._2.get(idx) match {
      case Some(task) => task
      case None => throw new NoSuchElementException(s"$idx")
    }
  }


  def directPredRelation(tauA: Task, tauB: Task): Boolean = adjMatrix(idByTask(tauA))(idByTask(tauB)) == TaskSet.EDGE

  def directSuccRelation(tauA: Task, tauB: Task): Boolean = adjMatrix(idByTask(tauB))(idByTask(tauA)) == TaskSet.EDGE


  /**
    * Return tasks without any successor
    * @return the set of tasks without any successor
    */
  def tasksWithoutSucc(): Set[Task] = {
    val taskWithSuccsMap = tasksAndSuccs.getOrElse(Map[Task, Set[Task]]())
    set.toSet.diff(taskWithSuccsMap.keySet)
  }

  /**
    * Indicates if tasks are independent
    * @param tauI first task
    * @param tauJ second task
    * @return true if the two tasks have no predecessor and no successor
    */
  def isolatedTask(tauI: Task, tauJ: Task): Boolean = {
    directPreds(tauI).isEmpty && directSuccs(tauI).isEmpty && directPreds(tauJ).isEmpty && directSuccs(tauJ).isEmpty
  }

  /**
    * Removes seq of tasks
    * @param setOfTasks set of tasks to remove
    * @return resulting taskSet
    */
  def remove(setOfTasks: Task*): TaskSet = {
    val withoutKeysDep: Map[Task, Set[Task]] = (tasksAndSuccs.getOrElse(Map.empty) -- setOfTasks).map{
      case (task, succs) => (task, succs.diff(setOfTasks.toSet))
    }
    val nextDep = withoutKeysDep.filterNot(_._2.isEmpty)
    val nextSet: Seq[Task] = set.filterNot(task => setOfTasks.contains(task))
    if(nextSet.isEmpty) throw new IllegalArgumentException("You can not remove tasks in such way to create an empty taskSet")
    TaskSet(nextSet, Some(nextDep))
  }

  /**
    * Return the set of all absolute deadlines (no duplicate) in increasing order of a set of tasks between 0 and supBound
    * @param supBound maximum time bound
    * @return the set of absolute deadlines
    */
  def absDeadlines(supBound: BigInt): Seq[Int] = {

    val sortedSet = this.set.sortWith(_.d < _.d)

    var setOfAbsDl = new ArrayBuffer[Int]()
    val curentAbsDl: Array[Int] = Array.ofDim(size)
    var firstMin = sortedSet.head.o + sortedSet.head.d
    var firstMinIdx = 0

    for (i <- 0 until size){
      curentAbsDl(i) = sortedSet(i).o + sortedSet(i).d
      if(curentAbsDl(i) < firstMin){
        firstMin = curentAbsDl(i)
        firstMinIdx = i
      }
    }

    setOfAbsDl += firstMin
    curentAbsDl(firstMinIdx) += sortedSet(firstMinIdx).t

    var minAbsDl = 0
    while (minAbsDl < supBound) {
      var minAbsDlIdx = 0
      minAbsDl = curentAbsDl(0)
      for (i <- 0 until size) {
        if (curentAbsDl(i) < minAbsDl) {
          minAbsDlIdx = i
          minAbsDl = curentAbsDl(i)
        }
      }
      if(minAbsDl != setOfAbsDl.last && minAbsDl <= supBound) {
        setOfAbsDl += minAbsDl
      }
      curentAbsDl(minAbsDlIdx) += sortedSet(minAbsDlIdx).t
    }
    setOfAbsDl
  }

  /**
    * Restricts taskSet to the given tasks
    * @param setOfTasks set of tasks the taskSet to be restricted to
    * @return restricted TaskSet
    */
  def restrictedTo(setOfTasks: Task*): TaskSet = {
    val diff: Seq[Task] = this.set.toSet.diff(setOfTasks.toSet).toSeq
    remove(diff:_*)
  }

}
object TaskSet {
  val EDGE = 1
  val EMPTY = 0
}