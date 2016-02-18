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

package main.scala.clustering

import main.scala.taskmodel.{Task, TaskSet}
import scala.collection.mutable


class ClusteringCompanion(taskSet: TaskSet) {

  private val closedAdjMatrix: Vector[Vector[Int]] = initAdjMatrix

  /**
    * Fuse two tasks with the chosen deadline type
    * @param task1 first task
    * @param task2 second task
    * @param dl deadline of the cluster
    * @return a taskset with the two tasks grouped together
    */
  def fusion(task1: Task, task2: Task, dl: ClusterDeadline.Value): TaskSet = {
    val compoundTask: Task = dl match {
      case ClusterDeadline.DlMin =>
        if(task1.d < task2.d)
          Task(task1.name + ClusteringCompanion.taskSep + task2.name, task1.c + task2.c, task1.d, task1.t, 0, None)
        else
          Task(task2.name + ClusteringCompanion.taskSep + task1.name, task1.c + task2.c, task2.d, task2.t, 0, None)

      case ClusterDeadline.DlMax =>
        if(task1.d > task2.d)
          Task(task1.name + ClusteringCompanion.taskSep + task2.name, task1.c + task2.c, task1.d, task1.t, 0, None)
        else
          Task(task2.name + ClusteringCompanion.taskSep + task1.name, task1.c + task2.c, task2.d, task2.t, 0, None)

      case ClusterDeadline.DlPred =>
        if(taskSet.directPredRelation(task1, task2))
          Task(task1.name + ClusteringCompanion.taskSep + task2.name, task1.c + task2.c, task1.d, task1.t, 0, None)
        else
          Task(task2.name + ClusteringCompanion.taskSep + task1.name, task1.c + task2.c, task2.d, task2.t, 0, None)

      case ClusterDeadline.DlSucc =>
        if(taskSet.directPredRelation(task1, task2))
          Task(task2.name + ClusteringCompanion.taskSep + task1.name, task1.c + task2.c, task2.d, task2.t, 0, None)
        else
          Task(task1.name + ClusteringCompanion.taskSep + task2.name, task1.c + task2.c, task1.d, task1.t, 0, None)

    }

    val deps: Option[Map[Task, Set[Task]]] = this.taskSet.tasksAndSuccs match {
      case Some(oldMap) =>
        val newMap = new mutable.HashMap[Task, Set[Task]]()
        val oldSetTask1 =  if (oldMap.isDefinedAt(task1)) Some(oldMap(task1)) else None
        val oldSetTask2 =  if (oldMap.isDefinedAt(task2)) Some(oldMap(task2)) else None
        val oldSet = oldSetTask1.getOrElse(Set.empty) ++ oldSetTask2.getOrElse(Set.empty)
        val newSet = oldSet-(task1, task2)
        if(newSet.nonEmpty)
          newMap += ((compoundTask, newSet))
        oldMap.foreach(kv => {
          val (task, set) = kv
          if(task != task1 && task!= task2){
            if(set.contains(task1) || set.contains(task2))
              newMap += ((task, set-(task1, task2) + compoundTask))
            else newMap += ((task, set))
          }
        })
        Option(newMap.toMap)

      case None => None
    }
    TaskSet(taskSet.set.filter(task => task != task1 && task != task2).+:(compoundTask), deps)
  }

  /**
    * Check if two tasks can be put on the same cluster
    * @param task1,first task
    * @param task2 second task
    * @return true if yes
    */
  def regroupable(task1: Task, task2: Task): Boolean = {
    val tEquals = task1.t == task2.t
    val idx1 = taskSet.adjMap._1(task1)
    val idx2 = taskSet.adjMap._1(task2)
    task1.t == task2.t && closedAdjMatrix(idx1)(idx2) != ClusteringCompanion.ADDED_EDGE
  }


  /**
    * Performs the transitive closure of a DAG (Warshall algorithm in O(n^^3))
    * @param m adjacency matrix
    * @return
    */
  private def transitiveClosure(m: Vector[Vector[Int]], addedEgdeVal: Int = ClusteringCompanion.ADDED_EDGE): Vector[Vector[Int]] = {
    val m2:Array[Array[Int]] = m.map(_.toArray).toArray

    for(k <- m.indices;i <- m.indices;j <- m.indices) {
      if((m2(i)(k) == ClusteringCompanion.EDGE || m2(i)(k) == addedEgdeVal) &&
        (m2(k)(j) == ClusteringCompanion.EDGE || m2(k)(j) == addedEgdeVal)){
        if(m2(i)(j) == ClusteringCompanion.EMPTY) m2(i)(j) = addedEgdeVal
      }
    }
    m2.map(_.toVector).toVector
  }

  /**
    * Performs the transitive reduction of a DAG
    * @param m adjacency matrix
    * @return
    */
  private def transitiveReduction(m: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    val mClosed: Vector[Vector[Int]] = transitiveClosure(m, ClusteringCompanion.EDGE)
    val mReduced:Array[Array[Int]] = mClosed.map(_.toArray).toArray

    for(i <- mReduced.indices; j <- mReduced.indices)
      if(mReduced(i)(j) == ClusteringCompanion.EDGE)
        for(k <- mReduced.indices)
          if (mClosed(j)(k) == ClusteringCompanion.EDGE && mReduced(i)(k) != ClusteringCompanion.EMPTY)
            mReduced(i)(k) = ClusteringCompanion.EMPTY

    mReduced.map(_.toVector).toVector
  }

  private def initAdjMatrix: Vector[Vector[Int]] = {
    val mtx = transitiveReduction(taskSet.adjMatrix)
    transitiveClosure(mtx)
  }

}

object ClusteringCompanion {

  val EMPTY = 0
  val EDGE = 1
  val ADDED_EDGE = 2
  val taskSep = "_"
}