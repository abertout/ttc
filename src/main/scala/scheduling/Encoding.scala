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

import main.scala.taskmodel.{Task, TaskSet}
import main.scala.utils.MapUtils

import scala.annotation.tailrec
import scala.collection.immutable.Queue


object Encoding {

  /**
    * Return an ordered queue of a tasks with equal period in topologic order with task without predecessor first
    * Do not handle task with extended precedences
    * @param taskSet task set
    * @return
    */

  def topologicSort(taskSet: TaskSet): Queue[Task] = {
    val withoutSucc: Queue[Task] = Queue(taskSet.tasksWithoutSucc().toSeq:_*)
    val tasksAndPreds = taskSet.set.map(t => (t, taskSet.directPreds(t).toSet)).toMap
    val tasksAndSuccs = taskSet.tasksAndSuccs.getOrElse(Map.empty[Task, Set[Task]])
    topo(withoutSucc, tasksAndSuccs, tasksAndPreds,Queue.empty[Task]).reverse
  }

  @tailrec private def topo(queue: Queue[Task], succs: Map[Task, Set[Task]], preds: Map[Task, Set[Task]], sortedTask: Queue[Task])
  : Queue[Task] = {
    if(queue.isEmpty)
      sortedTask
    else{
      val (task,q) = queue.dequeue
      val newSortedTask = sortedTask.enqueue(task)
      val predecessors = preds(task)
      val newPreds = preds - task
      val newSuccs = succs.map(kv => (kv._1, kv._2 - task))
      val qq = for(p <- predecessors if newSuccs(p).isEmpty ) yield p
      topo(q.enqueue(qq), newSuccs, newPreds, newSortedTask)
    }
  }


  /**
    *  Encodes task set with precedence constraints to task set without by adjusting deadlines and offset
    *  If the initial task set with precedences is schedulable, the encoded task set is also schedulable  and reciprocally
    *  Proved to be valid for EDF  in
    *  Chetto, H., Silly, M., & Bouchentouf, T.
    *  (1990). Dynamic scheduling of real-time tasks under precedence constraints. Real-Time Systems, 2(3), 181-194.
    *  Proved that holds also for DM in
    *  Forget, J., Boniol, F., Grolleau, E., Lesens, D., & Pagetti, C.
    *  (2010, April). Scheduling dependent periodic tasks without synchronization mechanisms. In Real-Time and Embedded Technology and Applications Symposium (RTAS), 2010 16th IEEE (pp. 301-310). IEEE.
    * @param taskSet task set
    * @return encoded task set
    */
  def predsEncoding(taskSet: TaskSet): TaskSet = {
    val tasksByP = taskSet.set.groupBy(_.t).values
    val taskSetsByP = tasksByP.map { l =>
      val subDepMap = MapUtils.filterMap(l, taskSet.tasksAndSuccs.getOrElse(Map.empty)).filter(_._2.nonEmpty)
      TaskSet(l,
        if(subDepMap.nonEmpty) Some(subDepMap) else None
      )
    }


    @tailrec def encDl(dMap: Map[Task, Int], taskSet: TaskSet, queue: Queue[Task]): Map[Task, Int] = {
      if(queue.isEmpty)
        dMap
      else{
        val (task, q) = queue.dequeue
        val succs = taskSet.directSuccs(task)
        val succMin =  if(succs.nonEmpty) succs.map(succ => dMap(succ) - succ.c).min else Int.MaxValue
        val min = math.min(dMap(task), succMin)
        encDl(dMap + ((task, min)), taskSet, q)
      }
    }

    @tailrec def encO(oMap: Map[Task, Int], taskSet: TaskSet, queue: Queue[Task]): Map[Task, Int] = {
      if(queue.isEmpty)
        oMap
      else{
        val (task, q) = queue.dequeue
        val preds = taskSet.directPreds(task)
        val predMax = if(preds.nonEmpty) preds.map(pred => oMap(pred) + pred.c).max else Int.MinValue
        val max = math.max(oMap(task), predMax)
        encO(oMap + ((task, max)), taskSet, q)
      }
    }


    val encTaskSets = taskSetsByP.map(t => {
      val dlMap: Map[Task, Int] = t.set.map(task => (task, task.d)).toMap
      val oMap: Map[Task, Int] = t.set.map(task => (task, task.o)).toMap
      val encodedDlMap = encDl(dlMap, t, Encoding.topologicSort(t).reverse)
      val encodedOMap = encO(oMap, t, Encoding.topologicSort(t))
      for(task <- t.set) yield task.copy(d = encodedDlMap(task), o = encodedOMap(task))
    })

    TaskSet(encTaskSets.flatten.toSeq, taskSet.tasksAndSuccs)
  }




}
