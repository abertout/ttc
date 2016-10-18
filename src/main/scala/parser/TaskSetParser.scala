/*
 * Copyright (c) CNRS - CRIStAL Laboratory - Emeraude Team
 * contributor: Antoine Bertout (2012-2015)
 * Copyright (c)  Antoine Bertout (2015-2016)
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

package ttc.parser

import java.io.{FileNotFoundException, FileReader}
import ttc.taskmodel.{Task, TaskSet}
import scala.util.parsing.combinator.JavaTokenParsers

abstract class Stmt
case class Comment(s: String) extends  Stmt
case class PredDecl(p: (String, List[String])) extends Stmt
case class TaskDecl(t : Task) extends Stmt

object TaskSetParser extends JavaTokenParsers{

  def taskSetDecl: Parser[List[Stmt]] =  rep1sep(taskSetStmt, sep) ^^ (List() ++ _)

  def sep: Parser[String] = """[,;\t\r\n\s\f]+""".r |""

  def taskSetStmt: Parser[Stmt] = taskDecl | precedDecl | comment

  def taskId: Parser[String] = """[A-Za-z0-9_]+""".r ^^ ( _.toString )

  def precedDecl: Parser[PredDecl] =  onePrecedDecl | nPrecedDecl

  def onePrecedDecl: Parser[PredDecl] =  taskId~"->"~taskId ^^ {
    case task~"->"~task2 => PredDecl(task, List(task2))
  }
  def nPrecedDecl: Parser[PredDecl] = taskId~"->"~"("~listTaskId~")" ^^ {
    case task~"->"~"("~tasklist~")" => PredDecl((task,tasklist))
  }
  def listTaskId: Parser[List[String]] =  rep1sep(taskId, ",")

  def taskDecl: Parser[TaskDecl] = taskId~"("~wholeNumber~","~wholeNumber~","~wholeNumber~optTaskParam~")" ^^ {
    case name~"("~c~","~d~","~t~o~")" => TaskDecl(Task(name,c.toInt,d.toInt,t.toInt,o.toInt,None))
  }

  def optTaskParam: Parser[Int] = opt(","~>wholeNumber) ^^ {
    case None => 0
    case Some(number) => number.toInt
  }
  def comment: Parser[Comment] = """/\\*(.|\\n|\\r)*\\*/""".r ^^ Comment

  /**
    * Find task by name
    * @param taskset task set
    * @param name task name
    * @return
    */
  private def taskByName(taskset: Seq[Task], name: String): Option[Task] = {
    taskset.collectFirst { case t if t.id == name => t}
  }

  private def taskSetFromParsed(parsed: List[Stmt]): TaskSet = {

    /* Partition the statements between precedences and task declaration*/
    val (preds,tasks) = parsed.foldLeft(List.empty[(String,List[String])], List.empty[Task]){
      case ((acc1, acc2),p) => p match {
        case TaskDecl(task) => (acc1, acc2.::(task))
        case PredDecl(pred) => (acc1.::(pred), acc2)
        case Comment(comment) => (acc1, acc2)
      }
    }

    /* Transform precedence constraints (String,List[String]) to Map[Task, Set[Task]] */
    val predsRel: Map[Task, Set[Task]] = preds.groupBy(_._1).map{
      case (k,v) =>
        (taskByName(tasks, k).getOrElse(throw new NoSuchElementException),v.flatMap{
          case (k2, v2) => v2
        }.map(taskByName(tasks, _).getOrElse(throw new NoSuchElementException)).toSet)
    }
    val taskSuccsOpt = if(predsRel.isEmpty) None else Some(predsRel)
    TaskSet(tasks, taskSuccsOpt)
  }

  /**
    * Generates taskSet from resource
    *
    * @param resourceName name of taskSet in resources directory
    * @return
    */
  def taskSetFromResource(resourceName: String): TaskSet = {
    val resource = Option(getClass.getResource(resourceName).getFile)
    if(resource.isEmpty) throw new FileNotFoundException(s"Resource $resourceName not found")
    val stream = getClass.getResourceAsStream(resourceName)
    val s = scala.io.Source.fromInputStream(stream).mkString
    val parsed: List[Stmt] = parse(taskSetDecl, s).getOrElse(List.empty[Stmt])
    taskSetFromParsed(parsed)
  }
  /**
    * Generates taskSet from string input
    * @param input string input
    * @return
    */
  def taskSetFromString(input: String): TaskSet = {
    val parsed = parse(taskSetDecl, input).getOrElse(List.empty[Stmt])
    taskSetFromParsed(parsed)
  }
}
