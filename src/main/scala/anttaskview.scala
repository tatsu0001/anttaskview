package tools.anttask.view

import java.lang.reflect.Method
import java.lang.reflect.Modifier

abstract class ViewType
case class Task() extends ViewType
case class Class(taskName: String) extends ViewType

trait MethodViewable {
  def accept(method: Method): Boolean = {
    true
  }
}

trait NotNative extends MethodViewable {
  override def accept(method: Method): Boolean = {
    !Modifier.isNative(method.getModifiers)
  }
}

abstract class ViewScope extends NotNative
case class Public() extends ViewScope {
  override def accept(method: Method): Boolean = {
    super.accept(method) && Modifier.isPublic(method.getModifiers)
  }
}
case class Protected() extends ViewScope {
  override def accept(method: Method): Boolean = {
    super.accept(method) && (Public().accept(method) || Modifier.isProtected(method.getModifiers))
  }
}
case class Private() extends ViewScope

object ViewScope {
  def apply(label: String) = {
    label match {
      case "public" => Public()
      case "protected" => Protected()
      case "private" => Private()
      case _ => Public()
    }
  }
}

case class AntTaskView(
      viewType: ViewType = Task(),
      viewScope: ViewScope = Public()
      )

  object AntTaskView {
    import scopt._
      import java.lang.ClassLoader
      import java.util.Properties
      import scala.collection.JavaConverters._
      import scala.collection.SortedMap

      private val cmdName = "anttaskhelp"

      def loadAntTaskProperties() = {
        val properties = new Properties
          properties.load(ClassLoader.getSystemClassLoader.getResourceAsStream("org/apache/tools/ant/taskdefs/defaults.properties"))
          SortedMap(properties.asScala.toList:_*)
      }

    def createOptionParser(): OptionParser[AntTaskView] = {
      new OptionParser[AntTaskView](cmdName) {
        head(cmdName, "0.1")
          opt[String]('t', "task").optional.action { (t, opt) => opt.copy(viewType = Class(t)) }
          opt[String]("scope").optional.action { 
            (s, opt) => { opt.copy(viewScope = ViewScope(s)) }
          }
      }
    }

    def main(args: Array[String]): Unit = {
      createOptionParser.parse(args, AntTaskView()).map {
        opt => {
          val antTaskProp = loadAntTaskProperties
            opt.viewType match {
              case Task() => {
                antTaskProp.foreach { 
                  _ match { case (k, v) => println(k + " - " + v) }
                }
              }
              case Class(taskName) => {
                antTaskProp.get(taskName).map {
                  className =>
                    java.lang.Class.forName(className)
                      .getMethods
                      .filter(opt.viewScope.accept(_))
                      .sortBy(_.toString)
                      .foreach(println)
                }
              }
            }
        }
      }
    }
  }
