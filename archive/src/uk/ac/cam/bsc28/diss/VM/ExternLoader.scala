package uk.ac.cam.bsc28.diss.VM

import java.net.{URI, URLClassLoader}

import uk.ac.cam.bsc28.diss.VM.ExternLoader.{DataType, ChannelCallable}

import scala.reflect.runtime.{universe=>ru}

object ExternLoader {
  private type DataType = Either[String, Long]

  type ClassType = ru.ClassMirror

  trait ChannelCallable {
    def receive(data: DataType)
    def send(): DataType
  }
}

class ExternLoader {

  private val pathURL = new URI("file://" + "/usr/local/lib/picl/").toURL // TODO: configure
  private val classLoader = new URLClassLoader(Array(pathURL))
  private val mirror = ru.runtimeMirror(classLoader)

  private def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  private class ChannelCallableImpl(r: ru.MethodMirror, s: ru.MethodMirror) extends ChannelCallable {

    def receive(data: DataType) = {
      r(data)
    }

    def send(): DataType = {
      s().asInstanceOf[DataType]
    }

  }

  def loadClassNamed(n: String): ExternLoader.ClassType = {
    val stdio = mirror.staticClass(n)
    val claz = mirror.reflectClass(stdio)

    claz
  }

  def newInstance(claz: ExternLoader.ClassType): ChannelCallable = {
    val ctor = claz.reflectConstructor(claz.symbol.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod)
    val inst = ctor()

    val recSym = claz.symbol.typeSignature.member(ru.newTermName("receive")).asMethod
    val sendSym = claz.symbol.typeSignature.member(ru.newTermName("send")).asMethod

    val instMirror = mirror.reflect(inst)

    val send = instMirror.reflectMethod(sendSym)
    val receive = instMirror.reflectMethod(recSym)

    new ChannelCallableImpl(receive, send)
  }

}
