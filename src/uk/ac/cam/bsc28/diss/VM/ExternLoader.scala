package uk.ac.cam.bsc28.diss.VM

import java.net.{URI, URLClassLoader}
import com.sun.prism.PixelFormat.DataType

import scala.reflect.runtime.{universe=>ru}

class ExternLoader {

  private def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  private type DataType = Either[String, Long]

  trait ChannelCallable {
    def receive(data: DataType)
    def send(): DataType
  }

  private class ChannelCallableImpl(r: ru.MethodMirror, s: ru.MethodMirror) extends ChannelCallable {

    def receive(data: DataType) = {
      r(data)
    }

    def send(): DataType = {
      s().asInstanceOf[DataType]
    }

  }

  def loadClassNamed(n: String): ChannelCallable = {
    val pathURL = new URI("file://" + getClass.getResource("").getPath).toURL
    val classLoader = new URLClassLoader(Array(pathURL))
    val mirror = ru.runtimeMirror(classLoader)
    val stdio = mirror.staticClass(n)
    val claz = mirror.reflectClass(stdio)
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
