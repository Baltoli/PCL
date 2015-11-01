package uk.ac.cam.bsc28.diss.VM

import scala.collection.mutable

class ArithmeticStack {
  val stack: mutable.Stack[Long] = mutable.Stack()

  def push(v: Long): Unit = {
    stack synchronized {
      stack push v
    }
  }

  def binaryLong(f: (Long, Long) => Long): Long = {
    val a = stack pop()
    val b = stack pop()
    f(a, b)
  }

  def binaryBool(f: (Long, Long) => Boolean): Unit = {
    val a = stack pop()
    val b = stack pop()
    stack push(if(f(a, b)) 1 else 0)
  }

  def unaryBool(f: Long => Boolean): Unit = {
    val a = stack pop()
    stack push(if(f(a)) 1 else 0)
  }

  def operate(instruction: StackOperator): Unit = {
    stack synchronized {
      instruction match {
        case Push(v) => this push v
        case Add() => binaryLong(_ + _)
        case Subtract() => binaryLong(_ - _)
        case Multiply() => binaryLong(_ * _)
        case Divide() => binaryLong(_ / _)
        case CompareEqual() => binaryBool(_ == _)
        case CompareZero() => unaryBool(_ == 0)
        case CompareNames(n, m) => stack push(if(n == m) 1 else 0)
      }
    }
  }
}