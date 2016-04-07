package com.rea.higherorder

object ComposingExercises {


  def add(a: Int, b: Int) = a + b

  // We want to partially apply add
  def addTwo: Int => Int = a => add(2, a)

  def subtract(a: Int, b: Int) = a - b

  // We want to partially apply subtract
  def minusThree: Int => Int = a => subtract(a, 3)

  // How do we create a new function from addTwo and minusThree?
  val addTwoMinusThree: Int => Int = a => minusThree(addTwo(a))
  
  def prettyPrint(i:Int) = s"The number is $i"
  
  val addTwoMinusThreePrettyPrint: Int => String = a => prettyPrint(addTwoMinusThree(a))

  // How do we convert (compose) f and g into a brand new function?
  def compose_[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}


