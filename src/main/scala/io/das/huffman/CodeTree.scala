package io.das.huffman

abstract class CodeTree{
  def weight:Int
  def chars:Set[Char]
}

object CodeTree{
  def print(root: CodeTree):String= root match{
    case Fork(left,right) => " (" + CodeTree.print(left) + ", " + CodeTree.print(right) + ") "
    case Leaf(char,weight) => "(%s,%d)".format(char,weight)
  }
}
