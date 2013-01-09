package io.das.huffman.CodeTree


abstract class CodeTree{
  def weight:Int
  def chars:Set[Char]
}

object CodeTree{
  def print(root: CodeTree):String= root match{
    case Fork(left,right, chars) => " (" + CodeTree.print(left) + ", " + CodeTree.print(right) + ") "
    case Leaf(char,weight) => "(%s,%d)".format(char,weight)
  }
}
