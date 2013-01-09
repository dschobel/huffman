package io.das.huffman

case class Fork(left: CodeTree, right: CodeTree) extends CodeTree{
  def weight: Int = left.weight + right.weight
  def chars: Set[Char] = left.chars ++ right.chars
}
