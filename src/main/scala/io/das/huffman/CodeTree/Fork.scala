package io.das.huffman.CodeTree


case class Fork(left: CodeTree, right: CodeTree, chars: Set[Char]) extends CodeTree{
  def weight: Int = left.weight + right.weight
}
