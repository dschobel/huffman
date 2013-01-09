package io.das.huffman

case class Leaf(char: Char, weight: Int) extends CodeTree
{
  def chars = Set(char)
}