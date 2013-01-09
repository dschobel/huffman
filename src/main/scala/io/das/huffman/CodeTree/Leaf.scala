package io.das.huffman.CodeTree


/**
 * User: daniel
 * Date: 7/01/13
 * Time: 9:16 AM
 */
case class Leaf(char: Char, weight: Int) extends CodeTree
{
  def chars = Set(char)
}