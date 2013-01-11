package io.das.huffman

/**
 * A code tree is a binary tree consisting of Forks and Leafs
 */
abstract class CodeTree{
  def weight:Int
  def chars:Set[Char]
}

object CodeTree{
  /**
   * Performs a traversal of the code tree, accumulating a string representation
   * @param root the root node of the code tree from which to descend
   * @return a string representation of the code tree
   */
  def print(root: CodeTree):String= root match{
    case Fork(left,right) => " (" + CodeTree.print(left) + ", " + CodeTree.print(right) + ") "
    case Leaf(char,weight) => "(%s,%d)".format(char,weight)
  }
}

/**
 * A fork node in the code tree
 * @param left the left child
 * @param right the right child
 */
case class Fork(left: CodeTree, right: CodeTree) extends CodeTree{
  def weight: Int = left.weight + right.weight
  def chars: Set[Char] = left.chars ++ right.chars
}

/**
 * A leaf node
 * @param char the character represented by this leaf node
 * @param weight the weight of the character
 */
case class Leaf(char: Char, weight: Int) extends CodeTree { def chars = Set(char) }
