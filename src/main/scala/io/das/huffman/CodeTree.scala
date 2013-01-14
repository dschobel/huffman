package io.das.huffman

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

/**
 * A code tree is a binary tree consisting of Forks and Leafs
 */
trait CodeTree{
  def weight:Int
  def chars:Set[Char]

  /**
   * Decode a sequence of bits using the provided code tree
   * @param bits the bit stream
   * @return a sequence of characters representing the decoded bits
   */
  def decode(bits: List[Int]): List[Char] = {
    /**
     * Descends the code tree, chomping off bits until we hit a leaf
     * @param tree the code tree
     * @param bits the sequence of bits
     * @return the character we produced and the remaining bits
     */
    def descend(tree: CodeTree, bits: List[Int]): (Char, List[Int]) = tree match {
      case Fork(left, right) => descend(if (bits.head == 0) left else right, bits.tail)
      case Leaf(char, weight) => (char, bits) //we found a leaf node and we're done descending
    }

    /**
     * control flow method, allowing us to recur until the bit stream is empty
     * @param bits the sequence of bits to process
     * @param acc the result accumulator
     * @return the result sequence of characters
     */
    def loop(bits: List[Int], acc: List[Char] = Nil): List[Char] = {
      if (bits.isEmpty)
        acc
      else {
        val (c, remainingBits) = descend(this, bits)
        loop(remainingBits, acc ::: List(c))
      }
    }

    loop(bits)
  }

  /**
   * Encodes a character sequence using the provided code tree
   * @param text the character sequence to be encoded
   * @param acc the result accumulator
   * @return the sequence of bits representing the encoded text
   */
  def encode(text: Iterable[Char], acc: List[Int] = Nil): List[Int]={

    /**
     * Find the bit encoding for a single character
     * @param char the character to encode
     * @param node a node of a code tree
     * @param acc the result accumulator
     * @return a sequence of bits representing the encoding of the character
     */
    def getCode(char: Char, node: CodeTree, acc: List[Int] = Nil): List[Int] = node match{
      case Leaf(_,_) => acc
      case Fork(left,right) => {
        if (left.chars.contains(char))
          getCode(char,left, acc ::: List(0))
        else
          getCode(char,right, acc ::: List(1))
      }
    }

    if (text.isEmpty)
      acc
    else{
      val bits = getCode(text.head,this)
      encode(text.tail, acc ::: bits)
    }
  }
}

object CodeTree{
  /**
   * Count the frequency of characters in a characters sequence
   * @param text source of characters
   * @return a mapping of characters to frequency counts
   */
  def countFrequency(text: Iterable[Char]): Map[Char,Int]= text.foldLeft(Map.empty[Char,Int])((map,char) => map + (char -> (map.getOrElse(char,0) + 1)))
  /**
   * Generate a code tree reflecting the character frequencies in a source of characters
   * @param text the character source
   * @return a Code Tree
   */
  def makeCodeTreeFromText(text: Iterable[Char]): CodeTree = makeCodeTreeFromFreqs(countFrequency(text))

  /**
   * Repeatedly apply 'fx' until condition 'done' is met
   * @param data list of values to transform
   * @param done a predicate on the list of values to indicate when to terminate
   * @param fx the function to transform the list of values between rounds
   * @tparam T the type of the values in list of values
   * @return the transformed list of values
   */
  def processUntil[T](data: List[T])(done: List[T] => Boolean)(fx: List[T] => List[T]):List[T]={
    if (!done(data)){
      processUntil(fx(data))(done)(fx)
    }
    else{
      data
    }
  }


  /**
   * For code tree lists with more than one elements, combine the first two elements into a fork and
   * reinsert the fork into list based at the correct position
   * @param xs the list of code trees
   * @return return a list containing the new fork
   *         otherwise return the original list if the original had only one element
   */
  def combineCodeTrees(xs: List[CodeTree]): List[CodeTree]= xs match{
    case Nil => Nil
    case x :: Nil => xs
    case x :: y :: xs1 => {
      val  f = Fork(x,y)//combine two least weighted nodes
      val (less, greater) = xs1.span(_.weight < f.weight) // partition remaining leaves
      // around the new fork's weight
      //combine results
      less ::: List(f) ::: greater
    }
  }

  /**
   * Create a code tree from a character frequency map
   * @param frequencyMap a map of characters to their frequencies
   * @return a code tree reflecting the character frequency map
   */
  def makeCodeTreeFromFreqs(frequencyMap: Map[Char,Int]): CodeTree ={
    val charsByAscendingFreq = frequencyMap.toList.sortBy({case(c,f) => f}) //sort by ascending frequency
    val leaves = charsByAscendingFreq.map{case(c,f) => Leaf(c,f)}           //build a list of leaves

    processUntil[CodeTree](leaves)(_.length == 1)(combineCodeTrees).head //reduce the list into a tree structure
                                                                         //by repeatedly calling combineCodeTrees
  }
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

