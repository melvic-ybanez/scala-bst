object Main {
  def main(args: Array[String]) {
    val bst = BST() + 6 + 4 + 2 + 1 + 3 ++ BST(5, 8, 7, 10, 9)
    println("contains 5: " + bst.contains(5))
    println("contains 11: " + bst.contains(11))
    println("exists 5: " + bst.exists(_ == 5))
    println("exists 11: " + bst.exists(_ == 11))
    println("toString: " + bst)
    println("filter 2: " + bst.filter(_ % 2 == 0).toList)
    println("map: " + bst.map(_ + 1).toList)
    println("flatMap: " + bst.flatMap(x => BST(x + 1)).toList)
    println()
    println("Traversals...")
    printTraversals(bst)
    
    val (opt, bst1) = bst - 6
    println()
    println("Remove 6: " + opt.getOrElse("Not found"))
    println("Rest of bst...")
    printTraversals(bst1)
  }
  
  def printTraversals[T](bst: BST[T]) = {
    println("preOrder: " + bst.preOrder(List[T]())(_ :: _).reverse)
    println("postOrder: " + bst.postOrder(List[T]())(_ :: _).reverse)
    println("inOrder: " + bst.inOrder(List[T]())(_ :: _).reverse)
    println("levelOrder: " + bst.levelOrder(List[T]())(_ :: _).reverse)
  }
}