import scala.collection.immutable.Queue

trait BSTElem[+A] {
  def key: Int
  def value: A
}

trait BST[+A] { 
  implicit def elemToInt[T](elem: BSTElem[T]) = elem.key
  
  def +[B >: A](elem: BSTElem[B]): BST[B]  
  def ++[B >: A](bst: BST[B]): BST[B]
  def -[B >: A](elem: BSTElem[B]): (Option[BSTElem[A]], BST[B])    
  
  def exists(p: BSTElem[A] => Boolean): Boolean
  def contains[B >: A](e: BSTElem[B]): Boolean
  def filter(p: BSTElem[A] => Boolean): BST[A] = filterAcc(EmptyBST)(p)
  def filterAcc[B >: A](acc: BST[B])(p: BSTElem[A] => Boolean): BST[B]
  
  def flatMap[B](f: BSTElem[A] => BST[B]): BST[B]
  def map[B](f: BSTElem[A] => BSTElem[B]): BST[B]
  
  def inOrder[B](z: B)(f: (BSTElem[A], B) => B): B
  def preOrder[B](z: B)(f: (BSTElem[A], B) => B): B
  def postOrder[B](z: B)(f: (BSTElem[A], B) => B): B
  def levelOrder[B](z: B)(f: (BSTElem[A], B) => B): B
  
  def withLeft[B >: A](newLeft: BST[B]): BST[B]
  def withRight[B >: A](newRight: BST[B]): BST[B]  
  def orElse[B >: A](tree: BST[B]): BST[B]
  def minChild[B >: A] = minChildAcc(this)
  def minChildAcc[B >: A](acc: BST[B]): BST[B]
  
  def toList = preOrder(List[BSTElem[A]]())(_ :: _).reverse
}

case object EmptyBST extends BST[Nothing] {
  def +[B](elem: BSTElem[B]) = BST(elem)
  def ++[B](bst: BST[B]) = bst 
  def -[B](elem: BSTElem[B]) = (None, EmptyBST)  
  
  def flatMap[B](f: BSTElem[Nothing] => BST[B]): BST[B] = EmptyBST    
  def map[B](f: BSTElem[Nothing] => BSTElem[B]): BST[B] = EmptyBST 
  
  def exists(p: BSTElem[Nothing] => Boolean) = false
  def contains[B](e: BSTElem[B]) = false
  def filterAcc[B](acc: BST[B])(p: BSTElem[Nothing] => Boolean) = acc
  
  def inOrder[B](z: B)(f: (BSTElem[Nothing], B) => B) = z
  def preOrder[B](z: B)(f: (BSTElem[Nothing], B) => B) = z
  def postOrder[B](z: B)(f: (BSTElem[Nothing], B) => B) = z
  def levelOrder[B](z: B)(f: (BSTElem[Nothing], B) => B) = z
  
  def withLeft[B](newLeft: BST[B]) = newLeft
  def withRight[B](newRight: BST[B]) = newRight
  def orElse[B](tree: BST[B]) = tree
  def minChildAcc[B](acc: BST[B]) = acc
}

case class NonEmptyBST[A](elem: BSTElem[A], left: BST[A], right: BST[A]) extends BST[A] {
  def +[B >: A](newElem: BSTElem[B]) = 
    if (newElem < elem) withLeft(left + newElem)
    else if (newElem > elem) withRight(right + newElem)
    else this
    
  def ++[B >: A](bst: BST[B]) = bst.preOrder[BST[B]](this)((e, acc) => acc + e)
  
  def -[B >: A](e: BSTElem[B]) =     
    if (e < elem) (left - e) match {
      case (optElem, bst) => (optElem, withLeft(bst))
    } else if (e > elem) (right - e) match {
      case (optElem, bst) => (optElem, withRight(bst))
    } else (Some(elem), (left, right) match {
      case (EmptyBST, EmptyBST) => EmptyBST
      case (EmptyBST, tree) => tree
      case (tree, EmptyBST) => tree
      case (left, tree) => tree.minChild.orElse(tree).withLeft(left)
    }) 
    
  def exists(p: BSTElem[A] => Boolean) = p(elem) || left.exists(p) || right.exists(p)
  def contains[B >: A](e: BSTElem[B]) = exists(_ == e)
  def filterAcc[B >: A](acc: BST[B])(p: BSTElem[A] => Boolean) = 
    right.filterAcc(left.filterAcc(if (p(elem)) acc + elem else acc)(p))(p)
    
  def flatMap[B](f: BSTElem[A] => BST[B]) = preOrder(f(elem))((e, acc) => acc ++ f(e))
  def map[B](f: BSTElem[A] => BSTElem[B]) = preOrder[BST[B]](BST(f(elem)))((e, acc) => acc + f(elem))
  
  def inOrder[B](z: B)(f: (BSTElem[A], B) => B) = 
    right.inOrder(f(elem, left.inOrder(z)(f)))(f)
  def preOrder[B](z: B)(f: (BSTElem[A], B) => B) = 
    right.preOrder(left.preOrder(f(elem, z))(f))(f)    
  def postOrder[B](z: B)(f: (BSTElem[A], B) => B) = 
    f(elem, right.postOrder(left.postOrder(z)(f))(f))
  
  def levelOrder[B](z: B)(f: (BSTElem[A], B) => B) = {
    def levelOrder(acc: B, queue: Queue[BST[A]]): B = 
      queue match {
        case Queue() => acc
        case h +: t => h match {
          case EmptyBST => levelOrder(acc, t)
          case NonEmptyBST(e, l, r) => levelOrder(f(elem, z), queue.enqueue(l).enqueue(r))
        }
      }  
    
    levelOrder(z, Queue(this))
  }
  
  def withLeft[B >: A](newLeft: BST[B]) = NonEmptyBST(elem, newLeft, right)
  def withRight[B >: A](newRight: BST[B]) = NonEmptyBST(elem, left, newRight)
  def minChildAcc[B >: A](acc: BST[B]) = left.minChildAcc(this)
  def orElse[B >: A](tree: BST[B]) = this
}

object BST {
  def apply[A](elem: BSTElem[A]) = NonEmptyBST(elem, EmptyBST, EmptyBST)
}