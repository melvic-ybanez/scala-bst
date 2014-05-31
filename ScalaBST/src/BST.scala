import scala.collection.immutable.Queue

trait BST[+A] {
  def +[B >: A <: Ordered[B]](elem: B): BST[B]  
  def ++[B >: A <: Ordered[B]](bst: BST[B]): BST[B]
  def -[B >: A <: Ordered[B]](elem: B): (Option[A], BST[B])
  
  def exists(p: A => Boolean): Boolean
  def contains[B >: A <: Ordered[B]](e: B): Boolean
  def filter[B >: A <: Ordered[B]](p: A => Boolean): BST[B] = filterAcc[B](EmptyBST)(p)
  def filterAcc[B >: A <: Ordered[B]](acc: BST[B])(p: A => Boolean): BST[B]
  
  def flatMap[B <: Ordered[B]](f: A => BST[B]): BST[B]
  def map[B <: Ordered[B]](f: A => B): BST[B]
  
  def inOrder[B](z: B)(f: (A, B) => B): B
  def preOrder[B](z: B)(f: (A, B) => B): B
  def postOrder[B](z: B)(f: (A, B) => B): B
  def levelOrder[B](z: B)(f: (A, B) => B): B
  
  def withLeft[B >: A <: Ordered[B]](newLeft: BST[B]): BST[B]
  def withRight[B >: A <: Ordered[B]](newRight: BST[B]): BST[B]  
  def orElse[B >: A <: Ordered[B]](tree: BST[B]): BST[B]
  def minChild[B >: A <: Ordered[B]]: BST[B] = minChildAcc[B](this)
  def minChildAcc[B >: A <: Ordered[B]](acc: BST[B]): BST[B]
  
  def toList = preOrder(List[A]())(_ :: _).reverse
}

case object EmptyBST extends BST[Nothing] {
  def +[B <: Ordered[B]](elem: B) = BST(elem)
  def ++[B](bst: BST[B]) = bst 
  def -[B](elem: B) = (None, EmptyBST)
  
  def flatMap[B <: Ordered[B]](f: Nothing => BST[B]): BST[B] = EmptyBST    
  def map[B <: Ordered[B]](f: Nothing => B): BST[B] = EmptyBST 
  
  def exists(p: Nothing => Boolean) = false
  def contains[B <: Ordered[B]](e: B) = false
  def filterAcc[B <: Ordered[B]](acc: BST[B])(p: Nothing => Boolean) = acc
  
  def inOrder[B](z: B)(f: (Nothing, B) => B) = z
  def preOrder[B](z: B)(f: (Nothing, B) => B) = z
  def postOrder[B](z: B)(f: (Nothing, B) => B) = z
  def levelOrder[B](z: B)(f: (Nothing, B) => B) = z
  
  def withLeft[B <: Ordered[B]](newLeft: BST[B]) = newLeft
  def withRight[B <: Ordered[B]](newRight: BST[B]) = newRight
  def orElse[B <: Ordered[B]](tree: BST[B]) = tree
  def minChildAcc[B <: Ordered[B]](acc: BST[B]) = acc
}

case class NonEmptyBST[A <: Ordered[A]](elem: A, left: BST[A], right: BST[A]) extends BST[A] {
  def +[B >: A <: Ordered[B]](newElem: B) = 
    if (newElem < elem) withLeft(left + newElem)
    else if (newElem > elem) withRight(right + newElem)
    else this
    
  def ++[B >: A <: Ordered[B]](bst: BST[B]) = bst.preOrder[BST[B]](this)((e, acc) => acc + e)
  
  def -[B >: A <: Ordered[B]](e: B) =     
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
    
  def exists(p: A => Boolean) = p(elem) || left.exists(p) || right.exists(p)
  def contains[B >: A <: Ordered[B]](e: B) = exists(_ == e)
  def filterAcc[B >: A <: Ordered[B]](acc: BST[B])(p: A => Boolean) = 
    right.filterAcc(left.filterAcc(if (p(elem)) acc + elem else acc)(p))(p)
    
  def flatMap[B <: Ordered[B]](f: A => BST[B]) = preOrder(f(elem))((e, acc) => acc ++ f(e))
  def map[B <: Ordered[B]](f: A => B) = preOrder[BST[B]](BST(f(elem)))((e, acc) => acc + f(elem))
  
  def inOrder[B](z: B)(f: (A, B) => B) = 
    right.inOrder(f(elem, left.inOrder(z)(f)))(f)
  def preOrder[B](z: B)(f: (A, B) => B) = 
    right.preOrder(left.preOrder(f(elem, z))(f))(f)    
  def postOrder[B](z: B)(f: (A, B) => B) = 
    f(elem, right.postOrder(left.postOrder(z)(f))(f))
  
  def levelOrder[B](z: B)(f: (A, B) => B) = {
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
  
  def withLeft[B >: A <: Ordered[B]](newLeft: BST[B]) = NonEmptyBST(elem, newLeft, right)
  def withRight[B >: A <: Ordered[B]](newRight: BST[B]) = NonEmptyBST(elem, left, newRight)
  def minChildAcc[B >: A <: Ordered[B]](acc: BST[B]) = left.minChildAcc(this)
  def orElse[B >: A <: Ordered[B]](tree: BST[B]) = this
}

object BST {
  def apply[A <: Ordered[A]]() = EmptyBST
  def apply[A <: Ordered[A]](elem: A) = NonEmptyBST(elem, EmptyBST, EmptyBST)
}