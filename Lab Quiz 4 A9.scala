package lab_quiz_4


/////////  Q1 (40 points; 10 points per part): Study the following code to answer the question below         ///////////

class BinaryTreeNode[A](var value: A, var left: BinaryTreeNode[A], var right: BinaryTreeNode[A]) {

  def preOrderTraversal[A](node: BinaryTreeNode[A], f: A => Unit): Unit = {
    if (node != null) {
      f(node.value)
      preOrderTraversal(node.left, f)
      preOrderTraversal(node.right, f)
    }
  }

  def inOrderTraversal[A](node: BinaryTreeNode[A], f: A => Unit): Unit = {
    if (node != null) {
      inOrderTraversal(node.left, f)
      f(node.value)
      inOrderTraversal(node.right, f)
    }
  }

  def postOrderTraversal[A](node: BinaryTreeNode[A], f: A => Unit): Unit = {
    if (node != null) {
      postOrderTraversal(node.left, f)
      postOrderTraversal(node.right, f)
      f(node.value)
    }
  }

  def q1(): Unit = {
    val root = new BinaryTreeNode[Int](17, null, null)
    root.right = new BinaryTreeNode[Int](14, null, null)
    root.right.left = new BinaryTreeNode[Int](8, null, null)
    root.left = new BinaryTreeNode[Int](-3, null, null)
    root.left.right = new BinaryTreeNode[Int](16, null, null)
    root.right.right = new BinaryTreeNode[Int](-4, null, null)
    root.right.right.right = new BinaryTreeNode[Int](-3, null, null)
    root.right.right.right.left = new BinaryTreeNode[Int](-11, null, null)
    root.right.right.right.left.right = new BinaryTreeNode[Int](20, null, null)

    preOrderTraversal(root, println)
    println
    inOrderTraversal(root, println)
    println
    postOrderTraversal(root, println)
  }
}

// a). Draw the tree created by running q1()

// b). Write the pre-order traversal of the tree (preOrderTraversal(root, println))

// c). Write the in-order traversal of the tree (inOrderTraversal(root, println))

// d). Write the post-order traversal of the tree (postOrderTraversal(root, println))




///////////  Q2 (20 points): Study the following code to answer the question below  ////////////////////////////////////
class BinarySearchTree[A](comparator: (A, A) => Boolean) {

  var root: BinaryTreeNode[A] = null

  def insert(a: A): Unit = {
    if (this.root == null) {
      this.root = new BinaryTreeNode(a, null, null)
    } else {
      insertHelper(a, this.root)
    }
  }

  def insertHelper(a: A, node: BinaryTreeNode[A]): Unit = {
    if (comparator(node.value, a)) {
      if (node.right == null) {
        node.right = new BinaryTreeNode[A](a, null, null)
      } else {
        insertHelper(a, node.right)
      }
    } else {
      if (node.left == null) {
        node.left = new BinaryTreeNode[A](a, null, null)
      } else {
        insertHelper(a, node.left)
      }
    }
  }


  def q2(): Unit = {

    val comp = (a: Int, b: Int) => a < b

    val bst = new BinarySearchTree[Int](comp)

    bst.insert(10)
    bst.insert(1)
    bst.insert(11)
    bst.insert(-6)
    bst.insert(8)
    bst.insert(19)
    bst.insert(13)
    bst.insert(-7)
  }
}


// Draw the binary search tree created when q2() is called.



/////////////////          Q3 (20 points):                                                     /////////////////////////

/**
  * Write the following infix expression using postfix notation
  *
  * 17 - 38 + (3 + 1 * 11) - 20 / 2
  */



////////////////    Q4 (20 points): Study the following code to answer the question below  /////////////////////////////

class BinaryTreeNode_[A](var value: A, var left: BinaryTreeNode_[A], var right: BinaryTreeNode_[A]) {

  def compute(func: (Int, A, Int) => Int): Int = {
    val leftResult = if (this.left != null) this.left.compute(func) else 0
    val rightResult = if (this.right != null) this.right.compute(func) else 2
    func(leftResult, this.value, rightResult)
  }


  def q4(): Unit = {
    val root = new BinaryTreeNode_[Int](10, null, null)
    root.left = new BinaryTreeNode_[Int](2, null, null)
    root.right = new BinaryTreeNode_[Int](-8, null, null)
    root.left.right = new BinaryTreeNode_[Int](16, null, null)
    root.right.left = new BinaryTreeNode_[Int](4, null, null)
    root.right.right = new BinaryTreeNode_[Int](6, null, null)

    val customFunction = (a: Int, b: Int, c: Int) => a / 2 * b - c - 2

    println("Final Answer: " + root.compute(customFunction))
  }

}


//What is printed by the last line of q4()?



